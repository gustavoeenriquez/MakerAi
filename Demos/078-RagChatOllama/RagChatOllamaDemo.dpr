program RagChatOllamaDemo;

// =============================================================================
// DEMO 078 - RAG completo 100% local: pgvector + Ollama (embeddings y chat)
// =============================================================================
// El demo 077 llega hasta recuperar los fragmentos relevantes. Este cierra el
// ciclo: recupera, arma el contexto y se lo pasa a un LLM local para que
// responda. Sin API keys, sin salir de la maquina.
//
// El demo esta construido para que se VEA por que hace falta el RAG: cada
// pregunta se hace DOS veces contra el mismo modelo.
//
//   SIN contexto : el modelo no tiene forma de conocer un catalogo privado, y
//                  responde igual: inventa titulos, directores y anios con
//                  aplomo. Es la alucinacion en estado puro.
//   CON contexto : se recuperan las fichas relevantes de PostgreSQL y se
//                  inyectan en el system prompt. El modelo pasa a responder
//                  sobre datos reales, y puede citar.
//
// Requiere que la tabla vectorial exista y este poblada: correr antes el
// demo 077 (que crea el esquema, siembra peliculas e indexa).
//
// Configuracion por variables de entorno (mismos defaults que 077):
//   PGHOST localhost   PGPORT 5432   PGDATABASE peliculasdb
//   PGUSER postgres    PGPASSWORD (obligatoria)
//   OLLAMA_URL http://localhost:11434/
//
// Modos de uso:
//   RagChatOllamaDemo.exe
//   RagChatOllamaDemo.exe --chat gpt-oss:20b
//   RagChatOllamaDemo.exe --embed mxbai-embed-large --dim 1024
//   RagChatOllamaDemo.exe --ask "que peliculas de terror hay?"
//   RagChatOllamaDemo.exe --otel
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.JSON,
  System.Diagnostics,
  System.Net.HttpClient,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.DApt,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.Phys,
  FireDAC.Comp.Client,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Ollama,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings.Ollama,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vector.Driver.Postgres,
  uMakerAi.Telemetry;

const
  ENTIDAD = 'PELICULAS_OLLAMA';
  TOP_K = 4;

  // El system prompt es la mitad del trabajo en un RAG. Estas tres reglas son
  // las que evitan el fallo mas comun: que el modelo complete con lo que "sabe"
  // cuando el contexto no alcanza.
  SYSTEM_RAG = 'Eres el asistente de un catalogo de peliculas.' + sLineBreak +
    'Responde UNICAMENTE con la informacion del CATALOGO que viene abajo.' + sLineBreak +
    'Si el catalogo no contiene la respuesta, di exactamente: "No tengo esa informacion en el catalogo".' + sLineBreak +
    'No inventes titulos, directores ni anios. Cita el titulo de las peliculas que uses.' + sLineBreak +
    'Responde en espanol, en 4 lineas como maximo.' + sLineBreak + sLineBreak + 'CATALOGO:' + sLineBreak;

var
  Conn: TFDConnection;
  Driver: TAiRAGVectorPostgresDriver;
  Rag: TAiRAGVector;
  Embedder: TAiOllamaEmbeddings;
  Chat: TAiOllamaChat;
  Telemetry: TAiTelemetry;
  ModeloEmb, ModeloChat, TablaVec, OllamaUrl: string;
  DimEmb: Integer;

  // ---------------------------------------------------------------------------

function Env(const AName, ADefault: string): string;
begin
  Result := GetEnvironmentVariable(AName);
  if Result = '' then
    Result := ADefault;
end;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

function FindLibPq: string;
var
  V: Integer;
  P: string;
begin
  Result := GetEnvironmentVariable('PGLIBPQ');
  if (Result <> '') and FileExists(Result) then
    Exit;
  for V := 18 downto 12 do
  begin
    P := Format('C:\Program Files\PostgreSQL\%d\bin\libpq.dll', [V]);
    if FileExists(P) then
      Exit(P);
  end;
  Result := '';
end;

procedure Separador(const ATexto: string);
begin
  Writeln('');
  Writeln('----------------------------------------------------------------------');
  Writeln(' ' + ATexto);
  Writeln('----------------------------------------------------------------------');
end;

// ---------------------------------------------------------------------------
// Infraestructura
// ---------------------------------------------------------------------------
procedure Preparar;
var
  Link: TFDPhysPgDriverLink;
  LibPq: string;
  Http: THTTPClient;
  Resp: IHTTPResponse;
  Total: Integer;
begin
  Writeln('== Preparacion ==');

  // --- PostgreSQL ---
  Link := TFDPhysPgDriverLink.Create(nil);
  LibPq := FindLibPq;
  if LibPq <> '' then
    Link.VendorLib := LibPq;

  Conn := TFDConnection.Create(nil);
  Conn.DriverName := 'PG';
  Conn.Params.Values['Server'] := Env('PGHOST', 'localhost');
  Conn.Params.Values['Port'] := Env('PGPORT', '5432');
  Conn.Params.Values['Database'] := Env('PGDATABASE', 'peliculasdb');
  Conn.Params.Values['User_Name'] := Env('PGUSER', 'postgres');
  Conn.Params.Values['Password'] := Env('PGPASSWORD', '');
  Conn.Params.Values['CharacterSet'] := 'UTF8';
  Conn.LoginPrompt := False;
  if Conn.Params.Values['Password'] = '' then
    raise Exception.Create('Falta la variable de entorno PGPASSWORD');
  Conn.Connected := True;
  Writeln('   postgres: ' + Conn.Params.Values['Database'] + '@' + Conn.Params.Values['Server']);

  // --- Ollama ---
  Http := THTTPClient.Create;
  try
    Http.ConnectionTimeout := 5000;
    Http.ResponseTimeout := 5000;
    try
      Resp := Http.Get(OllamaUrl.TrimRight(['/']) + '/api/tags');
    except
      on E: Exception do
        raise Exception.Create('Ollama no responde en ' + OllamaUrl + '. Arrancalo con "ollama serve".');
    end;
    if Resp.StatusCode <> 200 then
      raise Exception.CreateFmt('Ollama devolvio HTTP %d', [Resp.StatusCode]);
    if not Resp.ContentAsString(TEncoding.UTF8).Contains(ModeloChat) then
      raise Exception.CreateFmt('El modelo de chat "%s" no esta en Ollama. Descargalo con: ollama pull %s',
        [ModeloChat, ModeloChat]);
  finally
    Http.Free;
  end;
  Writeln('   ollama: embeddings=' + ModeloEmb + ' (dim ' + IntToStr(DimEmb) + ')  chat=' + ModeloChat);

  // --- Indice vectorial (lo crea y puebla el demo 077) ---
  if Conn.ExecSQLScalar('select count(*) from information_schema.tables where table_name = ' + QuotedStr(TablaVec)) = 0
  then
    raise Exception.Create('No existe la tabla ' + TablaVec + '. Corre antes el demo 077-RagPostgresConsole' +
      ' con --model ' + ModeloEmb + ' --dim ' + IntToStr(DimEmb));

  Total := Conn.ExecSQLScalar(Format('select count(*) from %s where entidad = %s', [TablaVec, QuotedStr(ENTIDAD)]));
  if Total = 0 then
    raise Exception.Create('La tabla ' + TablaVec + ' esta vacia. Corre antes el demo 077 para indexar.');
  Writeln(Format('   indice: %s con %d vectores', [TablaVec, Total]));

  // --- Componentes MakerAI ---
  Embedder := TAiOllamaEmbeddings.Create(nil);
  Embedder.Url := OllamaUrl;
  Embedder.Model := ModeloEmb;
  Embedder.Dimensions := DimEmb;

  Driver := TAiRAGVectorPostgresDriver.Create(nil);
  Driver.Connection := Conn;
  Driver.TableName := TablaVec;
  Driver.CurrentEntidad := ENTIDAD;

  Rag := TAiRAGVector.Create(nil);
  Rag.Embeddings := Embedder;
  Rag.Driver := Driver;
  Rag.Entidad := ENTIDAD;
  // Hibrido: los embeddings aportan el sentido y BM25 rescata los terminos
  // exactos (nombres propios, titulos), fusionados con RRF.
  Rag.SearchOptions.UseEmbeddings := True;
  Rag.SearchOptions.UseBM25 := True;
  Rag.SearchOptions.UseRRF := True;

  Chat := TAiOllamaChat.Create(nil);
  // TRAMPA de los modelos de razonamiento en Ollama: num_predict (que el driver
  // toma de Max_tokens) limita TODOS los tokens generados, y el thinking cuenta.
  // Con qwen3.5:4b y el default de 3000, el razonamiento se comia el presupuesto
  // entero y la respuesta llegaba VACIA: sin error, solo content vacio.
  // Para un RAG no interesa la cadena de pensamiento sino la respuesta, asi que
  // se desactiva con think=false (via ModelExtraBodyParams, que el driver mezcla
  // en la raiz del request). Con --think se deja activo, pero entonces hace falta
  // un presupuesto de tokens mucho mayor.
  Chat.Url := OllamaUrl;
  Chat.Model := ModeloChat;
  Chat.Asynchronous := False; // consola: respuesta completa de una vez
  if HasFlag('--think') then
    Chat.Max_tokens := 16000
  else
  begin
    Chat.Max_tokens := 2000;
    Chat.ModelConfig.ModelExtraBodyParams := '{"think": false}';
  end;
end;

// ---------------------------------------------------------------------------
// Recuperacion
// ---------------------------------------------------------------------------
function RecuperarContexto(const APregunta: string; out ACuantos: Integer): string;
var
  Target: TAiEmbeddingNode;
  Res: TAiRAGVector;
begin
  Result := '';
  ACuantos := 0;
  Target := Rag.CreateEmbeddingNode(APregunta);
  try
    Res := Driver.Search(Target, ENTIDAD, TOP_K, 0.0, nil, Rag.SearchOptions);
    try
      if Assigned(Res) then
      begin
        ACuantos := Res.Count;
        // Arma el bloque de texto con los fragmentos y sus metadatos. Los
        // scores se omiten: al modelo no le aportan y gastan tokens.
        Result := Rag.VectorToContextText(Res, True, False);
      end;
    finally
      Res.Free;
    end;
  finally
    Target.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Preguntar al modelo, con y sin contexto
// ---------------------------------------------------------------------------
procedure Preguntar(const APregunta: string);
var
  Contexto, Respuesta: string;
  Cuantos: Integer;
  SW: TStopwatch;
begin
  Separador('PREGUNTA: ' + APregunta);

  // ---------- 1. Sin RAG ----------
  Writeln('');
  Writeln('  [SIN CONTEXTO]  el modelo responde solo con lo que trae de fabrica');
  Chat.Messages.Clear;
  Chat.SystemPrompt.Text := 'Responde en espanol, en 4 lineas como maximo.';
  SW := TStopwatch.StartNew;
  try
    Respuesta := Chat.AddMessageAndRun(APregunta, 'user', []);
  except
    on E: Exception do
      Respuesta := '(error: ' + E.Message + ')';
  end;
  Writeln('  ' + Respuesta.Trim.Replace(sLineBreak, sLineBreak + '  '));
  Writeln(Format('  (%d ms)', [SW.ElapsedMilliseconds]));

  // ---------- 2. Con RAG ----------
  Writeln('');
  Contexto := RecuperarContexto(APregunta, Cuantos);
  Writeln(Format('  [CON CONTEXTO]  %d fichas recuperadas de PostgreSQL e inyectadas en el system prompt', [Cuantos]));
  Chat.Messages.Clear;
  Chat.SystemPrompt.Text := SYSTEM_RAG + Contexto;
  SW := TStopwatch.StartNew;
  try
    Respuesta := Chat.AddMessageAndRun(APregunta, 'user', []);
  except
    on E: Exception do
      Respuesta := '(error: ' + E.Message + ')';
  end;
  Writeln('  ' + Respuesta.Trim.Replace(sLineBreak, sLineBreak + '  '));
  Writeln(Format('  (%d ms)', [SW.ElapsedMilliseconds]));
end;

// ---------------------------------------------------------------------------

var
  Libre: string;

begin
  Telemetry := nil;
  Conn := nil;
  Driver := nil;
  Rag := nil;
  Embedder := nil;
  Chat := nil;
  try
    try
      ModeloChat := ArgValue('--chat', 'qwen3.5:4b');
      // Por defecto nomic-embed-text: en las pruebas del demo 077 se comporto
      // mejor que mxbai-embed-large sobre este corpus en espanol.
      ModeloEmb := ArgValue('--embed', 'nomic-embed-text');
      DimEmb := StrToIntDef(ArgValue('--dim', ''), 0);
      if DimEmb = 0 then
      begin
        if ModeloEmb.StartsWith('nomic-embed-text') then
          DimEmb := 768
        else if ModeloEmb.StartsWith('all-minilm') then
          DimEmb := 384
        else
          DimEmb := 1024;
      end;
      TablaVec := 'pelicula_vector_ollama_' + IntToStr(DimEmb);
      OllamaUrl := Env('OLLAMA_URL', 'http://localhost:11434/');

      if HasFlag('--otel') then
      begin
        Telemetry := TAiTelemetry.Create(nil);
        Telemetry.ServiceName := 'RagChatOllamaDemo';
        Telemetry.Enabled := True;
        Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
      end;

      if HasFlag('--debug') then
      begin
        MakerAiDebugLogEnabled := True;
        MakerAiDebugLogPath := 'demo078.log';
        Writeln('[debug] log en demo078.log');
      end;
      Writeln('=== DEMO 078: RAG completo 100% local (pgvector + Ollama) ===');
      Writeln('');
      Preparar;

      Libre := ArgValue('--ask', '');
      if Libre <> '' then
        Preguntar(Libre)
      else
      begin
        // Preguntas sobre un catalogo PRIVADO: el modelo no puede saberlas.
        // Sin contexto inventara; con contexto respondera con datos reales.
        Preguntar('Que peliculas de terror dirigio Carla Mendez?');
        Preguntar('De que trata Orbita Cero y en que anio se estreno?');
        Preguntar('Recomiendame una pelicula de animacion y explica por que.');
        // Esta NO esta en el catalogo: sirve para comprobar que el modelo
        // admite que no sabe en vez de inventar.
        Preguntar('Quien dirigio El Padrino?');
      end;

      Writeln('');
      Writeln('OK: RAG de extremo a extremo con modelos locales.');
      ExitCode := 0;
    except
      on E: Exception do
      begin
        Writeln('');
        Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Chat.Free;
    Rag.Free;
    Driver.Free;
    Embedder.Free;
    if Assigned(Conn) then
    begin
      Conn.Connected := False;
      Conn.Free;
    end;
    if Assigned(Telemetry) then
    begin
      Telemetry.Flush;
      Telemetry.Free;
    end;
  end;
end.
