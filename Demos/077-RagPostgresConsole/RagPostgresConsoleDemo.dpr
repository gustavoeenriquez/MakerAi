program RagPostgresConsoleDemo;

// =============================================================================
// DEMO 077 - RAG vectorial sobre PostgreSQL + pgvector, desde consola
// =============================================================================
// Los demos de RAG con Postgres (021, 023, 025) son todos graficos, asi que no
// habia forma de ejercitar el stack sin abrir el IDE. Este lo hace headless, y
// con embeddings LOCALES via Ollama: no necesita ninguna API key.
//
// Recorrido:
//   1. Conecta a PostgreSQL y comprueba la extension pgvector.
//   2. Comprueba que Ollama responde y que el modelo de embeddings existe.
//   3. Crea el esquema vectorial si falta (CreateSchema es idempotente:
//      CREATE TABLE/INDEX IF NOT EXISTS, incluido el indice HNSW).
//   4. Siembra peliculas de ejemplo en la tabla 'pelicula' (solo las que
//      falten, comparando por titulo).
//   5. Indexa las peliculas que aun no tienen vector, generando el embedding
//      con Ollama y persistiendo via TAiRAGVectorPostgresDriver.
//   6. Busquedas semanticas, y una con filtro de metadatos (TAiFilterCriteria).
//
// IMPORTANTE sobre dimensiones: la tabla 'pelicula_vector' de esta base usa
// vector(1536) porque se indexo con text-embedding-3-small de OpenAI. Los
// modelos de Ollama producen otra dimension (mxbai-embed-large = 1024,
// nomic-embed-text = 768) y ademas viven en OTRO espacio vectorial, asi que
// mezclarlos en la misma tabla daria resultados sin sentido. Por eso este demo
// usa su propia tabla, con la dimension del modelo que se le indique.
//
// Configuracion por variables de entorno (con valores por defecto):
//   PGHOST      localhost      PGPORT      5432
//   PGDATABASE  peliculasdb    PGUSER      postgres
//   PGPASSWORD  (obligatoria)  PGLIBPQ     ruta a libpq.dll (autodeteccion)
//   OLLAMA_URL  http://localhost:11434/
//
// Modos de uso:
//   RagPostgresConsoleDemo.exe
//   RagPostgresConsoleDemo.exe --model nomic-embed-text --dim 768
//   RagPostgresConsoleDemo.exe --no-seed      (no inserta peliculas nuevas)
//   RagPostgresConsoleDemo.exe --reindex      (borra los vectores y reindexa)
//   RagPostgresConsoleDemo.exe --otel         (trazas OTLP a localhost:4318)
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.JSON,
  System.Net.HttpClient,
  System.Generics.Collections,
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
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings.Ollama,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vector.Driver.Postgres,
  uMakerAi.Telemetry;

const
  ENTIDAD = 'PELICULAS_OLLAMA';

type
  TPeliSeed = record
    Titulo: string;
    Anio: Integer;
    Director: string;
    GeneroId: Integer;
    Sinopsis: string;
  end;

  // Peliculas de ejemplo. Se eligieron sinopsis con vocabulario variado para
  // que la busqueda semantica tenga algo que distinguir: hay pares que
  // comparten genero pero no palabras, y viceversa.
const
  SEMILLAS: array [0 .. 14] of TPeliSeed = (
    (Titulo: 'El Ultimo Tren a Ninguna Parte'; Anio: 2013; Director: 'Irene Vasquez'; GeneroId: 2;
     Sinopsis: 'Una mujer viaja durante tres dias en ferrocarril para despedirse de su padre moribundo.'),
    (Titulo: 'Protocolo Fenix'; Anio: 2021; Director: 'Diego Salazar'; GeneroId: 1;
     Sinopsis: 'Un equipo de operaciones especiales debe recuperar un arma robada antes del amanecer.'),
    (Titulo: 'La Casa de los Relojes'; Anio: 2019; Director: 'Carla Mendez'; GeneroId: 5;
     Sinopsis: 'Una restauradora descubre que cada reloj de la mansion marca la hora de una muerte.'),
    (Titulo: 'Orbita Cero'; Anio: 2024; Director: 'Marco Velez'; GeneroId: 4;
     Sinopsis: 'La tripulacion de una estacion espacial pierde contacto con la Tierra y racionan el oxigeno.'),
    (Titulo: 'Mi Suegra Astronauta'; Anio: 2022; Director: 'Pablo Ortiz'; GeneroId: 3;
     Sinopsis: 'Un yerno torpe debe convivir una semana con la primera mujer que piso Marte.'),
    (Titulo: 'Pequenos Gigantes de Papel'; Anio: 2023; Director: 'Estudio Pixelia'; GeneroId: 6;
     Sinopsis: 'Origamis que cobran vida defienden una biblioteca infantil de la humedad y el olvido.'),
    (Titulo: 'Cicatrices de Sal'; Anio: 2011; Director: 'Sofia Duarte'; GeneroId: 2;
     Sinopsis: 'Dos hermanas pescadoras se reencuentran tras veinte anos en un pueblo costero en ruinas.'),
    (Titulo: 'Codigo Escarlata'; Anio: 2020; Director: 'Diego Salazar'; GeneroId: 1;
     Sinopsis: 'Una analista descubre que el atentado que investiga lo planeo su propio departamento.'),
    (Titulo: 'El Susurro del Pozo'; Anio: 2016; Director: 'Carla Mendez'; GeneroId: 5;
     Sinopsis: 'Un pueblo entero enmudece cada vez que alguien se asoma al pozo del cementerio.'),
    (Titulo: 'Las Lunas de Ithaca'; Anio: 2018; Director: 'Marco Velez'; GeneroId: 4;
     Sinopsis: 'Colonos de una luna helada deciden si regresan a casa o cortan el vinculo para siempre.'),
    (Titulo: 'Divorcio en Tres Actos'; Anio: 2017; Director: 'Pablo Ortiz'; GeneroId: 3;
     Sinopsis: 'Una pareja de actores ensaya su separacion frente al publico cada noche.'),
    (Titulo: 'El Jardin de Engranajes'; Anio: 2025; Director: 'Estudio Pixelia'; GeneroId: 6;
     Sinopsis: 'Una nina construye flores mecanicas para devolverle la primavera a una ciudad gris.'),
    (Titulo: 'Nadie Duerme en Agosto'; Anio: 2012; Director: 'Irene Vasquez'; GeneroId: 2;
     Sinopsis: 'Durante una ola de calor, los vecinos de un edificio descubren los secretos de todos.'),
    (Titulo: 'Frontera Rota'; Anio: 2015; Director: 'Lucia Fernandez'; GeneroId: 1;
     Sinopsis: 'Un guardabosques se enfrenta solo a una banda que cruza mercancia por la sierra.'),
    (Titulo: 'La Vigilia'; Anio: 2014; Director: 'Sofia Duarte'; GeneroId: 5;
     Sinopsis: 'Una enfermera de turno nocturno jura que los pacientes hablan mientras duermen.'));

var
  Conn: TFDConnection;
  Driver: TAiRAGVectorPostgresDriver;
  Rag: TAiRAGVector;
  Embedder: TAiOllamaEmbeddings;
  Telemetry: TAiTelemetry;
  ModeloEmb: string;
  DimEmb: Integer;
  TablaVec: string;

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
  Base: string;
  V: Integer;
begin
  Result := GetEnvironmentVariable('PGLIBPQ');
  if (Result <> '') and FileExists(Result) then
    Exit;
  // Autodeteccion: la mas nueva primero
  for V := 18 downto 12 do
  begin
    Base := Format('C:\Program Files\PostgreSQL\%d\bin\libpq.dll', [V]);
    if FileExists(Base) then
      Exit(Base);
  end;
  Result := ''; // que FireDAC lo busque en el PATH
end;

// ---------------------------------------------------------------------------
// 1. PostgreSQL
// ---------------------------------------------------------------------------
procedure ConectarPostgres;
var
  Link: TFDPhysPgDriverLink;
  LibPq, Ver: string;
begin
  Writeln('== 1. PostgreSQL ==');

  Link := TFDPhysPgDriverLink.Create(nil);
  LibPq := FindLibPq;
  if LibPq <> '' then
  begin
    Link.VendorLib := LibPq;
    Writeln('   libpq: ' + LibPq);
  end
  else
    Writeln('   libpq: no localizada, se buscara en el PATH');

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
  Ver := Conn.ExecSQLScalar('select version()');
  Writeln('   conectado a ' + Conn.Params.Values['Database'] + '@' + Conn.Params.Values['Server'] + ':' +
    Conn.Params.Values['Port']);
  Writeln('   ' + Copy(Ver, 1, 60));

  if Conn.ExecSQLScalar('select count(*) from pg_extension where extname = ''vector''') = 0 then
    raise Exception.Create('La extension pgvector no esta instalada en esta base');
  Writeln('   pgvector: presente (' + VarToStr(Conn.ExecSQLScalar('select extversion from pg_extension where extname=''vector''')) + ')');
end;

// ---------------------------------------------------------------------------
// 2. Ollama
// ---------------------------------------------------------------------------
procedure VerificarOllama(const AUrl: string);
var
  Http: THTTPClient;
  Resp: IHTTPResponse;
  Root: TJSONObject;
  Models: TJSONArray;
  V: TJSONValue;
  Nombre, Lista: string;
  Encontrado: Boolean;
begin
  Writeln('');
  Writeln('== 2. Ollama ==');

  Http := THTTPClient.Create;
  try
    Http.ConnectionTimeout := 5000;
    Http.ResponseTimeout := 5000;
    try
      Resp := Http.Get(AUrl.TrimRight(['/']) + '/api/tags');
    except
      on E: Exception do
        raise Exception.Create('Ollama no responde en ' + AUrl + '. Arrancalo con "ollama serve". Detalle: ' + E.Message);
    end;
    if Resp.StatusCode <> 200 then
      raise Exception.CreateFmt('Ollama devolvio HTTP %d', [Resp.StatusCode]);

    Root := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8)) as TJSONObject;
    try
      Encontrado := False;
      Lista := '';
      if Root.TryGetValue<TJSONArray>('models', Models) then
        for V in Models do
          if V is TJSONObject then
          begin
            Nombre := TJSONObject(V).GetValue<string>('name', '');
            if Nombre.StartsWith(ModeloEmb) then
              Encontrado := True;
            if Nombre.Contains('embed') then
              Lista := Lista + '  ' + Nombre;
          end;
      if not Encontrado then
        raise Exception.CreateFmt('El modelo "%s" no esta en Ollama. Modelos de embeddings disponibles:%s' + sLineBreak +
          'Descargalo con: ollama pull %s', [ModeloEmb, Lista, ModeloEmb]);
      Writeln('   modelo de embeddings: ' + ModeloEmb + '  (dim ' + IntToStr(DimEmb) + ')');
    finally
      Root.Free;
    end;
  finally
    Http.Free;
  end;
end;

// ---------------------------------------------------------------------------
// 3-4. Esquema y siembra
// ---------------------------------------------------------------------------
procedure CrearEsquema;
begin
  Writeln('');
  Writeln('== 3. Esquema vectorial ==');
  Driver.CreateSchema(TablaVec, DimEmb);
  Writeln('   tabla ' + TablaVec + ' lista (vector(' + IntToStr(DimEmb) + '), indice HNSW coseno, GIN sobre properties y FTS)');
  Writeln('   nota: no se toca pelicula_vector, que es vector(1536) de OpenAI');
end;

function SembrarPeliculas: Integer;
var
  Q: TFDQuery;
  S: TPeliSeed;
  I: Integer;
begin
  Result := 0;
  Writeln('');
  Writeln('== 4. Siembra de peliculas ==');

  if HasFlag('--no-seed') then
  begin
    Writeln('   --no-seed: no se inserta nada');
    Exit;
  end;

  Q := TFDQuery.Create(nil);
  try
    Q.Connection := Conn;
    for I := Low(SEMILLAS) to High(SEMILLAS) do
    begin
      S := SEMILLAS[I];
      // Idempotente: se compara por titulo para poder correr el demo varias veces
      Q.SQL.Text := 'select count(*) from pelicula where titulo = :t';
      Q.ParamByName('t').AsString := S.Titulo;
      Q.Open;
      if Q.Fields[0].AsInteger > 0 then
      begin
        Q.Close;
        Continue;
      end;
      Q.Close;

      Q.SQL.Text := 'insert into pelicula (titulo, anio, director, genero_id, sinopsis, creado) ' +
        'values (:t, :a, :d, :g, :s, now())';
      Q.ParamByName('t').AsString := S.Titulo;
      Q.ParamByName('a').AsInteger := S.Anio;
      Q.ParamByName('d').AsString := S.Director;
      Q.ParamByName('g').AsInteger := S.GeneroId;
      Q.ParamByName('s').AsString := S.Sinopsis;
      Q.ExecSQL;
      Inc(Result);
    end;
  finally
    Q.Free;
  end;

  Writeln(Format('   insertadas %d peliculas nuevas (las repetidas se omiten por titulo)', [Result]));
  // ExecSQLScalar devuelve Variant: hay que convertirlo, %d no lo acepta
  Writeln('   total en el catalogo: ' + VarToStr(Conn.ExecSQLScalar('select count(*) from pelicula')));
end;

// ---------------------------------------------------------------------------
// 5. Indexado
// ---------------------------------------------------------------------------
function IndexarPendientes: Integer;
var
  Q: TFDQuery;
  Meta: TAiEmbeddingMetaData;
  Yaestan: TDictionary<string, Boolean>;
  Texto, PeliId: string;
  Nodo: TAiEmbeddingNode;
begin
  Result := 0;
  Writeln('');
  Writeln('== 5. Indexado con Ollama ==');

  if HasFlag('--reindex') then
  begin
    Driver.Clear(ENTIDAD);
    Writeln('   --reindex: vectores previos borrados');
  end;

  Yaestan := TDictionary<string, Boolean>.Create;
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := Conn;

    // Que peliculas tienen ya vector en ESTA tabla y entidad
    Q.SQL.Text := Format('select properties->>''pelicula_id'' as pid from %s where entidad = :e', [TablaVec]);
    Q.ParamByName('e').AsString := ENTIDAD;
    Q.Open;
    while not Q.Eof do
    begin
      if not Q.Fields[0].IsNull then
        Yaestan.AddOrSetValue(Q.Fields[0].AsString, True);
      Q.Next;
    end;
    Q.Close;
    Writeln(Format('   ya indexadas: %d', [Yaestan.Count]));

    Q.SQL.Text := 'select p.id, p.titulo, p.anio, p.director, coalesce(g.nombre, ''?'') as genero, ' +
      'coalesce(p.sinopsis, '''') as sinopsis from pelicula p left join genero g on g.id = p.genero_id order by p.id';
    Q.Open;
    while not Q.Eof do
    begin
      PeliId := Q.FieldByName('id').AsString;
      if Yaestan.ContainsKey(PeliId) then
      begin
        Q.Next;
        Continue;
      end;

      // QUE se vectoriza importa tanto como el modelo. La primera version de
      // este demo indexaba "Titulo (anio). Director: X. Genero: Y. Sinopsis",
      // y la recuperacion era pobre: todos los documentos empezaban con la
      // misma plantilla, asi que el embedding quedaba dominado por el
      // boilerplate y los scores se apinaban en una franja estrecha (0.57-0.66).
      // Poniendo el contenido con carga semantica primero -titulo y sinopsis- y
      // dejando la ficha al final, la separacion mejora notablemente.
      // La ficha igual conviene incluirla: permite preguntar por director o
      // genero en lenguaje natural. Y para filtrar con exactitud estan los
      // metadatos, que no dependen del embedding.
      Texto := Format('%s. %s (%s, %d, dirigida por %s)', [Q.FieldByName('titulo').AsString,
        Q.FieldByName('sinopsis').AsString, Q.FieldByName('genero').AsString, Q.FieldByName('anio').AsInteger,
        Q.FieldByName('director').AsString]);

      Meta := TAiEmbeddingMetaData.Create;
      try
        // Los metadatos son lo que despues permite filtrar sin recalcular vectores
        Meta['pelicula_id'] := PeliId;
        Meta['titulo'] := Q.FieldByName('titulo').AsString;
        Meta['anio'] := Q.FieldByName('anio').AsInteger;
        Meta['director'] := Q.FieldByName('director').AsString;
        Meta['genero'] := Q.FieldByName('genero').AsString;

        Nodo := Rag.AddItem(Texto, Meta); // genera el embedding y persiste via driver
        if Assigned(Nodo) then
          Inc(Result);
        Write('.');
      finally
        Meta.Free;
      end;
      Q.Next;
    end;
    Q.Close;
  finally
    Q.Free;
    Yaestan.Free;
  end;

  Writeln('');
  Writeln(Format('   indexadas ahora: %d', [Result]));
  Writeln(Format('   total de vectores en %s: %s', [TablaVec,
    VarToStr(Conn.ExecSQLScalar(Format('select count(*) from %s where entidad = %s', [TablaVec, QuotedStr(ENTIDAD)])))]));
end;

// ---------------------------------------------------------------------------
// 6. Busquedas
// ---------------------------------------------------------------------------
procedure Buscar(const APrompt: string; ALimit: Integer; AFilter: TAiFilterCriteria; const ATitulo: string = '');
var
  Res: TAiRAGVector;
  Nodo: TAiEmbeddingNode;
  Target: TAiEmbeddingNode;
begin
  Writeln('');
  if ATitulo <> '' then
    Writeln('   ' + ATitulo)
  else
    Writeln('   consulta: "' + APrompt + '"');

  Target := Rag.CreateEmbeddingNode(APrompt);
  try
    Res := Driver.Search(Target, ENTIDAD, ALimit, 0.0, AFilter, Rag.SearchOptions);
    try
      if (Res = nil) or (Res.Count = 0) then
        Writeln('     (sin resultados)')
      else
        for Nodo in Res.Items do
          Writeln(Format('     [%.4f] %-28s %s %s', [Nodo.Idx, VarToStr(Nodo.MetaData['titulo']),
            VarToStr(Nodo.MetaData['anio']), VarToStr(Nodo.MetaData['genero'])]));
    finally
      Res.Free;
    end;
  finally
    Target.Free;
  end;
end;

procedure Busquedas;
var
  Filtro: TAiFilterCriteria;
begin
  Writeln('');
  Writeln('== 6. Busqueda semantica ==');

  // Ninguna de estas consultas comparte las palabras exactas con la sinopsis:
  // si devuelve lo esperado, es el embedding haciendo su trabajo.
  Buscar('naves espaciales y viajes fuera del sistema solar', 3, nil);
  Buscar('algo que da miedo en una casa vieja', 3, nil);
  Buscar('una historia para reirse con la familia politica', 3, nil);
  Buscar('reencuentro entre familiares despues de muchos anos', 3, nil);

  Writeln('');
  Writeln('== 7. Busqueda con filtro de metadatos ==');
  Writeln('   (el filtro se aplica en SQL sobre properties, sin recalcular vectores)');

  Filtro := TAiFilterCriteria.Create;
  try
    Filtro.AddGreater('anio', 2019);
    Buscar('ciencia ficcion espacial', 5, Filtro);
  finally
    Filtro.Free;
  end;

  // -------------------------------------------------------------------------
  // Solo-embeddings vs hibrido
  // -------------------------------------------------------------------------
  // Los embeddings entienden el sentido pero se pierden con terminos raros y
  // nombres propios; BM25 hace justo lo contrario. El driver de Postgres
  // resuelve el lexico con ts_rank_cd sobre la columna search_vector (indice
  // GIN, configuracion 'spanish') y fusiona ambos rankings con RRF.
  Writeln('');
  Writeln('== 8. Solo-embeddings vs hibrido (BM25 + RRF) ==');

  Rag.SearchOptions.UseEmbeddings := True;
  Rag.SearchOptions.UseBM25 := False;
  Rag.SearchOptions.UseRRF := False;
  Buscar('origamis que defienden una biblioteca', 3, nil, 'solo embeddings  -> "origamis que defienden una biblioteca"');

  Rag.SearchOptions.UseBM25 := True;
  Rag.SearchOptions.UseRRF := True;
  Buscar('origamis que defienden una biblioteca', 3, nil, 'hibrido BM25+RRF  -> la misma consulta');

  // Un nombre propio es el caso donde mas se nota
  Rag.SearchOptions.UseBM25 := False;
  Rag.SearchOptions.UseRRF := False;
  Buscar('peliculas de Estudio Pixelia', 3, nil, 'solo embeddings  -> "peliculas de Estudio Pixelia"');

  Rag.SearchOptions.UseBM25 := True;
  Rag.SearchOptions.UseRRF := True;
  Buscar('peliculas de Estudio Pixelia', 3, nil, 'hibrido BM25+RRF  -> la misma consulta');
end;

// ---------------------------------------------------------------------------

begin
  Telemetry := nil;
  Conn := nil;
  Driver := nil;
  Rag := nil;
  Embedder := nil;
  try
    try
      ModeloEmb := ArgValue('--model', 'mxbai-embed-large');
      DimEmb := StrToIntDef(ArgValue('--dim', ''), 0);
      if DimEmb = 0 then
      begin
        // Dimensiones conocidas de los modelos de embeddings de Ollama
        if ModeloEmb.StartsWith('nomic-embed-text') then
          DimEmb := 768
        else if ModeloEmb.StartsWith('all-minilm') then
          DimEmb := 384
        else
          DimEmb := 1024; // mxbai-embed-large
      end;
      TablaVec := 'pelicula_vector_ollama_' + IntToStr(DimEmb);

      if HasFlag('--otel') then
      begin
        Telemetry := TAiTelemetry.Create(nil);
        Telemetry.ServiceName := 'RagPostgresConsoleDemo';
        Telemetry.Enabled := True;
        Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
      end;

      Writeln('=== DEMO 077: RAG vectorial sobre PostgreSQL + pgvector (embeddings locales) ===');
      Writeln('');

      ConectarPostgres;
      VerificarOllama(Env('OLLAMA_URL', 'http://localhost:11434/'));

      Embedder := TAiOllamaEmbeddings.Create(nil);
      Embedder.Url := Env('OLLAMA_URL', 'http://localhost:11434/');
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

      CrearEsquema;
      SembrarPeliculas;
      IndexarPendientes;
      Busquedas;

      Writeln('');
      Writeln('OK: el ciclo completo de RAG sobre pgvector funciono con embeddings locales.');
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
