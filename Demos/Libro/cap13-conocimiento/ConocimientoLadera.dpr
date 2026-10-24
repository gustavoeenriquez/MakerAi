program ConocimientoLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 13 - Conocimiento: embeddings, base vectorial y VQL
// =============================================================================
// A. Embeddings: la pregunta y cada seccion del manual se convierten en vectores
//    (TAiOpenAiEmbeddings) y se mide la similitud del coseno. Un pasaje de foro con
//    una inyeccion de prompt queda MAS cerca que el manual.
// B. La base vectorial (TAiRAGVector en memoria, con metadatos): recuperar los mas
//    parecidos, reordenar con Jev (TAiJevRAGReranker: evidencia por pasaje, la
//    inyeccion se descarta) y la misma busqueda en VQL con un WHERE por metadatos.
// C. Lucia responde sin contexto y con el contexto recuperado.
//
// Uso: ConocimientoLadera.exe   (OPENAI_API_KEY y TYPESAFE_API_KEY)
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils, System.StrUtils, System.Classes, System.IOUtils, System.Variants,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings.OpenAi,
  uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.Index, uMakerAi.RAG.MetaData,
  uMakerAi.Jev.RAG,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations;

const
  PREGUNTA = '¿Cuánto cuesta el envío a Medellín y en cuántos días llega?';
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP; Nariño Altura 42.000 COP; Sierra Nevada Orgánico 36.000 COP; ' +
    'Descafeinado Suave 34.000 COP.' + sLineBreak +
    'Responde en español, breve. Si no sabes algo, dilo.';
  VQL =
    'MATCH (manual)' + sLineBreak +
    'SEARCH ''¿Cuánto cuesta el envío a Medellín y en cuántos días llega?''' + sLineBreak +
    'USING EMBEDDINGS' + sLineBreak +
    'WHERE fuente = ''manual''' + sLineBreak +
    'RETURN TEXT, METADATA, SCORE' + sLineBreak +
    'LIMIT 2';

var
  Titulos, Textos: TStringList;

procedure Titulo(const S: string);
begin
  Writeln('==== ', S, ' ', StringOfChar('=', 62 - Length(S)));
  Writeln;
end;

procedure LeerManual(const ACarpeta: string);
var
  Archivo, L, Tit, Texto: string;

  procedure Guardar;
  begin
    if (Texto.Trim <> '') and (Tit <> '') then
    begin
      Titulos.Add(Tit);
      Textos.Add(Tit + '. ' + Texto.Trim);
    end;
    Texto := '';
  end;

begin
  // Un pasaje por seccion: un titulo (linea corta sin punto final) y sus parrafos
  for Archivo in TDirectory.GetFiles(ACarpeta, '*.txt') do
  begin
    Tit := '';
    Texto := '';
    for L in TFile.ReadAllLines(Archivo, TEncoding.UTF8) do
    begin
      if L.Trim = '' then
        Continue;
      if (L.Length < 60) and not L.Trim.EndsWith('.') and not L.Contains(':') then
      begin
        Guardar;
        Tit := L.Trim;
      end
      else if Tit <> '' then
        Texto := Texto + L.Trim + ' ';
    end;
    Guardar;
  end;
end;

procedure Similitudes(Emb: TAiOpenAiEmbeddings);
var
  I: Integer;
  VPregunta, V: TAiEmbeddingData;
begin
  // Cada texto se convierte en un vector; la cercania es el coseno del angulo entre ellos
  VPregunta := Emb.CreateEmbedding(PREGUNTA, 'ladera');
  Writeln('Modelo: ', Emb.Model, ' · dimensiones: ', Length(VPregunta));
  Writeln('Similitud con "', PREGUNTA, '":');
  for I := 0 to Textos.Count - 1 do
  begin
    V := Emb.CreateEmbedding(Textos[I], 'ladera');
    Writeln(Format('   %.3f  %s', [TAiEmbeddingsCore.CosineSimilarity(VPregunta, V), Titulos[I]]));
  end;
  Writeln;
end;

function Indexar(Emb: TAiOpenAiEmbeddings): TAiRAGVector;
var
  I: Integer;
  Meta: TAiEmbeddingMetaData;
begin
  Result := TAiRAGVector.Create(nil, True);
  Result.Embeddings := Emb;
  Meta := TAiEmbeddingMetaData.Create;
  try
    for I := 0 to Textos.Count - 1 do
    begin
      Meta['seccion'] := Titulos[I];
      Meta['fuente'] := IfThen(Titulos[I].StartsWith('Foro'), 'foro', 'manual');
      Result.AddItem(Textos[I], Meta);          // embedding + nodo con metadatos
    end;
  finally
    Meta.Free;
  end;
  Result.SaveToFile(ExtractFilePath(ParamStr(0)) + 'manual.mkVRag');   // para no volver a pagar los embeddings
  Writeln(Result.Count, ' pasajes indexados (guardados en manual.mkVRag)');
  Writeln;
end;

function Recuperar(RAG: TAiRAGVector; Reranker: TAiJevRAGReranker): string;
var
  Res: TAiRAGVector;
  I: Integer;
  N: TAiEmbeddingNode;
begin
  Result := '';
  Res := RAG.Search(PREGUNTA, 4, 0.3, nil);
  try
    Writeln('Recuperados por coseno (los 4 mas cercanos):');
    for I := 0 to Res.Count - 1 do
      Writeln(Format('   %.3f  %s', [Res.Items[I].Idx, VarToStr(Res.Items[I].MetaData['seccion'])]));
    Res.RerankWith(PREGUNTA, Reranker);
    Writeln('Reordenados con Jev (evidencia por pasaje; -1 = inyeccion detectada, se descarta):');
    for I := 0 to Res.Count - 1 do
    begin
      N := Res.Items[I];
      Writeln(Format('   %5.2f  %s', [N.Idx, VarToStr(N.MetaData['seccion'])]));
      if N.Idx >= 0.2 then
        Result := Result + N.Text + sLineBreak;
    end;
    Writeln(Format('   (%d inyeccion(es); %d llamadas a Jev, %d tokens, US$ %.6f)',
      [Reranker.LastInjected, Reranker.Usage.Requests, Reranker.Usage.InputTokens, Reranker.Usage.CostUSD]));
  finally
    Res.Free;
  end;
  Writeln;
end;

procedure PreguntarALucia(const AContexto: string);
var
  Conn: TAiChatConnection;
  R, P: string;
begin
  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'OpenAi';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.SystemPrompt.Text := PROMPT_SISTEMA;
    P := PREGUNTA;
    if AContexto <> '' then
      P := 'Responde con este CONTEXTO del manual de la tienda:' + sLineBreak + AContexto + sLineBreak +
        'PREGUNTA: ' + PREGUNTA;
    Writeln('Tú: ', PREGUNTA);
    R := Conn.AddMessageAndRun(P, 'user', []);
    Writeln('Lucía: ', R.Trim);
    Writeln(Format('   (%d tokens de entrada, %d de salida)', [Conn.Prompt_tokens, Conn.Completion_tokens]));
    Writeln;
  finally
    Conn.Free;
  end;
end;

var
  Emb: TAiOpenAiEmbeddings;
  RAG: TAiRAGVector;
  Reranker: TAiJevRAGReranker;
  Contexto: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  Titulos := TStringList.Create;
  Textos := TStringList.Create;
  Emb := TAiOpenAiEmbeddings.Create(nil);
  Reranker := TAiJevRAGReranker.Create(nil);
  RAG := nil;
  try
    Emb.ApiKey := '@OPENAI_API_KEY';
    LeerManual(ExtractFilePath(ParamStr(0)) + 'manual');
    Writeln(Titulos.Count, ' pasajes: ', Titulos.CommaText);
    Writeln;

    Titulo('A. Embeddings: que tan cerca esta cada pasaje de la pregunta');
    Similitudes(Emb);

    Titulo('B. La base vectorial: recuperar, reordenar y VQL');
    RAG := Indexar(Emb);
    Contexto := Recuperar(RAG, Reranker);
    Writeln('VQL> ', VQL.Replace(sLineBreak, ' '));
    Writeln(RAG.ExecuteVQL(VQL));
    Writeln;

    Titulo('C. Lucia sin el manual y con el manual');
    PreguntarALucia('');
    PreguntarALucia(Contexto);
  finally
    RAG.Free;
    Reranker.Free;
    Emb.Free;
    Textos.Free;
    Titulos.Free;
  end;
end.
