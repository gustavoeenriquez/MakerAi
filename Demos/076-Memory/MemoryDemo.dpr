program MemoryDemo;

// =============================================================================
// DEMO 076 - TAiMemory: memoria semantica persistente
// =============================================================================
// Un chat sin memoria arranca de cero en cada sesion. TAiMemory guarda hechos,
// preferencias y decisiones en SQLite (FTS5) y los recupera despues por
// relevancia, de modo que sobrevivan al cierre de la aplicacion.
//
// Recorrido del demo:
//   1. Store    : guardar memorias con tipo, importancia, tags y TTL.
//   2. Search   : buscar por texto (FTS5) y ver el score de cada resultado.
//   3. Recall   : traer solo lo importante, sin consulta previa.
//   4. Context  : armar el bloque de texto listo para inyectar en el system
//                 prompt, respetando un presupuesto de tokens.
//   5. Stats    : estado del namespace.
//   6. Persistencia: se reabre la base y se comprueba que todo sigue ahi.
//
// SIN API KEY funciona en modo FTS (ms_FTS): busqueda lexica sobre SQLite.
// Con OPENAI_API_KEY definida se engancha un embedder y la busqueda pasa a
// hibrida (ms_Hybrid): FTS + semantica fusionadas con RRF, que es lo que
// permite encontrar "cobro duplicado" buscando "problema de facturacion".
//
// Modos de uso:
//   MemoryDemo.exe            -> usa una base temporal y la borra al terminar
//   MemoryDemo.exe --keep     -> conserva el archivo .db para inspeccionarlo
//   MemoryDemo.exe --otel     -> exporta trazas OTLP a localhost:4318
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.TypInfo,
  // FireDAC: en una app de consola hay que enlazar explicitamente el driver y
  // las factories, o TAiMemory falla al abrir la base con "Object factory for
  // class ... is missing".
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.DApt,
  uMakerAi.Memory.Types,
  uMakerAi.Memory,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.Telemetry;

function HasFlag(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AName) then
      Exit(True);
end;

function NombreTipo(A: TMemoryType): string;
begin
  Result := GetEnumName(TypeInfo(TMemoryType), Ord(A));
end;

// -----------------------------------------------------------------------------

procedure Sembrar(AMem: TAiMemory);
begin
  Writeln('');
  Writeln('== 1. Store: guardar memorias tipadas ==');
  Writeln('');

  // Importancia 1-10: es lo que despues filtra Recall y ordena Context.
  AMem.Store('El usuario trabaja con Delphi 13 Florence en Windows', mt_Fact, 8, 'entorno,delphi');
  AMem.Store('Prefiere respuestas cortas y con ejemplos de codigo', mt_Preference, 9, 'estilo');
  AMem.Store('Se decidio usar SQLite como backend del RAG en vez de Postgres', mt_Decision, 9, 'arquitectura,rag');
  AMem.Store('El error "vec0 not found" se resuelve copiando vec0.dll junto al exe', mt_ErrorFix, 7, 'rag,sqlite');
  AMem.Store('Suele preguntar primero por el rendimiento antes que por la API', mt_Pattern, 5, 'estilo');
  AMem.Store('Un cobro duplicado se reclama por el formulario de facturacion', mt_Fact, 6, 'soporte');
  // Con TTL: caduca sola a los 7 dias. Util para contexto de una sola sesion.
  AMem.Store('Sesion actual: revisando el modulo de agentes', mt_Summary, 3, 'sesion', nil, 7);

  Writeln('   guardadas 7 memorias (una con TTL de 7 dias)');
  Writeln('   nota: Store deduplica por hash de contenido dentro del namespace,');
  Writeln('         asi que repetir el mismo texto actualiza en vez de duplicar.');
end;

procedure Buscar(AMem: TAiMemory; AModo: TMemorySearchMode; const AConsulta: string);
var
  Res: TMemorySearchResults;
  I: Integer;
begin
  Writeln('');
  Writeln('   consulta: "' + AConsulta + '"');
  Res := AMem.Search(AConsulta, 3, AModo);
  if Length(Res) = 0 then
    Writeln('     (sin resultados)')
  else
    for I := 0 to High(Res) do
      Writeln(Format('     [%.3f %-8s] %s', [Res[I].Score, Res[I].MatchType, Res[I].Entry.Content]));
end;

procedure Recordar(AMem: TAiMemory);
var
  Lista: TMemoryEntryList;
  E: TMemoryEntry;
begin
  Writeln('');
  Writeln('== 3. Recall: solo lo importante, sin consulta ==');
  Writeln('');

  // Sin query: devuelve lo de mayor importancia. Es lo que se usa al abrir una
  // sesion nueva, antes de que el usuario haya escrito nada.
  Lista := AMem.Recall(8, 10);
  try
    for E in Lista do
      Writeln(Format('     imp=%d %-14s %s', [E.Importance, NombreTipo(E.MemoryType), E.Content]));
  finally
    Lista.Free;
  end;
end;

procedure ArmarContexto(AMem: TAiMemory);
var
  Ctx: TMemoryContextResult;
begin
  Writeln('');
  Writeln('== 4. Context: bloque listo para el system prompt ==');
  Writeln('');

  // Presupuesto de tokens deliberadamente bajo para que se vea el recorte.
  Ctx := AMem.Context('Como configuro el RAG del proyecto?', 200, 1);
  Writeln(Format('   memorias incluidas: %d   tokens estimados: %d   recortado: %s',
    [Ctx.MemoryCount, Ctx.TokenEstimate, BoolToStr(Ctx.Truncated, True)]));
  Writeln('   ---------------- bloque generado ----------------');
  Writeln(Ctx.FormattedText);
  Writeln('   ------------------------------------------------');
end;

procedure MostrarStats(AMem: TAiMemory);
var
  S: TMemoryStats;
begin
  Writeln('');
  Writeln('== 5. Stats del namespace ==');
  Writeln('');
  S := AMem.Stats;
  Writeln(Format('   namespace=%s  total=%d  importancia media=%.2f  decay medio=%.2f  expiradas=%d',
    [S.Namespace, S.TotalCount, S.AvgImportance, S.AvgDecay, S.ExpiredCount]));
end;

// -----------------------------------------------------------------------------

var
  Mem: TAiMemory;
  Embedder: TAiOpenAiEmbeddings;
  Telemetry: TAiTelemetry;
  DbPath, ApiKey: string;
  ModoBusqueda: TMemorySearchMode;
  Total: Integer;

begin
  Telemetry := nil;
  Embedder := nil;
  DbPath := TPath.Combine(TPath.GetTempPath, 'makerai_demo076_memoria.db');

  try
    if HasFlag('--otel') then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'MemoryDemo';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    Writeln('=== DEMO 076: TAiMemory, memoria semantica persistente ===');

    // Empezar siempre limpio para que el demo sea reproducible
    if TFile.Exists(DbPath) then
      TFile.Delete(DbPath);

    ApiKey := GetEnvironmentVariable('OPENAI_API_KEY');
    if ApiKey <> '' then
    begin
      Embedder := TAiOpenAiEmbeddings.Create(nil);
      Embedder.ApiKey := ApiKey;
      Embedder.Model := 'text-embedding-3-small';
      ModoBusqueda := ms_Hybrid;
      Writeln('   embedder: OpenAI text-embedding-3-small -> busqueda HIBRIDA (FTS + semantica)');
    end
    else
    begin
      ModoBusqueda := ms_FTS;
      Writeln('   sin OPENAI_API_KEY -> busqueda FTS (lexica). Define la variable');
      Writeln('   para ver la busqueda semantica y la fusion RRF.');
    end;
    Writeln('   base de datos: ' + DbPath);

    Mem := TAiMemory.Create(nil);
    try
      Mem.Namespace := 'demo076';
      Mem.DbPath := DbPath;
      if Assigned(Embedder) then
        Mem.Embedder := Embedder;

      Sembrar(Mem);

      Writeln('');
      Writeln('== 2. Search: recuperar por relevancia ==');
      Buscar(Mem, ModoBusqueda, 'sqlite');
      Buscar(Mem, ModoBusqueda, 'como responde el usuario');
      // Esta es la que separa FTS de semantica: no comparte palabras con la
      // memoria que deberia encontrar ("cobro duplicado ... facturacion").
      Buscar(Mem, ModoBusqueda, 'problema con un cobro');

      Recordar(Mem);
      ArmarContexto(Mem);
      MostrarStats(Mem);
    finally
      Mem.Free;
    end;

    // --- 6. Persistencia: reabrir y comprobar que sigue todo ---
    Writeln('');
    Writeln('== 6. Persistencia: se cerro la base y se vuelve a abrir ==');
    Writeln('');
    Mem := TAiMemory.Create(nil);
    try
      Mem.Namespace := 'demo076';
      Mem.DbPath := DbPath;
      Total := Mem.Stats.TotalCount;
      Writeln(Format('   memorias recuperadas del disco: %d', [Total]));
      if Total > 0 then
        Writeln('   OK: la memoria sobrevive al cierre de la aplicacion')
      else
        Writeln('   ATENCION: no se recupero nada del disco');
    finally
      Mem.Free;
    end;

    if HasFlag('--keep') then
      Writeln('')
    else
    begin
      if TFile.Exists(DbPath) then
        TFile.Delete(DbPath);
      Writeln('');
      Writeln('   (base temporal borrada; usa --keep para conservarla)');
    end;

    Writeln('');
    Writeln('OK: demo de memoria completado.');

    if Assigned(Telemetry) then
    begin
      Telemetry.Flush;
      Telemetry.Free;
    end;
    Embedder.Free;
    ExitCode := 0;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ' + E.ClassName + ': ' + E.Message);
      Embedder.Free;
      ExitCode := 1;
    end;
  end;
end.
