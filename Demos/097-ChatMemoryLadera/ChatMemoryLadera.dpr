program ChatMemoryLadera;

// =============================================================================
// DEMO 097 - Memoria persistente en el chat
// =============================================================================
// El historial (Messages) dura lo que dura la conversacion. La memoria
// persistente sobrevive: Marta vuelve otro dia, con un chat nuevo, y Lucia la
// recuerda.
//
//   - TAiMemory       : memorias en SQLite (FTS5; con OPENAI_API_KEY, ademas
//                       embeddings y busqueda hibrida). Namespace por cliente.
//   - PersistentMemory: asignada al chat, en cada turno busca lo relevante y
//                       lo inyecta antes del prompt (MemoryTokenBudget).
//   - Analyzer        : un modelo barato lee la conversacion y decide que vale
//                       la pena recordar (preferencias, hechos). Aqui se llama
//                       AnalyzeNow al cerrar la sesion; con AnalysisInterval = N
//                       lo hace solo cada N turnos.
//
// Uso:
//   ChatMemoryLadera.exe           sesion 1 + sesion 2 (base nueva)
//   ChatMemoryLadera.exe --sesion2 solo la sesion 2, con lo que quedo guardado
//
// Empresa, clientes, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  // FireDAC: en una app de consola hay que enlazar el driver y las factories
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.DApt,
  uMakerAi.Memory.Types,
  uMakerAi.Memory,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera.' + sLineBreak +
    'Carta (bolsa de 340 g): Huila Reserva 38.000 COP (panela, frutos rojos, acidez brillante); ' +
    'Nariño Altura 42.000 COP (cítricos, caramelo, acidez alta); Sierra Nevada Orgánico 36.000 COP ' +
    '(chocolate amargo, nuez, acidez baja); Descafeinado Suave 34.000 COP (cacao suave).' + sLineBreak +
    'Si el mensaje trae un bloque "Contexto de memoria", son cosas que el cliente te contó en ' +
    'visitas anteriores: úsalas con naturalidad. Responde en español, cálido y breve (máximo 5 líneas).';

  SESION_1: array [0 .. 2] of string = (
    'Hola, soy Marta. En casa preparo el café en prensa francesa y lo tomo sin azúcar.',
    'Me encantan los cafés frutales. Ah, y la semana pasada compré el Nariño Altura y me pareció demasiado ácido.',
    'Listo, por hoy no compro nada. ¡Gracias!');

  SESION_2 = 'Hola Lucía, volví. ¿Qué café me recomiendas esta vez y cómo lo preparo?';

function NuevoChat(AMemoria: TAiMemory): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'OpenAi';
  Result.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
  Result.Params.Values['Asynchronous'] := 'False';
  Result.Tool_Active := False;
  Result.SystemPrompt.Text := PROMPT_SISTEMA;
  // La memoria se consulta en cada turno y lo relevante viaja con el prompt
  Result.PersistentMemory := AMemoria;
  Result.MemoryTokenBudget := 400;
end;

procedure Charlar(AChat: TAiChatConnection; const APregunta: string);
var
  R: string;
begin
  Writeln('Marta: ', APregunta);
  R := AChat.AddMessageAndRun(APregunta, 'user', []);
  if R = '' then
    R := '(sin respuesta) ' + AChat.LastError;
  Writeln('Lucía: ', R.Trim.Replace(sLineBreak, sLineBreak + '       '));
  Writeln;
end;

procedure MostrarMemorias(AMemoria: TAiMemory);
var
  Lista: TMemoryEntryList;
  E: TMemoryEntry;
begin
  Lista := AMemoria.Recall(1, 20);
  try
    Writeln(Format('--- %d memorias de Marta en %s ---', [Lista.Count, ExtractFileName(AMemoria.DbPath)]));
    for E in Lista do
      Writeln(Format('  imp=%d  %s', [E.Importance, E.Content]));
    Writeln;
  finally
    Lista.Free;
  end;
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

var
  Memoria: TAiMemory;
  Embedder: TAiOpenAiEmbeddings;
  Analizador, Chat: TAiChatConnection;
  DbPath, Pregunta: string;
  Guardadas: Integer;
begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;

  DbPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'memoria_ladera.db');
  if not HasFlag('--sesion2') and TFile.Exists(DbPath) then
    TFile.Delete(DbPath);

  Memoria := TAiMemory.Create(nil);
  Embedder := TAiOpenAiEmbeddings.Create(nil);
  Analizador := TAiChatConnection.Create(nil);
  try
    try
      // Embeddings: la busqueda encuentra "preparo en prensa" aunque la
      // pregunta diga "como lo preparo" (por significado, no solo palabras)
      Embedder.ApiKey := '@OPENAI_API_KEY';
      Memoria.Embedder := Embedder;
      Memoria.Namespace := 'cliente-marta';   // cada cliente, su memoria
      Memoria.DbPath := DbPath;

      // El que decide que recordar: un modelo barato, sincrono
      Analizador.DriverName := 'OpenAi';
      Analizador.Model := 'gpt-6-luna';
      Analizador.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
      Analizador.Params.Values['Asynchronous'] := 'False';
      Analizador.Tool_Active := False;
      Memoria.Analyzer := Analizador;
      Memoria.AnalysisInterval := 0;   // a mano, al cerrar la sesion

      if not HasFlag('--sesion2') then
      begin
        Writeln('==== Sesión 1: Marta llega por primera vez ', StringOfChar('=', 20));
        Writeln;
        Chat := NuevoChat(Memoria);
        try
          for Pregunta in SESION_1 do
            Charlar(Chat, Pregunta);
          Writeln(Format('(historial de la sesión: %d mensajes; se pierde al cerrar)', [Chat.Messages.Count]));
        finally
          Chat.Free;
        end;
        Guardadas := Memoria.AnalyzeNow;
        Writeln(Format('(el analizador guardó %d memorias al cerrar la sesión)', [Guardadas]));
        Writeln;
        MostrarMemorias(Memoria);
      end;

      Writeln('==== Sesión 2: otro día, chat nuevo ', StringOfChar('=', 27));
      Writeln;
      Writeln('Lo que la memoria inyecta para esta pregunta:');
      Writeln('  ', Memoria.BuildContext(SESION_2, 400).Trim.Replace(sLineBreak, sLineBreak + '  '));
      Writeln;
      Chat := NuevoChat(Memoria);
      try
        Writeln(Format('(historial al empezar: %d mensajes)', [Chat.Messages.Count]));
        Writeln;
        Charlar(Chat, SESION_2);
      finally
        Chat.Free;
      end;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Memoria.Free;
    Analizador.Free;
    Embedder.Free;
  end;
end.
