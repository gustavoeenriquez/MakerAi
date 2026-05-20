program ClaudeChatTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude;

type
  TTestConfig = record
    Name: string;
    ResponseFormat: string;
    ChatMedia: string;
    NativeOutput: string;
    JsonSchema: string;
    Thinking: string;
  end;

var
  CCfg: TTestConfig;

procedure ConfigureBase(A: TAiChatConnection);
begin
  A.DriverName := 'Claude';
  A.Model := 'claude-sonnet-4-5-20250929';

  A.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
  A.Params.Values['Url'] := 'https://api.anthropic.com/v1/';
  A.Params.Values['Max_Tokens'] := '16000';
  A.Params.Values['Temperature'] := '0.7';
  A.Params.Values['ResponseTimeOut'] := '600000';
  A.Params.Values['ThinkingLevel'] := 'tlDefault';

  A.Params.Values['NativeInputFiles'] := '[Tfc_Image, Tfc_Pdf]';
  A.Params.Values['NativeOutputFiles'] := '[]';
  A.Params.Values['ChatMediaSupports'] := '[Tcm_Image, Tcm_Pdf]';
  A.Params.Values['Asynchronous'] := 'False';
  A.Params.Values['Tool_Active'] := 'False';
end;

function RunSyncTest(const Cfg: TTestConfig; const Prompt: string;
                     Attachments: array of string): string;
var
  Ai: TAiChatConnection;
  MF: TAiMediaFile;
  Res: string;
  I: Integer;
  MediaList: TAiMediaFilesArray;
  FileName: string;
begin
  Writeln;
  Writeln('==============================');
  Writeln('PRUEBA: ', Cfg.Name);
  Writeln('PROMPT: ', Prompt);
  Writeln('==============================');

  // --------------------------------------------------------------
  // VALIDAR ARCHIVOS ANTES DE EJECUTAR LA PRUEBA
  // --------------------------------------------------------------
  for I := 0 to High(Attachments) do
  begin
    FileName := Attachments[I];

    if not FileExists(FileName) then
    begin
      Writeln('⚠ ADVERTENCIA: El archivo requerido para esta prueba NO existe.');
      Writeln('  Ruta: ', FileName);
      Writeln('  ==> Esta prueba se omitirá.');
      Exit('Archivo no encontrado: ' + FileName);
    end;
  end;

  // --------------------------------------------------------------
  // SI LOS ARCHIVOS EXISTEN → EJECUTAMOS LA PRUEBA NORMALMENTE
  // --------------------------------------------------------------

  Ai := TAiChatConnection.Create(nil);
  try
    // Configuración base estándar
    ConfigureBase(Ai);

    Ai.Params.Values['Response_format'] := Cfg.ResponseFormat;

    if Cfg.ChatMedia <> '' then
      Ai.Params.Values['ChatMediaSupports'] := Cfg.ChatMedia;

    if Cfg.NativeOutput <> '' then
      Ai.Params.Values['NativeOutputFiles'] := Cfg.NativeOutput;

    if Cfg.JsonSchema <> '' then
      Ai.Params.Values['JsonSchema'] := Cfg.JsonSchema;

    if Cfg.Thinking <> '' then
      Ai.Params.Values['ThinkingLevel'] := Cfg.Thinking;

    // ------------------------------
    // Manejo de archivos adjuntos
    // ------------------------------
    if Length(Attachments) > 0 then
    begin
      SetLength(MediaList, Length(Attachments));

      for I := 0 to High(Attachments) do
      begin
        MF := TAiMediaFile.Create;
        MF.LoadFromFile(Attachments[I]);
        MediaList[I] := MF;
      end;

      Res := Ai.AddMessageAndRun(Prompt, 'user', MediaList);
    end
    else
      Res := Ai.AddMessageAndRun(Prompt, 'user', []);

  finally
    Ai.Free;
  end;

  Writeln('RESPUESTA:');
  Writeln(Res);

  Result := Res;
end;


//
// =======================================
// PRUEBAS DEL LOG (SIN SHELL/EDIT/FUNC)
// =======================================
//

/// ///////////////////////
// CHAT BÁSICO
/// ///////////////////////

procedure Test_Chat1;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'CHAT BASICO 1';
  Cfg.ResponseFormat := 'tiaChatRfText';
  RunSyncTest(Cfg, 'hola, cómo estas hoy?', []);
end;


/// ///////////////////////
// JSON
/// ///////////////////////

procedure Test_JSON1;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'JSON FORMAT 1';
  Cfg.ResponseFormat := 'tiaChatRfJson';
  RunSyncTest(Cfg, 'genera un json de prueba de 3 lineas', []);
end;


/// ///////////////////////
// JSON SCHEMA
/// ///////////////////////

procedure Test_JSONSchema1;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'JSON SCHEMA 1';
  Cfg.ResponseFormat := 'tiaChatRfJsonSchema';
  Cfg.JsonSchema := '{ "type":"object","properties":{"nombre":{"type":"string"},"edad":{"type":"integer"},"email":{"type":"string","format":"email"}},"required":["nombre","email"] }';

  RunSyncTest(Cfg, 'genera un json de prueba', []);
end;


/// ///////////////////////
// WEB SEARCH
/// ///////////////////////

procedure Test_WebSearch1;
var
  Cfg: TTestConfig;
begin
  CCfg.Name := 'WEB SEARCH 1';
  Cfg.ResponseFormat := 'tiaChatRfText';
  Cfg.ChatMedia := '[Tcm_WebSearch, Tcm_Image, Tcm_Pdf]';

  RunSyncTest(Cfg, 'busca en internet sobre MakerAi Delphi Suite', []);
end;


/// ///////////////////////
// CODE INTERPRETER
/// ///////////////////////

procedure Test_CodeInterpreter1;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'CODE INTERPRETER 1';
  Cfg.ResponseFormat := 'tiaChatRfText';
  Cfg.ChatMedia := '[Tcm_code_interpreter, Tcm_Image, Tcm_Pdf]';

  RunSyncTest(Cfg, 'Genera un archivo .wav con un tono de 1500 hz', []);
end;


/// ///////////////////////
// EXTRACT TEXT
/// ///////////////////////

procedure Test_ExtractText1;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'EXTRACT TEXT FILE 1';
  Cfg.ResponseFormat := 'tiaChatRfText';
  Cfg.NativeOutput := '[tfc_ExtracttextFile]';

  RunSyncTest(Cfg, 'genera un html básico, un hola mundo', []);
end;

procedure Test_ExtractText2;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'EXTRACT TEXT FILE 2';
  Cfg.ResponseFormat := 'tiaChatRfText';
  Cfg.NativeOutput := '[tfc_ExtracttextFile]';

  RunSyncTest(Cfg, 'genera un código en pascal, un hola mundo en modo consola', []);
end;

/// ///////////////////////
// VISION
/// ///////////////////////

procedure Test_VisionImage;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'VISION IMAGEN';
  Cfg.ResponseFormat := 'tiaChatRfText';
  RunSyncTest(Cfg, 'describe esta imagen', ['medios/patito.png']);
end;

procedure Test_VisionPDF;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'VISION PDF';
  Cfg.ResponseFormat := 'tiaChatRfText';
  RunSyncTest(Cfg, 'extrae el contenido de esta factura', ['medios/factura.pdf']);
end;

procedure Test_VisionPDFJson;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'VISION PDF JSON';
  Cfg.ResponseFormat := 'tiaChatRfJson';
  Cfg.NativeOutput := '[tfc_ExtracttextFile]';

  RunSyncTest(Cfg, 'extrae el contenido de esta factura en un json', ['medios/factura.pdf']);
end;

/// ///////////////////////
// THINKING
/// ///////////////////////

procedure Test_Thinking;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'THINKING HIGH';
  Cfg.ResponseFormat := 'tiaChatRfText';
  Cfg.Thinking := 'tlHigh';

  RunSyncTest(Cfg, 'si tengo 10 velas y las enciendo todas...', []);
end;

/// ///////////////////////
// AGRADECIMIENTOS
/// ///////////////////////

procedure Test_Agradecimientos;
var
  Cfg: TTestConfig;
begin
  Cfg.Name := 'AGRADECIMIENTOS';
  Cfg.ResponseFormat := 'tiaChatRfText';

  RunSyncTest(Cfg, 'agradece a los que están viendo este video por vernos y llegar hasta el final', []);
end;


//
// ======================
// EJECUCIÓN GENERAL
// ======================
//

begin
  try
    Writeln('=== EJECUTANDO PRUEBAS ===');

    Test_Chat1;

    Test_JSON1;

    Test_JSONSchema1;

    Test_WebSearch1;

    Test_CodeInterpreter1;

    Test_ExtractText1;
    Test_ExtractText2;

    Test_VisionImage;
    Test_VisionPDF;
    Test_VisionPDFJson;

    Test_Thinking;
    Test_Agradecimientos;

    Writeln('');
    Writeln('=== FIN DE PRUEBAS ===');
    Readln;

  except
    on E: Exception do
      Writeln('ERROR: ', E.Message);
  end;

end.
