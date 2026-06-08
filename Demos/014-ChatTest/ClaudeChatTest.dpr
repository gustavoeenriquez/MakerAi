program ClaudeChatTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  System.Net.HttpClient,
  System.NetEncoding,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.MakerAi,
  uMakerAi.Chat.Groq,
  uMakerAi.Chat.Initializations;

const
  CPdfAgentes = 'medios\agentes.pdf';
  CPdfRag     = 'medios\rag.pdf';

// ─── Helper async ────────────────────────────────────────────────────────────

type
  TAsyncHelper = class
  public
    FDone:  TEvent;
    FResult: string;
    FError:  string;
    constructor Create;
    destructor  Destroy; override;
    procedure Reset;
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: string);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);
    function WaitAsync: string;
  end;

constructor TAsyncHelper.Create;
begin
  inherited;
  FDone := TEvent.Create(nil, True, False, '');
end;

destructor TAsyncHelper.Destroy;
begin
  FDone.Free;
  inherited;
end;

procedure TAsyncHelper.Reset;
begin
  FResult := '';
  FError  := '';
  FDone.ResetEvent;
end;

procedure TAsyncHelper.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  Write(aText);
end;

procedure TAsyncHelper.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  FResult := aText;
  FDone.SetEvent;
end;

procedure TAsyncHelper.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  if Assigned(AResponse) and (AResponse.StatusCode > 0) then
    FError := 'HTTP=' + IntToStr(AResponse.StatusCode) + ' | ' + ErrorMsg
  else
    FError := ErrorMsg;
  if Assigned(AException) then FError := FError + ' | ' + AException.Message;
  FDone.SetEvent;
end;

function TAsyncHelper.WaitAsync: string;
begin
  while FDone.WaitFor(0) <> wrSignaled do
    CheckSynchronize(100);
  if FError <> '' then Result := 'ERROR: ' + FError
  else Result := FResult;
end;

// ─── Helpers ─────────────────────────────────────────────────────────────────

function MakePdf(const APath: string): TAiMediaFilesArray;
var M: TAiMediaFile;
begin
  M := TAiMediaFile.Create;
  M.LoadFromFile(APath);
  SetLength(Result, 1);
  Result[0] := M;
end;

procedure PrintHeader(const ALabel: string);
begin
  WriteLn;
  WriteLn('==============================');
  WriteLn(ALabel);
  WriteLn('==============================');
end;

// ─── SECCION 1: Claude PDF (sync) ────────────────────────────────────────────

procedure ConfigureClaude(A: TAiChatConnection);
begin
  A.DriverName := 'Claude';
  A.Model      := 'claude-sonnet-4-6';
  A.Params.Values['ApiKey']          := '@CLAUDE_API_KEY';
  A.Params.Values['Max_Tokens']      := '8000';
  A.Params.Values['Temperature']     := '0.7';
  A.Params.Values['Asynchronous']    := 'False';
  A.Params.Values['Tool_Active']     := 'False';
  A.Params.Values['ModelCaps']       := '[cap_Image, cap_Pdf]';
  A.Params.Values['SessionCaps']     := '[cap_Image, cap_Pdf]';
end;

procedure Test_ClaudePdf_Agentes;
var
  Ai:  TAiChatConnection;
  Res: string;
begin
  PrintHeader('CLAUDE — PDF Agentes (sync)');
  if not FileExists(CPdfAgentes) then begin WriteLn('Archivo no encontrado: ' + CPdfAgentes); Exit; end;

  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureClaude(Ai);
    Res := Ai.AddMessageAndRun(
      'Qué es TAIAgentManager y cuál es su función principal? Responde en 2 oraciones.',
      'user', MakePdf(CPdfAgentes));
    WriteLn(Res);
  finally
    Ai.Free;
  end;
end;

procedure Test_ClaudePdf_Rag;
var
  Ai:  TAiChatConnection;
  Res: string;
begin
  PrintHeader('CLAUDE — PDF RAG (sync)');
  if not FileExists(CPdfRag) then begin WriteLn('Archivo no encontrado: ' + CPdfRag); Exit; end;

  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureClaude(Ai);
    Res := Ai.AddMessageAndRun(
      'Lista los tipos de RAG que soporta MakerAI según el documento.',
      'user', MakePdf(CPdfRag));
    WriteLn(Res);
  finally
    Ai.Free;
  end;
end;

procedure Test_ClaudePdf_MultiTurn;
var
  Ai:  TAiChatConnection;
  Res: string;
begin
  PrintHeader('CLAUDE — PDF multi-turno (sync)');
  if not FileExists(CPdfAgentes) then begin WriteLn('Archivo no encontrado: ' + CPdfAgentes); Exit; end;

  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureClaude(Ai);
    WriteLn('--- Turno 1 ---');
    Res := Ai.AddMessageAndRun('Qué es TAIBlackboard?', 'user', MakePdf(CPdfAgentes));
    WriteLn(Res);
    WriteLn('--- Turno 2 ---');
    Res := Ai.AddMessageAndRun('Dame un ejemplo corto de uso de TAIBlackboard.', 'user', []);
    WriteLn(Res);
  finally
    Ai.Free;
  end;
end;

// ─── SECCION 2: MakerAI PDF ───────────────────────────────────────────────────

procedure SetMakerAiCaps(Chat: TAiMakerAiChat; const AModel: string);
begin
  Chat.ApiKey    := '@MAKERAI_API_KEY';
  Chat.Model     := AModel;
  Chat.Tool_Active := False;

  if SameText(AModel, 'mk-gpt-oss-20b') then
  begin
    Chat.ModelCaps   := [cap_Reasoning, cap_Image, cap_Pdf];
    Chat.SessionCaps := [cap_Reasoning, cap_Image, cap_Pdf];
  end
  else if SameText(AModel, 'mk-basic-8b') then
  begin
    Chat.ModelCaps   := [cap_Pdf];
    Chat.SessionCaps := [cap_Pdf];
  end
  else // mk-scout, mk-pro, etc.
  begin
    Chat.ModelCaps   := [cap_Image, cap_Pdf];
    Chat.SessionCaps := [cap_Image, cap_Pdf];
  end;
end;

procedure OnMakerAiProgress(AStep, AFile: string; APct: Integer);
begin
  WriteLn(Format('  [progreso] %-10s  %s  %d%%', [AStep, AFile, APct]));
end;

procedure Test_MakerAiPdf_Sync;
var
  Chat: TAiMakerAiChat;
  Res:  string;
  H:    TAsyncHelper;
begin
  PrintHeader('MAKERAI mk-scout — PDF Agentes (sync)');
  if not FileExists(CPdfAgentes) then begin WriteLn('Archivo no encontrado: ' + CPdfAgentes); Exit; end;

  H := TAsyncHelper.Create;
  Chat := TAiMakerAiChat.Create(nil);
  try
    SetMakerAiCaps(Chat, 'mk-scout');
    Chat.Asynchronous := False;
    Chat.OnError      := H.OnError;
    Chat.OnProgress   := OnMakerAiProgress;
    H.Reset;
    Res := Chat.AddMessageAndRun(
      'Qué es TAIAgentManager y cuál es su función principal? Responde en 2 oraciones.',
      'user', MakePdf(CPdfAgentes));
    if H.FError <> '' then WriteLn('ERROR: ' + H.FError)
    else WriteLn(Res);
  finally
    Chat.Free;
    H.Free;
  end;
end;

procedure Test_MakerAiPdf_Async;
var
  Chat: TAiMakerAiChat;
  Res:  string;
  H:    TAsyncHelper;
begin
  PrintHeader('MAKERAI mk-scout — PDF RAG (async)');
  if not FileExists(CPdfRag) then begin WriteLn('Archivo no encontrado: ' + CPdfRag); Exit; end;

  H := TAsyncHelper.Create;
  Chat := TAiMakerAiChat.Create(nil);
  try
    SetMakerAiCaps(Chat, 'mk-scout');
    Chat.Asynchronous     := True;
    Chat.OnReceiveData    := H.OnData;
    Chat.OnReceiveDataEnd := H.OnDataEnd;
    Chat.OnError          := H.OnError;
    Chat.OnProgress       := OnMakerAiProgress;
    H.Reset;
    Chat.AddMessageAndRun(
      'Lista los tipos de RAG que soporta MakerAI según el documento.',
      'user', MakePdf(CPdfRag));
    Res := H.WaitAsync;
    WriteLn;
    if Res.StartsWith('ERROR:') then WriteLn(Res);
  finally
    Chat.Free;
    H.Free;
  end;
end;

procedure Test_MakerAiPdf_MultiTurn;
var
  Chat: TAiMakerAiChat;
  Res:  string;
  H:    TAsyncHelper;
begin
  PrintHeader('MAKERAI mk-gpt-oss-20b — PDF multi-turno (async)');
  if not FileExists(CPdfAgentes) then begin WriteLn('Archivo no encontrado: ' + CPdfAgentes); Exit; end;

  H := TAsyncHelper.Create;
  Chat := TAiMakerAiChat.Create(nil);
  try
    SetMakerAiCaps(Chat, 'mk-gpt-oss-20b');
    Chat.Asynchronous     := True;
    Chat.OnReceiveData    := H.OnData;
    Chat.OnReceiveDataEnd := H.OnDataEnd;
    Chat.OnError          := H.OnError;
    Chat.OnProgress       := OnMakerAiProgress;

    // Turno 1: con PDF
    WriteLn('--- Turno 1 (con PDF) ---');
    H.Reset;
    Chat.AddMessageAndRun('Qué es TAIBlackboard y para qué se usa?',
      'user', MakePdf(CPdfAgentes));
    Res := H.WaitAsync;
    WriteLn;
    if Res.StartsWith('ERROR:') then begin WriteLn(Res); Exit; end;

    // Turno 2: sin PDF (misma sesión → session_id preservado)
    WriteLn;
    WriteLn('--- Turno 2 (sin PDF, follow-up) ---');
    H.Reset;
    Chat.AddMessageAndRun('Dame un ejemplo corto de uso de TAIBlackboard.', 'user', []);
    Res := H.WaitAsync;
    WriteLn;
    if Res.StartsWith('ERROR:') then WriteLn(Res)
    else if Res.IsEmpty then WriteLn('[RESPUESTA VACIA]');
  finally
    Chat.Free;
    H.Free;
  end;
end;

// ─── SECCION 3: Groq gpt-oss-20b + code_interpreter ─────────────────────────

const
  // Groq compound-beta: ejecuta codigo en sandbox, output en reasoning_content (<output>).
  // Max_Tokens=8192 limita solo el content visible; reasoning_content no tiene esa restriccion.
  CPromptWav = 'Execute this exact Python code right now and show me the output:'
    + #10'```python'
    + #10'import numpy as np, struct, io, base64'
    + #10'sr=44100; dur=1; freq=333'
    + #10't=np.linspace(0,dur,int(sr*dur),False)'
    + #10'samples=(np.sin(2*np.pi*freq*t)*32767).astype("int16")'
    + #10'buf=io.BytesIO()'
    + #10'n=len(samples)'
    + #10'buf.write(b"RIFF"); buf.write(struct.pack("<I",36+n*2))'
    + #10'buf.write(b"WAVE"); buf.write(b"fmt ")'
    + #10'buf.write(struct.pack("<IHHIIHH",16,1,1,sr,sr*2,2,16))'
    + #10'buf.write(b"data"); buf.write(struct.pack("<I",n*2))'
    + #10'buf.write(samples.tobytes()); buf.seek(0)'
    + #10'print("WAV_B64_BEGIN")'
    + #10'print(base64.b64encode(buf.read()).decode())'
    + #10'print("WAV_B64_END")'
    + #10'```';

  // gpt-oss-20b: code_interpreter via tool call.
  // El base64 cae en executed_tools[].output (stdout del sandbox).
  // El modelo DEBE ejecutar el código sin modificarlo; decirle "Done" evita que
  // duplique el base64 en su respuesta de texto (ahorra tokens de content).
  // dur=1 → ~29K tokens output, cabe en Max_Tokens=65536 de gpt-oss-20b.
  // dur=3 → ~88K tokens output, excede el límite → context_length_exceeded.
  CPromptWav3s =
    'Use the code_interpreter tool to run the following Python code exactly as written. '
    + 'Do not modify the code in any way — it must execute the print statements as provided. '
    + 'After the code runs successfully, reply with only the word: Done'
    + #10'```python'
    + #10'import numpy as np, struct, io, base64'
    + #10'sr=44100; dur=1; freq=333'
    + #10't=np.linspace(0,dur,int(sr*dur),False)'
    + #10'samples=(np.sin(2*np.pi*freq*t)*32767).astype("int16")'
    + #10'buf=io.BytesIO()'
    + #10'n=len(samples)'
    + #10'buf.write(b"RIFF"); buf.write(struct.pack("<I",36+n*2))'
    + #10'buf.write(b"WAVE"); buf.write(b"fmt ")'
    + #10'buf.write(struct.pack("<IHHIIHH",16,1,1,sr,sr*2,2,16))'
    + #10'buf.write(b"data"); buf.write(struct.pack("<I",n*2))'
    + #10'buf.write(samples.tobytes()); buf.seek(0)'
    + #10'print("WAV_B64_BEGIN")'
    + #10'print(base64.b64encode(buf.read()).decode())'
    + #10'print("WAV_B64_END")'
    + #10'```';

type
  TGroqHelper = class
  public
    FDone:       TEvent;
    FResult:     string;
    FError:      string;
    FSavedFiles: TStringList;  // paths of saved output files
    constructor Create;
    destructor  Destroy; override;
    procedure Reset;
    procedure OnData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: string);
    procedure OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSonObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);
    function WaitAsync: string;
    procedure PrintResults;
  end;

constructor TGroqHelper.Create;
begin
  inherited;
  FDone       := TEvent.Create(nil, True, False, '');
  FSavedFiles := TStringList.Create;
end;

destructor TGroqHelper.Destroy;
begin
  FDone.Free;
  FSavedFiles.Free;
  inherited;
end;

procedure TGroqHelper.Reset;
begin
  FResult := '';
  FError  := '';
  FSavedFiles.Clear;
  FDone.ResetEvent;
end;

procedure TGroqHelper.OnData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
begin
  Write(aText);
end;

procedure TGroqHelper.OnDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSonObject; aRole, aText: string);
var
  MF: TAiMediaFile;
  OutPath: string;
begin
  FResult := aText;
  if Assigned(aMsg) then
    for MF in aMsg.MediaFiles do
    begin
      OutPath := ExtractFilePath(ParamStr(0)) + MF.FileName;
      try
        if FSavedFiles.IndexOf(OutPath) < 0 then
        begin
          MF.SaveToFile(OutPath);
          FSavedFiles.Add(OutPath);
        end;
      except
        on E: Exception do
          FSavedFiles.Add('ERROR guardando ' + MF.FileName + ': ' + E.Message);
      end;
    end;
  FDone.SetEvent;
end;

procedure TGroqHelper.OnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
begin
  if Assigned(AResponse) and (AResponse.StatusCode > 0) then
    FError := 'HTTP=' + IntToStr(AResponse.StatusCode) + ' | ' + ErrorMsg
  else
    FError := ErrorMsg;
  if Assigned(AException) then FError := FError + ' | ' + AException.Message;
  FDone.SetEvent;
end;

function TGroqHelper.WaitAsync: string;
begin
  while FDone.WaitFor(0) <> wrSignaled do
    CheckSynchronize(100);
  if FError <> '' then Result := 'ERROR: ' + FError
  else Result := FResult;
end;

procedure TGroqHelper.PrintResults;
var
  I: Integer;
  Preview: string;
begin
  // Mostrar texto del modelo (si hay)
  if FResult <> '' then
  begin
    Preview := StringReplace(Copy(FResult, 1, 3000), #10, ' ', [rfReplaceAll]);
    Preview := StringReplace(Preview, #13, '', [rfReplaceAll]);
    WriteLn('  Texto: ' + Preview);
    if Length(FResult) > 3000 then WriteLn('  ...[truncado ' + IntToStr(Length(FResult)) + ' chars]');
  end
  else
    WriteLn('  Texto: (vacio)');
  // Mostrar estado de archivos
  if FError <> '' then
    WriteLn('  ERROR: ' + FError)
  else if FSavedFiles.Count = 0 then
    WriteLn('  Archivos: (sin archivos ejecutados)')
  else
    for I := 0 to FSavedFiles.Count - 1 do
      WriteLn('  Archivo guardado: ' + FSavedFiles[I]);
end;

// gpt-oss-20b: scatter plot 200x200 JPG via matplotlib.
// Usa FILE_B64_BEGIN:nombre.jpg / FILE_B64_END para que ProcessExecutedTools lo detecte.
const
  CPromptScatterPlot =
    'Use the code_interpreter tool to run the following Python code exactly as written. '
    + 'Do not modify the code in any way. '
    + 'After the code runs successfully, reply with only the word: Done'
    + #10'```python'
    + #10'import numpy as np, io, base64'
    + #10'import matplotlib'
    + #10'matplotlib.use("Agg")'
    + #10'import matplotlib.pyplot as plt'
    + #10'np.random.seed(42)'
    + #10'x = np.random.uniform(0, 1, 20)'
    + #10'y = np.random.uniform(0, 1, 20)'
    + #10'colors = plt.cm.rainbow(np.linspace(0, 1, 20))'
    + #10'fig, ax = plt.subplots(figsize=(2, 2), dpi=100)'
    + #10'ax.scatter(x, y, c=colors, s=60)'
    + #10'ax.set_xlabel("X"); ax.set_ylabel("Y"); ax.set_title("Scatter")'
    + #10'fig.tight_layout()'
    + #10'buf = io.BytesIO()'
    + #10'plt.savefig(buf, format="jpeg", dpi=100)'
    + #10'buf.seek(0)'
    + #10'print("FILE_B64_BEGIN:scatter.jpg")'
    + #10'print(base64.b64encode(buf.read()).decode())'
    + #10'print("FILE_B64_END")'
    + #10'plt.close()'
    + #10'```';

// gpt-oss-20b: modelo de razonamiento; embede archivos como base64 en texto (NO usa executed_tools).
// Para recibir archivos via executed_tools usar groq/compound o groq/compound-mini.
procedure ConfigureGroqCodeInterpreter(A: TAiChatConnection);
begin
  A.DriverName := 'Groq';
  A.Model      := 'groq/compound';  // usa executed_tools con archivos descargables
  A.Params.Values['ApiKey']          := '@GROQ_API_KEY';
  A.Params.Values['ResponseTimeOut'] := '120000';
  // Sin Temperature: compound-beta es agentico y puede ignorar/rechazar este param
end;

procedure Test_Groq_CodeInterpreter_Sync;
var
  Ai: TAiChatConnection;
  H:  TGroqHelper;
  Res: string;
begin
  PrintHeader('GROQ gpt-oss-20b — code_interpreter SYNC');
  H  := TGroqHelper.Create;
  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureGroqCodeInterpreter(Ai);
    Ai.Params.Values['Asynchronous'] := 'False';
    Ai.OnReceiveDataEnd := H.OnDataEnd;
    Ai.OnError          := H.OnError;
    H.Reset;
    WriteLn('Enviando (sync)...');
    Res := Ai.AddMessageAndRun(CPromptWav, 'user', []);
    WriteLn;
    WriteLn('Texto del modelo:');
    if Res <> '' then WriteLn(Res);
    WriteLn;
    H.PrintResults;
  finally
    Ai.Free;
    H.Free;
  end;
end;

procedure Test_Groq_CodeInterpreter_Async;
var
  Ai: TAiChatConnection;
  H:  TGroqHelper;
begin
  PrintHeader('GROQ gpt-oss-20b — code_interpreter ASYNC (streaming)');
  H  := TGroqHelper.Create;
  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureGroqCodeInterpreter(Ai);
    Ai.Params.Values['Asynchronous'] := 'True';
    Ai.OnReceiveData    := H.OnData;
    Ai.OnReceiveDataEnd := H.OnDataEnd;
    Ai.OnError          := H.OnError;
    H.Reset;
    WriteLn('Enviando (async)...');
    Ai.AddMessageAndRun(CPromptWav, 'user', []);
    H.WaitAsync;
    WriteLn;
    WriteLn;
    H.PrintResults;
  finally
    Ai.Free;
    H.Free;
  end;
end;

// gpt-oss-20b: reasoning + code_interpreter via tool call (Tool_Active=True)
// executed_tools[].code_interpreter.outputs[].text contiene el stdout del codigo.
procedure ConfigureGroqGptOss20b(A: TAiChatConnection);
begin
  A.DriverName := 'Groq';
  A.Model      := 'openai/gpt-oss-20b';
  A.Params.Values['ApiKey']          := '@GROQ_API_KEY';
  A.Params.Values['ResponseTimeOut'] := '180000';
end;

procedure Test_Groq_GptOss20b_Sync;
var
  Ai: TAiChatConnection;
  H:  TGroqHelper;
  Res: string;
begin
  PrintHeader('GROQ openai/gpt-oss-20b — code_interpreter 3s SYNC');
  H  := TGroqHelper.Create;
  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureGroqGptOss20b(Ai);
    Ai.Params.Values['Asynchronous'] := 'False';
    Ai.OnReceiveDataEnd := H.OnDataEnd;
    Ai.OnError          := H.OnError;
    H.Reset;
    WriteLn('Enviando (sync, dur=3s)...');
    Res := Ai.AddMessageAndRun(CPromptWav3s, 'user', []);
    WriteLn;
    WriteLn('Texto del modelo:');
    if Res <> '' then WriteLn(Copy(Res, 1, 500));
    WriteLn;
    H.PrintResults;
  finally
    Ai.Free;
    H.Free;
  end;
end;

procedure Test_Groq_GptOss20b_ScatterPlot;
var
  Ai: TAiChatConnection;
  H:  TGroqHelper;
  Res: string;
begin
  PrintHeader('GROQ openai/gpt-oss-20b — ScatterPlot 200x200 JPG (SYNC)');
  H  := TGroqHelper.Create;
  Ai := TAiChatConnection.Create(nil);
  try
    ConfigureGroqGptOss20b(Ai);
    Ai.Params.Values['Asynchronous'] := 'False';
    Ai.OnReceiveDataEnd := H.OnDataEnd;
    Ai.OnError          := H.OnError;
    H.Reset;
    WriteLn('Enviando (sync)...');
    Res := Ai.AddMessageAndRun(CPromptScatterPlot, 'user', []);
    WriteLn;
    WriteLn('Texto del modelo:');
    if Res <> '' then WriteLn(Copy(Res, 1, 200));
    WriteLn;
    H.PrintResults;
  finally
    Ai.Free;
    H.Free;
  end;
end;

// ─── Main ─────────────────────────────────────────────────────────────────────

begin
  try
    WriteLn('=== PRUEBAS Groq code_interpreter ===');

    WriteLn;
    WriteLn('████ SECCION 3: Groq code_interpreter ████');
    Test_Groq_GptOss20b_ScatterPlot;  // gpt-oss-20b, scatter plot 200x200 JPG

    WriteLn;
    WriteLn('=== FIN DE PRUEBAS ===');

  except
    on E: Exception do
      WriteLn('FATAL: ' + E.ClassName + ': ' + E.Message);
  end;
end.
