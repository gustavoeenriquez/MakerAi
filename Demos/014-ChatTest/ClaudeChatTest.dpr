program ClaudeChatTest;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.MakerAi,
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

// ─── Main ─────────────────────────────────────────────────────────────────────

begin
  try
    WriteLn('=== PRUEBAS PDF — Claude + MakerAI ===');

    // ── Claude ──
    WriteLn;
    WriteLn('████ SECCION 1: Claude PDF ████');
    Test_ClaudePdf_Agentes;
    Test_ClaudePdf_Rag;
    Test_ClaudePdf_MultiTurn;

    // ── MakerAI ──
    WriteLn;
    WriteLn('████ SECCION 2: MakerAI PDF ████');
    Test_MakerAiPdf_Sync;
    Test_MakerAiPdf_Async;
    Test_MakerAiPdf_MultiTurn;

    WriteLn;
    WriteLn('=== FIN DE PRUEBAS ===');

  except
    on E: Exception do
      WriteLn('FATAL: ' + E.ClassName + ': ' + E.Message);
  end;
end.
