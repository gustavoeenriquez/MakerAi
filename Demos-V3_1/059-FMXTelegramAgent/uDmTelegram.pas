unit uDmTelegram;

// =============================================================================
// MakerAI - Demo 059: uDmTelegram
//
// Telegram-specific DataModule. Owns FastTelega (TftBot + TftLongPoll) and
// the TAgentManager that manages one TDmAgent per user (chat_id).
//
// Design-time: TAiPrompts with bot text messages (welcome, help).
// Code-created: TftBot, polling thread, TAgentManager.
//
// Wiring in uFrmTelegramAgent:
//   DmTelegram.OnLog := procedure(dir, text) -> MemoLog.Lines.Add(...)
//   DmTelegram.Start('@TELEGRAM_BOT_TOKEN');
// =============================================================================

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils, System.Math,
  System.Generics.Collections, System.IOUtils,
  System.Net.HttpClient,
  fastTelega.Bot,
  fastTelega.LongPoll,
  fastTelega.AvailableTypes,
  uMakerAi.Prompts,
  uDMAgent;

const
  TG_API_BASE = 'https://api.telegram.org';
  TG_API      = 'https://api.telegram.org/bot';
  TG_FILE_API = 'https://api.telegram.org/file/bot';

type
  TDmTelegram = class(TDataModule)
    BotPrompts: TAiPrompts;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FBot     : TftBot;
    FThread  : TThread;
    FToken   : string;
    FDataDir : string;
    FManager : TAgentManager;
    FOnLog   : TAgentLogEvent;

    // FastTelega event handlers
    procedure OnCmdStart(const Obj: TObject);
    procedure OnCmdHelp (const Obj: TObject);
    procedure OnCmdClear(const Obj: TObject);
    procedure OnMessage (const Obj: TObject);

    // Helpers
    function  ResolveToken(const AToken: string): string;
    procedure Send(AChatId: Integer; const AText: string);
    procedure SendAudio(AChatId: Integer; const ABase64, AFormat: string);
    procedure DoLog(const ADir, AText: string);
  public
    procedure Start(const AToken: string);
    procedure Stop;
    function  IsRunning: Boolean;
    function  AgentCount: Integer;

    property DataDir : string         read FDataDir write FDataDir;
    property OnLog   : TAgentLogEvent read FOnLog   write FOnLog;
  end;

var
  DmTelegram: TDmTelegram;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses System.Net.Mime, System.NetEncoding, System.Net.HttpClientComponent;

// =============================================================================
// Lifecycle
// =============================================================================

procedure TDmTelegram.DataModuleCreate(Sender: TObject);
begin
  FDataDir := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'MakerAI');
end;

procedure TDmTelegram.DataModuleDestroy(Sender: TObject);
begin
  Stop;
end;

// =============================================================================
// Start / Stop
// =============================================================================

procedure TDmTelegram.Start(const AToken: string);
begin
  if IsRunning then Stop;

  FToken := ResolveToken(AToken);
  if FToken = '' then
  begin
    DoLog('ERR', 'Token is empty or env var not set');
    Exit;
  end;

  FManager := TAgentManager.Create(FDataDir);

  FBot := TftBot.Create(FToken, TG_API_BASE);
  FBot.Events.OnCommand('start', OnCmdStart);
  FBot.Events.OnCommand('help',  OnCmdHelp);
  FBot.Events.OnCommand('clear', OnCmdClear);
  FBot.Events.OnAnyMessage(OnMessage);

  FThread := TThread.CreateAnonymousThread(procedure
  var
    LP: TftLongPoll;
  begin
    LP := TftLongPoll.Create(FBot, 100, 3, nil);
    try
      while not TThread.CheckTerminated do
      try
        LP.Start;
      except
        on E: Exception do
          DoLog('ERR', '[POLL] ' + E.ClassName + ': ' + E.Message);
      end;
    finally
      LP.Free;
    end;
  end);
  FThread.FreeOnTerminate := False;
  FThread.Start;

  DoLog('SYS', 'Bot started | token=' + Copy(FToken, 1, 10) + '...');
end;

procedure TDmTelegram.Stop;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  FreeAndNil(FBot);
  FreeAndNil(FManager);
  DoLog('SYS', 'Bot stopped');
end;

function TDmTelegram.IsRunning: Boolean;
begin
  Result := Assigned(FThread) and not FThread.Finished;
end;

function TDmTelegram.AgentCount: Integer;
begin
  Result := IfThen(Assigned(FManager), FManager.Count, 0);
end;

// =============================================================================
// Telegram command handlers
// =============================================================================

procedure TDmTelegram.OnCmdStart(const Obj: TObject);
var
  Msg: TftMessage;
begin
  if not (Obj is TftMessage) then Exit;
  Msg := TftMessage(Obj);
  Send(Msg.Chat.Id, BotPrompts.GetString('welcome'));
end;

procedure TDmTelegram.OnCmdHelp(const Obj: TObject);
var
  Msg: TftMessage;
begin
  if not (Obj is TftMessage) then Exit;
  Msg := TftMessage(Obj);
  Send(Msg.Chat.Id, BotPrompts.GetString('help'));
end;

procedure TDmTelegram.OnCmdClear(const Obj: TObject);
var
  Msg   : TftMessage;
  Agent : TDmAgent;
  Id    : string;
begin
  if not (Obj is TftMessage) then Exit;
  Msg := TftMessage(Obj);
  Id  := IntToStr(Msg.Chat.Id);
  if FManager.TryGet(Id, Agent) then
    Agent.ClearHistory;
  FManager.Remove(Id);   // force fresh agent on next message
  Send(Msg.Chat.Id, 'Chat history cleared.');
  DoLog('SYS', 'History cleared for chat_id=' + Id);
end;

procedure TDmTelegram.OnMessage(const Obj: TObject);
var
  Msg     : TftMessage;
  Text    : string;
  Response: string;
  Agent   : TDmAgent;
  Id      : string;
begin
  if not (Obj is TftMessage) then Exit;
  Msg := TftMessage(Obj);

  // Skip command messages — handled by OnCmd* above
  Text := Msg.Text;
  if (Text <> '') and (Text[1] = '/') then Exit;
  if Text = '' then Exit;

  Id    := IntToStr(Msg.Chat.Id);
  Agent := FManager.GetOrCreate(Id);

  // Wire log so each agent's log goes to our DoLog
  Agent.OnLog := DoLog;

  DoLog('IN', '[' + Id + '] ' + Copy(Text, 1, 80));

  try
    Response := Agent.ProcessText(Text);
  except
    on E: Exception do
    begin
      DoLog('ERR', 'Agent exception: ' + E.Message);
      Response := 'Internal error. Please try again.';
    end;
  end;

  // Send audio if the agent captured base64 audio from a TTS tool
  if Agent.AudioB64 <> '' then
    SendAudio(Msg.Chat.Id, Agent.AudioB64, Agent.AudioFormat);

  if Response = '' then Response := '(no response)';
  Send(Msg.Chat.Id, Response);
end;

// =============================================================================
// Helpers
// =============================================================================

function TDmTelegram.ResolveToken(const AToken: string): string;
begin
  if (Length(AToken) > 1) and (AToken[1] = '@') then
    Result := GetEnvironmentVariable(Copy(AToken, 2, MaxInt))
  else
    Result := AToken;
end;

procedure TDmTelegram.Send(AChatId: Integer; const AText: string);
var
  Txt: string;
begin
  Txt := AText;
  if Length(Txt) > 4000 then Txt := Copy(Txt, 1, 4000) + #10 + '[...truncated]';
  try
    FBot.API.sendMessage(AChatId, Txt);
  except
    on E: Exception do
      DoLog('ERR', 'sendMessage: ' + E.Message);
  end;
end;

procedure TDmTelegram.SendAudio(AChatId: Integer; const ABase64, AFormat: string);
var
  Http  : TNetHTTPClient;
  Parts : TMultipartFormData;
  Bytes : TBytes;
  Stream: TBytesStream;
  Ext   : string;
  Mime  : string;
begin
  if ABase64 = '' then Exit;
  Ext := LowerCase(AFormat);
  if Ext = '' then Ext := 'wav';
  case AnsiIndexText(Ext, ['mp3','wav','ogg','m4a','aac','flac','pcm']) of
    0: Mime := 'audio/mpeg';
    1: Mime := 'audio/wav';
    2: Mime := 'audio/ogg';
    3: Mime := 'audio/mp4';
    4: Mime := 'audio/aac';
    5: Mime := 'audio/flac';
    6: Mime := 'audio/wav';   // pcm → send as wav
  else  Mime := 'audio/octet-stream';
  end;
  try
    Bytes := TNetEncoding.Base64.DecodeStringToBytes(ABase64);
  except
    on E: Exception do
    begin
      DoLog('ERR', 'sendAudio base64 decode: ' + E.Message);
      Exit;
    end;
  end;
  Http   := TNetHTTPClient.Create(nil);
  Stream := TBytesStream.Create(Bytes);
  Parts  := TMultipartFormData.Create;
  try
    // sendVoice (MP3/OGG) → aparece como nota de voz; sendAudio para otros formatos
    var Endpoint: string;
    var FieldName: string;
    if AnsiIndexText(Ext, ['mp3','ogg','m4a']) >= 0 then
    begin
      Endpoint  := '/sendVoice';
      FieldName := 'voice';
    end
    else
    begin
      Endpoint  := '/sendAudio';
      FieldName := 'audio';
    end;
    Parts.AddField('chat_id', IntToStr(AChatId));
    Parts.AddStream(FieldName, Stream, 'voice.' + Ext, Mime);
    try
      Http.Post(TG_API + FToken + Endpoint, Parts);
      DoLog('SYS', Endpoint + ' OK: ' + (Length(Bytes) div 1024).ToString + ' KB');
    except
      on E: Exception do
        DoLog('ERR', 'sendAudio: ' + E.Message);
    end;
  finally
    Parts.Free;
    Stream.Free;
    Http.Free;
  end;
end;

procedure TDmTelegram.DoLog(const ADir, AText: string);
begin
  if Assigned(FOnLog) then FOnLog(ADir, AText);
end;

end.
