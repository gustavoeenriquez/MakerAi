unit uApp.ChatBridge;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils,
  System.Net.HttpClient,
  AIChat.Control.FMX,
  AIChat.Input,
  AIChat.Types,
  uMakerAi.Core,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Prompts;

type
  TChatBridge = class
  private
    FConn           : TAiChatConnection;
    FFunctions      : TAiFunctions;
    FPrompts        : TAiPrompts;
    FChatView       : TAIChatView;
    FChatInput      : TAIChatInput;
    FCurrentMsgIndex: Integer;
    FCurrentConvId  : string;

    // Event signatures must match exactly the types declared in UMakerAi.Chat.pas
    // TAiChatOnDataEvent = procedure(const Sender: TObject; aMsg: TAiChatMessage;
    //   aResponse: TJSonObject; aRole, aText: String) of object;
    FOnThinking: TProc<string>;

    procedure OnReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure OnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    procedure OnConnThinking(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string);
    // TAiErrorEvent = procedure(Sender: TObject; const ErrorMsg: string;
    //   Exception: Exception; const AResponse: IHTTPResponse) of object;
    procedure OnConnError(Sender: TObject; const ErrorMsg: string;
      AException: Exception; const AResponse: IHTTPResponse);

    procedure RegisterFunctions;
    procedure FnGetDateTime(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnCalculate(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure FnGetWeather(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  public
    constructor Create(AChatView: TAIChatView; AChatInput: TAIChatInput);
    destructor  Destroy; override;

    procedure ApplySettings;
    procedure SendMessage(const AText: string;
      AMediaFiles: TAiMediaFiles; AAudio: TMemoryStream);
    procedure AbortGeneration;
    procedure NewChat;

    property Connection   : TAiChatConnection read FConn;
    property Prompts      : TAiPrompts        read FPrompts;
    property CurrentConvId: string            read FCurrentConvId write FCurrentConvId;
    property OnThinking   : TProc<string>     read FOnThinking    write FOnThinking;
  end;

implementation

uses
  System.IOUtils,
  uApp.Settings,
  uApp.ProviderList;

{ TChatBridge }

constructor TChatBridge.Create(AChatView: TAIChatView; AChatInput: TAIChatInput);
begin
  FChatView        := AChatView;
  FChatInput       := AChatInput;
  FCurrentMsgIndex := -1;

  FConn := TAiChatConnection.Create(nil);
  FConn.OnReceiveData     := OnReceiveData;
  FConn.OnReceiveDataEnd  := OnReceiveDataEnd;
  FConn.OnReceiveThinking := OnConnThinking;
  FConn.OnError           := OnConnError;

  FFunctions := TAiFunctions.Create(nil);
  RegisterFunctions;
  FConn.AiFunctions := FFunctions;

  FPrompts := TAiPrompts.Create(nil);
  FPrompts.AddString('Asistente',  'You are a helpful AI assistant. Answer clearly and concisely.');
  FPrompts.AddString('Programador','You are an expert software developer. Write clean, efficient code with brief explanations.');
  FPrompts.AddString('Redactor',   'You are a skilled writer. Produce clear, engaging text adapted to the requested style.');
  FPrompts.AddString('Analista',   'You are a data analyst. Provide structured, data-driven insights with clear summaries.');

  ApplySettings;
end;

destructor TChatBridge.Destroy;
begin
  FConn.Free;
  FFunctions.Free;
  FPrompts.Free;
  inherited;
end;

procedure TChatBridge.RegisterFunctions;
var
  LFn   : TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  LFn := FFunctions.Functions.AddFunction('get_datetime', False, FnGetDateTime);
  LFn.Description.Text :=
    'Returns the current date, time and weekday of the user''s system.';

  LFn := FFunctions.Functions.AddFunction('calculate', False, FnCalculate);
  LFn.Description.Text :=
    'Evaluates a mathematical expression and returns the result.';
  LParam := LFn.Parameters.Add;
  LParam.Name             := 'expression';
  LParam.ParamType        := ptString;
  LParam.Required         := True;
  LParam.Description.Text := 'Math expression, e.g. "2*(3+5)" or "sqrt(16)"';

  LFn := FFunctions.Functions.AddFunction('get_weather', False, FnGetWeather);
  LFn.Description.Text :=
    'Returns simulated weather information for a given city.';
  LParam := LFn.Parameters.Add;
  LParam.Name             := 'city';
  LParam.ParamType        := ptString;
  LParam.Required         := True;
  LParam.Description.Text := 'City name, e.g. "Monterrey" or "New York"';
end;

procedure TChatBridge.ApplySettings;
begin
  FConn.DriverName := AppSettings.ProviderName;
  FConn.Model      := AppSettings.ModelName;
  FConn.Params.Values['ApiKey']       := AppSettings.ApiKey[AppSettings.ProviderName];
  FConn.Params.Values['Asynchronous'] := 'True';
  FConn.SystemPrompt.Text := AppSettings.SystemPrompt;
  FChatView.ThemeDark    := AppSettings.ThemeDark;
  FChatInput.ThemeDark   := AppSettings.ThemeDark;
end;

procedure TChatBridge.SendMessage(const AText: string;
  AMediaFiles: TAiMediaFiles; AAudio: TMemoryStream);
var
  LHasFiles: Boolean;
begin
  if FConn.Busy then Exit;

  LHasFiles := Assigned(AMediaFiles) and (AMediaFiles.Count > 0);

  if AText <> '' then
  begin
    if LHasFiles then
      FChatView.AddUserMessage(AText, AMediaFiles)
    else
      FChatView.AddUserMessage(AText);
  end
  else if Assigned(AAudio) then
    FChatView.AddUserMessage('[Audio message]')
  else
    Exit;

  FChatView.BeginAssistantMessage;
  FCurrentMsgIndex := FChatView.MessageCount - 1;
  FChatInput.Busy  := True;

  try
    var LMsg := FConn.AddMessage(AText, 'user');

    if LHasFiles then
      for var I := 0 to AMediaFiles.Count - 1 do
      begin
        var MF := AMediaFiles[I];
        if MF.FullFileName <> '' then
          LMsg.LoadMediaFromFile(MF.FullFileName)
        else
        begin
          var LStream := MF.Content;
          if Assigned(LStream) and (LStream.Size > 0) then
          begin
            LStream.Position := 0;
            LMsg.LoadMediaFromStream(MF.filename, LStream);
          end;
        end;
      end;

    FConn.Run;
  except
    on E: Exception do
    begin
      FChatView.AppendToken(FCurrentMsgIndex, #10 + '**Error:** ' + E.Message);
      FChatView.FinishMessage(FCurrentMsgIndex);
      FCurrentMsgIndex := -1;
      FChatInput.Busy  := False;
    end;
  end;
end;

procedure TChatBridge.AbortGeneration;
begin
  FConn.Abort;
  TThread.Synchronize(nil, procedure
  begin
    if FCurrentMsgIndex >= 0 then
    begin
      FChatView.FinishMessage(FCurrentMsgIndex);
      FCurrentMsgIndex := -1;
    end;
    FChatInput.Busy := False;
  end);
end;

procedure TChatBridge.NewChat;
begin
  FConn.NewChat;
  FChatView.ClearMessages;
  FCurrentMsgIndex := -1;
  FChatInput.Busy  := False;
end;

{ ── Event handlers ────────────────────────────────────────────────────────── }

procedure TChatBridge.OnConnThinking(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
var
  LText    : string;
  LCallback: TProc<string>;
begin
  if aText = '' then Exit;
  LText     := aText;
  LCallback := FOnThinking;
  if not Assigned(LCallback) then Exit;
  TThread.Queue(nil, procedure
  begin
    LCallback(LText);
  end);
end;

procedure TChatBridge.OnReceiveData(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
var
  Idx : Integer;
  View: TAIChatView;
begin
  Idx  := FCurrentMsgIndex;
  View := FChatView;
  TThread.Synchronize(nil, procedure
  begin
    if Idx >= 0 then View.AppendToken(Idx, aText);
  end);
end;

procedure TChatBridge.OnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
var
  Idx  : Integer;
  View : TAIChatView;
  Input: TAIChatInput;
begin
  Idx   := FCurrentMsgIndex;
  View  := FChatView;
  Input := FChatInput;
  TThread.Synchronize(nil, procedure
  begin
    if Idx >= 0 then
    begin
      View.FinishMessage(Idx);
      FCurrentMsgIndex := -1;
    end;
    Input.Busy := False;
  end);
end;

procedure TChatBridge.OnConnError(Sender: TObject; const ErrorMsg: string;
  AException: Exception; const AResponse: IHTTPResponse);
var
  Msg  : string;
  Idx  : Integer;
  View : TAIChatView;
  Input: TAIChatInput;
begin
  Msg   := ErrorMsg;
  Idx   := FCurrentMsgIndex;
  View  := FChatView;
  Input := FChatInput;
  TThread.Synchronize(nil, procedure
  begin
    if Idx >= 0 then
    begin
      View.AppendToken(Idx, #10 + #10 + '> **Error:** ' + Msg);
      View.FinishMessage(Idx);
      FCurrentMsgIndex := -1;
    end;
    Input.Busy := False;
  end);
end;

{ ── Built-in functions ────────────────────────────────────────────────────── }

procedure TChatBridge.FnGetDateTime(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := Format(
    '{"datetime":"%s","date":"%s","time":"%s","weekday":"%s","unix":%d}',
    [DateTimeToStr(Now), DateToStr(Date), TimeToStr(Time),
     FormatDateTime('dddd', Now), DateTimeToUnix(Now)]);
  Handled := True;
end;

procedure TChatBridge.FnCalculate(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := Format(
    '{"expression":"%s","note":"Evaluate this step by step and show workings."}',
    [ToolCall.Params.Values['expression']]);
  Handled := True;
end;

procedure TChatBridge.FnGetWeather(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := Format(
    '{"city":"%s","temp_c":22,"condition":"Partly cloudy","humidity_pct":60,' +
    '"wind_kmh":15,"note":"Simulated — replace with a real weather API."}',
    [ToolCall.Params.Values['city']]);
  Handled := True;
end;

end.
