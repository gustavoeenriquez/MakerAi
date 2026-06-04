// MIT License
// Copyright (c) 2025 Gustavo Enr�quez
// uMakerAi Framework - Bridge System

unit uMakerAi.Chat.Bridge;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat, uMakerAi.Chat.Messages;

type
  { TAiBaseChatBridge: Motor interno de delegaci�n }
  TAiBaseChatBridge = class(TAICustomTool)
  private
    FTargetChat: TAiChat;
    FCurrentResMsg: TAiChatMessage;
    FCurrentMediaFile: TAiMediaFile;
    procedure SetTargetChat(const Value: TAiChat);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleTargetData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
    procedure HandleTargetEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
    procedure HandleTargetError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const aResponse: IHTTPResponse);
    procedure PrepareTargetChat(AContext: IAiToolContext; AResMsg: TAiChatMessage; AMediaFile: TAiMediaFile = nil);
    property TargetChat: TAiChat read FTargetChat write SetTargetChat;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { --- BRIDGES ESPECIALIZADOS --- }

  { Audio / Voz }
  TAiChatSpeechBridge = class(TAiSpeechToolBase)
  private
    FEngine: TAiBaseChatBridge;
    FPromptTranscription: string;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteTranscription(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const aText: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
    property PromptTranscription: string read FPromptTranscription write FPromptTranscription;
  end;

  { Visi�n }
  TAiChatVisionBridge = class(TAiVisionToolBase)
  private
    FEngine: TAiBaseChatBridge;
    FPromptVision: string;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteImageDescription(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
    property PromptVision: string read FPromptVision write FPromptVision;
  end;

  { Im�genes }
  TAiChatImageBridge = class(TAiImageToolBase)
  private
    FEngine: TAiBaseChatBridge;
    FPromptImage: string;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
    property PromptImage: string read FPromptImage write FPromptImage;
  end;

  { Video }
  TAiChatVideoBridge = class(TAiVideoToolBase)
  private
    FEngine: TAiBaseChatBridge;
    FPromptVideo: string;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
    property PromptVideo: string read FPromptVideo write FPromptVideo;
  end;

  { Documentos (PDF, Word, etc) }
  TAiChatDocumentBridge = class(TAiDocumentToolBase)
  private
    FEngine: TAiBaseChatBridge;
    FPromptDocument: string;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteDocumentAnalysis(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
    property PromptDocument: string read FPromptDocument write FPromptDocument;
  end;

  { Code Interpreter }
  TAiChatCodeInterpreterBridge = class(TAiCodeInterpreterToolBase)
  private
    FEngine: TAiBaseChatBridge;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteCode(const ACode, ALanguage: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
  end;

  { B�squeda Web }
  TAiChatWebSearchBridge = class(TAiWebSearchToolBase)
  private
    FEngine: TAiBaseChatBridge;
    function GetChat: TAiChat;
    procedure SetChat(const Value: TAiChat);
  protected
    procedure ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Chat: TAiChat read GetChat write SetChat;
  end;

procedure Register;

implementation

{ Hack para acceder a FContext protected }
type
  TToolAccess = class(TAICustomTool);

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiChatSpeechBridge, TAiChatVisionBridge, TAiChatImageBridge, TAiChatVideoBridge, TAiChatDocumentBridge, TAiChatCodeInterpreterBridge, TAiChatWebSearchBridge]);
end;

{ TAiBaseChatBridge }

constructor TAiBaseChatBridge.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TAiBaseChatBridge.SetTargetChat(const Value: TAiChat);
begin
  if FTargetChat <> Value then
  begin
    FTargetChat := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiBaseChatBridge.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FTargetChat) then
    FTargetChat := nil;
end;

procedure TAiBaseChatBridge.PrepareTargetChat(AContext: IAiToolContext; AResMsg: TAiChatMessage; AMediaFile: TAiMediaFile);
begin
  if not Assigned(FTargetChat) then
    raise Exception.Create('Error: Bridge sin Chat destino asignado.');
  Self.SetContext(AContext);
  FCurrentResMsg := AResMsg;
  FCurrentMediaFile := AMediaFile;
  FTargetChat.OnReceiveData := HandleTargetData;
  FTargetChat.OnReceiveDataEnd := HandleTargetEnd;
  FTargetChat.OnError := HandleTargetError;
  FTargetChat.Asynchronous := IsAsync;
end;

procedure TAiBaseChatBridge.HandleTargetData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
begin
  ReportData(FCurrentResMsg, aRole, aText, aResponse);
end;

procedure TAiBaseChatBridge.HandleTargetEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: String);
begin
  if Assigned(FCurrentMediaFile) then
  begin
    FCurrentMediaFile.Transcription := aText;
    FCurrentMediaFile.Procesado := True;
  end;
  ReportDataEnd(FCurrentResMsg, aRole, aText, aResponse);
end;

procedure TAiBaseChatBridge.HandleTargetError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const aResponse: IHTTPResponse);
begin
  ReportError(ErrorMsg, Exception);
end;

{ --- IMPLEMENTACI�N DE BRIDGES --- }

{ TAiChatSpeechBridge }
constructor TAiChatSpeechBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
  FPromptTranscription := 'Transcribe este audio literalmente. Solo el texto.';
end;

destructor TAiChatSpeechBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatSpeechBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatSpeechBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatSpeechBridge.ExecuteTranscription(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg, AMediaFile);
  ReportState(acsReasoning, 'Delegando transcripci�n...');
  FEngine.TargetChat.AddMessageAndRun(FPromptTranscription, 'user', [AMediaFile]);
end;

procedure TAiChatSpeechBridge.ExecuteSpeechGeneration(const aText: string; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg);
  ReportState(acsWriting, 'Delegando generaci�n de voz...');
  FEngine.TargetChat.AddMessageAndRun('Genera un audio con el siguiente texto: ' + aText, 'user', []);
end;

{ TAiChatVisionBridge }
constructor TAiChatVisionBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
  FPromptVision := 'Describe esta imagen detalladamente.';
end;

destructor TAiChatVisionBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatVisionBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatVisionBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatVisionBridge.ExecuteImageDescription(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg, AMediaFile);
  ReportState(acsReasoning, 'Delegando visi�n...');
  FEngine.TargetChat.AddMessageAndRun(FPromptVision, 'user', [AMediaFile]);
end;

{ TAiChatImageBridge }
constructor TAiChatImageBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
  FPromptImage := 'Genera una imagen basada en esta descripci�n: ';
end;

destructor TAiChatImageBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatImageBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatImageBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatImageBridge.ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg);
  ReportState(acsWriting, 'Delegando generaci�n de imagen...');
  FEngine.TargetChat.AddMessageAndRun(FPromptImage + APrompt, 'user', []);
end;

{ TAiChatVideoBridge }
constructor TAiChatVideoBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
  FPromptVideo := 'Genera un video corto basado en esta descripci�n: ';
end;

destructor TAiChatVideoBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatVideoBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatVideoBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatVideoBridge.ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg);
  ReportState(acsWriting, 'Delegando generaci�n de video...');
  FEngine.TargetChat.AddMessageAndRun(FPromptVideo + TAiChatMessage(AskMsg).Prompt, 'user', []);
end;

{ TAiChatDocumentBridge }
constructor TAiChatDocumentBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
  FPromptDocument := 'Analiza este documento y resume los puntos clave.';
end;

destructor TAiChatDocumentBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatDocumentBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatDocumentBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatDocumentBridge.ExecuteDocumentAnalysis(AMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg, AMediaFile);
  ReportState(acsReasoning, 'Delegando an�lisis de documento...');
  FEngine.TargetChat.AddMessageAndRun(FPromptDocument, 'user', [AMediaFile]);
end;

{ TAiChatCodeInterpreterBridge }
constructor TAiChatCodeInterpreterBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
end;

destructor TAiChatCodeInterpreterBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatCodeInterpreterBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatCodeInterpreterBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatCodeInterpreterBridge.ExecuteCode(const ACode, ALanguage: string; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg);
  ReportState(acsWriting, 'Delegando ejecuci�n de c�digo...');
  FEngine.TargetChat.AddMessageAndRun('Ejecuta o explica este c�digo en ' + ALanguage + ': ' + ACode, 'user', []);
end;

{ TAiChatWebSearchBridge }
constructor TAiChatWebSearchBridge.Create(AOwner: TComponent);
begin
  inherited;
  FEngine := TAiBaseChatBridge.Create(nil);
end;

destructor TAiChatWebSearchBridge.Destroy;
begin
  FEngine.Free;
  inherited;
end;

function TAiChatWebSearchBridge.GetChat: TAiChat;
begin
  Result := FEngine.TargetChat;
end;

procedure TAiChatWebSearchBridge.SetChat(const Value: TAiChat);
begin
  FEngine.TargetChat := Value;
end;

procedure TAiChatWebSearchBridge.ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage);
begin
  FEngine.PrepareTargetChat(TToolAccess(Self).FContext, ResMsg);
  ReportState(acsReasoning, 'Delegando b�squeda web...');
  FEngine.TargetChat.AddMessageAndRun('Busca en internet informaci�n sobre: ' + AQuery, 'user', []);
end;

end.
