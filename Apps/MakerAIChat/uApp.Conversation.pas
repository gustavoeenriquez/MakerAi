unit uApp.Conversation;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Generics.Collections;

type
  TSavedMessage = record
    Role   : string;
    Content: string;
  end;

  TConversation = class
  private
    FId          : string;
    FTitle       : string;
    FMessages    : TList<TSavedMessage>;
    FMessagesJSON: string;   // full ChatView.SaveToJSON blob (text + base64 attachments)
    FCreatedAt   : TDateTime;
    FProvider    : string;
    FModel       : string;
  public
    constructor Create(const ATitle: string);
    destructor  Destroy; override;
    procedure AddMessage(const ARole, AContent: string);
    procedure Clear;
    function  ToJSON: TJSONObject;
    procedure FromJSON(AObj: TJSONObject);
    property Id          : string               read FId           write FId;
    property Title       : string               read FTitle        write FTitle;
    property Messages    : TList<TSavedMessage> read FMessages;
    property MessagesJSON: string               read FMessagesJSON write FMessagesJSON;
    property CreatedAt   : TDateTime            read FCreatedAt;
    property Provider    : string               read FProvider     write FProvider;
    property Model       : string               read FModel        write FModel;
  end;

  TConversationStore = class
  private
    FItems   : TObjectList<TConversation>;
    FFilePath: string;
    function  GetCount: Integer;
    function  GetItem(AIndex: Integer): TConversation;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Load;
    procedure Save;
    function  AddNew(const ATitle: string): TConversation;
    procedure Remove(AIndex: Integer);
    procedure MoveToTop(AIndex: Integer);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TConversation read GetItem; default;
  end;

var
  ConversationStore: TConversationStore;

implementation

uses
  uApp.Settings;

function DataDir: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'MakerAIChat');
end;

{ TConversation }

constructor TConversation.Create(const ATitle: string);
begin
  FId        := FormatDateTime('yyyymmddhhnnsszzz', Now);
  FTitle     := ATitle;
  FCreatedAt := Now;
  FMessages  := TList<TSavedMessage>.Create;
end;

destructor TConversation.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TConversation.AddMessage(const ARole, AContent: string);
var
  M: TSavedMessage;
begin
  M.Role    := ARole;
  M.Content := AContent;
  FMessages.Add(M);
end;

procedure TConversation.Clear;
begin
  FMessages.Clear;
end;

function TConversation.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id',           FId);
  Result.AddPair('title',        FTitle);
  Result.AddPair('createdAt',    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));
  Result.AddPair('provider',     FProvider);
  Result.AddPair('model',        FModel);
  Result.AddPair('messagesJson', FMessagesJSON);
  var MsgArr := TJSONArray.Create;
  for var M in FMessages do
  begin
    var MObj := TJSONObject.Create;
    MObj.AddPair('role',    M.Role);
    MObj.AddPair('content', M.Content);
    MsgArr.AddElement(MObj);
  end;
  Result.AddPair('messages', MsgArr);
end;

procedure TConversation.FromJSON(AObj: TJSONObject);
begin
  FId           := AObj.GetValue<string>('id',           '');
  FTitle        := AObj.GetValue<string>('title',        'Conversation');
  FProvider     := AObj.GetValue<string>('provider',     '');
  FModel        := AObj.GetValue<string>('model',        '');
  FMessagesJSON := AObj.GetValue<string>('messagesJson', '');
  var MsgArr := AObj.GetValue('messages') as TJSONArray;
  if Assigned(MsgArr) then
    for var I := 0 to MsgArr.Count - 1 do
    begin
      var MObj := MsgArr.Items[I] as TJSONObject;
      var M: TSavedMessage;
      M.Role    := MObj.GetValue<string>('role',    '');
      M.Content := MObj.GetValue<string>('content', '');
      FMessages.Add(M);
    end;
end;

{ TConversationStore }

constructor TConversationStore.Create;
begin
  FItems    := TObjectList<TConversation>.Create(True);
  FFilePath := TPath.Combine(DataDir, 'conversations.json');
  Load;
end;

destructor TConversationStore.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TConversationStore.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TConversationStore.GetItem(AIndex: Integer): TConversation;
begin
  Result := FItems[AIndex];
end;

procedure TConversationStore.Load;
begin
  if not TFile.Exists(FFilePath) then Exit;
  try
    var JSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(FFilePath)) as TJSONObject;
    if JSON = nil then Exit;
    try
      var Arr := JSON.GetValue('conversations') as TJSONArray;
      if Assigned(Arr) then
        for var I := 0 to Arr.Count - 1 do
        begin
          var Conv := TConversation.Create('');
          Conv.FromJSON(Arr.Items[I] as TJSONObject);
          FItems.Add(Conv);
        end;
    finally
      JSON.Free;
    end;
  except
  end;
end;

procedure TConversationStore.Save;
begin
  if not TDirectory.Exists(DataDir) then
    TDirectory.CreateDirectory(DataDir);
  var Root := TJSONObject.Create;
  try
    var Arr := TJSONArray.Create;
    for var I := 0 to FItems.Count - 1 do
      Arr.AddElement(FItems[I].ToJSON);
    Root.AddPair('conversations', Arr);
    TFile.WriteAllText(FFilePath, Root.Format);
  finally
    Root.Free;
  end;
end;

function TConversationStore.AddNew(const ATitle: string): TConversation;
begin
  Result := TConversation.Create(ATitle);
  Result.Provider := AppSettings.ProviderName;
  Result.Model    := AppSettings.ModelName;
  FItems.Insert(0, Result);
end;

procedure TConversationStore.Remove(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TConversationStore.MoveToTop(AIndex: Integer);
begin
  if AIndex <= 0 then Exit;
  var Conv := FItems[AIndex];
  FItems.Extract(Conv);
  FItems.Insert(0, Conv);
end;

initialization
  ConversationStore := TConversationStore.Create;
finalization
  FreeAndNil(ConversationStore);
end.
