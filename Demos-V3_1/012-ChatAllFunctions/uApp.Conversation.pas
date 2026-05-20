unit uApp.Conversation;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  System.Generics.Collections;

type
  TSavedMessage = class
    Role   : string;
    Content: string;
  end;

  TConversation = class
  private
    FId          : string;
    FTitle       : string;
    FMessages    : TObjectList<TSavedMessage>;
    FMessagesJSON: string;
  public
    constructor Create(const AId, ATitle: string);
    destructor  Destroy; override;
    procedure   Clear;
    procedure   AddMessage(const ARole, AContent: string);
    function    ToJSON: TJSONObject;
    procedure   FromJSON(AObj: TJSONObject);
    property Id          : string                     read FId           write FId;
    property Title       : string                     read FTitle        write FTitle;
    property Messages    : TObjectList<TSavedMessage> read FMessages;
    property MessagesJSON: string                     read FMessagesJSON write FMessagesJSON;
  end;

  TConversationStore = class
  private
    FList: TObjectList<TConversation>;
    function  GetFilePath: string;
    function  GetItem(AIndex: Integer): TConversation;
    function  GetCount: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    function    AddNew(const ATitle: string): TConversation;
    procedure   Remove(AIndex: Integer);
    procedure   Load;
    procedure   Save;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TConversation read GetItem; default;
  end;

var
  ConversationStore: TConversationStore;

implementation

{ TConversation }

constructor TConversation.Create(const AId, ATitle: string);
begin
  inherited Create;
  FId       := AId;
  FTitle    := ATitle;
  FMessages := TObjectList<TSavedMessage>.Create(True);
end;

destructor TConversation.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TConversation.Clear;
begin
  FMessages.Clear;
end;

procedure TConversation.AddMessage(const ARole, AContent: string);
var
  M: TSavedMessage;
begin
  M         := TSavedMessage.Create;
  M.Role    := ARole;
  M.Content := AContent;
  FMessages.Add(M);
end;

function TConversation.ToJSON: TJSONObject;
var
  JMsgs: TJSONArray;
  JMsg : TJSONObject;
  M    : TSavedMessage;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id',    FId);
  Result.AddPair('title', FTitle);
  if FMessagesJSON <> '' then
    Result.AddPair('messagesJson', FMessagesJSON);
  JMsgs := TJSONArray.Create;
  for M in FMessages do
  begin
    JMsg := TJSONObject.Create;
    JMsg.AddPair('role',    M.Role);
    JMsg.AddPair('content', M.Content);
    JMsgs.Add(JMsg);
  end;
  Result.AddPair('messages', JMsgs);
end;

procedure TConversation.FromJSON(AObj: TJSONObject);
var
  JMsgs: TJSONArray;
  JMsg : TJSONObject;
  I    : Integer;
  M    : TSavedMessage;
begin
  AObj.TryGetValue<string>('id',           FId);
  AObj.TryGetValue<string>('title',        FTitle);
  AObj.TryGetValue<string>('messagesJson', FMessagesJSON);
  FMessages.Clear;
  JMsgs := AObj.GetValue<TJSONArray>('messages');
  if Assigned(JMsgs) then
    for I := 0 to JMsgs.Count - 1 do
    begin
      JMsg := JMsgs.Items[I] as TJSONObject;
      M    := TSavedMessage.Create;
      JMsg.TryGetValue<string>('role',    M.Role);
      JMsg.TryGetValue<string>('content', M.Content);
      FMessages.Add(M);
    end;
end;

{ TConversationStore }

constructor TConversationStore.Create;
begin
  inherited;
  FList := TObjectList<TConversation>.Create(True);
  Load;
end;

destructor TConversationStore.Destroy;
begin
  FList.Free;
  inherited;
end;

function TConversationStore.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TConversationStore.GetItem(AIndex: Integer): TConversation;
begin
  Result := FList[AIndex];
end;

function TConversationStore.GetFilePath: string;
var
  LDir: string;
begin
  LDir := TPath.Combine(TPath.GetHomePath, 'MakerAIChat');
  if not TDirectory.Exists(LDir) then
    TDirectory.CreateDirectory(LDir);
  Result := TPath.Combine(LDir, 'conversations.json');
end;

function TConversationStore.AddNew(const ATitle: string): TConversation;
begin
  Result := TConversation.Create(FormatDateTime('yyyymmddhhnnsszzz', Now), ATitle);
  FList.Insert(0, Result);
end;

procedure TConversationStore.Remove(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FList.Count) then
    FList.Delete(AIndex);
end;

procedure TConversationStore.Load;
var
  LPath: string;
  LRoot: TJSONValue;
  JArr : TJSONArray;
  I    : Integer;
  Conv : TConversation;
begin
  LPath := GetFilePath;
  if not TFile.Exists(LPath) then Exit;
  try
    LRoot := TJSONValue.ParseJSONValue(TFile.ReadAllText(LPath));
    if not (LRoot is TJSONArray) then begin LRoot.Free; Exit; end;
    JArr := TJSONArray(LRoot);
    try
      for I := 0 to JArr.Count - 1 do
        if JArr.Items[I] is TJSONObject then
        begin
          Conv := TConversation.Create('', '');
          Conv.FromJSON(TJSONObject(JArr.Items[I]));
          FList.Add(Conv);
        end;
    finally
      JArr.Free;
    end;
  except
  end;
end;

procedure TConversationStore.Save;
var
  LPath: string;
  JArr : TJSONArray;
  I    : Integer;
begin
  LPath := GetFilePath;
  JArr  := TJSONArray.Create;
  try
    for I := 0 to FList.Count - 1 do
      JArr.Add(FList[I].ToJSON);
    TFile.WriteAllText(LPath, JArr.ToJSON);
  finally
    JArr.Free;
  end;
end;

initialization
  ConversationStore := TConversationStore.Create;

finalization
  FreeAndNil(ConversationStore);

end.
