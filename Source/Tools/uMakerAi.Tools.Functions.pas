// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Tools.Functions
// Gestión de funciones/herramientas locales y clientes MCP para llamada por LLMs.
unit uMakerAi.Tools.Functions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.MCPClient.Core,
  uMakerAi.Chat.Messages;

type

  TFunctionActionItem = class;

  TFunctionEvent = procedure(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean) of object;

  TToolstype = (tt_function, ttNone);

  TToolsParamType = (ptString, ptInteger, ptBoolean, ptFloat, ptDate,
      ptTime, ptDateTime, ptBase64);

  TFunctionParamsItem = class(TCollectionItem)
  private
    FName: string;
    FCollection: TCollection;
    FParamType: TToolsParamType;
    FRequired: Boolean;
    FDescription: TStrings;
    FEnum: string;
    procedure SetName(const Value: string);
    procedure SetDescription(const Value: TStrings);
    procedure SetEnum(const Value: string);
    procedure SetParamType(const Value: TToolsParamType);
    procedure SetRequired(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetNamePath: string; override;
    function ToJSon(Detail: Boolean = False): TJSONObject;
    procedure SetJSon(Value: TJSONObject);
  published
    property Name: string read FName write SetName;
    property ParamType: TToolsParamType read FParamType write SetParamType;
    property Description: TStrings read FDescription write SetDescription;
    property Enum: string read FEnum write SetEnum;
    property Required: Boolean read FRequired write SetRequired;
  end;

  TFunctionParamsItems = class(TOwnedCollection)
  private
    FOwner: TPersistent;
    function GetParams(Index: Integer): TFunctionParamsItem;
    procedure SetParams(Index: Integer; const Value: TFunctionParamsItem);
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
    function GetParamByName(aParamName: string): TFunctionParamsItem;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TFunctionParamsItem;
    function ToJSon(Detail: Boolean = False): TJSONObject;
    property Params[Index: Integer]: TFunctionParamsItem read GetParams write SetParams; default;
  end;

  TFunctionActionItem = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FName: string;
    FOnAction: TFunctionEvent;
    FCollection: TCollection;
    FDefault: Boolean;
    FDescription: TStrings;
    FTagObject: TObject;
    FTag: Integer;
    FParams: TFunctionParamsItems;
    FToolType: TToolstype;
    FScript: TStrings;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnAction(const Value: TFunctionEvent);
    procedure SetDefault(const Value: Boolean);
    procedure SetFunctionDoc(const Value: TStrings);
    procedure SetTag(const Value: Integer);
    procedure SetTagObject(const Value: TObject);
    procedure SetToolType(const Value: TToolstype);
    procedure SetScript(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function GetNamePath: string; override;
    function ToJSon(Detail: Boolean = False): TJSONObject;
    procedure SetJSon(Value: TJSONObject);
    property TagObject: TObject read FTagObject write SetTagObject;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FunctionName: string read GetDisplayName write SetDisplayName;
    property OnAction: TFunctionEvent read FOnAction write SetOnAction;
    property Description: TStrings read FDescription write SetFunctionDoc;
    property Script: TStrings read FScript write SetScript;
    property Parameters: TFunctionParamsItems read FParams write FParams;
    property Default: Boolean read FDefault write SetDefault;
    property Tag: Integer read FTag write SetTag;
    property ToolType: TToolstype read FToolType write SetToolType;
  end;

  TFunctionActionItems = class(TOwnedCollection)
  private
    FOwner: TPersistent;
    function GetActionItem(Index: Integer): TFunctionActionItem;
    procedure SetActionItem(Index: Integer; Value: TFunctionActionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
    function GetItemByName(aTagName: string): TFunctionActionItem;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TFunctionActionItem;
    function IndexOf(Nombre: string): Integer;
    function GetFunction(Nombre: string): TFunctionActionItem;
    function AddFunction(Nombre: string; Enabled: Boolean;
        Action: TFunctionEvent): TFunctionActionItem;
    function ToJSon(aDetail: Boolean = False): TJSONArray;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    property Items[Index: Integer]: TFunctionActionItem
        read GetActionItem write SetActionItem; default;
  end;

  TMCPClientItem = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FMCPClient: TMCPClientCustom;
    FConnected: Boolean;
    FParams: TStrings;
    FEnvVars: TStrings;
    FName: string;
    function GetName: string;
    function GetTransportType: TToolTransportType;
    procedure SetName(const Value: string);
    procedure SetTransportType(const Value: TToolTransportType);
    procedure SetEnabled(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    function GetParams: TStrings;
    procedure SetParams(const Value: TStrings);
    function GetConfiguration: string;
    procedure SetConfiguration(const Value: string);
    function GetEnvVars: TStrings;
    procedure SetEnvVars(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure UpdateClientProperties;
    property MCPClient: TMCPClientCustom read FMCPClient;
  published
    property Connected: Boolean read FConnected write SetConnected;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property TransportType: TToolTransportType
        read GetTransportType write SetTransportType;
    property Params: TStrings read GetParams write SetParams;
    property EnvVars: TStrings read GetEnvVars write SetEnvVars;
    property Configuration: string read GetConfiguration write SetConfiguration;
  end;

  TMCPClientItems = class(TOwnedCollection)
  private
    FOwner: TPersistent;
    function GetClient(Index: Integer): TMCPClientItem;
    procedure SetClient(Index: Integer; const Value: TMCPClientItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add: TMCPClientItem;
    function GetClientByName(const AName: string): TMCPClientItem;
    function GetClientsList: TStringList;
    function GetFunctionList(Name: string): TStringList;
    property Items[Index: Integer]: TMCPClientItem
        read GetClient write SetClient; default;
  end;

  TAiFunctions = class(TComponent)
  private
    FFunctions: TFunctionActionItems;
    FMCPClients: TMCPClientItems;
    FOnStatusUpdate: TMCPStatusEvent;
    FOnLog: TMCPLogEvent;
    FOnMCPStreamMessage: TMCPStreamMessageEvent;
    procedure SetOnMCPStreamMessage(const Value: TMCPStreamMessageEvent);
  protected
    procedure DoLog(const Msg: string); virtual;
    procedure DoStatusUpdate(const StatusMsg: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function GetTools(aToolFormat: TToolFormat): string; virtual;
    function DoCallFunction(ToolCall: TAiToolsFunction): Boolean; virtual;
    function SetFunctionEnable(FunctionName: string; Enabled: Boolean): Boolean;
    function SetMCPClientEnable(AName: string; Enabled: Boolean): Boolean;
    function ExtractFunctionNames: TStringList;
    procedure AddMCPClient(aMCPClient: TMCPClientCustom);
    function ImportClaudeMCPConfiguration(AConfig: TJSONObject): Integer; overload;
    function ImportClaudeMCPConfiguration(const AJsonFilePath: string = ''): Integer; overload;
  published
    property Functions: TFunctionActionItems read FFunctions write FFunctions;
    property MCPClients: TMCPClientItems read FMCPClients write FMCPClients;
    property OnLog: TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate: TMCPStatusEvent
        read FOnStatusUpdate write FOnStatusUpdate;
    property OnMCPStreamMessage: TMCPStreamMessageEvent
        read FOnMCPStreamMessage write SetOnMCPStreamMessage;
  end;

  // Normalización multi-formato de herramientas
  TNormalizedTool = class
  private
    FName: string;
    FDescription: string;
    FInputSchema: TJSONObject;
  public
    constructor Create(const AName, ADescription: string;
        AInputSchema: TJSONObject);
    destructor Destroy; override;
    property Name: string read FName;
    property Description: string read FDescription;
    property InputSchema: TJSONObject read FInputSchema;
  end;

  // Lista de herramientas normalizadas (owning)
  TNormalizedToolList = class(TObjectList)
  public
    constructor Create;
    function GetTool(I: Integer): TNormalizedTool;
    property Tools[I: Integer]: TNormalizedTool read GetTool; default;
  end;

  TJsonToolUtils = class
  private
    class function DetectInputFormat(AJsonTool: TJSONObject): TToolFormat;
    class procedure NormalizeFromMCP(AJsonTool: TJSONObject;
        AToolList: TNormalizedToolList);
    class procedure NormalizeFromOpenAI(AJsonTool: TJSONObject;
        AToolList: TNormalizedToolList);
    class procedure NormalizeFromAnthropic(AJsonTool: TJSONObject;
        AToolList: TNormalizedToolList);
    class procedure NormalizeFromGemini(AJsonTool: TJSONObject;
        AToolList: TNormalizedToolList);
    class function FormatAsMCP(ANormalizedTool: TNormalizedTool): TJSONObject;
    class function FormatAsOpenAI(ANormalizedTool: TNormalizedTool): TJSONObject;
    class function FormatAsOpenAIResponses(ANormalizedTool: TNormalizedTool): TJSONObject;
    class function FormatAsAnthropic(ANormalizedTool: TNormalizedTool): TJSONObject;
    class function FormatAsGeminiFunctionDeclaration(
        ANormalizedTool: TNormalizedTool): TJSONObject;
    class function MergeToolLists(const ASourceName: string;
        ASourceJson: TJSONObject; AInputFormat: TToolFormat;
        ATargetJson: TJSONObject; AOutputFormat: TToolFormat): TJSONObject; overload;
    class procedure CleanInputSchema(ASchema: TJSONObject);
    class procedure CleanJsonTree(AValue: TJSONData);
    class procedure EnforceStrictSchema(ASchema: TJSONData);
  public
    class function MergeToolLists(const ASourceName: string;
        ASourceJson: TJSONObject; ATargetJson: TJSONObject;
        AOutputFormat: TToolFormat): TJSONObject; overload;
    class procedure NormalizeToolsFromSource(const ASourceName: string;
        ASourceJson: TJSONObject; ANormalizedList: TNormalizedToolList);
    class function FormatToolList(ANormalizedList: TNormalizedToolList;
        AOutputFormat: TToolFormat): TJSONObject;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiFunctions]);
end;

// =============================================================================
// Helpers JSON locales (fpjson)
// =============================================================================

function JFindObj(J: TJSONObject; const Key: string): TJSONObject;
var
  D: TJSONData;
begin
  D := J.Find(Key);
  if (D <> nil) and (D is TJSONObject) then
    Result := TJSONObject(D)
  else
    Result := nil;
end;

function JFindArr(J: TJSONObject; const Key: string): TJSONArray;
var
  D: TJSONData;
begin
  D := J.Find(Key);
  if (D <> nil) and (D is TJSONArray) then
    Result := TJSONArray(D)
  else
    Result := nil;
end;

function JGetStr(J: TJSONObject; const Key: string;
    const Def: string = ''): string;
begin
  Result := J.Get(Key, Def);
end;

function JGetInt(J: TJSONObject; const Key: string;
    const Def: Integer = 0): Integer;
begin
  Result := J.Get(Key, Def);
end;

function JGetBool(J: TJSONObject; const Key: string;
    const Def: Boolean = False): Boolean;
begin
  Result := J.Get(Key, Def);
end;

function JTryGetStr(J: TJSONObject; const Key: string;
    out Val: string): Boolean;
var
  D: TJSONData;
begin
  D := J.Find(Key);
  if D <> nil then
  begin
    Val := D.AsString;
    Result := True;
  end
  else
  begin
    Val := '';
    Result := False;
  end;
end;

function JTryGetBool(J: TJSONObject; const Key: string;
    out Val: Boolean): Boolean;
var
  D: TJSONData;
begin
  D := J.Find(Key);
  if (D <> nil) and (D is TJSONBoolean) then
  begin
    Val := D.AsBoolean;
    Result := True;
  end
  else
  begin
    Val := False;
    Result := False;
  end;
end;

// =============================================================================
// TNormalizedTool
// =============================================================================

constructor TNormalizedTool.Create(const AName, ADescription: string;
    AInputSchema: TJSONObject);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FInputSchema := AInputSchema;
end;

destructor TNormalizedTool.Destroy;
begin
  FInputSchema.Free;
  inherited;
end;

// =============================================================================
// TNormalizedToolList
// =============================================================================

constructor TNormalizedToolList.Create;
begin
  inherited Create;
  OwnsObjects := True;
end;

function TNormalizedToolList.GetTool(I: Integer): TNormalizedTool;
begin
  Result := TNormalizedTool(inherited Items[I]);
end;

// =============================================================================
// TFunctionParamsItem
// =============================================================================

constructor TFunctionParamsItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCollection := ACollection;
  FDescription := TStringList.Create;
end;

destructor TFunctionParamsItem.Destroy;
begin
  FDescription.Free;
  inherited;
end;

function TFunctionParamsItem.GetDisplayName: string;
begin
  inherited GetDisplayName;
  Result := FName;
end;

function TFunctionParamsItem.GetNamePath: string;
begin
  Result := inherited GetNamePath + Format('Param%d', [Index]);
end;

procedure TFunctionParamsItem.SetDescription(const Value: TStrings);
begin
  FDescription.Text := Value.Text;
end;

procedure TFunctionParamsItem.SetDisplayName(const Value: string);
var
  I: Integer;
  Param: TFunctionParamsItem;
begin
  if CompareText(Value, FName) <> 0 then
  begin
    if FCollection <> nil then
      for I := 0 to FCollection.Count - 1 do
      begin
        Param := TFunctionParamsItem(FCollection.Items[I]);
        if (Param <> Self) and (Param is TFunctionParamsItem) and
           (CompareText(Value, Param.FName) = 0) then
          raise Exception.Create('El nombre del parámetro está duplicado');
      end;
    FName := Value;
    Changed(False);
  end;

  if FName = '' then
  begin
    FName := Value;
    Changed(False);
  end;
end;

procedure TFunctionParamsItem.SetEnum(const Value: string);
begin
  FEnum := Value;
end;

procedure TFunctionParamsItem.SetJSon(Value: TJSONObject);
var
  aType, ADescription: string;
  jEnum: TJSONArray;
  I: Integer;
  Lista: TStringList;
begin
  aType := JGetStr(Value, 'type');
  if aType = 'string' then
    ParamType := ptString
  else if aType = 'integer' then
    ParamType := ptInteger
  else if aType = 'boolean' then
    ParamType := ptBoolean
  else if aType = 'float' then
    ParamType := ptFloat
  else if aType = 'date' then
    ParamType := ptDate
  else if aType = 'time' then
    ParamType := ptTime
  else if aType = 'datetime' then
    ParamType := ptDateTime
  else
    ParamType := ptString;

  if JTryGetStr(Value, 'description', ADescription) then
    FDescription.Text := ADescription;

  jEnum := JFindArr(Value, 'enum');
  if jEnum <> nil then
  begin
    Lista := TStringList.Create;
    try
      for I := 0 to jEnum.Count - 1 do
        Lista.Add(jEnum.Items[I].AsString);
      FEnum := Lista.CommaText;
    finally
      Lista.Free;
    end;
  end;
end;

procedure TFunctionParamsItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TFunctionParamsItem.SetParamType(const Value: TToolsParamType);
begin
  FParamType := Value;
end;

procedure TFunctionParamsItem.SetRequired(const Value: Boolean);
begin
  FRequired := Value;
end;

function TFunctionParamsItem.ToJSon(Detail: Boolean): TJSONObject;
var
  Tipo: string;
  jEnum: TJSONArray;
  I: Integer;
  Lista: TStringList;
begin
  Result := TJSONObject.Create;
  case FParamType of
    ptString, ptBase64: Tipo := 'string';
    ptInteger:          Tipo := 'integer';
    ptBoolean:          Tipo := 'boolean';
    ptFloat:            Tipo := 'float';
    ptDate:             Tipo := 'date';
    ptTime:             Tipo := 'time';
    ptDateTime:         Tipo := 'datetime';
  end;

  Result.Add('type', Tipo);
  if Trim(FDescription.Text) <> '' then
    Result.Add('description', Trim(FDescription.Text));

  if Trim(FEnum) <> '' then
  begin
    Lista := TStringList.Create;
    try
      Lista.CommaText := Trim(FEnum);
      if Lista.Count > 0 then
      begin
        jEnum := TJSONArray.Create;
        Result.Add('enum', jEnum);
        for I := 0 to Lista.Count - 1 do
        begin
          if FParamType in [ptString, ptDate, ptTime, ptDateTime, ptBase64] then
            jEnum.Add(Trim(Lista[I]))
          else
            jEnum.Add(Trim(Lista[I]));
        end;
      end;
    finally
      Lista.Free;
    end;
  end;
end;

// =============================================================================
// TFunctionParamsItems
// =============================================================================

function TFunctionParamsItems.Add: TFunctionParamsItem;
begin
  Result := TFunctionParamsItem(inherited Add);
end;

constructor TFunctionParamsItems.Create(AOwner: TPersistent;
    AItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(Self, AItemClass);
end;

destructor TFunctionParamsItems.Destroy;
begin
  inherited;
end;

function TFunctionParamsItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TFunctionParamsItems.GetParamByName(
    aParamName: string): TFunctionParamsItem;
var
  I: Integer;
  CurItem: TFunctionParamsItem;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    CurItem := Items[I] as TFunctionParamsItem;
    if CompareText(CurItem.Name, aParamName) = 0 then
    begin
      Result := CurItem;
      Break;
    end;
  end;
end;

function TFunctionParamsItems.GetParams(Index: Integer): TFunctionParamsItem;
begin
  Result := TFunctionParamsItem(inherited Items[Index]);
end;

procedure TFunctionParamsItems.SetItemName(Item: TCollectionItem);
var
  I, J: Integer;
  ItemName: string;
  CurItem: TFunctionParamsItem;
begin
  J := 1;
  while True do
  begin
    ItemName := Format('Param%d', [J]);
    I := 0;
    while I < Count do
    begin
      CurItem := Items[I] as TFunctionParamsItem;
      if (CurItem <> Item) and (CompareText(CurItem.Name, ItemName) = 0) then
      begin
        Inc(J);
        Break;
      end;
      Inc(I);
    end;
    if I >= Count then
    begin
      (Item as TFunctionParamsItem).Name := ItemName;
      Break;
    end;
  end;
end;

procedure TFunctionParamsItems.SetParams(Index: Integer;
    const Value: TFunctionParamsItem);
begin
  Items[Index].Assign(Value);
end;

function TFunctionParamsItems.ToJSon(Detail: Boolean): TJSONObject;
var
  CurItem: TFunctionParamsItem;
  jProperties: TJSONObject;
  jRequired: TJSONArray;
  I: Integer;
begin
  Result := nil;

  if Count > 0 then
  begin
    Result := TJSONObject.Create;
    Result.Add('type', 'object');

    jProperties := TJSONObject.Create;
    jRequired := TJSONArray.Create;
    Result.Add('properties', jProperties);

    for I := 0 to Count - 1 do
    begin
      CurItem := TFunctionParamsItem(Items[I]);
      jProperties.Add(CurItem.Name, CurItem.ToJSon(Detail));
      if CurItem.Required then
        jRequired.Add(CurItem.Name);
    end;

    if jRequired.Count > 0 then
      Result.Add('required', jRequired)
    else
      jRequired.Free;
  end;
end;

procedure TFunctionParamsItems.Update(Item: TCollectionItem);
begin
  inherited;
end;

// =============================================================================
// TFunctionActionItem
// =============================================================================

constructor TFunctionActionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCollection := ACollection;
  FEnabled := True;
  FDescription := TStringList.Create;
  FScript := TStringList.Create;
  FParams := TFunctionParamsItems.Create(Self, TFunctionParamsItem);
end;

destructor TFunctionActionItem.Destroy;
begin
  FDescription.Free;
  FParams.Free;
  FScript.Free;
  inherited;
end;

function TFunctionActionItem.GetDisplayName: string;
begin
  inherited GetDisplayName;
  Result := FName;
end;

function TFunctionActionItem.GetNamePath: string;
begin
  Result := inherited GetNamePath + Self.FunctionName;
end;

procedure TFunctionActionItem.SetDefault(const Value: Boolean);
var
  I: Integer;
begin
  if Value = True then
    for I := 0 to FCollection.Count - 1 do
      TFunctionActionItem(FCollection.Items[I]).Default := False;
  FDefault := Value;
end;

procedure TFunctionActionItem.SetDisplayName(const Value: string);
var
  I: Integer;
  Action: TFunctionActionItem;
begin
  if CompareText(Value, FName) <> 0 then
  begin
    if FCollection <> nil then
      for I := 0 to FCollection.Count - 1 do
      begin
        Action := TFunctionActionItem(FCollection.Items[I]);
        if (Action <> Self) and (Action is TFunctionActionItem) and
           (CompareText(Value, Action.FunctionName) = 0) then
          raise Exception.Create('nombre de la acción duplicado');
      end;
    FName := Value;
    Changed(False);
  end;

  if FName = '' then
  begin
    FName := Value;
    Changed(False);
  end;
end;

procedure TFunctionActionItem.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TFunctionActionItem.SetFunctionDoc(const Value: TStrings);
begin
  if Length(Value.Text) > 1024 then
    raise Exception.Create(
        'Supera el límite máximo de la descripción de 1024 caracteres');
  FDescription.Text := Value.Text;
end;

procedure TFunctionActionItem.SetJSon(Value: TJSONObject);
var
  aTipo, ADescription, aParamName, aScript: string;
  jFunc, jParameters, jProperties, jParam: TJSONObject;
  jReq: TJSONArray;
  I: Integer;
  aEnabled, aDefault: Boolean;
  aParam: TFunctionParamsItem;
begin
  aTipo := JGetStr(Value, 'type');
  if aTipo = 'function' then
    ToolType := tt_function
  else
    ToolType := ttNone;

  if JTryGetBool(Value, 'enabled', aEnabled) then
    Enabled := aEnabled;
  if JTryGetBool(Value, 'default', aDefault) then
    Default := aDefault;

  jFunc := JFindObj(Value, 'function');
  if jFunc <> nil then
  begin
    if JTryGetStr(jFunc, 'description', ADescription) then
      Description.Text := ADescription;
    if JTryGetStr(jFunc, 'script', aScript) then
      FScript.Text := aScript;
    JTryGetBool(jFunc, 'enabled', FEnabled);

    jParameters := JFindObj(jFunc, 'parameters');
    if jParameters <> nil then
    begin
      if JGetStr(jParameters, 'type') = 'object' then
      begin
        jProperties := JFindObj(jParameters, 'properties');
        if jProperties <> nil then
        begin
          for I := 0 to jProperties.Count - 1 do
          begin
            aParamName := jProperties.Names[I];
            jParam := TJSONObject(jProperties.Items[I]);
            aParam := Parameters.Add;
            aParam.Name := aParamName;
            aParam.SetJSon(jParam);
          end;
        end;

        jReq := JFindArr(jParameters, 'required');
        if jReq <> nil then
        begin
          for I := 0 to jReq.Count - 1 do
          begin
            if Assigned(Parameters.GetParamByName(jReq.Items[I].AsString)) then
              Parameters.GetParamByName(jReq.Items[I].AsString).Required := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFunctionActionItem.SetOnAction(const Value: TFunctionEvent);
begin
  FOnAction := Value;
  Changed(False);
end;

procedure TFunctionActionItem.SetScript(const Value: TStrings);
begin
  FScript.Text := Value.Text;
end;

procedure TFunctionActionItem.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

procedure TFunctionActionItem.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TFunctionActionItem.SetToolType(const Value: TToolstype);
begin
  FToolType := Value;
end;

function TFunctionActionItem.ToJSon(Detail: Boolean): TJSONObject;
var
  Fun, Params: TJSONObject;
begin
  Result := nil;

  if FEnabled and (FToolType = tt_function) then
  begin
    Result := TJSONObject.Create;
    Fun := TJSONObject.Create;
    Fun.Add('name', FunctionName);
    Fun.Add('description', Trim(FDescription.Text));

    if Detail then
    begin
      Fun.Add('script', Trim(FScript.Text));
      Fun.Add('enabled', FEnabled);
      Fun.Add('default', FDefault);
    end;

    Params := Parameters.ToJSon(Detail);
    if Assigned(Params) then
      Fun.Add('parameters', Params);

    Result.Add('type', 'function');
    Result.Add('function', Fun);
  end;
end;

// =============================================================================
// TFunctionActionItems
// =============================================================================

function TFunctionActionItems.Add: TFunctionActionItem;
begin
  Result := TFunctionActionItem(inherited Add);
end;

function TFunctionActionItems.AddFunction(Nombre: string; Enabled: Boolean;
    Action: TFunctionEvent): TFunctionActionItem;
begin
  Result := TFunctionActionItem(Add);
  Result.FunctionName := Nombre;
  Result.Enabled := Enabled;
  Result.OnAction := Action;
end;

constructor TFunctionActionItems.Create(AOwner: TPersistent;
    AItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(Self, AItemClass);
end;

destructor TFunctionActionItems.Destroy;
begin
  inherited;
end;

function TFunctionActionItems.GetActionItem(
    Index: Integer): TFunctionActionItem;
begin
  Result := TFunctionActionItem(inherited Items[Index]);
end;

function TFunctionActionItems.GetFunction(Nombre: string): TFunctionActionItem;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Nombre);
  if Idx >= 0 then
    Result := TFunctionActionItem(Items[Idx]);
end;

function TFunctionActionItems.GetItemByName(
    aTagName: string): TFunctionActionItem;
var
  I: Integer;
  CurItem, DefItem: TFunctionActionItem;
begin
  Result := nil;
  DefItem := nil;
  for I := 0 to Count - 1 do
  begin
    CurItem := Items[I] as TFunctionActionItem;
    if CurItem.Default and CurItem.Enabled then
      DefItem := CurItem;
    if (CompareText(CurItem.FunctionName, aTagName) = 0) and CurItem.Enabled then
    begin
      Result := CurItem;
      Break;
    end;
  end;
  if Result = nil then
    Result := DefItem;
end;

function TFunctionActionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TFunctionActionItems.IndexOf(Nombre: string): Integer;
var
  I: Integer;
  Item: TFunctionActionItem;
begin
  Result := -1;
  for I := 0 to Self.Count - 1 do
  begin
    Item := TFunctionActionItem(Self.Items[I]);
    if (Item <> nil) and (AnsiUpperCase(Nombre) = AnsiUpperCase(Item.FunctionName)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TFunctionActionItems.LoadFromFile(FileName: string);
var
  Funcs: TJSONArray;
  jFunc: TJSONObject;
  Lista: TStringStream;
  FuncName: string;
  Item: TFunctionActionItem;
  I: Integer;
  Parsed: TJSONData;
begin
  if not FileExists(FileName) then
    raise Exception.Create('No se encuentra el archivo "' + FileName + '" en el sistema');

  Lista := TStringStream.Create('');
  try
    Lista.LoadFromFile(FileName);
    Parsed := GetJSON(Lista.DataString);
    if not (Parsed is TJSONArray) then
    begin
      Parsed.Free;
      raise Exception.Create('El archivo no contiene un array JSON válido');
    end;
    Funcs := TJSONArray(Parsed);
    try
      Self.Clear;
      for I := 0 to Funcs.Count - 1 do
      begin
        jFunc := TJSONObject(Funcs.Items[I]);
        if JGetStr(jFunc, 'type') = 'function' then
        begin
          FuncName := JGetStr(JFindObj(jFunc, 'function'), 'name');
          Item := Self.Add;
          Item.FunctionName := FuncName;
          Item.SetJSon(jFunc);
        end;
      end;
    finally
      Funcs.Free;
    end;
  finally
    Lista.Free;
  end;
end;

procedure TFunctionActionItems.SaveToFile(FileName: string);
var
  Funcs: TJSONArray;
  Lista: TStringStream;
begin
  Funcs := ToJSon(True);
  Lista := TStringStream.Create(Funcs.FormatJSON);
  try
    Lista.SaveToFile(FileName);
  finally
    Funcs.Free;
    Lista.Free;
  end;
end;

procedure TFunctionActionItems.SetActionItem(Index: Integer;
    Value: TFunctionActionItem);
begin
  Items[Index].Assign(Value);
end;

procedure TFunctionActionItems.SetItemName(Item: TCollectionItem);
var
  I, J: Integer;
  ItemName: string;
  CurItem: TFunctionActionItem;
begin
  J := 1;
  while True do
  begin
    ItemName := Format('FunctionItem%d', [J]);
    I := 0;
    while I < Count do
    begin
      CurItem := Items[I] as TFunctionActionItem;
      if (CurItem <> Item) and
         (CompareText(CurItem.FunctionName, ItemName) = 0) then
      begin
        Inc(J);
        Break;
      end;
      Inc(I);
    end;
    if I >= Count then
    begin
      (Item as TFunctionActionItem).FunctionName := ItemName;
      Break;
    end;
  end;
end;

function TFunctionActionItems.ToJSon(aDetail: Boolean): TJSONArray;
var
  I: Integer;
  Item: TFunctionActionItem;
  JObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for I := 0 to Count - 1 do
  begin
    Item := TFunctionActionItem(GetItem(I));
    JObj := Item.ToJSon(aDetail);
    if Assigned(JObj) then
      Result.Add(JObj);
  end;
end;

procedure TFunctionActionItems.Update(Item: TCollectionItem);
begin
  inherited;
end;

// =============================================================================
// TAiFunctions
// =============================================================================

procedure TAiFunctions.AddMCPClient(aMCPClient: TMCPClientCustom);
var
  NewItem: TMCPClientItem;
begin
  if not Assigned(aMCPClient) then
    raise Exception.Create('Se intentó añadir un objeto TMCPClient nulo.');

  if Trim(aMCPClient.Name) = '' then
    raise Exception.Create(
        'El TMCPClient debe tener una propiedad Name asignada antes de ser añadido.');

  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
    raise Exception.CreateFmt(
        'Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  aMCPClient.OnStreamMessage := FOnMCPStreamMessage;
  aMCPClient.OnLog := FOnLog;
  aMCPClient.OnStatusUpdate := FOnStatusUpdate;

  NewItem := FMCPClients.Add;

  if Assigned(NewItem.FMCPClient) then
    FreeAndNil(NewItem.FMCPClient);
  NewItem.FMCPClient := aMCPClient;

  NewItem.FParams.Assign(aMCPClient.Params);
  NewItem.FEnvVars.Assign(aMCPClient.EnvVars);
  NewItem.Name := aMCPClient.Name;
  NewItem.Enabled := aMCPClient.Enabled;
  NewItem.TransportType := aMCPClient.TransportType;
  NewItem.Connected := False;

  DoLog(Format('Cliente MCP "%s" añadido y sincronizado.', [aMCPClient.Name]));
end;

constructor TAiFunctions.Create(AOwner: TComponent);
begin
  inherited;
  FFunctions := TFunctionActionItems.Create(Self, TFunctionActionItem);
  FMCPClients := TMCPClientItems.Create(Self);
end;

destructor TAiFunctions.Destroy;
begin
  FFunctions.Free;
  FMCPClients.Free;
  inherited;
end;

function TAiFunctions.DoCallFunction(ToolCall: TAiToolsFunction): Boolean;
var
  Funcion: TFunctionActionItem;
  PosAt: Integer;
  ServerName, ActualToolName: string;
  ClientItem: TMCPClientItem;
  ArgsObject, ResultObject: TJSONObject;
  MediaList: TObjectList; // lista temporal de media
  ResMsg: TAiChatMessage;
  MF: TAiMediaFile;
  I: Integer;
  Parsed: TJSONData;
begin
  Result := False;

  MediaList := TObjectList.Create;
  MediaList.OwnsObjects := True;

  try
    if Copy(ToolCall.Name, 1, 9) = 'local_99_' then
      ToolCall.Name := Copy(ToolCall.Name, 10, Length(ToolCall.Name));

    PosAt := Pos('_99_', ToolCall.Name);

    if PosAt = 0 then // Función local
    begin
      Funcion := FFunctions.GetFunction(ToolCall.Name);
      if Assigned(Funcion) and Assigned(Funcion.OnAction) then
        Funcion.OnAction(Self, Funcion, ToolCall.Name, ToolCall, Result);
    end
    else // Llamada a cliente MCP
    begin
      ServerName := Copy(ToolCall.Name, 1, PosAt - 1);
      ActualToolName := Copy(ToolCall.Name, PosAt + 4, Length(ToolCall.Name));

      ClientItem := FMCPClients.GetClientByName(ServerName);
      if Assigned(ClientItem) and ClientItem.Enabled and
         ClientItem.MCPClient.Available then
      begin
        ArgsObject := nil;
        ResultObject := nil;
        try
          ClientItem.MCPClient.OnLog := FOnLog;
          ClientItem.MCPClient.OnStatusUpdate := FOnStatusUpdate;

          if (ToolCall.Arguments <> '') and (ToolCall.Arguments <> '{}') then
          begin
            Parsed := GetJSON(ToolCall.Arguments);
            if not (Parsed is TJSONObject) then
            begin
              Parsed.Free;
              ArgsObject := TJSONObject.Create;
            end
            else
              ArgsObject := TJSONObject(Parsed);
            // Note: CallTool stub ignores AMedia (TObject param)
            ResultObject := ClientItem.MCPClient.CallTool(
                ActualToolName, ArgsObject, MediaList);
          end
          else
          begin
            ArgsObject := TJSONObject.Create;
            ResultObject := ClientItem.MCPClient.CallTool(
                ActualToolName, ArgsObject, MediaList);
          end;

          if Assigned(ResultObject) then
          begin
            ToolCall.Response := ResultObject.AsJSON;
            ResultObject.Free;

            if MediaList.Count > 0 then
            begin
              if Assigned(ToolCall.ResMsg) then
              begin
                ResMsg := TAiChatMessage(ToolCall.ResMsg);
                for I := 0 to MediaList.Count - 1 do
                begin
                  MF := TAiMediaFile(MediaList[I]);
                  ResMsg.MediaFiles.Add(MF);
                end;
                MediaList.OwnsObjects := False;
              end;
            end;
          end
          else
            ToolCall.Response := '{}';

          Result := True;
        except
          on E: Exception do
          begin
            FreeAndNil(ResultObject);
            Result := False;
          end;
        end;
      end;
    end;

  finally
    MediaList.Free;
  end;
end;

procedure TAiFunctions.DoLog(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Msg);
end;

procedure TAiFunctions.DoStatusUpdate(const StatusMsg: string);
begin
  if Assigned(FOnStatusUpdate) then
    FOnStatusUpdate(Self, StatusMsg);
end;

function TAiFunctions.ExtractFunctionNames: TStringList;
var
  JsonArray: TJSONArray;
  FunctionObj: TJSONObject;
  FunctionName, JsonString: string;
  I: Integer;
  Parsed: TJSONData;
begin
  Result := TStringList.Create;
  try
    JsonString := GetTools(TToolFormat.tfOpenAI);

    Parsed := GetJSON(JsonString);
    if not (Parsed is TJSONArray) then
    begin
      Parsed.Free;
      raise Exception.Create('JSON inválido o no es un array');
    end;
    JsonArray := TJSONArray(Parsed);
    try
      for I := 0 to JsonArray.Count - 1 do
      begin
        if not (JsonArray.Items[I] is TJSONObject) then Continue;
        if JGetStr(TJSONObject(JsonArray.Items[I]), 'type') = 'function' then
        begin
          FunctionObj := JFindObj(TJSONObject(JsonArray.Items[I]), 'function');
          if FunctionObj <> nil then
          begin
            FunctionName := JGetStr(FunctionObj, 'name');
            FunctionName := StringReplace(FunctionName, '_99_', '-->', [rfReplaceAll]);
            Result.Add(FunctionName);
          end;
        end;
      end;
    finally
      JsonArray.Free;
    end;
  except
    on E: Exception do
    begin
      Result.Free;
      raise Exception.Create('Error al procesar JSON: ' + E.Message);
    end;
  end;
end;

function TAiFunctions.GetTools(aToolFormat: TToolFormat): string;
var
  LAllNormalizedTools: TNormalizedToolList;
  FinalToolsObj: TJSONObject;
  LocalToolsObj: TJSONObject;
  ClientItem: TMCPClientItem;
  ClientToolsJson: TJSONObject;
  I: Integer;
  SourceJsonStr: string;
  Parsed: TJSONData;
  LResultArray: TJSONArray;
begin
  LAllNormalizedTools := TNormalizedToolList.Create;
  FinalToolsObj := nil;
  LocalToolsObj := nil;

  try
    // 1. Normalizar funciones locales
    LocalToolsObj := TJSONObject.Create;
    LocalToolsObj.Add('tools', FFunctions.ToJSon);
    TJsonToolUtils.NormalizeToolsFromSource('local', LocalToolsObj,
        LAllNormalizedTools);

    // 2. Normalizar funciones de clientes MCP
    if not (csDesigning in ComponentState) then
    begin
      for I := 0 to FMCPClients.Count - 1 do
      begin
        ClientItem := TMCPClientItem(FMCPClients.Items[I]);
        if not ClientItem.Enabled then Continue;

        if not ClientItem.MCPClient.Initialized then
          ClientItem.MCPClient.Initialize;

        if ClientItem.MCPClient.Available then
        begin
          SourceJsonStr := ClientItem.MCPClient.Tools.Text;
          if SourceJsonStr <> '' then
          begin
            Parsed := GetJSON(SourceJsonStr);
            try
              if Parsed is TJSONObject then
              begin
                ClientToolsJson := TJSONObject(Parsed);
                TJsonToolUtils.NormalizeToolsFromSource(
                    ClientItem.Name, ClientToolsJson, LAllNormalizedTools);
              end;
            finally
              Parsed.Free;
            end;
          end;
        end;
      end;
    end;

    // 3. Formatear lista maestra al formato solicitado
    FinalToolsObj := TJsonToolUtils.FormatToolList(LAllNormalizedTools, aToolFormat);

    LResultArray := JFindArr(FinalToolsObj, 'tools');
    if LResultArray <> nil then
      Result := LResultArray.AsJSON
    else
      Result := '[]';

  finally
    LAllNormalizedTools.Free;
    FinalToolsObj.Free;
    LocalToolsObj.Free;
  end;
end;

function TAiFunctions.ImportClaudeMCPConfiguration(
    const AJsonFilePath: string): Integer;
var
  LFinalPath: string;
  LJsonContent: string;
  LRootObj: TJSONObject;
  SS: TStringStream;
  Parsed: TJSONData;
begin
  Result := 0;
  LFinalPath := AJsonFilePath;

  if LFinalPath = '' then
  begin
{$IFDEF MSWINDOWS}
    LFinalPath := IncludeTrailingPathDelimiter(
        GetEnvironmentVariable('APPDATA')) +
        'Claude\claude_desktop_config.json';
{$ENDIF}
{$IFDEF UNIX}
    LFinalPath := IncludeTrailingPathDelimiter(GetUserDir) +
        '.config/Claude/claude_desktop_config.json';
{$ENDIF}
  end;

  if not FileExists(LFinalPath) then
  begin
    DoLog('ImportClaude: Archivo no encontrado en ' + LFinalPath);
    Exit;
  end;

  try
    SS := TStringStream.Create('');
    try
      SS.LoadFromFile(LFinalPath);
      LJsonContent := SS.DataString;
    finally
      SS.Free;
    end;

    if Trim(LJsonContent) = '' then
    begin
      DoLog('ImportClaude: El archivo está vacío.');
      Exit;
    end;

    Parsed := GetJSON(LJsonContent);
    if Assigned(Parsed) and (Parsed is TJSONObject) then
    begin
      LRootObj := TJSONObject(Parsed);
      try
        DoLog('Importando servidores MCP desde: ' + LFinalPath);
        Result := ImportClaudeMCPConfiguration(LRootObj);
      finally
        LRootObj.Free;
      end;
    end
    else
    begin
      if Assigned(Parsed) then Parsed.Free;
      DoLog('ImportClaude: El contenido no es un objeto JSON válido.');
    end;

  except
    on E: Exception do
      DoLog('ImportClaude: Error fatal al leer o parsear: ' + E.Message);
  end;
end;

procedure TAiFunctions.Loaded;
var
  I: Integer;
begin
  inherited;
  if Assigned(FMCPClients) then
    for I := 0 to FMCPClients.Count - 1 do
      if Assigned(FMCPClients[I]) then
        FMCPClients[I].UpdateClientProperties;
end;

function TAiFunctions.ImportClaudeMCPConfiguration(
    AConfig: TJSONObject): Integer;
var
  LMcpServers, LServerObj, LEnvObj: TJSONObject;
  LArgsArray: TJSONArray;
  LClientItem: TMCPClientItem;
  LServerName, LCommand, LUrl, LArgsString, LArg: string;
  I, J: Integer;
  LEnvName: string;
begin
  Result := 0;
  if not Assigned(AConfig) then Exit;

  LMcpServers := JFindObj(AConfig, 'mcpServers');
  if LMcpServers = nil then
  begin
    DoLog('ImportClaude: No se encontró el nodo "mcpServers".');
    Exit;
  end;

  for I := 0 to LMcpServers.Count - 1 do
  begin
    LServerName := LMcpServers.Names[I];
    if not (LMcpServers.Items[I] is TJSONObject) then Continue;

    // Evitar duplicados
    if Assigned(FMCPClients.GetClientByName(LServerName)) then Continue;

    LServerObj := TJSONObject(LMcpServers.Items[I]);

    LClientItem := FMCPClients.Add;
    LClientItem.Name := LServerName;

    // Caso A: Servidor local (StdIo)
    if JTryGetStr(LServerObj, 'command', LCommand) then
    begin
      LClientItem.TransportType := tpStdIo;
      LClientItem.Params.Values['Command'] := LCommand;

      LArgsArray := JFindArr(LServerObj, 'args');
      if LArgsArray <> nil then
      begin
        LArgsString := '';
        for J := 0 to LArgsArray.Count - 1 do
        begin
          LArg := LArgsArray.Items[J].AsString;
          if (Pos(' ', LArg) > 0) and (Copy(LArg, 1, 1) <> '"') then
            LArg := '"' + LArg + '"';
          LArgsString := LArgsString + LArg + ' ';
        end;
        LClientItem.Params.Values['Arguments'] := Trim(LArgsString);
      end;

      LClientItem.Params.Values['RootDir'] := GetUserDir;
    end
    // Caso B: Servidor remoto (SSE)
    else if JTryGetStr(LServerObj, 'url', LUrl) then
    begin
      LClientItem.TransportType := tpSSE;
      LClientItem.Params.Values['URL'] := LUrl;
    end;

    // Variables de entorno
    LEnvObj := JFindObj(LServerObj, 'env');
    if LEnvObj <> nil then
    begin
      for J := 0 to LEnvObj.Count - 1 do
      begin
        LEnvName := LEnvObj.Names[J];
        LClientItem.EnvVars.Values[LEnvName] := LEnvObj.Items[J].AsString;
      end;
    end;

    LClientItem.Enabled := True;
    LClientItem.UpdateClientProperties;

    Inc(Result);
    DoLog(Format('ImportClaude: Servidor "%s" cargado exitosamente.', [LServerName]));
  end;
end;

function TAiFunctions.SetFunctionEnable(FunctionName: string;
    Enabled: Boolean): Boolean;
var
  Item: TFunctionActionItem;
begin
  Result := False;
  Item := Functions.GetFunction(FunctionName);
  if Assigned(Item) then
  begin
    Item.Enabled := Enabled;
    Result := True;
  end;
end;

function TAiFunctions.SetMCPClientEnable(AName: string;
    Enabled: Boolean): Boolean;
var
  Item: TMCPClientItem;
begin
  Result := False;
  Item := FMCPClients.GetClientByName(AName);
  if Assigned(Item) then
  begin
    Item.Enabled := Enabled;
    Result := True;
  end;
end;

procedure TAiFunctions.SetOnMCPStreamMessage(
    const Value: TMCPStreamMessageEvent);
var
  I: Integer;
begin
  FOnMCPStreamMessage := Value;
  for I := 0 to FMCPClients.Count - 1 do
    if Assigned(FMCPClients[I].MCPClient) then
      FMCPClients[I].MCPClient.OnStreamMessage := Value;
end;

// =============================================================================
// TMCPClientItem
// =============================================================================

constructor TMCPClientItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FEnabled := True;
  FConnected := False;
  FParams := TStringList.Create;
  FEnvVars := TStringList.Create;
  FName := 'MCPClient';
  // Phase 2: usa stub base para todos los tipos de transporte
  FMCPClient := TMCPClientCustom.Create(nil);
  FMCPClient.Name := FName;
  FParams.Assign(FMCPClient.Params);
end;

destructor TMCPClientItem.Destroy;
begin
  FParams.Free;
  FEnvVars.Free;
  if Assigned(FMCPClient) then
    FreeAndNil(FMCPClient);
  inherited;
end;

function TMCPClientItem.GetConfiguration: string;
begin
  Result := Format('(%s, Click [...] to edit)', ['Properties']);
end;

function TMCPClientItem.GetDisplayName: string;
begin
  Result := GetName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TMCPClientItem.GetEnvVars: TStrings;
begin
  Result := FEnvVars;
end;

function TMCPClientItem.GetName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TMCPClientItem.GetParams: TStrings;
begin
  Result := FParams;
end;

function TMCPClientItem.GetTransportType: TToolTransportType;
begin
  Result := tpStdIo;
  if Assigned(FMCPClient) then
    Result := FMCPClient.TransportType;
end;

procedure TMCPClientItem.SetConfiguration(const Value: string);
begin
  // No-op: solo debe existir para el Object Inspector
end;

procedure TMCPClientItem.SetConnected(const Value: Boolean);
var
  ClientTools: TJSONObject;
begin
  if FConnected = Value then Exit;

  if not Value then
  begin
    FConnected := Value;
    Changed(False);
    Exit;
  end;

  try
    try
      ClientTools := Self.MCPClient.ListTools;
      if not Assigned(ClientTools) then
        raise Exception.Create(Format(
            '[ERR] Fallo de conexión para "%s".'#13#10#13#10 +
            'Revise la configuración y los logs del servidor.', [Self.Name]))
      else
      begin
        ClientTools.Free;
        FConnected := Value;
        Changed(False);
      end;
    except
      on E: Exception do
        raise Exception.Create(Format(
            '[ERR] Excepción al validar "%s".'#13#10#13#10'%s: %s',
            [Self.Name, E.ClassName, E.Message]));
    end;
  finally
  end;
end;

procedure TMCPClientItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TMCPClientItem.SetEnvVars(const Value: TStrings);
begin
  FEnvVars.Assign(Value);
  if Assigned(FMCPClient) and Assigned(Value) then
  begin
    FMCPClient.EnvVars.Assign(Value);
    Changed(False);
  end;
end;

procedure TMCPClientItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    inherited SetDisplayName(Value);
    if Assigned(FMCPClient) then
      FMCPClient.Name := Value;
    Changed(False);
  end;
end;

procedure TMCPClientItem.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
  if Assigned(FMCPClient) and Assigned(Value) then
  begin
    FMCPClient.Params.Assign(Value);
    Changed(False);
  end;
end;

procedure TMCPClientItem.SetTransportType(const Value: TToolTransportType);
begin
  // Phase 2: Solo actualiza el tipo en el cliente existente.
  // En Phase 6, aquí se crearían las subclases específicas (StdIo, Http, SSE, MakerAi).
  if not Assigned(FMCPClient) or (FMCPClient.TransportType <> Value) then
  begin
    FreeAndNil(FMCPClient);
    FMCPClient := TMCPClientCustom.Create(nil);
    FMCPClient.TransportType := Value;
    UpdateClientProperties;
    Changed(False);
  end;
end;

procedure TMCPClientItem.UpdateClientProperties;
begin
  if not Assigned(FMCPClient) then Exit;
  FMCPClient.TransportType := Self.GetTransportType;
  FMCPClient.Name := FName;
  FMCPClient.Enabled := Self.FEnabled;
  FMCPClient.Params.Assign(Self.FParams);
  FMCPClient.EnvVars.Assign(Self.FEnvVars);
end;

// =============================================================================
// TMCPClientItems
// =============================================================================

function TMCPClientItems.Add: TMCPClientItem;
begin
  Result := TMCPClientItem(inherited Add);
end;

constructor TMCPClientItems.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  inherited Create(Self, TMCPClientItem);
end;

destructor TMCPClientItems.Destroy;
begin
  inherited;
end;

function TMCPClientItems.GetClient(Index: Integer): TMCPClientItem;
begin
  Result := TMCPClientItem(inherited Items[Index]);
end;

function TMCPClientItems.GetClientByName(const AName: string): TMCPClientItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if CompareText(Items[I].Name, AName) = 0 then
    begin
      Result := Items[I];
      Exit;
    end;
  end;
end;

function TMCPClientItems.GetClientsList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Count - 1 do
    Result.Add(Items[I].Name);
end;

function TMCPClientItems.GetFunctionList(Name: string): TStringList;
begin
  Result := nil;
end;

function TMCPClientItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMCPClientItems.SetClient(Index: Integer;
    const Value: TMCPClientItem);
begin
  Items[Index].Assign(Value);
end;

procedure TMCPClientItems.Update(Item: TCollectionItem);
begin
  inherited;
end;

// =============================================================================
// TJsonToolUtils — Normalización y formateo de herramientas
// =============================================================================

class procedure TJsonToolUtils.CleanInputSchema(ASchema: TJSONObject);
begin
  CleanJsonTree(ASchema);
end;

class procedure TJsonToolUtils.CleanJsonTree(AValue: TJSONData);
var
  LObject: TJSONObject;
  LArray: TJSONArray;
  I: Integer;
begin
  if not Assigned(AValue) then Exit;

  if AValue is TJSONObject then
  begin
    LObject := TJSONObject(AValue);
    LObject.Delete('additionalProperties');
    LObject.Delete('$schema');
    for I := 0 to LObject.Count - 1 do
      CleanJsonTree(LObject.Items[I]);
  end
  else if AValue is TJSONArray then
  begin
    LArray := TJSONArray(AValue);
    for I := 0 to LArray.Count - 1 do
      CleanJsonTree(LArray.Items[I]);
  end;
end;

class function TJsonToolUtils.DetectInputFormat(
    AJsonTool: TJSONObject): TToolFormat;
var
  LTypeValue: TJSONData;
begin
  Result := tfUnknown;
  if not Assigned(AJsonTool) then Exit;

  if AJsonTool.Find('functionDeclarations') <> nil then
  begin
    Result := tfGemini;
    Exit;
  end;

  LTypeValue := AJsonTool.Find('type');
  if (LTypeValue <> nil) and (LTypeValue is TJSONString) and
     (LTypeValue.AsString = 'function') then
  begin
    if AJsonTool.Find('function') <> nil then
    begin
      Result := tfOpenAI;
      Exit;
    end
    else if AJsonTool.Find('name') <> nil then
    begin
      Result := tfOpenAIResponses;
      Exit;
    end;
    Result := tfOpenAIResponses;
    Exit;
  end;

  if AJsonTool.Find('input_schema') <> nil then
  begin
    Result := tfClaude;
    Exit;
  end;

  if AJsonTool.Find('inputSchema') <> nil then
  begin
    Result := tfMCP;
    Exit;
  end;
end;

class procedure TJsonToolUtils.EnforceStrictSchema(ASchema: TJSONData);
var
  JObj, JProps: TJSONObject;
  jReq: TJSONArray;
  PropName: string;
  I, J: Integer;
  ExistsInReq: Boolean;
  D: TJSONData;
begin
  if not (ASchema is TJSONObject) then Exit;

  JObj := TJSONObject(ASchema);

  JProps := JFindObj(JObj, 'properties');
  if (JProps <> nil) or (JGetStr(JObj, 'type') = 'object') then
  begin
    JObj.Delete('additionalProperties');
    JObj.Add('additionalProperties', False);

    if JProps <> nil then
    begin
      D := JObj.Find('required');
      if (D <> nil) and (D is TJSONArray) then
        jReq := TJSONArray(D)
      else
      begin
        jReq := TJSONArray.Create;
        JObj.Add('required', jReq);
      end;

      for I := 0 to JProps.Count - 1 do
      begin
        PropName := JProps.Names[I];
        ExistsInReq := False;
        for J := 0 to jReq.Count - 1 do
        begin
          if jReq.Items[J].AsString = PropName then
          begin
            ExistsInReq := True;
            Break;
          end;
        end;
        if not ExistsInReq then
          jReq.Add(PropName);
        EnforceStrictSchema(JProps.Items[I]);
      end;
    end;
  end;
end;

class procedure TJsonToolUtils.NormalizeFromMCP(AJsonTool: TJSONObject;
    AToolList: TNormalizedToolList);
var
  LName, LDescription: string;
  LInputSchema: TJSONObject;
  D: TJSONData;
begin
  if not JTryGetStr(AJsonTool, 'name', LName) then Exit;
  JTryGetStr(AJsonTool, 'description', LDescription);

  D := AJsonTool.Find('inputSchema');
  if (D <> nil) and (D is TJSONObject) then
  begin
    LInputSchema := TJSONObject(D.Clone);
    CleanInputSchema(LInputSchema);
  end
  else
    LInputSchema := TJSONObject.Create;

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

class procedure TJsonToolUtils.NormalizeFromAnthropic(AJsonTool: TJSONObject;
    AToolList: TNormalizedToolList);
var
  LName, LDescription: string;
  LInputSchema: TJSONObject;
  D: TJSONData;
begin
  if not JTryGetStr(AJsonTool, 'name', LName) then Exit;
  JTryGetStr(AJsonTool, 'description', LDescription);

  D := AJsonTool.Find('input_schema');
  if (D <> nil) and (D is TJSONObject) then
  begin
    LInputSchema := TJSONObject(D.Clone);
    CleanInputSchema(LInputSchema);
  end
  else
    LInputSchema := TJSONObject.Create;

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

class procedure TJsonToolUtils.NormalizeFromOpenAI(AJsonTool: TJSONObject;
    AToolList: TNormalizedToolList);
var
  LName, LDescription: string;
  LInputSchema: TJSONObject;
  LFunctionObject, LDataSource: TJSONObject;
  D: TJSONData;
begin
  LFunctionObject := JFindObj(AJsonTool, 'function');
  if LFunctionObject <> nil then
    LDataSource := LFunctionObject
  else
    LDataSource := AJsonTool;

  if not JTryGetStr(LDataSource, 'name', LName) then Exit;
  JTryGetStr(LDataSource, 'description', LDescription);

  D := LDataSource.Find('parameters');
  if (D <> nil) and (D is TJSONObject) then
  begin
    LInputSchema := TJSONObject(D.Clone);
    CleanInputSchema(LInputSchema);
  end
  else
    LInputSchema := TJSONObject.Create;

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

class procedure TJsonToolUtils.NormalizeFromGemini(AJsonTool: TJSONObject;
    AToolList: TNormalizedToolList);
var
  LFuncDeclarations: TJSONArray;
  I: Integer;
  LFuncDecl: TJSONObject;
  LSchema: TJSONObject;
  LName, LDescription: string;
  D: TJSONData;
begin
  LFuncDeclarations := JFindArr(AJsonTool, 'functionDeclarations');
  if LFuncDeclarations = nil then Exit;

  for I := 0 to LFuncDeclarations.Count - 1 do
  begin
    if not (LFuncDeclarations.Items[I] is TJSONObject) then Continue;
    LFuncDecl := TJSONObject(LFuncDeclarations.Items[I]);

    if not JTryGetStr(LFuncDecl, 'name', LName) then Continue;
    JTryGetStr(LFuncDecl, 'description', LDescription);

    D := LFuncDecl.Find('parameters');
    if (D <> nil) and (D is TJSONObject) then
    begin
      LSchema := TJSONObject(D.Clone);
      CleanInputSchema(LSchema);
    end
    else
      LSchema := TJSONObject.Create;

    AToolList.Add(TNormalizedTool.Create(LName, LDescription, LSchema));
  end;
end;

class procedure TJsonToolUtils.NormalizeToolsFromSource(
    const ASourceName: string; ASourceJson: TJSONObject;
    ANormalizedList: TNormalizedToolList);
var
  LSourceToolsArray: TJSONArray;
  I, J: Integer;
  LSourceTool: TJSONObject;
  LDetectedFormat: TToolFormat;
  LInitialCount: Integer;
begin
  if not Assigned(ASourceJson) then Exit;

  LSourceToolsArray := JFindArr(ASourceJson, 'tools');
  if (LSourceToolsArray = nil) or (LSourceToolsArray.Count = 0) then Exit;

  if not (LSourceToolsArray.Items[0] is TJSONObject) then Exit;

  LDetectedFormat := DetectInputFormat(
      TJSONObject(LSourceToolsArray.Items[0]));
  if LDetectedFormat = tfUnknown then Exit;

  for I := 0 to LSourceToolsArray.Count - 1 do
  begin
    if not (LSourceToolsArray.Items[I] is TJSONObject) then Continue;
    LSourceTool := TJSONObject(LSourceToolsArray.Items[I]);

    LInitialCount := ANormalizedList.Count;

    case LDetectedFormat of
      tfMCP:
        NormalizeFromMCP(LSourceTool, ANormalizedList);
      tfClaude:
        NormalizeFromAnthropic(LSourceTool, ANormalizedList);
      tfOpenAI, tfOpenAIResponses:
        NormalizeFromOpenAI(LSourceTool, ANormalizedList);
      tfGemini:
        NormalizeFromGemini(LSourceTool, ANormalizedList);
    end;

    // Aplicar prefijo de fuente a las herramientas recién añadidas
    if ASourceName <> '' then
    begin
      for J := LInitialCount to ANormalizedList.Count - 1 do
        ANormalizedList[J].FName :=
            Format('%s_99_%s', [ASourceName, ANormalizedList[J].Name]);
    end;
  end;
end;

class function TJsonToolUtils.FormatAsMCP(
    ANormalizedTool: TNormalizedTool): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('name', ANormalizedTool.Name);
  Result.Add('description', ANormalizedTool.Description);
  Result.Add('inputSchema', TJSONObject(ANormalizedTool.InputSchema.Clone));
end;

class function TJsonToolUtils.FormatAsAnthropic(
    ANormalizedTool: TNormalizedTool): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('name', ANormalizedTool.Name);
  Result.Add('description', ANormalizedTool.Description);
  Result.Add('input_schema', TJSONObject(ANormalizedTool.InputSchema.Clone));
end;

class function TJsonToolUtils.FormatAsOpenAI(
    ANormalizedTool: TNormalizedTool): TJSONObject;
var
  LFunctionObject: TJSONObject;
begin
  LFunctionObject := TJSONObject.Create;
  LFunctionObject.Add('name', ANormalizedTool.Name);
  LFunctionObject.Add('description', ANormalizedTool.Description);

  if Assigned(ANormalizedTool.InputSchema) and
     (ANormalizedTool.InputSchema.Count > 0) then
    LFunctionObject.Add('parameters',
        TJSONObject(ANormalizedTool.InputSchema.Clone));

  Result := TJSONObject.Create;
  Result.Add('type', 'function');
  Result.Add('function', LFunctionObject);
end;

class function TJsonToolUtils.FormatAsOpenAIResponses(
    ANormalizedTool: TNormalizedTool): TJSONObject;
var
  LParams: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', 'function');
  Result.Add('name', ANormalizedTool.Name);

  if ANormalizedTool.Description <> '' then
    Result.Add('description', ANormalizedTool.Description);

  if Assigned(ANormalizedTool.InputSchema) and
     (ANormalizedTool.InputSchema.Count > 0) then
  begin
    LParams := TJSONObject(ANormalizedTool.InputSchema.Clone);
    EnforceStrictSchema(LParams);
    Result.Add('parameters', LParams);
  end
  else
  begin
    LParams := TJSONObject.Create;
    LParams.Add('type', 'object');
    LParams.Add('properties', TJSONObject.Create);
    LParams.Add('additionalProperties', False);
    LParams.Add('required', TJSONArray.Create);
    Result.Add('parameters', LParams);
  end;

  Result.Add('strict', True);
end;

class function TJsonToolUtils.FormatAsGeminiFunctionDeclaration(
    ANormalizedTool: TNormalizedTool): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('name', ANormalizedTool.Name);
  Result.Add('description', ANormalizedTool.Description);

  if Assigned(ANormalizedTool.InputSchema) and
     (ANormalizedTool.InputSchema.Count > 0) then
    Result.Add('parameters',
        TJSONObject(ANormalizedTool.InputSchema.Clone));
end;

class function TJsonToolUtils.FormatToolList(
    ANormalizedList: TNormalizedToolList;
    AOutputFormat: TToolFormat): TJSONObject;
var
  LFinalToolsArray: TJSONArray;
  LNormTool: TNormalizedTool;
  LFormattedTool: TJSONObject;
  LDeclarationsArray: TJSONArray;
  LGeminiToolWrapper: TJSONObject;
  I: Integer;
begin
  Result := TJSONObject.Create;
  LFinalToolsArray := TJSONArray.Create;
  Result.Add('tools', LFinalToolsArray);

  if ANormalizedList.Count = 0 then Exit;

  if AOutputFormat = tfGemini then
  begin
    LDeclarationsArray := TJSONArray.Create;
    for I := 0 to ANormalizedList.Count - 1 do
      LDeclarationsArray.Add(
          FormatAsGeminiFunctionDeclaration(ANormalizedList[I]));

    LGeminiToolWrapper := TJSONObject.Create;
    LGeminiToolWrapper.Add('functionDeclarations', LDeclarationsArray);
    LFinalToolsArray.Add(LGeminiToolWrapper);
  end
  else
  begin
    for I := 0 to ANormalizedList.Count - 1 do
    begin
      LNormTool := ANormalizedList[I];
      LFormattedTool := nil;
      case AOutputFormat of
        tfMCP:             LFormattedTool := FormatAsMCP(LNormTool);
        tfClaude:          LFormattedTool := FormatAsAnthropic(LNormTool);
        tfOpenAI:          LFormattedTool := FormatAsOpenAI(LNormTool);
        tfOpenAIResponses: LFormattedTool := FormatAsOpenAIResponses(LNormTool);
      end;
      if Assigned(LFormattedTool) then
        LFinalToolsArray.Add(LFormattedTool);
    end;
  end;
end;

class function TJsonToolUtils.MergeToolLists(const ASourceName: string;
    ASourceJson: TJSONObject; ATargetJson: TJSONObject;
    AOutputFormat: TToolFormat): TJSONObject;
var
  LSourceToolsArray: TJSONArray;
  LFirstTool: TJSONObject;
  LDetectedFormat: TToolFormat;
begin
  LDetectedFormat := tfUnknown;
  if Assigned(ASourceJson) then
  begin
    LSourceToolsArray := JFindArr(ASourceJson, 'tools');
    if (LSourceToolsArray <> nil) and (LSourceToolsArray.Count > 0) and
       (LSourceToolsArray.Items[0] is TJSONObject) then
    begin
      LFirstTool := TJSONObject(LSourceToolsArray.Items[0]);
      LDetectedFormat := DetectInputFormat(LFirstTool);
    end;
  end;

  if LDetectedFormat = tfUnknown then
  begin
    Result := ATargetJson;
    Exit;
  end;

  Result := MergeToolLists(ASourceName, ASourceJson, LDetectedFormat,
      ATargetJson, AOutputFormat);
end;

class function TJsonToolUtils.MergeToolLists(const ASourceName: string;
    ASourceJson: TJSONObject; AInputFormat: TToolFormat;
    ATargetJson: TJSONObject; AOutputFormat: TToolFormat): TJSONObject;
var
  LSourceToolsArray, LFinalToolsArray: TJSONArray;
  LNormalizedTools: TNormalizedToolList;
  I: Integer;
  LSourceTool: TJSONObject;
  LNormTool: TNormalizedTool;
  LFormattedTool: TJSONObject;
  LDeclarationsArray: TJSONArray;
  LGeminiToolWrapper: TJSONObject;
  D: TJSONData;
begin
  if not Assigned(ASourceJson) then
  begin
    Result := ATargetJson;
    Exit;
  end;

  LSourceToolsArray := JFindArr(ASourceJson, 'tools');
  if LSourceToolsArray = nil then
  begin
    Result := ATargetJson;
    Exit;
  end;

  if not Assigned(ATargetJson) then
  begin
    ATargetJson := TJSONObject.Create;
    ATargetJson.Add('tools', TJSONArray.Create);
  end;

  D := ATargetJson.Find('tools');
  if (D = nil) or not (D is TJSONArray) then
    raise Exception.Create(
        'Target JSON object does not contain a "tools" array.');
  LFinalToolsArray := TJSONArray(D);

  LNormalizedTools := TNormalizedToolList.Create;
  try
    for I := 0 to LSourceToolsArray.Count - 1 do
    begin
      if not (LSourceToolsArray.Items[I] is TJSONObject) then Continue;
      LSourceTool := TJSONObject(LSourceToolsArray.Items[I]);

      case AInputFormat of
        tfMCP:
          NormalizeFromMCP(LSourceTool, LNormalizedTools);
        tfClaude:
          NormalizeFromAnthropic(LSourceTool, LNormalizedTools);
        tfOpenAI, tfOpenAIResponses:
          NormalizeFromOpenAI(LSourceTool, LNormalizedTools);
        tfGemini:
          NormalizeFromGemini(LSourceTool, LNormalizedTools);
      else
        Continue;
      end;
    end;

    if LNormalizedTools.Count = 0 then
    begin
      Result := ATargetJson;
      Exit;
    end;

    // Pre-procesar nombres
    for I := 0 to LNormalizedTools.Count - 1 do
    begin
      LNormTool := LNormalizedTools[I];
      if ASourceName <> '' then
        LNormTool.FName := Format('%s_99_%s', [ASourceName, LNormTool.Name]);
    end;

    // Formatear y añadir al destino
    if AOutputFormat = tfGemini then
    begin
      LDeclarationsArray := TJSONArray.Create;
      for I := 0 to LNormalizedTools.Count - 1 do
        LDeclarationsArray.Add(
            FormatAsGeminiFunctionDeclaration(LNormalizedTools[I]));

      LGeminiToolWrapper := TJSONObject.Create;
      LGeminiToolWrapper.Add('functionDeclarations', LDeclarationsArray);
      LFinalToolsArray.Add(LGeminiToolWrapper);
    end
    else
    begin
      for I := 0 to LNormalizedTools.Count - 1 do
      begin
        LNormTool := LNormalizedTools[I];
        LFormattedTool := nil;
        case AOutputFormat of
          tfMCP:             LFormattedTool := FormatAsMCP(LNormTool);
          tfClaude:          LFormattedTool := FormatAsAnthropic(LNormTool);
          tfOpenAI:          LFormattedTool := FormatAsOpenAI(LNormTool);
          tfOpenAIResponses: LFormattedTool := FormatAsOpenAIResponses(LNormTool);
        end;
        if Assigned(LFormattedTool) then
          LFinalToolsArray.Add(LFormattedTool);
      end;
    end;

  finally
    LNormalizedTools.Free;
  end;

  Result := ATargetJson;
end;

end.
