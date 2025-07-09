unit uMakerAi.ToolFunctions;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, Rest.JSON,  System.IOUtils,
  Data.Db, uMakerAi.Core;


type

  TFunctionActionItem = class;

  TFunctionEvent = Procedure(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: String; ToolCall: TAiToolsFunction; Var Handled: Boolean) Of Object;

  TToolstype = (tt_function, ttNone);

  TToolsParamType = (ptString, ptInteger, ptBoolean, ptFloat, ptDate, ptTime, ptDateTime, ptBase64);

  TFunctionParamsItem = Class(TCollectionItem)
  Private
    FName: String;
    FCollection: TCollection;
    FParamType: TToolsParamType;
    FRequired: Boolean;
    FDescription: TStrings;
    FEnum: String;
    procedure SetName(const Value: String);
    procedure SetDescription(const Value: TStrings);
    procedure SetEnum(const Value: String);
    procedure SetParamType(const Value: TToolsParamType);
    procedure SetRequired(const Value: Boolean);
  protected
    function GetDisplayName: string; Override;
    procedure SetDisplayName(const Value: string); Override;
  public
    constructor Create(Collection: TCollection); Override;
    Destructor Destroy; Override;
    function GetNamePath: string; override;
    Function ToJSon(Detail: Boolean = False): TJSonObject;
    procedure SetJSon(Value: TJSonObject);

  Published
    Property Name: String read FName write SetName;
    Property ParamType: TToolsParamType read FParamType write SetParamType;
    Property Description: TStrings read FDescription write SetDescription;
    Property Enum: String read FEnum write SetEnum;
    Property Required: Boolean read FRequired write SetRequired;
  End;

  TFunctionParamsItems = class(TOwnedCollection)
  Private
    [weak]
    FOwner: TPersistent;
    function GetParams(Index: Integer): TFunctionParamsItem;
    procedure SetParams(Index: Integer; const Value: TFunctionParamsItem);
  Protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
    Function GetParamByName(aParamName: String): TFunctionParamsItem;
  Public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    Destructor Destroy; Override;
    function Add: TFunctionParamsItem;
    Function ToJSon(Detail: Boolean = False): TJSonObject;
    property Params[Index: Integer]: TFunctionParamsItem read GetParams write SetParams; default;
  Published
  end;

  TFunctionActionItem = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FName: String;
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
    { Private declarations }
  protected
    function GetDisplayName: string; Override;
    procedure SetDisplayName(const Value: string); Override;
  public
    constructor Create(Collection: TCollection); Override;
    Destructor Destroy; Override;
    function GetNamePath: string; override;
    Function ToJSon(Detail: Boolean = False): TJSonObject;
    Procedure SetJSon(Value: TJSonObject);

    Property TagObject: TObject read FTagObject write SetTagObject;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FunctionName: string read GetDisplayName write SetDisplayName;
    property OnAction: TFunctionEvent read FOnAction write SetOnAction;
    // Property OnFunctionGetInfo: TFunctionGetInfoEvent read FOnFunctionGetInoAction write SetOnActionTag;
    Property Description: TStrings read FDescription write SetFunctionDoc;
    Property Script: TStrings read FScript write SetScript;
    Property Parameters: TFunctionParamsItems Read FParams Write FParams;
    Property Default: Boolean read FDefault write SetDefault;
    Property Tag: Integer read FTag write SetTag;
    Property ToolType: TToolstype read FToolType write SetToolType;
  end;

  TFunctionActionItems = class(TOwnedCollection)
  Private
    [weak]
    FOwner: TPersistent;
    function GetActionItem(Index: Integer): TFunctionActionItem;
    procedure SetActionItem(Index: Integer; Value: TFunctionActionItem);
  Protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
    procedure Update(Item: TCollectionItem); override;
    Function GetItemByName(aTagName: String): TFunctionActionItem;
  Public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    Destructor Destroy; Override;
    function Add: TFunctionActionItem;
    Function IndexOf(Nombre: String): Integer;
    Function GetFunction(Nombre: String): TFunctionActionItem;
    Function AddFunction(Nombre: String; Enabled: Boolean; Action: TFunctionEvent): TFunctionActionItem;
    Function ToJSon(aDetail: Boolean = False): TJSonArray;
    Procedure SaveToFile(FileName: String);
    Procedure LoadFromFile(FileName: String);

    property Items[Index: Integer]: TFunctionActionItem read GetActionItem write SetActionItem; default;
  Published
  end;

  TAiFunctions = Class(TComponent)
  Private
    FFunctions: TFunctionActionItems;
  Protected
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    function GetTools: String; Virtual;
    Function DoCallFunction(ToolCall: TAiToolsFunction): Boolean; Virtual;
    // SetFunctionEnable  Retorna True si encuentra la función y puede actualizar el estado
    Function SetFunctionEnable(FunctionName : String; Enabled : Boolean) : Boolean;
  Published
    Property Functions: TFunctionActionItems read FFunctions write FFunctions;
  End;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiFunctions]);
end;

{ TWebTagActionItem }

constructor TFunctionActionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := Collection;
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
  Inherited GetDisplayName;
  Result := FName;
end;

function TFunctionActionItem.GetNamePath: string;
begin
  Result := inherited GetNamePath + Self.FunctionName; // Format('MyField%d',[Index]);
end;

procedure TFunctionActionItem.SetDefault(const Value: Boolean);
var
  I: Integer;
begin
  // Primero elimina el default de los otros items
  If Value = True then
  Begin
    for I := 0 to FCollection.Count - 1 do
      TFunctionActionItem(FCollection.Items[I]).Default := False;
  End;

  // Self.Collection

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
        if (Action <> Self) and (Action is TFunctionActionItem) and (CompareText(Value, Action.FunctionName) = 0) then
          raise Exception.Create('nombre de la acción duplicado');
      end;
    FName := Value;
    Changed(False);
  end;

  If FName = '' then
  Begin
    FName := Value;
    Changed(False);
  End;
end;

procedure TFunctionActionItem.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TFunctionActionItem.SetFunctionDoc(const Value: TStrings);
begin
  If Length(Value.Text) > 1024 then
     Raise Exception.Create('Supera el límite máximo de la descripción de 1024 caracteres');

  FDescription.Text := Value.Text;
end;

procedure TFunctionActionItem.SetJSon(Value: TJSonObject);
Var
  aTipo, aDescription, aParamName, aScript: String;
  jFunc, jParameters, jProperties, jParam: TJSonObject;
  jReq: TJSonArray;
  jVal: TJSonValue;
  aEnabled, aDefault: Boolean;
  aParam: TFunctionParamsItem;
  I: Integer;
begin
  aTipo := Value.GetValue<String>('type');
  If aTipo = 'function' then
    ToolType := TToolstype.tt_function
  Else
    ToolType := TToolstype.ttNone;

  If Value.TryGetValue<Boolean>('enabled', aEnabled) then
    Enabled := aEnabled;

  If Value.TryGetValue<Boolean>('default', aDefault) then
    Default := aDefault;

  If Value.TryGetValue<TJSonObject>('function', jFunc) then
  Begin
    If jFunc.TryGetValue<String>('description', aDescription) then
      Description.Text := aDescription;

    If jFunc.TryGetValue<String>('script', aScript) then
      FScript.Text := aScript;

    jFunc.TryGetValue<Boolean>('enabled', FEnabled);

    If jFunc.TryGetValue<TJSonObject>('parameters', jParameters) then
    Begin
      If jParameters.GetValue<String>('type') = 'object' then
      Begin
        If jParameters.TryGetValue<TJSonObject>('properties', jProperties) then
        Begin
          For I := 0 to jProperties.Count - 1 do
          Begin

            aParamName := jProperties.Pairs[I].JsonString.Value;
            jParam := TJSonObject(jProperties.Pairs[I].JsonValue);

            aParam := Parameters.Add;
            aParam.Name := aParamName;
            aParam.SetJSon(jParam);
          End;
        End;

        If jParameters.TryGetValue<TJSonArray>('required', jReq) then
        Begin
          For jVal in jReq do
          Begin
            If Assigned(Parameters.GetParamByName(jVal.Value)) then
              Parameters.GetParamByName(jVal.Value).Required := True;
          End;
        End;
      End;
    End;
  End;

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

function TFunctionActionItem.ToJSon(Detail: Boolean = False): TJSonObject;
Var
  Fun, Params: TJSonObject;

begin
  // Más adelante pueden crear otro tipo de tools, por ahora solo hay funciones
  Result := Nil;

  If (Self.Enabled) and (Self.ToolType = tt_function) then
  Begin
    Result := TJSonObject.Create;
    Fun := TJSonObject.Create;
    Fun.AddPair('name', FunctionName);
    Fun.AddPair('description', FDescription.Text.Trim);

    If Detail then
    Begin
      Fun.AddPair('script', FScript.Text.Trim);
      Fun.AddPair('enabled', FEnabled);
      Fun.AddPair('default', FDefault);
    End;

    Params := Parameters.ToJSon(Detail);

    If Assigned(Params) then
      Fun.AddPair('parameters', Params);

    Result.AddPair('type', 'function');
    Result.AddPair('function', Fun);
  End;

end;


{ TFunctionActionItems }

function TFunctionActionItems.Add: TFunctionActionItem;
Var
  Item: TCollectionItem;
begin
  Item := Inherited Add;
  Result := TFunctionActionItem(Item);
end;

function TFunctionActionItems.AddFunction(Nombre: String; Enabled: Boolean; Action: TFunctionEvent): TFunctionActionItem;
Var
  Item: TFunctionActionItem;
Begin
  Item := TFunctionActionItem(Add);
  Item.FunctionName := Nombre;
  Item.Enabled := Enabled;
  Item.OnAction := Action;
  Result := Item;
End;

constructor TFunctionActionItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(Self, ItemClass);
end;

destructor TFunctionActionItems.Destroy;
begin

  inherited;
end;

function TFunctionActionItems.GetActionItem(Index: Integer): TFunctionActionItem;
begin
  Result := TFunctionActionItem(inherited Items[Index]);
end;

function TFunctionActionItems.GetFunction(Nombre: String): TFunctionActionItem;
Var
  I: Integer;
  Item: TFunctionActionItem;
Begin
  Result := Nil;
  I := IndexOf(Nombre);
  If I >= 0 then
  Begin
    Item := TFunctionActionItem(Items[I]);
    Result := Item;
  End;
end;

function TFunctionActionItems.GetItemByName(aTagName: String): TFunctionActionItem;
var
  I: Integer;
  CurItem, DefItem: TFunctionActionItem;

begin
  I := 0;
  Result := Nil;
  DefItem := Nil;
  while I < Count do
  begin
    CurItem := Items[I] as TFunctionActionItem;

    If (CurItem.Default and CurItem.Enabled) then // Si hay algún item por defecto lo encuentra aquí
      DefItem := CurItem;

    if (CompareText(CurItem.FunctionName, aTagName) = 0) and CurItem.Enabled then
    begin
      Result := CurItem;
      Break;
    end;
    Inc(I);
  end;

  If Result = Nil then // Si no se encuentra una coincidencia se envía al evento por defecto
    Result := DefItem;

end;

function TFunctionActionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TFunctionActionItems.IndexOf(Nombre: String): Integer;
Var
  I: Integer;
  Item: TFunctionActionItem;
Begin
  Result := -1;
  For I := 0 to Self.Count - 1 do
  Begin
    Item := TFunctionActionItem(Self.Items[I]);
    If (Item <> Nil) and (AnsiUpperCase(Nombre) = AnsiUpperCase(Item.FunctionName)) then
    Begin
      Result := I;
      Break;
    End;
  End;
end;

procedure TFunctionActionItems.LoadFromFile(FileName: String);
Var
  Funcs: TJSonArray;
  jFunc: TJSonObject;
  jVal: TJSonValue;
  Lista: TStringStream;
  FuncName: String;
  Item: TFunctionActionItem;
begin
  If Not TFile.Exists(FileName) then
    Raise Exception.Create('No se encuentra el archivo "' + FileName + '" en el sistema');

  Lista := TStringStream.Create('', TEncoding.UTF8);
  try
    Lista.LoadFromFile(FileName);
    Funcs := TJSonObject.ParseJSONValue(Lista.DataString) as TJSonArray;
    try
      Self.Clear;

      For jVal in Funcs do
      Begin
        jFunc := TJSonObject(jVal);
        If jFunc.GetValue<String>('type') = 'function' then
        Begin
          FuncName := jFunc.GetValue<TJSonObject>('function').GetValue<String>('name');
          Item := Self.Add;
          Item.FunctionName := FuncName;
          Item.SetJSon(jFunc);
        End;
      End;
    Finally
      Funcs.Free;
    End;
  Finally
    Lista.Free;
  End;
end;

procedure TFunctionActionItems.SaveToFile(FileName: String);
Var
  Funcs: TJSonArray;
  Lista: TStringStream;
begin
  Funcs := ToJSon(True);
  Lista := TStringStream.Create(Funcs.Format, TEncoding.UTF8);
  Try
    Lista.SaveToFile(FileName);
  Finally
    Funcs.Free;
    Lista.Free;
  End;
end;

procedure TFunctionActionItems.SetActionItem(Index: Integer; Value: TFunctionActionItem);
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
    ItemName := Format('FunctionItem%d', [J]); { do not localize }
    I := 0;
    while I < Count do
    begin
      CurItem := Items[I] as TFunctionActionItem;
      if (CurItem <> Item) and (CompareText(CurItem.FunctionName, ItemName) = 0) then
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

function TFunctionActionItems.ToJSon(aDetail: Boolean = False): TJSonArray;
Var
  I: Integer;
  Item: TFunctionActionItem;
  JObj: TJSonObject;
begin
  Result := TJSonArray.Create;

  // Result.AddPair()

  For I := 0 to Count - 1 do
  Begin
    Item := TFunctionActionItem(GetItem(I));
    JObj := Item.ToJSon(aDetail);
    If Assigned(JObj) then
      Result.Add(JObj);
  End;
end;

procedure TFunctionActionItems.Update(Item: TCollectionItem);
begin
  inherited;
  { + !!  if (FWebDispatcher <> nil) and
    not (csLoading in FWebDispatcher.ComponentState) then }
end;

{ TFunctionParamsItem }

constructor TFunctionParamsItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCollection := Collection;
  FDescription := TStringList.Create;
end;

destructor TFunctionParamsItem.Destroy;
begin
  Description.Free;
  inherited;
end;

function TFunctionParamsItem.GetDisplayName: string;
begin
  Inherited GetDisplayName;
  Result := FName;
end;

function TFunctionParamsItem.GetNamePath: string;
begin
  Result := inherited GetNamePath + Format('Param%d', [Index]);
  // Result := inherited GetNamePath + Self.name; // Format('MyField%d',[Index]);
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
        if (Param <> Self) and (Param is TFunctionParamsItem) and (CompareText(Value, Param.FName) = 0) then
          raise Exception.Create('El nombre del parámetro está duplicado');
      end;
    FName := Value;
    Changed(False);
  end;

  If FName = '' then
  Begin
    FName := Value;
    Changed(False);
  End;
end;

procedure TFunctionParamsItem.SetEnum(const Value: String);
begin
  FEnum := Value;
end;

procedure TFunctionParamsItem.SetJSon(Value: TJSonObject);
Var
  aType, aDescription: String;
  jEnum: TJSonArray;
  jVal: TJSonValue;
  Lista: TStringList;
begin
  aType := Value.GetValue<String>('type');
  If aType = 'string' then
    ParamType := TToolsParamType.ptString
  Else If aType = 'integer' then
    ParamType := TToolsParamType.ptInteger
  Else If aType = 'boolean' then
    ParamType := TToolsParamType.ptBoolean
  Else If aType = 'float' then
    ParamType := TToolsParamType.ptFloat
  Else If aType = 'date' then
    ParamType := TToolsParamType.ptDate
  Else If aType = 'time' then
    ParamType := TToolsParamType.ptTime
  Else If aType = 'datetime' then
    ParamType := TToolsParamType.ptDateTime
  Else
    ParamType := TToolsParamType.ptString;

  If Value.TryGetValue<String>('description', aDescription) then
    FDescription.Text := aDescription;

  If Value.TryGetValue<TJSonArray>('enum', jEnum) then
  Begin
    Lista := TStringList.Create;
    Try
      For jVal in jEnum do
      Begin
        If ParamType in [ptInteger, ptBoolean, ptFloat] then
          Lista.Add(Value.ToString)
        Else
          Lista.Add('"' + Value.ToString + '"');
      End;
    Finally
      Lista.Free;
    End;
  End;
end;

procedure TFunctionParamsItem.SetName(const Value: String);
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

function TFunctionParamsItem.ToJSon(Detail: Boolean = False): TJSonObject;
Var
  Tipo: String;
  jEnum: TJSonArray;
  I: Integer;
  Lista: TStringList;
begin
  Result := TJSonObject.Create;
  Case FParamType of
    ptString, ptBase64:
      Tipo := 'string';
    ptInteger:
      Tipo := 'integer';
    ptBoolean:
      Tipo := 'boolean';
    ptFloat:
      Tipo := 'float';
    ptDate:
      Tipo := 'date';
    ptTime:
      Tipo := 'time';
    ptDateTime:
      Tipo := 'datetime';
  End;

  Result.AddPair('type', Tipo);
  If Self.Description.Text.Trim <> '' then
    Result.AddPair('description', Description.Text.Trim);
  If Enum.Trim <> '' then
  Begin
    Lista := TStringList.Create;
    Try
      Lista.CommaText := Enum.Trim;
      I := Lista.Count;
      If I > 0 then
      Begin
        jEnum := TJSonArray.Create;
        Result.AddPair('enum', jEnum);
        For I := 0 to Lista.Count - 1 do
        Begin
          If FParamType in [ptString, ptDate, ptTime, ptDateTime, ptBase64] then
            jEnum.Add('"' + Lista[I].Trim + '"')
          Else If FParamType in [ptInteger, ptBoolean, ptFloat] then
            jEnum.Add(Lista[I].Trim)
        End;
      End;
    Finally
      Lista.Free;
    End;
  End;
end;

{ TFunctionParamsItems }

function TFunctionParamsItems.Add: TFunctionParamsItem;
Var
  Item: TCollectionItem;
begin
  Item := Inherited Add;
  Result := TFunctionParamsItem(Item);
end;

constructor TFunctionParamsItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(Self, ItemClass);
end;

destructor TFunctionParamsItems.Destroy;
begin

  inherited;
end;

function TFunctionParamsItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TFunctionParamsItems.GetParamByName(aParamName: String): TFunctionParamsItem;
var
  I: Integer;
  CurItem: TFunctionParamsItem;
begin
  I := 0;
  Result := Nil;
  while I < Count do
  begin
    CurItem := Items[I] as TFunctionParamsItem;

    if (CompareText(CurItem.Name, aParamName) = 0) then
    begin
      Result := CurItem;
      Break;
    end;
    Inc(I);
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
    ItemName := Format('Param%d', [J]); { do not localize }
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

procedure TFunctionParamsItems.SetParams(Index: Integer; const Value: TFunctionParamsItem);
begin
  Items[Index].Assign(Value);
end;

function TFunctionParamsItems.ToJSon(Detail: Boolean = False): TJSonObject;
Var
  CurItem: TFunctionParamsItem;
  jProperties: TJSonObject;
  jRequired: TJSonArray;
  I: Integer;
begin
  Result := Nil;

  If Count > 0 then
  Begin
    Result := TJSonObject.Create;

    Result.AddPair('type', 'object');

    jProperties := TJSonObject.Create;
    jRequired := TJSonArray.Create;
    Result.AddPair('properties', jProperties);

    For I := 0 to Count - 1 do
    Begin
      CurItem := TFunctionParamsItem(Items[I]);
      jProperties.AddPair(CurItem.Name, CurItem.ToJSon(Detail));

      If CurItem.Required then
        jRequired.Add(CurItem.Name);
    End;
    If jRequired.Count > 0 then
      Result.AddPair('required', jRequired)
    else
      jRequired.Free;
  End;
end;

procedure TFunctionParamsItems.Update(Item: TCollectionItem);
begin
  inherited;
  { + !!  if (FWebDispatcher <> nil) and
    not (csLoading in FWebDispatcher.ComponentState) then }

end;

{ TAiFunctions }

constructor TAiFunctions.Create(AOwner: TComponent);
begin
  inherited;
  FFunctions := TFunctionActionItems.Create(Self, TFunctionActionItem);
end;

destructor TAiFunctions.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

function TAiFunctions.DoCallFunction(ToolCall: TAiToolsFunction): Boolean;
Var
  Funcion: TFunctionActionItem;
begin
  Result := False;
  Funcion := FFunctions.GetFunction(ToolCall.Name);

  If Assigned(Funcion) then
    Funcion.OnAction(Self, Funcion, ToolCall.Name, ToolCall, Result);
end;

function TAiFunctions.GetTools: String;
Var
  Funcs: TJSonArray;
begin
  Funcs := Functions.ToJSon;
  Try
    If Assigned(Funcs) then
    Begin
      Result := Funcs.Format;
    End;
  Finally
    Funcs.Free;
  End;
end;

function TAiFunctions.SetFunctionEnable(FunctionName: String; Enabled: Boolean): Boolean;
Var
  Item : TFunctionActionItem;
begin
  Result := False;
  Item := Functions.GetFunction(FunctionName);
  If Assigned(Item) then
  Begin
    Item.Enabled := Enabled;
    Result := True;
  End;
end;

end.

