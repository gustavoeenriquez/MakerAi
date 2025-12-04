// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Tools.Functions;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, Rest.JSON, System.IOUtils,
  Data.Db,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Core, uMakerAi.MCPClient.Core;

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

  TMCPClientItem = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FMCPClient: TMCPClientCustom;
    FConnected: Boolean;
    FParams: TStrings;
    FEnvVars: TStrings;
    // Propiedades "proxy" para facilitar la configuración en el Inspector de Objetos
    function GetName: string;
    function GetTransportType: TToolTransportType;
    procedure SetName(const Value: string);
    procedure SetTransportType(const Value: TToolTransportType);
    procedure SetEnabled(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    // function GetDisabledFunctions: TStrings;
    function GetParams: TStrings;
    // procedure SetDisabledFunctions(const Value: TStrings);
    procedure SetParams(const Value: TStrings);
    function GetConfiguration: string;
    procedure SetConfiguration(const Value: string);
    function GetEnvVars: TStrings;
    procedure SetEnvVars(const Value: TStrings);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure UpdateClientProperties;
    // Propiedad para acceder al objeto cliente real
    property MCPClient: TMCPClientCustom read FMCPClient;
  published
    Property Connected: Boolean read FConnected write SetConnected;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property TransportType: TToolTransportType read GetTransportType write SetTransportType;
    property Params: TStrings read GetParams write SetParams;
    Property EnvVars: TStrings read GetEnvVars write SetEnvVars;
    // property DisabledFunctions: TStrings read GetDisabledFunctions write SetDisabledFunctions;
    property Configuration: string read GetConfiguration write SetConfiguration;

  end;

  TMCPClientItems = class(TOwnedCollection)
  private
    [weak]
    FOwner: TPersistent;

    function GetClient(Index: Integer): TMCPClientItem;
    procedure SetClient(Index: Integer; const Value: TMCPClientItem);

  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    Destructor Destroy; Override;
    function Add: TMCPClientItem;
    function GetClientByName(const AName: string): TMCPClientItem;
    Function GetClientsList: TStringList;
    Function GetFunctionList(Name: String): TStringList;
    property Items[Index: Integer]: TMCPClientItem read GetClient write SetClient; default;
  end;

  TAiFunctions = Class(TComponent)
  Private
    FFunctions: TFunctionActionItems;
    FMCPClients: TMCPClientItems;
    FOnStatusUpdate: TMCPStatusEvent;
    FOnLog: TMCPLogEvent;
    FOnMCPStreamMessage: TMCPStreamMessageEvent;
    procedure SetOnMCPStreamMessage(const Value: TMCPStreamMessageEvent);
  Protected

    procedure DoLog(const Msg: string); virtual;
    procedure DoStatusUpdate(const StatusMsg: string); virtual;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    procedure Loaded; override;
    function GetTools(aToolFormat: TToolFormat): String; Virtual;
    Function DoCallFunction(ToolCall: TAiToolsFunction): Boolean; Virtual;
    // SetFunctionEnable  Retorna True si encuentra la función y puede actualizar el estado
    Function SetFunctionEnable(FunctionName: String; Enabled: Boolean): Boolean;
    Function SetMCPClientEnable(Name: String; Enabled: Boolean): Boolean;
    function ExtractFunctionNames: TStringList;

    // IMPORTANTE: el parámetro aMCPClient debe ser creado con owner = Nil  aMCPClient:= TMCPClientCustom(NIL);
    procedure AddMCPClient(aMCPClient: TMCPClientCustom);

    // Sobrecarga 1: Recibe el objeto JSON ya parseado (ideal si el JSON viene de una API o stream)
    function ImportClaudeMCPConfiguration(AConfig: TJSonObject): Integer; overload;

    // Sobrecarga 2: Recibe la ruta del archivo (o usa la por defecto si está vacía)
    function ImportClaudeMCPConfiguration(const AJsonFilePath: string = ''): Integer; overload;
  Published
    Property Functions: TFunctionActionItems read FFunctions write FFunctions;
    Property MCPClients: TMCPClientItems read FMCPClients write FMCPClients;

    property OnLog: TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate: TMCPStatusEvent read FOnStatusUpdate write FOnStatusUpdate;
    property OnMCPStreamMessage: TMCPStreamMessageEvent read FOnMCPStreamMessage write SetOnMCPStreamMessage;

  End;


  // Es necesario normalizar los formatos de llamado a las funciones según el driver
  // ya que Antrhopic, Openai y Gemini tienen sutiles diferencias.

  // Clase interna para representar una herramienta de forma normalizada (neutral)
  TNormalizedTool = class
  private
    FName: string;
    FDescription: string;
    FInputSchema: TJSonObject; // Siempre clonado y de nuestra propiedad
  public
    constructor Create(const AName, ADescription: string; AInputSchema: TJSonObject);
    destructor Destroy; override;
    property Name: string read FName;
    property Description: string read FDescription;
    property InputSchema: TJSonObject read FInputSchema;
  end;

  TJsonToolUtils = class
  private
    // --- MÉTODOS DE DETECCIÓN, NORMALIZACIÓN Y FORMATEO ---
    class function DetectInputFormat(AJsonTool: TJSonObject): TToolFormat;

    class procedure NormalizeFromMCP(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
    class procedure NormalizeFromOpenAI(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
    class procedure NormalizeFromAnthropic(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
    class procedure NormalizeFromGemini(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);

    class function FormatAsMCP(ANormalizedTool: TNormalizedTool): TJSonObject;
    class function FormatAsOpenAI(ANormalizedTool: TNormalizedTool): TJSonObject;
    class function FormatAsOpenAIResponses(ANormalizedTool: TNormalizedTool): TJSonObject;
    class function FormatAsAnthropic(ANormalizedTool: TNormalizedTool): TJSonObject;
    // class function FormatAsGemini(ANormalizedTool: TNormalizedTool): TJSonObject;

    class function FormatAsGeminiFunctionDeclaration(ANormalizedTool: TNormalizedTool): TJSonObject;
    // class function FormatAsCohere(ANormalizedTool: TNormalizedTool): TJSonObject;

    // Versión original explícita (útil si la detección falla o para casos específicos)
    class function MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; AInputFormat: TToolFormat; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject; overload;

    class procedure CleanInputSchema(ASchema: TJSonObject);

    class procedure CleanJsonTree(AValue: TJSONValue);
    class procedure EnforceStrictSchema(ASchema: TJSONValue);
  public

    // Sobrecarga con detección automática del formato de entrada
    class function MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject; overload;

    // Normaliza las herramientas de un objeto JSON fuente y las añade a una lista.
    class procedure NormalizeToolsFromSource(const ASourceName: string; ASourceJson: TJSonObject; ANormalizedList: TList<TNormalizedTool>);

    // Formatea una lista de herramientas normalizadas al formato de salida deseado.
    class function FormatToolList(ANormalizedList: TList<TNormalizedTool>; AOutputFormat: TToolFormat): TJSonObject;

  end;

procedure Register;

implementation

uses uMakerAi.Chat;

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
  aTipo, ADescription, aParamName, aScript: String;
  jFunc, jParameters, jProperties, jParam: TJSonObject;
  jReq: TJSonArray;
  jVal: TJSONValue;
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
    If jFunc.TryGetValue<String>('description', ADescription) then
      Description.Text := ADescription;

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
  jVal: TJSONValue;
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
  aType, ADescription: String;
  jEnum: TJSonArray;
  jVal: TJSONValue;
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

  If Value.TryGetValue<String>('description', ADescription) then
    FDescription.Text := ADescription;

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

{procedure TAiFunctions.AddMCPClient(aMCPClient: TMCPClientCustom);
var
  NewItem: TMCPClientItem;
begin
  if not Assigned(aMCPClient) then
    raise Exception.Create('Se intentó añadir un objeto TMCPClient nulo.');

  if aMCPClient.Name.Trim.IsEmpty then
    raise Exception.Create('El TMCPClient debe tener una propiedad Name asignada antes de ser añadido.');

  // 1. Verificar si ya existe un cliente con el mismo nombre para evitar duplicados.
  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
    raise Exception.CreateFmt('Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  aMCPClient.OnStreamMessage := FOnMCPStreamMessage;

  // Y también nos aseguramos de conectar OnLog y OnStatusUpdate
  aMCPClient.OnLog := FOnLog;
  aMCPClient.OnStatusUpdate := FOnStatusUpdate;

  // 2. Crear un nuevo item en la colección.
  // Este Add crea un TMCPClientItem que, a su vez, crea un TMCPClientStdIo por defecto.
  NewItem := FMCPClients.Add;

  // 3. Reemplazar el cliente por defecto con el que nos ha pasado el usuario.
  // Primero, liberamos el que se creó automáticamente.
  FreeAndNil(NewItem.FMCPClient);

  // Ahora, asignamos el cliente del usuario. El NewItem se convierte en el propietario.
  NewItem.FMCPClient := aMCPClient;

  // 4. Sincronizar las propiedades del wrapper con el estado del cliente.
  // Las propiedades como Name, Params, etc., ya funcionan como proxies,
  // pero Enabled es una propiedad directa del TMCPClientItem.
  NewItem.Enabled := aMCPClient.Enabled;
  NewItem.Connected := False; // Siempre se añade como no conectado. La conexión es una acción posterior.

  // Opcional: registrar el evento
  DoLog(Format('Cliente MCP "%s" añadido programáticamente.', [aMCPClient.Name]));
end;
}

procedure TAiFunctions.AddMCPClient(aMCPClient: TMCPClientCustom);
var
  NewItem: TMCPClientItem;
begin
  if not Assigned(aMCPClient) then
    raise Exception.Create('Se intentó añadir un objeto TMCPClient nulo.');

  if aMCPClient.Name.Trim.IsEmpty then
    raise Exception.Create('El TMCPClient debe tener una propiedad Name asignada antes de ser añadido.');

  // 1. Verificar duplicados
  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
    raise Exception.CreateFmt('Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  // Conectar eventos
  aMCPClient.OnStreamMessage := FOnMCPStreamMessage;
  aMCPClient.OnLog := FOnLog;
  aMCPClient.OnStatusUpdate := FOnStatusUpdate;

  // 2. Crear nuevo item (este crea su propio FMCPClient nulo o por defecto y FParams VACÍOS)
  NewItem := FMCPClients.Add;

  // 3. Reemplazar cliente interno
  if Assigned(NewItem.FMCPClient) then FreeAndNil(NewItem.FMCPClient);
  NewItem.FMCPClient := aMCPClient;

  // 4. --- [CORRECCIÓN CRÍTICA] SINCRONIZACIÓN INVERSA ---
  // Debemos copiar la configuración del cliente real HACIA el wrapper (Item)
  // para que el wrapper tenga la "verdad" y no sobrescriba con vacíos después.

  NewItem.FParams.Assign(aMCPClient.Params);       // <--- ESTO FALTABA
  NewItem.FEnvVars.Assign(aMCPClient.EnvVars);     // <--- ESTO FALTABA
  NewItem.Name := aMCPClient.Name;                 // Sincroniza nombre
  NewItem.Enabled := aMCPClient.Enabled;           // Sincroniza enabled

  // Importante: Sincronizar el TransportType en el wrapper sin disparar la recreación del cliente
  // Accedemos a la variable privada o usamos un cast si es necesario,
  // pero al usar la propiedad TransportType del Item, este verificará que el objeto interno
  // ya tiene ese tipo y no lo destruirá.
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
  ArgsObject, ResultObject: TJSonObject;
  AExtractedMedia: TObjectList<TAiMediaFile>; // Lista temporal
  MF: TAiMediaFile;
  ResMsg: TAIChatMessage;
begin
  Result := False;

  AExtractedMedia := TObjectList<TAiMediaFile>.Create;

  try
    if SameText(Copy(ToolCall.Name, 1, 9), 'local_99_') then
      ToolCall.Name := Copy(ToolCall.Name, 10, Length(ToolCall.Name));

    PosAt := Pos('_99_', ToolCall.Name);

    if PosAt = 0 then // --- Es una función local ---
    begin
      Funcion := FFunctions.GetFunction(ToolCall.Name);
      if Assigned(Funcion) and Assigned(Funcion.OnAction) then
        Funcion.OnAction(Self, Funcion, ToolCall.Name, ToolCall, Result);
    end
    else // --- Es una llamada a un cliente MCP ---
    begin
      ServerName := Copy(ToolCall.Name, 1, PosAt - 1);
      ActualToolName := Copy(ToolCall.Name, PosAt + 4, Length(ToolCall.Name));

      ClientItem := FMCPClients.GetClientByName(ServerName);
      if Assigned(ClientItem) and ClientItem.Enabled and ClientItem.MCPClient.Available then
      begin
        ArgsObject := nil;
        ResultObject := nil;
        try
          ClientItem.MCPClient.OnLog := FOnLog;
          ClientItem.MCPClient.OnStatusUpdate := FOnStatusUpdate;

          If (ToolCall.Arguments <> '') and (ToolCall.Arguments <> '{}') then
          Begin
            ArgsObject := TJSonObject.ParseJSONValue(ToolCall.Arguments) as TJSonObject;
            // Pasamos nuestra lista ya creada
            ResultObject := ClientItem.MCPClient.CallTool(ActualToolName, ArgsObject, AExtractedMedia);
          End
          Else
          Begin
            // Pasamos nuestra lista ya creada
            ResultObject := ClientItem.MCPClient.CallTool(ActualToolName, ToolCall.Params, AExtractedMedia);
          End;

          if Assigned(ResultObject) then
          begin
            ToolCall.Response := ResultObject.ToJSon;
            ResultObject.Free;

            If AExtractedMedia.Count > 0 then
            Begin
              If Assigned(ToolCall.ResMsg) then
              Begin
                ResMsg := TAIChatMessage(ToolCall.ResMsg);

                For MF In AExtractedMedia do
                  ResMsg.MediaFiles.Add(MF);

                // IMPORTANTE: Decimos que la lista temporal ya no es dueña de los objetos,
                // porque ahora pertenecen a ResMsg.MediaFiles.
                AExtractedMedia.OwnsObjects := False;
              End;
            End;
          end
          else
          begin
            ToolCall.Response := '{}';
          end;

          Result := True;
        except
          on E: Exception do
          begin
            FreeAndNil(ArgsObject);
            FreeAndNil(ResultObject);
            Result := False;
          end;
        end;
      end
      Else
      Begin
        // Lógica si no encuentra cliente
      End;
    end;

  finally
    // Liberamos la lista temporal.
    // Si transferimos los archivos, OwnsObjects estará en False y no los borrará.
    // Si falló algo, OwnsObjects estará en True y borrará los temporales para no dejar fugas.
    if Assigned(AExtractedMedia) then
      AExtractedMedia.Free;
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
  JsonArray: TJSonArray;
  ArrayItem: TJSONValue;
  FunctionObj: TJSonObject;
  NameValue: TJSONValue;
  FunctionName, JsonString: string;
  I: Integer;
begin
  Result := TStringList.Create;

  try
    JsonString := GetTools(TToolFormat.tfOpenAI);

    // Parsear el JSON como array
    JsonArray := TJSonObject.ParseJSONValue(JsonString) as TJSonArray;
    if JsonArray = nil then
      raise Exception.Create('JSON inválido o no es un array');

    try
      // Iterar sobre cada elemento del array
      for I := 0 to JsonArray.Count - 1 do
      begin
        ArrayItem := JsonArray.Items[I];
        if ArrayItem is TJSonObject then
        begin
          // Verificar que sea de tipo "function"
          if TJSonObject(ArrayItem).GetValue('type') <> nil then
          begin
            if TJSonObject(ArrayItem).GetValue('type').Value = 'function' then
            begin
              // Obtener el objeto "function"
              FunctionObj := TJSonObject(ArrayItem).GetValue('function') as TJSonObject;
              if FunctionObj <> nil then
              begin
                // Obtener el valor del campo "name"
                NameValue := FunctionObj.GetValue('name');
                if NameValue <> nil then
                begin
                  FunctionName := NameValue.Value;
                  // Reemplazar _99_ por -->
                  FunctionName := StringReplace(FunctionName, '_99_', '-->', [rfReplaceAll]);
                  Result.Add(FunctionName);
                end;
              end;
            end;
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

{ function TAiFunctions.ExtractFunctionNames1: TStringList;
  var
  JsonObj: TJSonObject;
  ToolsArray: TJSonArray;
  ToolItem: TJSONValue;
  NameValue: TJSONValue;
  I: Integer;
  JsonString: string;
  begin
  Result := TStringList.Create;

  try
  JsonString := GetTools(TToolFormat.tfOpenAI);

  Result.Text := JSonString;
  Exit;


  // Parsear el JSON
  JsonObj := TJSonObject.ParseJSONValue(JsonString) as TJSonObject;

  if JsonObj = nil then
  raise Exception.Create('JSON inválido');

  try
  // Obtener el array "tools"
  ToolsArray := JsonObj.GetValue('tools') as TJSonArray;
  if ToolsArray <> nil then
  Begin
  // Iterar sobre cada elemento del array
  for I := 0 to ToolsArray.Count - 1 do
  begin
  ToolItem := ToolsArray.Items[I];
  if ToolItem is TJSonObject then
  begin
  // Obtener el valor del campo "name"
  NameValue := TJSonObject(ToolItem).GetValue('name');
  if NameValue <> nil then
  Result.Add(NameValue.Value);
  end;
  end;
  End;

  finally
  JsonObj.Free;
  end;

  except
  on E: Exception do
  begin
  Result.Free;
  raise Exception.Create('Error al procesar JSON: ' + E.Message);
  end;
  end;
  end;
}

{ function TAiFunctions.GetTools(aToolFormat: TToolFormat): String;
  var
  MergedToolsObj: TJSonObject;
  LocalToolsArray, jResToolArray: TJSonArray;
  ClientItem: TMCPClientItem;
  SourceJson: TJSonObject;
  SourceJsonStr: string;
  JsonValue: TJSonValue;
  I: Integer;
  begin
  MergedToolsObj := nil;
  LocalToolsArray := nil;
  SourceJson := nil;
  try
  jResToolArray := Nil;
  // 1. Obtener las funciones locales
  LocalToolsArray := FFunctions.ToJSon;

  // 2. Crear el objeto JSON final que contendrá todas las herramientas
  MergedToolsObj := TJSonObject.Create;
  // Agregamos las herramientas locales al nuevo array de herramientas.
  // Usamos Clone para que MergedToolsObj sea el dueño de los datos.
  MergedToolsObj.AddPair('tools', TJSonObject(LocalToolsArray.Clone));

  // 3. Iterar sobre los clientes MCP y fusionar sus herramientas

  if not(csDesigning in ComponentState) then
  Begin
  For I := 0 to FMCPClients.Count - 1 do
  Begin
  ClientItem := TMCPClientItem(FMCPClients.Items[I]);
  // ClientItem.Enabled := True;

  if ClientItem.Enabled then
  begin
  // Inicializar el cliente si no lo ha sido ya. Esto obtiene la lista de herramientas.
  if not ClientItem.MCPClient.Initialized then
  ClientItem.MCPClient.Initialize;

  // Si el cliente está disponible (la inicialización fue exitosa)...
  if ClientItem.MCPClient.Available then
  begin
  SourceJsonStr := ClientItem.MCPClient.Tools.Text;
  if not SourceJsonStr.IsEmpty then
  begin
  JsonValue := TJSonObject.ParseJSONValue(SourceJsonStr);
  if JsonValue is TJSonObject then
  begin
  SourceJson := TJSonObject(JsonValue);
  try
  // Usar la función de ayuda para fusionar las herramientas
  // Asumimos el formato OpenAI como un estándar común para la salida

  // TJsonToolUtils.MergeToolLists(ClientItem.Name, SourceJson, MergedToolsObj, TToolFormat.tfOpenAI);
  TJsonToolUtils.MergeToolLists(ClientItem.Name, SourceJson, MergedToolsObj, aToolFormat);

  finally
  SourceJson.Free;
  SourceJson := nil;
  end;
  end
  else
  JsonValue.Free;
  end;
  end;
  end;
  End;
  End;

  // 4. Formatear el JSON final a string

  Var
  JArr: TJSonArray;

  If MergedToolsObj.TryGetValue<TJSonArray>('tools', JArr) then
  jResToolArray := TJSonArray(JArr.Clone)
  Else
  jResToolArray := TJSonArray.Create;

  Try
  Result := jResToolArray.ToJSon;
  Finally
  jResToolArray.Free;
  End;

  finally
  FreeAndNil(MergedToolsObj);
  FreeAndNil(LocalToolsArray);
  // SourceJson ya se libera dentro del bucle
  end;
  end;
}

function TAiFunctions.GetTools(aToolFormat: TToolFormat): String;
var
  LAllNormalizedTools: TList<TNormalizedTool>;
  FinalToolsObj: TJSonObject;
  LocalToolsObj: TJSonObject;
  ClientItem: TMCPClientItem;
  ClientToolsJson: TJSonObject;
  JsonValue: TJSONValue;
  I: Integer;
begin

  LAllNormalizedTools := TList<TNormalizedTool>.Create;
  // LAllNormalizedTools.OwnsObjects := True; // ¡Muy importante!
  FinalToolsObj := nil;
  LocalToolsObj := nil;

  try
    // 1. NORMALIZAR FUNCIONES LOCALES
    // Convertimos el TJSonArray de funciones locales a un TJSonObject con la clave "tools"
    LocalToolsObj := TJSonObject.Create;
    LocalToolsObj.AddPair('tools', FFunctions.ToJSon);
    TJsonToolUtils.NormalizeToolsFromSource('local', LocalToolsObj, LAllNormalizedTools); // Usamos 'local' o un nombre vacío

    // 2. NORMALIZAR FUNCIONES DE CLIENTES MCP
    if not(csDesigning in ComponentState) then
    begin
      for I := 0 to FMCPClients.Count - 1 do
      begin
        ClientItem := TMCPClientItem(FMCPClients.Items[I]);
        if not ClientItem.Enabled then
          Continue;

        if not ClientItem.MCPClient.Initialized then
          ClientItem.MCPClient.Initialize;

        if ClientItem.MCPClient.Available then
        begin
          var
          SourceJsonStr := ClientItem.MCPClient.Tools.Text;
          if not SourceJsonStr.IsEmpty then
          begin
            JsonValue := TJSonObject.ParseJSONValue(SourceJsonStr);
            try
              if JsonValue is TJSonObject then
              begin
                ClientToolsJson := TJSonObject(JsonValue);
                TJsonToolUtils.NormalizeToolsFromSource(ClientItem.Name, ClientToolsJson, LAllNormalizedTools);
              end;
            finally
              JsonValue.Free;
            end;
          end;
        end;
      end;
    end;

    // 3. FORMATEAR LA LISTA MAESTRA COMPLETA AL FINAL
    FinalToolsObj := TJsonToolUtils.FormatToolList(LAllNormalizedTools, aToolFormat);

    // Extraer el array 'tools' para el resultado final
    Var
      LResultArray: TJSonArray;
    if FinalToolsObj.TryGetValue<TJSonArray>('tools', LResultArray) then
      Result := LResultArray.ToJSon
    else
      Result := '[]';

  finally
    LAllNormalizedTools.Free; // Libera la lista y todos los TNormalizedTool dentro
    FinalToolsObj.Free;
    LocalToolsObj.Free;
  end;
end;

// =============================================================================
// SOBRECARGA 2: Wrapper de Archivo (Recibe String)
// =============================================================================

function TAiFunctions.ImportClaudeMCPConfiguration(const AJsonFilePath: string): Integer;
var
  FinalPath: string;
  JsonContent: string;
  RootObj: TJSonObject;
begin
  Result := 0;
  FinalPath := AJsonFilePath;

  // 1. Detección automática de ruta si viene vacía
  if FinalPath.IsEmpty then
  begin
{$IFDEF MSWINDOWS}
    FinalPath := TPath.Combine(GetEnvironmentVariable('APPDATA'), 'Claude\claude_desktop_config.json');
{$ENDIF}
{$IFDEF POSIX}
    FinalPath := TPath.Combine(TPath.GetHomePath, 'Library/Application Support/Claude/claude_desktop_config.json');
{$ENDIF}
  end;

  if not TFile.Exists(FinalPath) then
  begin
    DoLog('ImportClaude: File not found at ' + FinalPath);
    Exit;
  end;

  DoLog('Importing MCP servers from file: ' + FinalPath);

  try
    // 2. Leer archivo y convertir a JSON
    JsonContent := TFile.ReadAllText(FinalPath, TEncoding.UTF8);
    RootObj := TJSonObject.ParseJSONValue(JsonContent) as TJSonObject;

    if Assigned(RootObj) then
      try
        // 3. LLAMAR A LA OTRA SOBRECARGA
        Result := ImportClaudeMCPConfiguration(RootObj);
      finally
        RootObj.Free; // Importante: Liberamos el JSON aquí porque este método lo creó
      end
    else
      DoLog('ImportClaude: File content is not a valid JSON object.');

  except
    on E: Exception do
      DoLog('ImportClaude: Fatal error reading file: ' + E.Message);
  end;
end;

procedure TAiFunctions.Loaded;
var
  i: Integer;
begin
  inherited;
  // Forzamos una actualización final de propiedades una vez cargado todo el FMX.
  // Esto corrige cualquier desajuste por el orden de carga.
  if Assigned(FMCPClients) then
  begin
    for i := 0 to FMCPClients.Count - 1 do
    begin
      // Aseguramos que el cliente interno exista y tenga los datos correctos
      if Assigned(FMCPClients[i]) then
        FMCPClients[i].UpdateClientProperties;
    end;
  end;
end;

// =============================================================================
// SOBRECARGA 1: Lógica Núcleo (Recibe TJSONObject)
// =============================================================================
function TAiFunctions.ImportClaudeMCPConfiguration(AConfig: TJSonObject): Integer;
var
  McpServers, ServerObj, EnvObj: TJSonObject;
  ArgsArray: TJSonArray;
  ServerPair, EnvPair: TJSONPair;
  NewClient: TMCPClientStdIo;
  ArgsString, ArgValStr, ServerName: string;
  I: Integer;
  Val: TJSONValue;
begin
  Result := 0;

  if not Assigned(AConfig) then
  begin
    DoLog('ImportClaude: JSON Configuration object is nil.');
    Exit;
  end;

  try
    // Buscar la clave raíz "mcpServers"
    if AConfig.TryGetValue<TJSonObject>('mcpServers', McpServers) then
    begin
      for ServerPair in McpServers do
      begin
        ServerName := ServerPair.JsonString.Value;

        // 1. Evitar duplicados
        if Assigned(FMCPClients.GetClientByName(ServerName)) then
        begin
          DoLog(Format('ImportClaude: Client "%s" already exists. Skipping.', [ServerName]));
          Continue;
        end;

        ServerObj := ServerPair.JsonValue as TJSonObject;
        if not Assigned(ServerObj) then
          Continue;

        // 2. Crear el cliente StdIo (Owner = nil)
        NewClient := TMCPClientStdIo.Create(nil);
        try
          NewClient.Name := ServerName;

          // --- COMMAND ---
          if ServerObj.TryGetValue('command', Val) then
            NewClient.Params.Values['Command'] := Val.Value;

          // --- ARGS ---
          ArgsString := '';
          if ServerObj.TryGetValue<TJSonArray>('args', ArgsArray) then
          begin
            for I := 0 to ArgsArray.Count - 1 do
            begin
              ArgValStr := ArgsArray.Items[I].Value;
              // Manejo de espacios en argumentos: Envolver en comillas si es necesario
              if (Pos(' ', ArgValStr) > 0) and (not ArgValStr.StartsWith('"')) then
                ArgValStr := '"' + ArgValStr + '"';

              if ArgsString.IsEmpty then
                ArgsString := ArgValStr
              else
                ArgsString := ArgsString + ' ' + ArgValStr;
            end;
          end;
          NewClient.Params.Values['Arguments'] := ArgsString;

          // --- ENV ---
          if ServerObj.TryGetValue<TJSonObject>('env', EnvObj) then
          begin
            for EnvPair in EnvObj do
            begin
              NewClient.EnvVars.Values[EnvPair.JsonString.Value] := EnvPair.JsonValue.Value;
            end;
          end;

          // Directorio raíz por defecto (opcional)
          NewClient.Params.Values['RootDir'] := TPath.GetHomePath;

          // 3. Agregar a la colección central
          try
            AddMCPClient(NewClient);
            Inc(Result);
            DoLog(Format('ImportClaude: Imported server "%s".', [ServerName]));
          except
            on E: Exception do
            begin
              DoLog(Format('ImportClaude: Error adding "%s": %s', [ServerName, E.Message]));
              if NewClient.Owner = nil then
                NewClient.Free;
            end;
          end;

        except
          on E: Exception do
          begin
            DoLog('ImportClaude: Unexpected error processing server entry: ' + E.Message);
            if NewClient.Owner = nil then
              NewClient.Free;
          end;
        end;
      end;
    end
    else
    begin
      DoLog('ImportClaude: "mcpServers" key not found in JSON.');
    end;
  except
    on E: Exception do
      DoLog('ImportClaude: Error parsing configuration object: ' + E.Message);
  end;
end;

function TAiFunctions.SetFunctionEnable(FunctionName: String; Enabled: Boolean): Boolean;
Var
  Item: TFunctionActionItem;
begin
  Result := False;
  Item := Functions.GetFunction(FunctionName);
  If Assigned(Item) then
  Begin
    Item.Enabled := Enabled;
    Result := True;
  End;
end;

function TAiFunctions.SetMCPClientEnable(Name: String; Enabled: Boolean): Boolean;
Var
  Item: TMCPClientItem;
begin
  Result := False;
  Item := Self.FMCPClients.GetClientByName(Name);
  If Assigned(Item) then
  Begin
    Item.Enabled := Enabled;
    Result := True;
  End;
end;

procedure TAiFunctions.SetOnMCPStreamMessage(const Value: TMCPStreamMessageEvent);
var
  I: Integer;
begin
  FOnMCPStreamMessage := Value;
  // Propagar a todos los clientes actuales
  for I := 0 to FMCPClients.Count - 1 do
    if Assigned(FMCPClients[I].MCPClient) then
      FMCPClients[I].MCPClient.OnStreamMessage := Value;
end;

{ MCPClient }

{ TMCPClientItem }

constructor TMCPClientItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FConnected := False;
  FParams := TStringList.Create;
  FEnvVars := TStringList.Create;

  // Por defecto, creamos un cliente StdIo
  FMCPClient := Nil; // TMCPClientStdIo.Create(nil); // Sin Owner para controlarlo nosotros
  // FMCPClient.Name := 'NewMCPClient';



  // CORRECCIÓN: Crear siempre el cliente por defecto (StdIo).
  // Esto asegura que si SetParams se llama antes que SetTransportType,
  // haya un objeto donde guardar los datos.
  FMCPClient := TMCPClientStdIo.Create(nil);
//  FMCPClient.Name := 'MCPClient' + IntToStr(ID); // Nombre temporal

  // Sincronizar params iniciales (defaults del StdIo hacia el Wrapper)
  FParams.Assign(FMCPClient.Params);

end;

destructor TMCPClientItem.Destroy;
begin
  FParams.Free;;
  FEnvVars.Free;

  If Assigned(FMCPClient) then
    FreeAndNil(FMCPClient);
  inherited;
end;

function TMCPClientItem.GetConfiguration: string;
begin
  Result := Format('(%s, Click [...] to edit)', ['Properties']);
end;

{
  function TMCPClientItem.GetDisabledFunctions: TStrings;
  begin
  if Assigned(FMCPClient) then
  Result := FMCPClient.DisabledFunctions
  else
  Result := nil;
  end;
}

function TMCPClientItem.GetDisplayName: string;
begin
  Result := GetName;
  if Result.IsEmpty then
    Result := inherited GetDisplayName;
end;

function TMCPClientItem.GetEnvVars: TStrings;
begin
  Result := FEnvVars;
  {
    if Assigned(FMCPClient) then
    Begin
    Result := FMCPClient.EnvVars;
    End
    else
    Result := nil;
  }
end;

function TMCPClientItem.GetName: string;
begin
  Result := '';
  if Assigned(FMCPClient) then
    Result := FMCPClient.Name
  else
    Result := inherited GetDisplayName;
end;

function TMCPClientItem.GetParams: TStrings;
begin
  Result := FParams;
  {
    if Assigned(FMCPClient) then
    Begin
    Result := FMCPClient.Params;
    End
    else
    Result := nil;
  }
end;

function TMCPClientItem.GetTransportType: TToolTransportType;
begin
  Result := tpStdIo; // Valor por defecto
  if Assigned(FMCPClient) then
    Result := FMCPClient.TransportType;
end;

procedure TMCPClientItem.SetConfiguration(const Value: string);
begin
  // No se necesita hacer nada aquí. solo debe existir.
end;

procedure TMCPClientItem.SetConnected(const Value: Boolean);
var
  // OwnerComponent: TAiFunctions;
  // IsDesignTime: Boolean;
  ClientTools: TJSonObject;
  // ToolCount: Integer;
  // ToolArray: TJSonArray;
begin
  // Solo actuar si el valor realmente cambia
  if FConnected = Value then
    Exit;

  // --- LÓGICA DE VALIDACIÓN EN TIEMPO DE DISEÑO ---

  // 1. Solo validar cuando se activa (se pone en True)
  if not Value then
  Begin
    FConnected := Value;
    Changed(False);
    Exit;
  End;

  try
    try
      // Intentamos obtener la lista de herramientas. Esta es la prueba "en vivo".
      ClientTools := Self.MCPClient.ListTools;

      if Not Assigned(ClientTools) then
      begin
        // Si ListTools devuelve nil, es un error de conexión o protocolo.
        Raise Exception.Create(Format('❌ Fallo de conexión para "%s".'#13#10#13#10'Revise la configuración (Command, URL, etc.) y los logs del servidor.', [Self.Name]));
      end
      Else
      Begin
        FConnected := Value;
        Changed(False);
      End;

    except
      on E: Exception do
      begin
        // Capturamos cualquier otra excepción
        Raise Exception.Create(Format('❌ Ocurrió una excepción al validar "%s".'#13#10#13#10'%s: %s', [Self.Name, E.ClassName, E.Message]));
      end;
    end;
  finally
  end;
end;

{
  procedure TMCPClientItem.SetDisabledFunctions(const Value: TStrings);
  begin
  if Assigned(FMCPClient) and Assigned(Value) then
  begin
  FMCPClient.DisabledFunctions.Assign(Value);
  Changed(False); // Notifica al IDE que el item ha cambiado.
  end;
  end;
}

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
    Changed(False); // Muy importante: Notifica al IDE que el item ha cambiado.
  end;
end;

procedure TMCPClientItem.SetName(const Value: string);
begin
  SetDisplayName(Value);
  if Assigned(FMCPClient) then
  begin
    if FMCPClient.Name <> Value then
    begin
      FMCPClient.Name := Value;
      Changed(False);
    end;
  end;
end;

procedure TMCPClientItem.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);

  if Assigned(FMCPClient) and Assigned(Value) then
  begin
    If Assigned(FMCPClient) then
      FMCPClient.Params.Assign(Value);
    Changed(False); // Muy importante: Notifica al IDE que el item ha cambiado.
  end;
end;

procedure TMCPClientItem.SetTransportType(const Value: TToolTransportType);
begin
  // Verificamos si realmente cambió o si el objeto no existe
  if not Assigned(FMCPClient) or (FMCPClient.TransportType <> Value) then
  begin
    // Liberamos el cliente anterior
    FreeAndNil(FMCPClient);

    // Creamos el nuevo según el tipo seleccionado
    case Value of
      tpStdIo:
        FMCPClient := TMCPClientStdIo.Create(nil);
      tpHttp:
        FMCPClient := TMCPClientHttp.Create(nil);
      tpSSE:
        FMCPClient := TMCPClientSSE.Create(nil);
      tpMakerAi:
        FMCPClient := TMCPClientMakerAi.Create(nil);
    else
      raise ENotSupportedException.Create('Protocolo MCP no soportado');
    end;

    // =========================================================================
    // CORRECCIÓN: Asignar el tipo explícitamente AL NUEVO OBJETO
    // antes de sincronizar el resto de propiedades.
    // =========================================================================
    FMCPClient.TransportType := Value;

    // Ahora sí, transferimos nombre, params, envVars, etc.
    UpdateClientProperties;

    Changed(False);
  end;
end;

procedure TMCPClientItem.UpdateClientProperties;
begin
  // Asegurarnos de que tenemos un cliente al que transferirle los datos
  if not Assigned(FMCPClient) then
    Exit;

  // 1. Transferir el tipo de transporte
  FMCPClient.TransportType := Self.GetTransportType; // Obtenemos el tipo desde el FMCPClient mismo

  // 2. Transferir el nombre (usando GetDisplayName que es el setter de 'Name')
  FMCPClient.Name := Self.GetDisplayName;

  // 3. Transferir el estado de 'Enabled'
  FMCPClient.Enabled := Self.FEnabled;

  // 4. Transferir los parámetros (copiar el contenido de nuestra lista local)
  FMCPClient.Params.Assign(Self.FParams);

  // 5. Transferir las variables de entorno
  FMCPClient.EnvVars.Assign(Self.FEnvVars);
end;

{ TMCPClientItems }

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

function TMCPClientItems.GetFunctionList(Name: String): TStringList;
{ Var
  Item: TMCPClientItem;
  jTools: TJSonObject;
}
begin
  Result := Nil;
  { Item := GetClientByName(Name);

    If Assigned(Item) and Assigned(Item.MCPClient) then
    jTools := Item.MCPClient.ListTools;
  }
end;

function TMCPClientItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMCPClientItems.SetClient(Index: Integer; const Value: TMCPClientItem);
begin
  Items[Index].Assign(Value);
end;

procedure TMCPClientItems.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TJsonToolUtils }

constructor TNormalizedTool.Create(const AName, ADescription: string; AInputSchema: TJSonObject);
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

class procedure TJsonToolUtils.CleanInputSchema(ASchema: TJSonObject);
begin
  // Llama a la función de trabajo recursiva para limpiar el árbol completo.
  CleanJsonTree(ASchema);
end;

// El verdadero motor: una función recursiva que recorre CUALQUIER árbol JSON.
class procedure TJsonToolUtils.CleanJsonTree(AValue: TJSONValue);
var
  LObject: TJSonObject;
  LArray: TJSonArray;
  LPair: TJSONPair;
  LItem: TJSONValue;
begin
  if not Assigned(AValue) then
    Exit;

  // Caso 1: El valor es un Objeto JSON
  if AValue is TJSonObject then
  begin
    LObject := AValue as TJSonObject;
    // 1. Acción: Limpiar las claves no deseadas en el nivel actual.
    LObject.RemovePair('additionalProperties');
    LObject.RemovePair('$schema');

    // 2. Travesía: Llamarse a sí misma para cada valor hijo del objeto.
    // Es importante iterar sobre una copia de los pares si se va a modificar,
    // pero como RemovePair maneja esto internamente, un bucle for-in es seguro.
    for LPair in LObject do
    begin
      CleanJsonTree(LPair.JsonValue);
    end;
  end
  // Caso 2: El valor es un Array JSON
  else if AValue is TJSonArray then
  begin
    LArray := AValue as TJSonArray;
    // Travesía: Llamarse a sí misma para cada elemento del array.
    for LItem in LArray do
    begin
      CleanJsonTree(LItem);
    end;
  end;
  // Caso 3: Es un valor simple (string, número, etc.). No se hace nada.
end;

{ TJsonToolUtils }

// ==============================================================================
// NUEVA FUNCIÓN DE DETECCIÓN
// ==============================================================================
class function TJsonToolUtils.DetectInputFormat(AJsonTool: TJSonObject): TToolFormat;
var
  LTypeValue: TJSONValue;
begin
  Result := tfUnknown;
  if not Assigned(AJsonTool) then
    Exit;

  // Gemini
  if AJsonTool.FindValue('functionDeclarations') <> nil then
    Exit(tfGemini);

  // OpenAI Family Detection
  if AJsonTool.TryGetValue('type', LTypeValue) and (LTypeValue is TJSONString) and (LTypeValue.Value = 'function') then
  begin
    // Diferenciación clave:
    // tfOpenAI (Legacy/Chat): Tiene una clave "function" que contiene los detalles.
    // tfOpenAIResponses (New): Tiene "name" directamente en la raíz y NO tiene clave "function".

    if AJsonTool.FindValue('function') <> nil then
      Exit(tfOpenAI)
    else if AJsonTool.FindValue('name') <> nil then
      Exit(tfOpenAIResponses);

    // Por defecto si es ambiguo, asumimos el nuevo estándar si tiene nombre
    Exit(tfOpenAIResponses);
  end;

  // Anthropic
  if AJsonTool.FindValue('input_schema') <> nil then
    Exit(tfClaude);

  // MCP
  if AJsonTool.FindValue('inputSchema') <> nil then
    Exit(tfMCP);
end;

class procedure TJsonToolUtils.EnforceStrictSchema(ASchema: TJSONValue);
var
  JObj, JProps: TJSonObject;
  jReq: TJSonArray;
  PropName: string;
  I: Integer;
  ExistsInReq: Boolean;
begin
  if not(ASchema is TJSonObject) then
    Exit;

  JObj := TJSonObject(ASchema);

  // Verificamos si es un objeto (tiene propiedades o es type object explícito)
  if (JObj.TryGetValue<TJSonObject>('properties', JProps)) or (JObj.GetValue<string>('type') = 'object') then
  begin
    // REGLA 1: additionalProperties: false es OBLIGATORIO
    if JObj.GetValue('additionalProperties') <> nil then
      JObj.RemovePair('additionalProperties');
    JObj.AddPair('additionalProperties', False);

    // REGLA 2: Todos los campos definidos en properties deben estar en required
    if Assigned(JProps) then
    begin
      // Obtener o crear el array required
      if not JObj.TryGetValue<TJSonArray>('required', jReq) then
      begin
        jReq := TJSonArray.Create;
        JObj.AddPair('required', jReq);
      end;

      for I := 0 to JProps.Count - 1 do
      begin
        PropName := JProps.Pairs[I].JsonString.Value;

        // Verificar si ya existe en required para no duplicar
        ExistsInReq := False;
        for var J := 0 to jReq.Count - 1 do
        begin
          if jReq.Items[J].Value = PropName then
          begin
            ExistsInReq := True;
            Break;
          end;
        end;

        if not ExistsInReq then
          jReq.Add(PropName);

        // Recursividad: Aplicar strict rules a los hijos (objetos anidados)
        EnforceStrictSchema(JProps.Pairs[I].JsonValue);
      end;
    end;
  end;
end;

// ==============================================================================
// MÉTODOS DE NORMALIZACIÓN (sin cambios)
// ==============================================================================
class procedure TJsonToolUtils.NormalizeFromMCP(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
var
  LName, LDescription: string;
  LInputSchema: TJSonObject;
  LSchemaValue: TJSONValue;
begin
  if not AJsonTool.TryGetValue<string>('name', LName) then
    Exit;
  AJsonTool.TryGetValue<string>('description', LDescription);

  if AJsonTool.TryGetValue('inputSchema', LSchemaValue) and (LSchemaValue is TJSonObject) then
  Begin
    LInputSchema := TJSonObject(LSchemaValue.Clone);
    CleanInputSchema(LInputSchema);
  End
  else
    LInputSchema := TJSonObject.Create; // Crear schema vacío si no existe

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

class procedure TJsonToolUtils.NormalizeFromAnthropic(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
var
  LName, LDescription: string;
  LInputSchema: TJSonObject;
  LSchemaValue: TJSONValue;
begin
  if not AJsonTool.TryGetValue<string>('name', LName) then
    Exit;
  AJsonTool.TryGetValue<string>('description', LDescription);

  if AJsonTool.TryGetValue('input_schema', LSchemaValue) and (LSchemaValue is TJSonObject) then
  Begin
    LInputSchema := TJSonObject(LSchemaValue.Clone);
    CleanInputSchema(LInputSchema);
  End
  else
    LInputSchema := TJSonObject.Create;

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

{ class procedure TJsonToolUtils.NormalizeFromOpenAI(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
  var
  LName, LDescription: string;
  LInputSchema: TJSonObject;
  LSchemaValue: TJSonValue;
  begin
  if not AJsonTool.TryGetValue<string>('name', LName) then
  Exit;
  AJsonTool.TryGetValue<string>('description', LDescription);

  if AJsonTool.TryGetValue('parameters', LSchemaValue) and (LSchemaValue is TJSonObject) then
  LInputSchema := TJSonObject(LSchemaValue.Clone)
  else
  LInputSchema := TJSonObject.Create;

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
  end;
}

// ==============================================================================
// FUNCIÓN DE NORMALIZACIÓN DE OPENAI CORREGIDA
// ==============================================================================
class procedure TJsonToolUtils.NormalizeFromOpenAI(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
var
  LName, LDescription: string;
  LInputSchema: TJSonObject;
  LSchemaValue: TJSONValue;
  LFunctionObject, LDataSource: TJSonObject; // LDataSource apuntará al objeto correcto
begin
  // Primero, determinamos de dónde leer los datos.
  // Intentamos encontrar el objeto anidado 'function'.
  if AJsonTool.TryGetValue<TJSonObject>('function', LFunctionObject) then
  begin
    // Formato anidado: {"type": "function", "function": {"name": ...}}
    LDataSource := LFunctionObject;
  end
  else
  begin
    // Formato plano: {"type": "function", "name": ...}
    LDataSource := AJsonTool;
  end;

  // Ahora extraemos los datos usando LDataSource, que apunta al lugar correcto.
  if not LDataSource.TryGetValue<string>('name', LName) then
    Exit; // Si no hay nombre, no es una herramienta válida.

  LDataSource.TryGetValue<string>('description', LDescription);

  if LDataSource.TryGetValue('parameters', LSchemaValue) and (LSchemaValue is TJSonObject) then
  Begin
    LInputSchema := TJSonObject(LSchemaValue.Clone);
    CleanInputSchema(LInputSchema);
  End
  else
    LInputSchema := TJSonObject.Create; // Crear schema vacío si no hay parámetros

  AToolList.Add(TNormalizedTool.Create(LName, LDescription, LInputSchema));
end;

class procedure TJsonToolUtils.NormalizeToolsFromSource(const ASourceName: string; ASourceJson: TJSonObject; ANormalizedList: TList<TNormalizedTool>);
var
  LSourceToolsArray: TJSonArray;
  I: Integer;
  LSourceTool: TJSonObject;
  LDetectedFormat: TToolFormat;
begin
  if not Assigned(ASourceJson) or not ASourceJson.TryGetValue<TJSonArray>('tools', LSourceToolsArray) or (LSourceToolsArray.Count = 0) then
    Exit; // No hay herramientas que procesar

  // Detectar formato a partir de la primera herramienta
  if not(LSourceToolsArray.Items[0] is TJSonObject) then
    Exit;
  LDetectedFormat := DetectInputFormat(LSourceToolsArray.Items[0] as TJSonObject);
  if LDetectedFormat = tfUnknown then
    Exit; // Formato no reconocido

  // Normalizar cada herramienta
  for I := 0 to LSourceToolsArray.Count - 1 do
  begin
    if not(LSourceToolsArray.Items[I] is TJSonObject) then
      Continue;
    LSourceTool := LSourceToolsArray.Items[I] as TJSonObject;

    // Guardar el recuento actual para saber qué herramientas se añadieron
    var
    LInitialCount := ANormalizedList.Count;

    case LDetectedFormat of
      tfMCP:
        NormalizeFromMCP(LSourceTool, ANormalizedList);
      tfClaude:
        NormalizeFromAnthropic(LSourceTool, ANormalizedList);

      tfOpenAI, tfOpenAIResponses: // Ambos usan el mismo normalizador de lectura
        NormalizeFromOpenAI(LSourceTool, ANormalizedList);

      tfGemini:
        NormalizeFromGemini(LSourceTool, ANormalizedList);
    end;

    // Aplicar el prefijo de fuente a las herramientas recién añadidas
    if not ASourceName.IsEmpty then
    begin
      for var J := LInitialCount to ANormalizedList.Count - 1 do
        ANormalizedList[J].FName := Format('%s_99_%s', [ASourceName, ANormalizedList[J].Name]);
    end;
  end;
end;

class procedure TJsonToolUtils.NormalizeFromGemini(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
var
  LFuncDeclarations: TJSonArray;
  I: Integer;
  LFuncDecl, LSchema: TJSonObject;
  LSchemaValue: TJSONValue;
  LName, LDescription: string;
begin
  // El formato Gemini tiene un array 'functionDeclarations' dentro de cada 'tool'
  if not AJsonTool.TryGetValue<TJSonArray>('functionDeclarations', LFuncDeclarations) then
    Exit;

  for I := 0 to LFuncDeclarations.Count - 1 do
  begin
    if not(LFuncDeclarations.Items[I] is TJSonObject) then
      Continue;
    LFuncDecl := LFuncDeclarations.Items[I] as TJSonObject;

    if not LFuncDecl.TryGetValue<string>('name', LName) then
      Continue;
    LFuncDecl.TryGetValue<string>('description', LDescription);

    if LFuncDecl.TryGetValue('parameters', LSchemaValue) and (LSchemaValue is TJSonObject) then
    Begin
      LSchema := TJSonObject(LSchemaValue.Clone);
      CleanInputSchema(LSchema);
    End
    else
      LSchema := TJSonObject.Create;

    AToolList.Add(TNormalizedTool.Create(LName, LDescription, LSchema));
  end;
end;

// ==============================================================================
// MÉTODOS DE FORMATEO (sin cambios)
// ==============================================================================
class function TJsonToolUtils.FormatAsMCP(ANormalizedTool: TNormalizedTool): TJSonObject;
begin
  Result := TJSonObject.Create;
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);
  Result.AddPair('inputSchema', TJSonObject(ANormalizedTool.InputSchema.Clone));
end;

class function TJsonToolUtils.FormatAsAnthropic(ANormalizedTool: TNormalizedTool): TJSonObject;
begin
  Result := TJSonObject.Create;
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);
  Result.AddPair('input_schema', TJSonObject(ANormalizedTool.InputSchema.Clone));
end;

{ class function TJsonToolUtils.FormatAsCohere(ANormalizedTool: TNormalizedTool): TJSonObject;
  var
  LParamDefs, LParam, LInputSchema, LProperties: TJSonObject;
  LRequiredArray: TJSonArray;
  LPair: TJSONPair;
  LReqValue, LDesc, LType: TJSONValue; // Variables declaradas al principio
  IsRequired: Boolean;
  begin
  Result := TJSonObject.Create;
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);

  LInputSchema := ANormalizedTool.InputSchema;
  if not Assigned(LInputSchema) or not LInputSchema.TryGetValue<TJSonObject>('properties', LProperties) then
  begin
  Exit;
  end;

  LParamDefs := TJSonObject.Create;
  Result.AddPair('parameter_definitions', LParamDefs);

  LRequiredArray := nil;
  LInputSchema.TryGetValue<TJSonArray>('required', LRequiredArray);

  for LPair in LProperties do
  begin
  if not (LPair.JsonValue is TJSonObject) then Continue;

  LParam := TJSonObject.Create;
  var LPropObj := LPair.JsonValue as TJSonObject;

  if LPropObj.TryGetValue<TJSONValue>('description', LDesc) then
  LParam.AddPair('description', LDesc.Clone as TJSONString);
  if LPropObj.TryGetValue<TJSONValue>('type', LType) then
  LParam.AddPair('type', LType.Clone as TJSONString);

  IsRequired := False;
  if Assigned(LRequiredArray) then
  begin
  for LReqValue in LRequiredArray do
  begin
  if SameText(LReqValue.Value, LPair.JsonString.Value) then
  begin
  IsRequired := True;
  Break;
  end;
  end;
  end;
  LParam.AddPair('required', TJSONBool.Create(IsRequired));

  LParamDefs.AddPair(LPair.JsonString.Value, LParam);
  end;
  end;
}

{
  class function TJsonToolUtils.FormatAsOpenAI(ANormalizedTool: TNormalizedTool): TJSonObject;
  begin
  Result := TJSonObject.Create;
  Result.AddPair('type', 'function');
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);
  Result.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));
  end;
}

// ==============================================================================
// FUNCIÓN DE FORMATEO PARA OPENAI (VERSIÓN CORREGIDA Y DEFINITIVA)
// ==============================================================================
class function TJsonToolUtils.FormatAsOpenAI(ANormalizedTool: TNormalizedTool): TJSonObject;
var
  LFunctionObject: TJSonObject;
begin
  // 1. Crear el objeto interno "function" que contendrá los detalles.
  LFunctionObject := TJSonObject.Create;
  LFunctionObject.AddPair('name', ANormalizedTool.Name);
  LFunctionObject.AddPair('description', ANormalizedTool.Description);

  // 2. Solo añadir 'parameters' si el schema tiene contenido.
  if Assigned(ANormalizedTool.InputSchema) and (ANormalizedTool.InputSchema.Count > 0) then
    LFunctionObject.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));

  // 3. Crear el objeto externo principal.
  Result := TJSonObject.Create;
  Result.AddPair('type', 'function');
  Result.AddPair('function', LFunctionObject); // <-- Añadir el objeto interno
end;

class function TJsonToolUtils.FormatAsOpenAIResponses(ANormalizedTool: TNormalizedTool): TJSonObject;
var
  LParams: TJSonObject;
begin
  // Formato Responses API (Plano + Strict)
  // {
  // "type": "function",
  // "name": "tool_name",
  // "description": "...",
  // "parameters": { ... },
  // "strict": true
  // }

  Result := TJSonObject.Create;
  Result.AddPair('type', 'function');
  Result.AddPair('name', ANormalizedTool.Name);

  if not ANormalizedTool.Description.IsEmpty then
    Result.AddPair('description', ANormalizedTool.Description);

  // Solo añadir parámetros si existen
  if Assigned(ANormalizedTool.InputSchema) and (ANormalizedTool.InputSchema.Count > 0) then
  begin
    // Clonamos el esquema original para no modificar la referencia base
    LParams := TJSonObject(ANormalizedTool.InputSchema.Clone);

    // ---> AQUI LA MAGIA: Forzamos el esquema a cumplir Strict Mode <---
    EnforceStrictSchema(LParams);

    Result.AddPair('parameters', LParams);
  end
  else
  begin
    // OpenAI Strict requiere un esquema de parámetros incluso si es vacío
    // Debe ser: "parameters": {"type": "object", "properties": {}, "additionalProperties": false, "required": []}
    LParams := TJSonObject.Create;
    LParams.AddPair('type', 'object');
    LParams.AddPair('properties', TJSonObject.Create);
    LParams.AddPair('additionalProperties', False);
    LParams.AddPair('required', TJSonArray.Create);
    Result.AddPair('parameters', LParams);
  end;

  // La API Responses recomienda fuertemente strict: true
  Result.AddPair('strict', True);
end;

class function TJsonToolUtils.FormatToolList(ANormalizedList: TList<TNormalizedTool>; AOutputFormat: TToolFormat): TJSonObject;
var
  LFinalToolsArray: TJSonArray;
  LNormTool: TNormalizedTool;
  LFormattedTool: TJSonObject;
begin
  Result := TJSonObject.Create;
  LFinalToolsArray := TJSonArray.Create;
  Result.AddPair('tools', LFinalToolsArray);

  if ANormalizedList.Count = 0 then
    Exit;

  if AOutputFormat = tfGemini then
  begin
    // --- LÓGICA ESPECIAL PARA GEMINI: Agrupar todo en un solo bloque ---
    var
    LDeclarationsArray := TJSonArray.Create;
    for LNormTool in ANormalizedList do
    begin
      LDeclarationsArray.Add(FormatAsGeminiFunctionDeclaration(LNormTool));
    end;

    var
    LGeminiToolWrapper := TJSonObject.Create;
    LGeminiToolWrapper.AddPair('functionDeclarations', LDeclarationsArray);
    LFinalToolsArray.Add(LGeminiToolWrapper);
  end
  else
  begin
    // --- LÓGICA ESTÁNDAR PARA OTROS FORMATOS: Una herramienta por objeto ---
    for LNormTool in ANormalizedList do
    begin
      LFormattedTool := nil;
      case AOutputFormat of
        tfMCP:
          LFormattedTool := FormatAsMCP(LNormTool);
        tfClaude:
          LFormattedTool := FormatAsAnthropic(LNormTool);
        tfOpenAI:
          LFormattedTool := FormatAsOpenAI(LNormTool);
        tfOpenAIResponses:
          LFormattedTool := FormatAsOpenAIResponses(LNormTool);
        // tfCohere:
        // LFormattedTool := FormatAsCohere(LNormTool);
      end;

      if Assigned(LFormattedTool) then
        LFinalToolsArray.Add(LFormattedTool);
    end;
  end;
end;

{ class function TJsonToolUtils.FormatAsGemini(ANormalizedTool: TNormalizedTool): TJSonObject;
  var
  LFuncDecl: TJSonObject;
  LFuncDeclsArray: TJSonArray;
  begin
  // Creamos la estructura anidada de Gemini
  LFuncDecl := TJSonObject.Create;
  LFuncDecl.AddPair('name', ANormalizedTool.Name);
  LFuncDecl.AddPair('description', ANormalizedTool.Description);
  LFuncDecl.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));

  LFuncDeclsArray := TJSonArray.Create;
  LFuncDeclsArray.Add(LFuncDecl);

  Result := TJSonObject.Create;
  Result.AddPair('functionDeclarations', LFuncDeclsArray);
  end;
}

class function TJsonToolUtils.FormatAsGeminiFunctionDeclaration(ANormalizedTool: TNormalizedTool): TJSonObject;
begin
  // Crea solo el objeto de la declaración de la función, no la envoltura.
  Result := TJSonObject.Create;
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);

  // Solo añadir 'parameters' si el schema tiene contenido.
  if Assigned(ANormalizedTool.InputSchema) and (ANormalizedTool.InputSchema.Count > 0) then
    Result.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));
end;

// ==============================================================================
// FUNCIONES PÚBLICAS
// ==============================================================================

// SOBRECARGA con detección automática
class function TJsonToolUtils.MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject;
var
  LSourceToolsArray: TJSonArray;
  LFirstTool: TJSonObject;
  LDetectedFormat: TToolFormat;
begin
  // 1. Intentar detectar el formato a partir de la primera herramienta en el array
  LDetectedFormat := tfUnknown;
  if Assigned(ASourceJson) and ASourceJson.TryGetValue<TJSonArray>('tools', LSourceToolsArray) and (LSourceToolsArray.Count > 0) and (LSourceToolsArray.Items[0] is TJSonObject) then
  begin
    LFirstTool := LSourceToolsArray.Items[0] as TJSonObject;
    LDetectedFormat := DetectInputFormat(LFirstTool);
  end;

  // 2. Si la detección falla, no podemos continuar.
  if LDetectedFormat = tfUnknown then
  begin
    // Podríamos lanzar una excepción o simplemente devolver el target sin cambios.
    // Devolver el target es más seguro.
    // raise EJSON.CreateFmt('Could not detect tool format for source "%s".', [ASourceName]);
    Result := ATargetJson;
    Exit;
  end;

  // 3. Llamar a la función principal con el formato detectado.
  Result := MergeToolLists(ASourceName, ASourceJson, LDetectedFormat, ATargetJson, AOutputFormat);
end;


// VERSIÓN EXPLÍCITA (lógica principal corregida y final)

class function TJsonToolUtils.MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; AInputFormat: TToolFormat; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject;
var
  LSourceToolsArray, LFinalToolsArray: TJSonArray;
  LNormalizedTools: TList<TNormalizedTool>;
  I: Integer;
  LSourceTool, LFormattedTool: TJSonObject;
  LNormTool: TNormalizedTool;
begin
  // 1. Validar y preparar JSON de origen y destino
  if not Assigned(ASourceJson) or not ASourceJson.TryGetValue<TJSonArray>('tools', LSourceToolsArray) then
  begin
    Result := ATargetJson; // No hay nada que procesar
    Exit;
  end;

  if not Assigned(ATargetJson) then
  begin
    ATargetJson := TJSonObject.Create;
    ATargetJson.AddPair('tools', TJSonArray.Create);
  end;

  if not ATargetJson.TryGetValue<TJSonArray>('tools', LFinalToolsArray) then
    raise Exception.Create('Target JSON object does not contain a "tools" array.');

  // 2. Normalizar las herramientas de la fuente a una lista interna
  LNormalizedTools := TList<TNormalizedTool>.Create;
  try
    for I := 0 to LSourceToolsArray.Count - 1 do
    begin
      if not(LSourceToolsArray.Items[I] is TJSonObject) then
        Continue;
      LSourceTool := LSourceToolsArray.Items[I] as TJSonObject;

      case AInputFormat of
        tfMCP:
          NormalizeFromMCP(LSourceTool, LNormalizedTools);
        tfClaude:
          NormalizeFromAnthropic(LSourceTool, LNormalizedTools);

        // Soportar ambos formatos de OpenAI en la entrada (la función NormalizeFromOpenAI detecta la estructura interna)
        tfOpenAI, tfOpenAIResponses:
          NormalizeFromOpenAI(LSourceTool, LNormalizedTools);

        tfGemini:
          NormalizeFromGemini(LSourceTool, LNormalizedTools);
      else
        Continue; // Ignorar formatos no soportados
      end;
    end;

    // Si no se normalizó ninguna herramienta, no hay nada más que hacer.
    if LNormalizedTools.Count = 0 then
    begin
      Result := ATargetJson;
      Exit;
    end;

    // 3. Pre-procesar los nombres de todas las herramientas normalizadas
    for LNormTool in LNormalizedTools do
    begin
      if not ASourceName.IsEmpty then
        LNormTool.FName := Format('%s_99_%s', [ASourceName, LNormTool.Name]);
    end;

    // 4. Formatear y añadir al destino
    if AOutputFormat = tfGemini then
    begin
      // --- LÓGICA ESPECIAL PARA GEMINI: Agrupar todo en un solo bloque ---
      var
      LDeclarationsArray := TJSonArray.Create;
      for LNormTool in LNormalizedTools do
      begin
        // Usar la función de ayuda que formatea una declaración individual
        LDeclarationsArray.Add(FormatAsGeminiFunctionDeclaration(LNormTool));
      end;

      // Crear el único objeto contenedor 'tool'
      var
      LGeminiToolWrapper := TJSonObject.Create;
      LGeminiToolWrapper.AddPair('functionDeclarations', LDeclarationsArray);

      // Añadir este único objeto al array final de herramientas
      LFinalToolsArray.Add(LGeminiToolWrapper);
    end
    else
    begin
      // --- LÓGICA ESTÁNDAR PARA OTROS FORMATOS: Una herramienta por objeto ---
      for LNormTool in LNormalizedTools do
      begin
        LFormattedTool := nil;
        case AOutputFormat of
          tfMCP:
            LFormattedTool := FormatAsMCP(LNormTool);
          tfClaude:
            LFormattedTool := FormatAsAnthropic(LNormTool);
          tfOpenAI:
            LFormattedTool := FormatAsOpenAI(LNormTool); // Legacy (Wrapper function)
          tfOpenAIResponses:
            LFormattedTool := FormatAsOpenAIResponses(LNormTool); // Nuevo (Plano)
        end;

        if Assigned(LFormattedTool) then
          LFinalToolsArray.Add(LFormattedTool);
      end;
    end;

  finally
    LNormalizedTools.Free; // Libera la lista y todos los objetos TNormalizedTool dentro de ella.
  end;

  Result := ATargetJson;
end;

end.
