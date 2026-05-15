// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr�quez
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
  System.SysUtils, System.StrUtils, System.Classes, System.Generics.Collections,
  System.JSON, Rest.JSON, System.IOUtils,
  System.Net.HttpClient, System.NetEncoding,
  System.SyncObjs,
  Data.Db,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Core, uMakerAi.MCPClient.Core, uMakerAi.Chat.Messages;

const
  // Separador interno entre nombre de servidor MCP y nombre de herramienta.
  // NO puede aparecer en el nombre de un TMCPClientItem (validado en SetName).
  MCP_TOOL_SEP = '_99_';

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
    // Schema completo en JSON (no descompuesto). Cuando está asignado,
    // ToJSon lo usa directamente en lugar de reconstruir desde TFunctionParamsItems.
    // Útil para herramientas con schemas complejos (anyOf, nested objects, etc.)
    FRawSchemaJson: String;
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
    // Schema JSON completo (alternativa a TFunctionParamsItems para schemas complejos)
    property RawSchemaJson: String read FRawSchemaJson write FRawSchemaJson;
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
    FOwned: Boolean;   // False = client is externally managed; destructor will not free it
    FConnected: Boolean;
    FParams: TStrings;
    FEnvVars: TStrings;
    FName: string;
    // Propiedades "proxy" para facilitar la configuraci�n en el Inspector de Objetos
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
    // Propagación automática al cliente real cuando se editan in-place
    procedure OnFParamsChanged(Sender: TObject);
    procedure OnFEnvVarsChanged(Sender: TObject);
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

  // TAutoMCPConfig: configuración agrupada del subsistema AutoMCP / PPM.
  // Aparece en el Object Inspector como propiedad expandible (+).
  // Nota: OnAutoMCPRequest se mantiene en TAiFunctions (pestaña Events del OI).
  TAutoMCPConfig = class(TPersistent)
  private
    FActive: Boolean;
    FRegistryUrl: String;
    FWorkingDir: String;
    FAllowed: TStringList;
    FBlocked: TStringList;
    FConfigFile: String;
    FAutoLoad: Boolean;
    FOnActiveChanged: TNotifyEvent; // notifica a TAiFunctions cuando Active cambia
    function GetAllowed: TStrings;
    function GetBlocked: TStrings;
    procedure SetAllowed(const Value: TStrings);
    procedure SetBlocked(const Value: TStrings);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // Active: cuando True, crea las 3 funciones PPM internas y las expone al LLM.
    // Cuando False, las oculta (sin destruirlas). Reactivas = vuelven disponibles.
    property Active: Boolean read FActive write SetActive default False;
    // RegistryUrl: URL base del registry PPM.
    property RegistryUrl: String read FRegistryUrl write FRegistryUrl;
    // Allowed: whitelist de paquetes permitidos. Si tiene entradas, solo esos pueden instalarse.
    property Allowed: TStrings read GetAllowed write SetAllowed;
    // Blocked: blacklist de paquetes bloqueados. Si Allowed está vacío, se permite todo excepto estos.
    property Blocked: TStrings read GetBlocked write SetBlocked;
    // WorkingDir: directorio base donde se instalan los paquetes PPM.
    // Vacío = ~/.ppm/mcp/<nombre_paquete>/ (valor por defecto del sistema).
    property WorkingDir: String read FWorkingDir write FWorkingDir;
    // ConfigFile: ruta al archivo JSON con servidores MCP (formato Claude Desktop).
    // Vacío = detecta automáticamente la ruta por defecto de Claude Desktop según el SO.
    property ConfigFile: String read FConfigFile write FConfigFile;
    // AutoLoad: si True, importa ConfigFile automáticamente al cargar el componente (Loaded).
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default False;
  end;

  TAiFunctions = Class(TComponent)
  Private
    FFunctions: TFunctionActionItems;
    FMCPClients: TMCPClientItems;
    FOnStatusUpdate: TMCPStatusEvent;
    FOnLog: TMCPLogEvent;
    FOnMCPStreamMessage: TMCPStreamMessageEvent;
    FAutoMCPConfig: TAutoMCPConfig;
    FOnAutoMCPRequest: TAutoMCPRequestEvent;
    FInstalledPackages: TStringList; // paquetes ya instalados en esta sesión
    // AutoMCP: funciones internas (invisibles al developer, no en FFunctions)
    FAutoMCPFunctions: TFunctionActionItems;
    FAutoMCPLock: TCriticalSection; // serializa llamadas a call_mcp_tool
    procedure SetOnMCPStreamMessage(const Value: TMCPStreamMessageEvent);
    procedure SetAutoMCPConfig(const Value: TAutoMCPConfig);
    function IsAutoMCPAllowed(const APkgName: string): Boolean;
    // Retorna la ruta efectiva del archivo de configuración MCP:
    // ConfigFile si está asignado, sino <exedir>\mcp_servers.json
    function GetEffectiveMCPConfigPath: String;
    // AutoMCP: crea ppm_search, ppm_install, call_mcp_tool como funciones internas
    procedure CreateInternalPPMFunctions;
    // Callback para TAutoMCPConfig.SetActive — sincroniza estado al cambiar Active
    procedure OnAutoMCPActiveChanged(Sender: TObject);
    // AutoMCP: handlers internos de las 3 funciones PPM
    procedure InternalAutoMCP_PpmSearch(ToolCall: TAiToolsFunction);
    procedure InternalAutoMCP_PpmInstall(ToolCall: TAiToolsFunction);
    procedure InternalAutoMCP_CallMcpTool(ToolCall: TAiToolsFunction);
  Protected

    procedure DoLog(const Msg: string); virtual;
    procedure DoStatusUpdate(const StatusMsg: string); virtual;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    procedure Loaded; override;
    function GetTools(aToolFormat: TToolFormat): String; Virtual;
    Function DoCallFunction(ToolCall: TAiToolsFunction): Boolean; Virtual;

    // InitAutoMCP: instala mcp-ppm como bootstrap del sistema AutoMCP.
    // Se llama automáticamente desde Loaded si AutoMCP=True.
    // Puede llamarse manualmente si AutoMCP se activa en runtime.
    procedure InitAutoMCP;
    // SetFunctionEnable  Retorna True si encuentra la funci�n y puede actualizar el estado
    Function SetFunctionEnable(FunctionName: String; Enabled: Boolean): Boolean;
    Function SetMCPClientEnable(Name: String; Enabled: Boolean): Boolean;
    function ExtractFunctionNames: TStringList;

    // IMPORTANTE: el par�metro aMCPClient debe ser creado con owner = Nil  aMCPClient:= TMCPClientCustom(NIL);
    procedure AddMCPClient(aMCPClient: TMCPClientCustom);
    // Like AddMCPClient but does NOT take ownership — client is managed by an external pool.
    procedure AddBorrowedMCPClient(aMCPClient: TMCPClientCustom);

    // Sobrecarga 1: Recibe el objeto JSON ya parseado (ideal si el JSON viene de una API o stream)
    function ImportClaudeMCPConfiguration(AConfig: TJSonObject): Integer; overload;

    // Sobrecarga 2: Recibe la ruta del archivo (o usa GetEffectiveMCPConfigPath si está vacía)
    function ImportClaudeMCPConfiguration(const AJsonFilePath: string = ''): Integer; overload;

    // SaveMCPConfiguration: serializa los MCPClients actuales al archivo JSON.
    // AFilePath vacío = usa GetEffectiveMCPConfigPath.
    // Retorna True si guardó correctamente.
    function SaveMCPConfiguration(const AFilePath: string = ''): Boolean;

    // Integración con PPM (registry público de herramientas MCP)
    // SearchPPMMCP: busca herramientas MCP en el registry. El llamador libera el TJSONObject.
    function SearchPPMMCP(const AQuery: String; APage: Integer = 1; APerPage: Integer = 20;
      const ARegistryUrl: String = 'https://registry.pascalai.org'): TJSONObject;

    // ImportMCPFromPPM: registra una herramienta MCP desde PPM como stub StdIo sin descargar.
    // Útil cuando el binario ya está instalado manualmente; el llamador debe asignar
    // Params['Command'] con la ruta al ejecutable antes de habilitar el item.
    // AVersion vacío = resuelve la última versión disponible.
    function ImportMCPFromPPM(const AName: String; const AVersion: String = '';
      const ARegistryUrl: String = 'https://registry.pascalai.org'): TMCPClientItem;

    // InstallMCPFromPPM: descarga el .paipkg desde el registry, extrae el binario y
    // registra el cliente StdIo listo para usar.
    // AVersion vacío    = resuelve la última versión disponible.
    // AInstallDir vacío = ~/.ppm/mcp/<AName>/
    // Retorna el TMCPClientItem configurado, o nil si falla.
    function InstallMCPFromPPM(const AName: String; const AVersion: String = '';
      const AInstallDir: String = '';
      const ARegistryUrl: String = 'https://registry.pascalai.org'): TMCPClientItem;

    // GetMCPSchema: retorna el JSON Schema de una herramienta MCP del registry.
    // El llamador es responsable de liberar el TJSONObject devuelto.
    // AVersion vacío = resuelve la última versión disponible.
    function GetMCPSchema(const AName: String; const AVersion: String = '';
      const ARegistryUrl: String = 'https://registry.pascalai.org'): TJSONObject;

    // GetAutoMCPSystemPrompt: retorna un system prompt listo para usar que instruye
    // al LLM a utilizar las herramientas PPM (ppm_search, ppm_install, call_mcp_tool).
    // El desarrollador debe asignarlo manualmente al SystemPrompt del componente de chat.
    // Incluye la lista de herramientas ya instaladas si las hay.
    function GetAutoMCPSystemPrompt: String;
  Published
    Property Functions: TFunctionActionItems read FFunctions write FFunctions;
    Property MCPClients: TMCPClientItems read FMCPClients write FMCPClients;

    property OnLog: TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate: TMCPStatusEvent read FOnStatusUpdate write FOnStatusUpdate;
    property OnMCPStreamMessage: TMCPStreamMessageEvent read FOnMCPStreamMessage write SetOnMCPStreamMessage;

    // AutoMCPConfig: configuración agrupada del subsistema AutoMCP / PPM.
    // Expandible en el Object Inspector con (+).
    property AutoMCPConfig: TAutoMCPConfig read FAutoMCPConfig write SetAutoMCPConfig;
    // OnAutoMCPRequest: callback dinámico para aprobar/denegar instalaciones en runtime.
    // Tiene prioridad sobre Allowed/Blocked. AAllow=True por defecto.
    property OnAutoMCPRequest: TAutoMCPRequestEvent read FOnAutoMCPRequest write FOnAutoMCPRequest;

  End;


  // Es necesario normalizar los formatos de llamado a las funciones seg�n el driver
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
    // --- M�TODOS DE DETECCI�N, NORMALIZACI�N Y FORMATEO ---
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

    // Versi�n original expl�cita (�til si la detecci�n falla o para casos espec�ficos)
    class function MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; AInputFormat: TToolFormat; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject; overload;

    class procedure CleanInputSchema(ASchema: TJSonObject);

    class procedure CleanJsonTree(AValue: TJSONValue);
    class procedure EnforceStrictSchema(ASchema: TJSONValue);
  public

    // Sobrecarga con detecci�n autom�tica del formato de entrada
    class function MergeToolLists(const ASourceName: string; ASourceJson: TJSonObject; ATargetJson: TJSonObject; AOutputFormat: TToolFormat): TJSonObject; overload;

    // Normaliza las herramientas de un objeto JSON fuente y las a�ade a una lista.
    class procedure NormalizeToolsFromSource(const ASourceName: string; ASourceJson: TJSonObject; ANormalizedList: TList<TNormalizedTool>);

    // Formatea una lista de herramientas normalizadas al formato de salida deseado.
    class function FormatToolList(ANormalizedList: TList<TNormalizedTool>; AOutputFormat: TToolFormat): TJSonObject;

  end;

procedure Register;

implementation

uses uMakerAi.Chat, System.Zip, System.IniFiles;

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
          raise Exception.Create('nombre de la acci�n duplicado');
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
    Raise Exception.Create('Supera el l�mite m�ximo de la descripci�n de 1024 caracteres');

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
  // M�s adelante pueden crear otro tipo de tools, por ahora solo hay funciones
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

    // Preferir RawSchemaJson (schema completo) sobre la reconstrucción desde TFunctionParamsItems
    if FRawSchemaJson <> '' then
      Params := TJSonObject(TJSonObject.ParseJSONValue(FRawSchemaJson))
    else
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

    If (CurItem.Default and CurItem.Enabled) then // Si hay alg�n item por defecto lo encuentra aqu�
      DefItem := CurItem;

    if (CompareText(CurItem.FunctionName, aTagName) = 0) and CurItem.Enabled then
    begin
      Result := CurItem;
      Break;
    end;
    Inc(I);
  end;

  If Result = Nil then // Si no se encuentra una coincidencia se env�a al evento por defecto
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
    var LParsed := TJSonObject.ParseJSONValue(Lista.DataString);
    if not (LParsed is TJSonArray) then
    begin
      LParsed.Free;
      Raise Exception.Create('El archivo no contiene un array JSON v�lido');
    end;
    Funcs := TJSonArray(LParsed);
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
          raise Exception.Create('El nombre del par�metro est� duplicado');
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
          Lista.Add(jVal.Value)
        Else
          Lista.Add(jVal.Value);
      End;
      FEnum := Lista.CommaText;
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

{ procedure TAiFunctions.AddMCPClient(aMCPClient: TMCPClientCustom);
  var
  NewItem: TMCPClientItem;
  begin
  if not Assigned(aMCPClient) then
  raise Exception.Create('Se intent� a�adir un objeto TMCPClient nulo.');

  if aMCPClient.Name.Trim.IsEmpty then
  raise Exception.Create('El TMCPClient debe tener una propiedad Name asignada antes de ser a�adido.');

  // 1. Verificar si ya existe un cliente con el mismo nombre para evitar duplicados.
  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
  raise Exception.CreateFmt('Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  aMCPClient.OnStreamMessage := FOnMCPStreamMessage;

  // Y tambi�n nos aseguramos de conectar OnLog y OnStatusUpdate
  aMCPClient.OnLog := FOnLog;
  aMCPClient.OnStatusUpdate := FOnStatusUpdate;

  // 2. Crear un nuevo item en la colecci�n.
  // Este Add crea un TMCPClientItem que, a su vez, crea un TMCPClientStdIo por defecto.
  NewItem := FMCPClients.Add;

  // 3. Reemplazar el cliente por defecto con el que nos ha pasado el usuario.
  // Primero, liberamos el que se cre� autom�ticamente.
  FreeAndNil(NewItem.FMCPClient);

  // Ahora, asignamos el cliente del usuario. El NewItem se convierte en el propietario.
  NewItem.FMCPClient := aMCPClient;

  // 4. Sincronizar las propiedades del wrapper con el estado del cliente.
  // Las propiedades como Name, Params, etc., ya funcionan como proxies,
  // pero Enabled es una propiedad directa del TMCPClientItem.
  NewItem.Enabled := aMCPClient.Enabled;
  NewItem.Connected := False; // Siempre se a�ade como no conectado. La conexi�n es una acci�n posterior.

  // Opcional: registrar el evento
  DoLog(Format('Cliente MCP "%s" a�adido program�ticamente.', [aMCPClient.Name]));
  end;
}

procedure TAiFunctions.AddMCPClient(aMCPClient: TMCPClientCustom);
var
  NewItem: TMCPClientItem;
begin
  if not Assigned(aMCPClient) then
    raise Exception.Create('Se intent� a�adir un objeto TMCPClient nulo.');

  if aMCPClient.Name.Trim.IsEmpty then
    raise Exception.Create('El TMCPClient debe tener una propiedad Name asignada antes de ser a�adido.');

  // 1. Verificar duplicados
  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
    raise Exception.CreateFmt('Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  // Conectar eventos
  aMCPClient.OnStreamMessage := FOnMCPStreamMessage;
  aMCPClient.OnLog := FOnLog;
  aMCPClient.OnStatusUpdate := FOnStatusUpdate;

  // 2. Crear nuevo item (este crea su propio FMCPClient nulo o por defecto y FParams VAC�OS)
  NewItem := FMCPClients.Add;

  // 3. Reemplazar cliente interno
  if Assigned(NewItem.FMCPClient) then
    FreeAndNil(NewItem.FMCPClient);
  NewItem.FMCPClient := aMCPClient;

  // 4. --- [CORRECCI�N CR�TICA] SINCRONIZACI�N INVERSA ---
  // Debemos copiar la configuraci�n del cliente real HACIA el wrapper (Item)
  // para que el wrapper tenga la "verdad" y no sobrescriba con vac�os despu�s.

  NewItem.FParams.Assign(aMCPClient.Params); // <--- ESTO FALTABA
  NewItem.FEnvVars.Assign(aMCPClient.EnvVars); // <--- ESTO FALTABA
  NewItem.Name := aMCPClient.Name; // Sincroniza nombre
  NewItem.Enabled := aMCPClient.Enabled; // Sincroniza enabled

  // Importante: Sincronizar el TransportType en el wrapper sin disparar la recreaci�n del cliente
  // Accedemos a la variable privada o usamos un cast si es necesario,
  // pero al usar la propiedad TransportType del Item, este verificar� que el objeto interno
  // ya tiene ese tipo y no lo destruir�.
  NewItem.TransportType := aMCPClient.TransportType;

  NewItem.Connected := False;

  DoLog(Format('Cliente MCP "%s" a�adido y sincronizado.', [aMCPClient.Name]));
end;

procedure TAiFunctions.AddBorrowedMCPClient(aMCPClient: TMCPClientCustom);
var
  NewItem: TMCPClientItem;
begin
  if not Assigned(aMCPClient) then
    raise Exception.Create('Se intent� a�adir un objeto TMCPClient nulo.');
  if aMCPClient.Name.Trim.IsEmpty then
    raise Exception.Create('El TMCPClient debe tener Name asignado.');
  if Assigned(FMCPClients.GetClientByName(aMCPClient.Name)) then
    raise Exception.CreateFmt('Ya existe un cliente MCP con el nombre "%s".', [aMCPClient.Name]);

  // Do NOT overwrite the client's existing event handlers — the pooled client
  // already has its log bridge wired from initialization.

  NewItem := FMCPClients.Add;
  if Assigned(NewItem.FMCPClient) then
    FreeAndNil(NewItem.FMCPClient);
  NewItem.FMCPClient := aMCPClient;
  NewItem.FOwned     := False;   // pool owns the client; this item is just a reference

  NewItem.FParams.Assign(aMCPClient.Params);
  NewItem.FEnvVars.Assign(aMCPClient.EnvVars);
  NewItem.Name          := aMCPClient.Name;
  NewItem.Enabled       := aMCPClient.Enabled;
  NewItem.TransportType := aMCPClient.TransportType;
  NewItem.Connected     := False;

  DoLog(Format('Cliente MCP "%s" a�adido (borrowed, no ownership).', [aMCPClient.Name]));
end;

constructor TAiFunctions.Create(AOwner: TComponent);
begin
  inherited;
  FFunctions := TFunctionActionItems.Create(Self, TFunctionActionItem);
  FMCPClients := TMCPClientItems.Create(Self);
  FAutoMCPConfig := TAutoMCPConfig.Create;
  FAutoMCPConfig.FOnActiveChanged := OnAutoMCPActiveChanged;
  FInstalledPackages := TStringList.Create;
  FAutoMCPFunctions := TFunctionActionItems.Create(Self, TFunctionActionItem);
  FAutoMCPLock := TCriticalSection.Create;
end;

destructor TAiFunctions.Destroy;
begin
  FFunctions.Free;
  FMCPClients.Free;
  FAutoMCPConfig.Free;
  FInstalledPackages.Free;
  FAutoMCPFunctions.Free;
  FAutoMCPLock.Free;
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
begin
  Result := False;

  AExtractedMedia := TObjectList<TAiMediaFile>.Create;

  try
    if SameText(Copy(ToolCall.Name, 1, Length('local' + MCP_TOOL_SEP)), 'local' + MCP_TOOL_SEP) then
      ToolCall.Name := Copy(ToolCall.Name, Length('local' + MCP_TOOL_SEP) + 1, Length(ToolCall.Name));

    PosAt := Pos(MCP_TOOL_SEP, ToolCall.Name);

    // AutoMCP: despachar a handlers internos antes de buscar en FFunctions.
    // Lazy init: crea las funciones si Active=True pero aún no existen.
    if FAutoMCPConfig.Active and (PosAt = 0) then
    begin
      if FAutoMCPFunctions.Count = 0 then
        CreateInternalPPMFunctions;
      if SameText(ToolCall.Name, 'ppm_search') then
      begin
        InternalAutoMCP_PpmSearch(ToolCall);
        Result := True;
        Exit;
      end
      else if SameText(ToolCall.Name, 'ppm_install') then
      begin
        InternalAutoMCP_PpmInstall(ToolCall);
        Result := True;
        Exit;
      end
      else if SameText(ToolCall.Name, 'call_mcp_tool') then
      begin
        InternalAutoMCP_CallMcpTool(ToolCall);
        Result := True;
        Exit;
      end;
    end;

    if PosAt = 0 then // --- Es una función local ---
    begin
      Funcion := FFunctions.GetFunction(ToolCall.Name);
      if Assigned(Funcion) and Assigned(Funcion.OnAction) then
        Funcion.OnAction(Self, Funcion, ToolCall.Name, ToolCall, Result);
    end
    else // --- Es una llamada a un cliente MCP ---
    begin
      ServerName := Copy(ToolCall.Name, 1, PosAt - 1);
      ActualToolName := Copy(ToolCall.Name, PosAt + Length(MCP_TOOL_SEP), Length(ToolCall.Name));

      ClientItem := FMCPClients.GetClientByName(ServerName);
      if Assigned(ClientItem) and ClientItem.Enabled and ClientItem.MCPClient.Available then
      begin
        ArgsObject := nil;
        ResultObject := nil;
        try
          ClientItem.MCPClient.OnLog := FOnLog;
          ClientItem.MCPClient.OnStatusUpdate := FOnStatusUpdate;

          var LParsedArgs := TJSonObject.ParseJSONValue(ToolCall.Arguments);
          if LParsedArgs is TJSonObject then
            ArgsObject := TJSonObject(LParsedArgs)
          else
          begin
            LParsedArgs.Free;
            ArgsObject := TJSonObject.Create; // Fallback: objeto vac�o si Arguments es ''
          end;
          ResultObject := ClientItem.MCPClient.CallTool(ActualToolName, ArgsObject, AExtractedMedia);

          if Assigned(ResultObject) then
          begin
            ToolCall.Response := ResultObject.ToJSon;
            ResultObject.Free;

            If AExtractedMedia.Count > 0 then
            Begin
              // Archivos extraídos → ToolCall.MediaFiles (los drivers los transfieren
              // al ToolMsg para que el LLM los vea en el siguiente turno).
              // Adicionalmente, se clonan al ResMsg para que el app los reciba
              // en OnReceiveDataEnd sin conflictos de ownership.
              For MF In AExtractedMedia do
              Begin
                ToolCall.MediaFiles.Add(MF); // original → ToolMsg (vía driver)

                If Assigned(ToolCall.ResMsg) then
                Begin
                  var LClone := TAiMediaFile.Create;
                  LClone.LoadFromBase64(MF.FileName, MF.Base64); // MimeType derivado del filename
                  TAiChatMessage(ToolCall.ResMsg).AddMediaFile(LClone); // clone → app
                End;
              End;
              AExtractedMedia.OwnsObjects := False;
            End;
          end
          else
          begin
            // CallTool devolvio nil: el proceso fallo al iniciar o no respondio.
            // Available ya fue puesto a False por TMCPClientStdIo.
            ToolCall.Response := Format(
              '{"error":"MCP server ''%s'' failed to initialize (binary crashed or did not respond). ' +
              'The binary may be missing runtime dependencies or be incompatible with this system."}',
              [ServerName]);
          end;

          Result := True;

        except
          on E: Exception do
          begin
            // ArgsObject no se libera aqui�: CallTool toma ownership del objeto.
            // Si la excepci�n ocurre ANTES de CallTool, ArgsObject se pierde,
            // pero es preferible a un double-free si ocurre DESPU�S.
            FreeAndNil(ResultObject);
            ClientItem.MCPClient.Available := False;
            ToolCall.Response := Format('{"error":"%s"}',
              [StringReplace(E.Message, '"', '\"', [rfReplaceAll])]);
            Result := True; // Devolver True para que el LLM reciba el mensaje de error
          end;
        end;
      end
      else
      begin
        // Cliente no encontrado, deshabilitado o no disponible.
        // Setear Response para que el driver pueda enviar un tool result válido al LLM.
        if not Assigned(ClientItem) then
          ToolCall.Response := Format('{"error":"MCP server ''%s'' not found"}', [ServerName])
        else if not ClientItem.Enabled then
          ToolCall.Response := Format('{"error":"MCP server ''%s'' is disabled"}', [ServerName])
        else
          ToolCall.Response := Format('{"error":"MCP server ''%s'' is not available"}', [ServerName]);
        Result := True;
      end;
    end;

  finally
    // Liberamos la lista temporal.
    // Si transferimos los archivos, OwnsObjects estar� en False y no los borrar�.
    // Si fall� algo, OwnsObjects estar� en True y borrar� los temporales para no dejar fugas.
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
    var LParsedVal := TJSonObject.ParseJSONValue(JsonString);
    if not (LParsedVal is TJSonArray) then
    begin
      LParsedVal.Free;
      raise Exception.Create('JSON inv�lido o no es un array');
    end;
    JsonArray := TJSonArray(LParsedVal);

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
                  // Reemplazar separador MCP por --> para visualización
                  FunctionName := StringReplace(FunctionName, MCP_TOOL_SEP, '-->', [rfReplaceAll]);
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
  raise Exception.Create('JSON inv�lido');

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

  // 2. Crear el objeto JSON final que contendr� todas las herramientas
  MergedToolsObj := TJSonObject.Create;
  // Agregamos las herramientas locales al nuevo array de herramientas.
  // Usamos Clone para que MergedToolsObj sea el due�o de los datos.
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

  // Si el cliente est� disponible (la inicializaci�n fue exitosa)...
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
  // Usar la funci�n de ayuda para fusionar las herramientas
  // Asumimos el formato OpenAI como un est�ndar com�n para la salida

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

  LAllNormalizedTools := TObjectList<TNormalizedTool>.Create(True);
  FinalToolsObj := nil;
  LocalToolsObj := nil;

  try
    // 1. NORMALIZAR FUNCIONES LOCALES
    // Convertimos el TJSonArray de funciones locales a un TJSonObject con la clave "tools"
    LocalToolsObj := TJSonObject.Create;
    var LLocalTools := FFunctions.ToJSon;
    LocalToolsObj.AddPair('tools', LLocalTools);
    TJsonToolUtils.NormalizeToolsFromSource('local', LocalToolsObj, LAllNormalizedTools); // Usamos 'local' o un nombre vacío

    // 1b. FUNCIONES INTERNAS DE AUTOMCP (ppm_search, ppm_install, call_mcp_tool)
    // Lazy init: si Active=True pero las funciones aún no existen (ej: Active activado
    // en runtime después de Loaded), las crea ahora.
    if FAutoMCPConfig.Active and not (csDesigning in ComponentState) then
    begin
      if FAutoMCPFunctions.Count = 0 then
        CreateInternalPPMFunctions;
      var LAutoObj := TJSonObject.Create;
      try
        LAutoObj.AddPair('tools', FAutoMCPFunctions.ToJSon);
        TJsonToolUtils.NormalizeToolsFromSource('local', LAutoObj, LAllNormalizedTools);
      finally
        LAutoObj.Free;
      end;
    end;

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
  LFinalPath: string;
  LJsonContent: string;
  LRootObj: TJSonObject;
begin
  Result := 0;
  LFinalPath := AJsonFilePath;

  // Si no se especifica ruta, usar la ruta efectiva del componente
  if LFinalPath.IsEmpty then
    LFinalPath := GetEffectiveMCPConfigPath;

  // 2. Validaci�n de existencia
  if not TFile.Exists(LFinalPath) then
  begin
    DoLog('ImportClaude: Archivo no encontrado en ' + LFinalPath);
    Exit;
  end;

  try
    // 3. Carga segura del contenido
    LJsonContent := TFile.ReadAllText(LFinalPath, TEncoding.UTF8);

    if LJsonContent.Trim.IsEmpty then
    begin
      DoLog('ImportClaude: El archivo est� vac�o.');
      Exit;
    end;

    // 4. Parseo y validaci�n del objeto JSON
    var
    LJsonValue := TJSonObject.ParseJSONValue(LJsonContent);

    if Assigned(LJsonValue) and (LJsonValue is TJSonObject) then
    begin
      LRootObj := LJsonValue as TJSonObject;
      try
        DoLog('Importando servidores MCP desde: ' + LFinalPath);
        // LLAMADA A LA VERSI�N REFACTOREADA (La que maneja StdIo y SSE)
        Result := ImportClaudeMCPConfiguration(LRootObj);
      finally
        LRootObj.Free;
      end;
    end
    else
    begin
      if Assigned(LJsonValue) then
        LJsonValue.Free;
      DoLog('ImportClaude: El contenido no es un objeto JSON v�lido.');
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
  // Forzamos una actualizaci�n final de propiedades una vez cargado todo el FMX.
  // Esto corrige cualquier desajuste por el orden de carga.
  if Assigned(FMCPClients) then
  begin
    for I := 0 to FMCPClients.Count - 1 do
    begin
      // Aseguramos que el cliente interno exista y tenga los datos correctos
      if Assigned(FMCPClients[I]) then
        FMCPClients[I].UpdateClientProperties;
    end;
  end;

  // Carga automática de servidores MCP desde archivo JSON (formato Claude Desktop)
  if FAutoMCPConfig.AutoLoad and not (csDesigning in ComponentState) then
  begin
    var LCount := ImportClaudeMCPConfiguration(FAutoMCPConfig.ConfigFile);
    if LCount > 0 then
      DoLog(Format('AutoLoad: %d servidor(es) MCP cargado(s) desde configuración.', [LCount]));
  end;

  // AutoMCP: las funciones internas se crean de forma lazy en GetTools/DoCallFunction.
  // InitAutoMCP puede llamarse manualmente para forzar la inicialización en runtime.
end;

// =============================================================================
// SOBRECARGA 1: L�gica N�cleo (Recibe TJSONObject)
// =============================================================================
{ function TAiFunctions.ImportClaudeMCPConfiguration(AConfig: TJSonObject): Integer;
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
  // Buscar la clave ra�z "mcpServers"
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

  // Directorio ra�z por defecto (opcional)
  NewClient.Params.Values['RootDir'] := TPath.GetHomePath;

  // 3. Agregar a la colecci�n central
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
}

function TAiFunctions.ImportClaudeMCPConfiguration(AConfig: TJSonObject): Integer;
var
  LMcpServers, LServerObj, LEnvObj: TJSonObject;
  LArgsArray: TJSonArray;
  LServerPair, LEnvPair: TJSONPair;
  LClientItem: TMCPClientItem;
  LServerName, LCommand, LUrl, LArgsString: string;
  I: Integer;
begin
  Result := 0;
  if not Assigned(AConfig) then
    Exit;

  // Intentamos encontrar el nodo ra�z
  if not AConfig.TryGetValue<TJSonObject>('mcpServers', LMcpServers) then
  begin
    DoLog('ImportClaude: No se encontr� el nodo "mcpServers".');
    Exit;
  end;

  for LServerPair in LMcpServers do
  begin
    LServerName := LServerPair.JsonString.Value;

    // 1. Evitar duplicados
    if Assigned(FMCPClients.GetClientByName(LServerName)) then
      Continue;

    LServerObj := LServerPair.JsonValue as TJSonObject;

    // 2. Crear el Item en la colecci�n (el Wrapper)
    LClientItem := FMCPClients.Add;
    LClientItem.Name := LServerName;

    // --- CASO A: Servidor Local (StdIo) ---
    if LServerObj.TryGetValue<string>('command', LCommand) then
    begin
      LClientItem.TransportType := tpStdIo;
      LClientItem.Params.Values['Command'] := LCommand;

      // Procesar Argumentos
      if LServerObj.TryGetValue<TJSonArray>('args', LArgsArray) then
      begin
        LArgsString := '';
        for I := 0 to LArgsArray.Count - 1 do
        begin
          var
          LArg := LArgsArray.Items[I].Value;
          // Si el argumento tiene espacios y no tiene comillas, lo envolvemos
          if (Pos(' ', LArg) > 0) and (not LArg.StartsWith('"')) then
            LArg := '"' + LArg + '"';

          LArgsString := LArgsString + LArg + ' ';
        end;
        LClientItem.Params.Values['Arguments'] := LArgsString.Trim;
      end;

      // RootDir por defecto
      LClientItem.Params.Values['RootDir'] := TPath.GetHomePath;
    end
    // --- CASO B: Servidor Remoto (URL / SSE) ---
    else if LServerObj.TryGetValue<string>('url', LUrl) then
    begin
      LClientItem.TransportType := tpSSE; // Est�ndar para MCP remoto
      LClientItem.Params.Values['URL'] := LUrl;
    end;

    // --- VARIABLES DE ENTORNO (Com�n a ambos) ---
    if LServerObj.TryGetValue<TJSonObject>('env', LEnvObj) then
    begin
      for LEnvPair in LEnvObj do
      begin
        LClientItem.EnvVars.Values[LEnvPair.JsonString.Value] := LEnvPair.JsonValue.Value;
      end;
    end;

    // 3. FINALIZACI�N Y SINCRONIZACI�N (Crucial en tu librer�a)
    LClientItem.Enabled := True;

    // Esto transfiere Params y EnvVars del Item al FMCPClient interno
    LClientItem.UpdateClientProperties;

    Inc(Result);
    DoLog(Format('ImportClaude: Servidor "%s" cargado exitosamente.', [LServerName]));
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

// ---------------------------------------------------------------------------
// PPM Integration — helpers privados a nivel de unidad
// ---------------------------------------------------------------------------

// GET simple, retorna el cuerpo como string o '' si falla.
function PPMHttpGetStr(const AUrl: String): String;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  Result := '';
  LClient := THTTPClient.Create;
  try
    LClient.ConnectionTimeout := 15000;
    LClient.ResponseTimeout   := 15000;
    try
      LResponse := LClient.Get(AUrl);
      if LResponse.StatusCode = 200 then
        Result := LResponse.ContentAsString(TEncoding.UTF8);
    except
      // Error de red: retorna vacío
    end;
  finally
    LClient.Free;
  end;
end;

// Descarga AUrl (siguiendo redirects 3xx) a ADestFile.
// Retorna True si la descarga fue exitosa y el archivo tiene contenido.
function PPMHttpDownload(const AUrl, ADestFile: String): Boolean;
var
  LClient: THTTPClient;
  LStream: TFileStream;
  LResponse: IHTTPResponse;
begin
  Result := False;
  LClient := THTTPClient.Create;
  try
    LClient.ConnectionTimeout := 15000;
    LClient.ResponseTimeout   := 120000;  { 2 min — .paipkg puede ser 5-10 MB }
    try
      LStream := TFileStream.Create(ADestFile, fmCreate);
      try
        LResponse := LClient.Get(AUrl, LStream);
        Result := (LResponse.StatusCode = 200) and (LStream.Size > 0);
      finally
        LStream.Free;
      end;
    except
      // Error de red o escritura: retorna False
    end;
  finally
    LClient.Free;
  end;
end;

// Resuelve la versión a usar: si AVersion no está vacío la retorna directamente;
// si está vacío consulta /v1/packages/:name y devuelve la última versión no-yanked.
function PPMResolveVersion(const ARegistryUrl, AName, AVersion: String): String;
var
  LBody: String;
  LJson, LPackage, LVer: TJSONObject;
  LVersions: TJSONArray;
  LYankedVal: TJSONValue;
  I: Integer;
begin
  Result := AVersion;
  if Result <> '' then
    Exit;

  LBody := PPMHttpGetStr(ARegistryUrl + '/v1/packages/' + AName);
  if LBody = '' then
    Exit;

  LJson := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
  if not Assigned(LJson) then
    Exit;
  try
    if LJson.TryGetValue<TJSONObject>('package', LPackage) and
       LPackage.TryGetValue<TJSONArray>('versions', LVersions) then
    begin
      for I := 0 to LVersions.Count - 1 do
      begin
        LVer := LVersions.Items[I] as TJSONObject;
        LYankedVal := LVer.FindValue('yanked');
        if not (Assigned(LYankedVal) and (LYankedVal is TJSONTrue)) then
        begin
          LVer.TryGetValue<String>('version', Result);
          Break;
        end;
      end;
    end;
  finally
    LJson.Free;
  end;
end;

// Extrae el contenido de un .paipkg (ZIP) a ADestDir y devuelve la ruta
// completa del entrypoint declarado en pai.package [mcp] entrypoint=...
// Si no hay manifiesto, intenta encontrar el primer .exe (Windows) o binario
// sin extensión (Linux) en la raíz del ZIP.
// Retorna '' si no se pudo extraer o no se encontró el entrypoint.
function PPMExtractBinary(const APaipkgPath, ADestDir: String): String;
var
  LZip       : TZipFile;
  I          : Integer;
  LFileName  : String;
  LDestPath  : String;
  LBytes     : TBytes;
  LStream    : TFileStream;
  LManifest  : String;
  LEntrypoint: String;
  LIni       : TMemIniFile;
begin
  Result := '';

  if not TDirectory.Exists(ADestDir) then
    TDirectory.CreateDirectory(ADestDir);

  LZip := TZipFile.Create;
  try
    LZip.Open(APaipkgPath, zmRead);
    try
      // Extraer todos los archivos al directorio de destino preservando estructura
      for I := 0 to LZip.FileCount - 1 do
      begin
        LFileName := LZip.FileName[I];
        // Ignorar entradas de directorio
        if LFileName.EndsWith('/') or LFileName.EndsWith('\') then Continue;

        // Normalizar separadores y construir ruta destino
        LFileName := StringReplace(LFileName, '/', PathDelim, [rfReplaceAll]);
        LFileName := StringReplace(LFileName, '\', PathDelim, [rfReplaceAll]);
        LDestPath := TPath.Combine(ADestDir, LFileName);

        // Crear subdirectorios intermedios si son necesarios
        var LDestDir := TPath.GetDirectoryName(LDestPath);
        if not TDirectory.Exists(LDestDir) then
          TDirectory.CreateDirectory(LDestDir);

        LZip.Read(LZip.FileName[I], LBytes);
        if Length(LBytes) = 0 then Continue;

        try
          LStream := TFileStream.Create(LDestPath, fmCreate);
          try
            LStream.WriteBuffer(LBytes[0], Length(LBytes));
          finally
            LStream.Free;
          end;
        except
          on E: EFCreateError do
          begin
            // Archivo en uso (proceso MCP corriendo) — saltear y continuar.
            // Si el entrypoint ya existe lo usaremos en el paso de manifiesto.
          end;
        end;
      end;
    finally
      LZip.Close;
    end;
  finally
    LZip.Free;
  end;

  // Leer entrypoint del manifiesto pai.package
  LManifest := TPath.Combine(ADestDir, 'pai.package');
  LEntrypoint := '';
  if TFile.Exists(LManifest) then
  begin
    LIni := TMemIniFile.Create(LManifest);
    try
{$IFDEF MSWINDOWS}
      LEntrypoint := LIni.ReadString('mcp', 'entrypoint', '');
{$ELSE}
      // En Linux preferir entrypoint_lin64; fallback a entrypoint
      LEntrypoint := LIni.ReadString('mcp', 'entrypoint_lin64', '');
      if LEntrypoint = '' then
        LEntrypoint := LIni.ReadString('mcp', 'entrypoint', '');
{$ENDIF}
    finally
      LIni.Free;
    end;
  end;

  if LEntrypoint <> '' then
  begin
    LDestPath := TPath.Combine(ADestDir, LEntrypoint);
    if TFile.Exists(LDestPath) then
    begin
{$IFDEF POSIX}
      TFile.SetAttributes(LDestPath,
        TFile.GetAttributes(LDestPath) + [TFileAttribute.faOwnerExecute]);
{$ENDIF}
      Result := LDestPath;
      Exit;
    end;
  end;

  // Fallback: primer .exe en Windows, o primer archivo sin extensión en Linux
  // Búsqueda recursiva para soportar paquetes con estructura bin/win64/
  if TDirectory.Exists(ADestDir) then
  begin
{$IFDEF MSWINDOWS}
    var LFiles := TDirectory.GetFiles(ADestDir, '*.exe', TSearchOption.soAllDirectories);
{$ELSE}
    var LFiles := TDirectory.GetFiles(ADestDir, '*', TSearchOption.soAllDirectories);
{$ENDIF}
    for var LF in LFiles do
    begin
{$IFNDEF MSWINDOWS}
      if TPath.GetExtension(LF) <> '' then Continue;
{$ENDIF}
      if SameText(TPath.GetFileName(LF), 'pai.package') then Continue;
{$IFDEF POSIX}
      TFile.SetAttributes(LF,
        TFile.GetAttributes(LF) + [TFileAttribute.faOwnerExecute]);
{$ENDIF}
      Result := LF;
      Break;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// AutoMCP helpers
// ---------------------------------------------------------------------------

procedure TAiFunctions.OnAutoMCPActiveChanged(Sender: TObject);
// Llamado cuando AutoMCPConfig.Active cambia en runtime.
// Active=True  → crea las funciones si aún no existen (lazy).
// Active=False → limpia FAutoMCPFunctions para un estado limpio al reactivar.
begin
  if csDesigning in ComponentState then Exit;
  if FAutoMCPConfig.Active then
  begin
    if FAutoMCPFunctions.Count = 0 then
      CreateInternalPPMFunctions;
    DoLog('[AutoMCP] Activado. Funciones: ppm_search, ppm_install, call_mcp_tool.');
  end
  else
  begin
    FAutoMCPFunctions.Clear;
    DoLog('[AutoMCP] Desactivado. Funciones internas eliminadas.');
  end;
end;

procedure TAiFunctions.InitAutoMCP;
// Fuerza la creación de las 3 funciones internas PPM en runtime.
// Normalmente NO es necesario llamarlo — las funciones se crean de forma lazy
// en el primer GetTools() o DoCallFunction() cuando Active=True.
// Útil si el developer necesita que las funciones existan antes del primer chat
// (ej: para mostrar el system prompt con las funciones ya listadas).
begin
  if csDesigning in ComponentState then Exit;
  if not FAutoMCPConfig.Active then Exit;
  if FAutoMCPFunctions.Count > 0 then Exit; // ya inicializado

  CreateInternalPPMFunctions;
  DoLog('[AutoMCP] Inicializado manualmente. Funciones: ppm_search, ppm_install, call_mcp_tool.');
end;

procedure TAiFunctions.CreateInternalPPMFunctions;
var
  LFn: TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  FAutoMCPFunctions.Clear;

  // --- ppm_search ---
  LFn := TFunctionActionItem(FAutoMCPFunctions.Add);
  LFn.FunctionName := 'ppm_search';
  LFn.Description.Text :=
    'Search for MCP tools in the PPM registry (registry.pascalai.org). ' +
    'Returns a list of available tools matching the query with their names, ' +
    'descriptions and available functions. Call this before ppm_install.';
  LFn.Enabled := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'query';
  LParam.Description.Text := 'Search keywords (e.g.: github, postgres, slack, kubernetes, filesystem)';
  LParam.ParamType := ptString;
  LParam.Required := True;

  // --- ppm_install ---
  LFn := TFunctionActionItem(FAutoMCPFunctions.Add);
  LFn.FunctionName := 'ppm_install';
  LFn.Description.Text :=
    'Download and install an MCP tool from the PPM registry. ' +
    'After installation the tool is immediately ready to use with call_mcp_tool. ' +
    'Call this autonomously when you need a tool — do not ask the user for confirmation. ' +
    'Use force=true to force reinstall/update even if already installed.';
  LFn.Enabled := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'tool_name';
  LParam.Description.Text := 'Exact PPM package name (e.g.: mcp-github, mcp-postgres, mcp-slack)';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'force';
  LParam.Description.Text := 'Set to true to force reinstall/update even if the tool is already installed';
  LParam.ParamType := ptBoolean;
  LParam.Required := False;

  // --- call_mcp_tool ---
  LFn := TFunctionActionItem(FAutoMCPFunctions.Add);
  LFn.FunctionName := 'call_mcp_tool';
  LFn.Description.Text :=
    'Call a specific function on an installed MCP tool. ' +
    'Use ppm_search to discover tools and ppm_install to install them first if needed. ' +
    'The available function names are returned by ppm_install.';
  LFn.Enabled := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'tool_name';
  LParam.Description.Text := 'Name of the installed MCP tool (e.g.: mcp-github)';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'function_name';
  LParam.Description.Text := 'Name of the function to call on the tool (from ppm_install result)';
  LParam.ParamType := ptString;
  LParam.Required := True;
  LParam := TFunctionParamsItem(LFn.Parameters.Add);
  LParam.Name := 'arguments';
  LParam.Description.Text := 'JSON string with the function arguments (e.g.: {"query":"pascal","limit":10})';
  LParam.ParamType := ptString;
  LParam.Required := True;
end;

procedure TAiFunctions.InternalAutoMCP_PpmSearch(ToolCall: TAiToolsFunction);
var
  LQuery: string;
  LResult: TJSONObject;
  LTools: TJSONArray;
  LSb: TStringBuilder;
begin
  LQuery := '';
  var LArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgs) then
  try
    LArgs.TryGetValue<string>('query', LQuery);
    LQuery := Trim(LQuery);
  finally
    LArgs.Free;
  end;

  DoLog('[AutoMCP] ppm_search: "' + LQuery + '"');
  DoStatusUpdate('Buscando "' + LQuery + '" en el registry PPM...');

  LResult := SearchPPMMCP(LQuery, 1, 20, FAutoMCPConfig.RegistryUrl);
  if not Assigned(LResult) then
  begin
    ToolCall.Response := 'No se pudo conectar con el registry PPM. Verifica la conexión a Internet.';
    Exit;
  end;

  LSb := TStringBuilder.Create;
  try
    if LResult.TryGetValue<TJSONArray>('tools', LTools) and (LTools.Count > 0) then
    begin
      var LShown   := 0;
      var LFiltered := 0;
      for var I := 0 to LTools.Count - 1 do
      begin
        var LTool := LTools.Items[I] as TJSONObject;
        var LName, LDesc, LVer: string;
        LTool.TryGetValue<string>('name', LName);
        LTool.TryGetValue<string>('description', LDesc);
        LTool.TryGetValue<string>('version', LVer);
        // Aplicar política Allowed/Blocked antes de mostrar al LLM
        if not IsAutoMCPAllowed(LName) then
        begin
          Inc(LFiltered);
          Continue;
        end;
        LSb.AppendLine(Format('• %s (v%s): %s', [LName, LVer, LDesc]));
        Inc(LShown);
      end;

      if LShown > 0 then
      begin
        // Insertar encabezado al inicio del resultado
        var LHeader := Format('Se encontraron %d herramientas para "%s":', [LShown, LQuery]);
        if LFiltered > 0 then
          LHeader := LHeader + Format(' (%d bloqueadas por política)', [LFiltered]);
        ToolCall.Response := LHeader + sLineBreak + LSb.ToString +
          sLineBreak + 'Para usar una herramienta: llama ppm_install("<nombre>"), luego call_mcp_tool.';
      end
      else if LFiltered > 0 then
        ToolCall.Response := Format(
          'Se encontraron %d herramientas para "%s" pero todas están bloqueadas por la política AutoMCP.',
          [LFiltered, LQuery])
      else
        ToolCall.Response := Format('No se encontraron herramientas para "%s" en el registry PPM.', [LQuery]);
    end
    else
      ToolCall.Response := Format('No se encontraron herramientas para "%s" en el registry PPM.', [LQuery]);
  finally
    LSb.Free;
    LResult.Free;
  end;
end;

procedure TAiFunctions.InternalAutoMCP_PpmInstall(ToolCall: TAiToolsFunction);
var
  LToolName: string;
  LForce: Boolean;
  LItem: TMCPClientItem;
  LFuncNames: TStringList;
begin
  LToolName := '';
  LForce    := False;
  var LArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgs) then
  try
    LArgs.TryGetValue<string>('tool_name', LToolName);
    LArgs.TryGetValue<Boolean>('force', LForce);
    LToolName := Trim(LToolName);
  finally
    LArgs.Free;
  end;

  if LToolName = '' then
  begin
    ToolCall.Response := '{"ok":false,"error":"tool_name parameter is required"}';
    Exit;
  end;

  // Verificar política de whitelist/blacklist
  if not IsAutoMCPAllowed(LToolName) then
  begin
    ToolCall.Response := Format(
      '{"ok":false,"error":"Installation of ''%s'' is not allowed by AutoMCP policy"}',
      [LToolName]);
    Exit;
  end;

  DoLog('[AutoMCP] ppm_install: "' + LToolName + '"' + IfThen(LForce, ' (force)', ''));
  DoStatusUpdate('Instalando "' + LToolName + '" desde PPM...');

  // Registrar el intento ANTES de instalarlo para evitar bucles infinitos en caso de error
  if FInstalledPackages.IndexOf(LowerCase(LToolName)) < 0 then
    FInstalledPackages.Add(LowerCase(LToolName));

  // Si ya está registrado: reutilizar salvo que force=true
  LItem := FMCPClients.GetClientByName(LToolName);
  if Assigned(LItem) and not LForce then
    DoLog('[AutoMCP] "' + LToolName + '" ya instalado, reutilizando.')
  else
  begin
    // force=true o no instalado: descargar/reinstalar desde PPM
    if Assigned(LItem) and LForce then
    begin
      DoLog('[AutoMCP] "' + LToolName + '" force reinstall — deteniendo proceso previo.');
      LItem.MCPClient.Disconnect;
    end;
    LItem := InstallMCPFromPPM(LToolName, '', FAutoMCPConfig.WorkingDir, FAutoMCPConfig.RegistryUrl);
  end;

  if not Assigned(LItem) then
  begin
    ToolCall.Response := Format(
      '{"ok":false,"error":"No se pudo instalar ''%s''. Verifica el nombre del paquete en PPM y la conexión a internet. No intentes instalar este paquete nuevamente en esta sesión."}',
      [LToolName]);
    Exit;
  end;

  // Inicializar el proceso MCP
  if not LItem.MCPClient.Initialized then
    LItem.MCPClient.Initialize;

  // Disabled en GetTools: el LLM accede solo via call_mcp_tool (evita race conditions)
  LItem.Enabled := False;

  // Recopilar funciones disponibles para informar al LLM
  LFuncNames := TStringList.Create;
  try
    var LToolsJson := TJSONObject.ParseJSONValue(LItem.MCPClient.Tools.Text);
    if Assigned(LToolsJson) then
    try
      var LToolsArr: TJSONArray;
      if LToolsJson.TryGetValue<TJSONArray>('tools', LToolsArr) then
        for var LT in LToolsArr do
        begin
          var LFName: string;
          if (LT is TJSONObject) and TJSONObject(LT).TryGetValue<string>('name', LFName) then
            LFuncNames.Add(LFName);
        end;
    finally
      LToolsJson.Free;
    end;

    var LFuncList := '';
    if LFuncNames.Count > 0 then
      LFuncList := LFuncNames.CommaText;

    if LFuncList <> '' then
      ToolCall.Response := Format(
        '{"ok":true,"message":"''%s'' listo. ACCIÓN REQUERIDA: llama AHORA call_mcp_tool con tool_name=''%s'' y function_name=uno de [%s] con los argumentos originales. NO llames ppm_search."}',
        [LToolName, LToolName, LFuncList])
    else
      ToolCall.Response := Format(
        '{"ok":true,"message":"''%s'' iniciado. Llama call_mcp_tool con tool_name=''%s'' y los argumentos originales. NO llames ppm_search."}',
        [LToolName, LToolName]);
  finally
    LFuncNames.Free;
  end;

  DoLog('[AutoMCP] "' + LToolName + '" listo en ' + LItem.Params.Values['Command']);
end;

procedure TAiFunctions.InternalAutoMCP_CallMcpTool(ToolCall: TAiToolsFunction);
var
  LToolName, LFuncName, LArgStr: string;
  LClientItem: TMCPClientItem;
  LArgsJson, LResult: TJSONObject;
  LExtractedMedia: TObjectList<TAiMediaFile>;
  MF: TAiMediaFile;
begin
  LToolName := '';
  LFuncName := '';
  LArgStr := '{}';
  var LArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if Assigned(LArgs) then
  try
    LArgs.TryGetValue<string>('tool_name', LToolName);
    LArgs.TryGetValue<string>('function_name', LFuncName);
    LArgs.TryGetValue<string>('arguments', LArgStr);
    LToolName := Trim(LToolName);
    LFuncName := Trim(LFuncName);
    LArgStr   := Trim(LArgStr);
    if LArgStr = '' then LArgStr := '{}';
  finally
    LArgs.Free;
  end;

  // Normalizar LFuncName: el LLM puede enviar el nombre combinado "server_99_func"
  // (resultado de la normalización en GetTools). Extraemos la parte después del último MCP_TOOL_SEP.
  if Pos(MCP_TOOL_SEP, LFuncName) > 0 then
  begin
    var LSepIdx := Pos(MCP_TOOL_SEP, LFuncName);
    while True do
    begin
      var LNext := PosEx(MCP_TOOL_SEP, LFuncName, LSepIdx + 1);
      if LNext = 0 then Break;
      LSepIdx := LNext;
    end;
    LFuncName := Copy(LFuncName, LSepIdx + Length(MCP_TOOL_SEP), MaxInt);
  end;

  DoLog(Format('[AutoMCP] call_mcp_tool: %s → %s', [LToolName, LFuncName]));
  DoStatusUpdate(Format('Ejecutando %s.%s...', [LToolName, LFuncName]));

  LClientItem := FMCPClients.GetClientByName(LToolName);
  if not Assigned(LClientItem) then
  begin
    ToolCall.Response := Format(
      '{"error":"Herramienta ''%s'' no está instalada. Usa ppm_install primero."}',
      [LToolName]);
    Exit;
  end;

  if not LClientItem.MCPClient.Initialized then
    LClientItem.MCPClient.Initialize;

  if not LClientItem.MCPClient.Available then
  begin
    ToolCall.Response := Format(
      '{"error":"MCP server ''%s'' no está disponible. Puede requerir reinstalación."}',
      [LToolName]);
    Exit;
  end;

  // Resolver el nombre exacto del tool en el servidor (dash vs underscore).
  // Si el nombre no coincide con ninguna función conocida, devuelve error con la
  // lista de nombres válidos para que el LLM los use directamente.
  if LFuncName <> '' then
  begin
    var LFuncFound := False;
    var LValidFuncs := TStringList.Create;
    try
      var LToolsRoot := TJSONObject.ParseJSONValue(LClientItem.MCPClient.Tools.Text) as TJSONObject;
      if Assigned(LToolsRoot) then
      try
        var LToolsArr: TJSONArray;
        if LToolsRoot.TryGetValue<TJSONArray>('tools', LToolsArr) then
          for var LT in LToolsArr do
            if LT is TJSONObject then
            begin
              var LActualName: string;
              if TJSONObject(LT).TryGetValue<string>('name', LActualName) then
              begin
                LValidFuncs.Add(LActualName);
                var LNormActual := StringReplace(LActualName, '-', '_', [rfReplaceAll]);
                var LNormFunc   := StringReplace(LFuncName,   '-', '_', [rfReplaceAll]);
                if SameText(LNormActual, LNormFunc) then
                begin
                  LFuncName := LActualName; // nombre exacto que el servidor reconoce
                  LFuncFound := True;
                end;
              end;
            end;
      finally
        LToolsRoot.Free;
      end;
      // Si el nombre no se encontró y tenemos la lista, devolver error con nombres válidos
      if not LFuncFound and (LValidFuncs.Count > 0) then
      begin
        ToolCall.Response := Format(
          '{"error":"La función ''%s'' no existe en ''%s''. Nombres válidos: [%s]. Usa uno de estos exactamente."}',
          [LFuncName, LToolName, LValidFuncs.CommaText]);
        Exit;
      end;
    finally
      LValidFuncs.Free;
    end;
  end;

  // Serializar llamadas concurrentes al mismo cliente
  FAutoMCPLock.Enter;
  try
    LArgsJson := TJSONObject.ParseJSONValue(LArgStr) as TJSONObject;
    if not Assigned(LArgsJson) then
      LArgsJson := TJSONObject.Create; // fallback: objeto vacío

    LExtractedMedia := TObjectList<TAiMediaFile>.Create;
    try
      LResult := LClientItem.MCPClient.CallTool(LFuncName, LArgsJson, LExtractedMedia);
      try
        if Assigned(LResult) then
          ToolCall.Response := LResult.ToJson
        else
          ToolCall.Response := Format(
            '{"error":"MCP server ''%s'' no respondió a la llamada ''+LFuncName+''"}',
            [LToolName]);

        // Propagar archivos media extraídos al ToolCall (imágenes, audio, etc.)
        for MF in LExtractedMedia do
        begin
          ToolCall.MediaFiles.Add(MF);
          if Assigned(ToolCall.ResMsg) then
          begin
            var LClone := TAiMediaFile.Create;
            LClone.LoadFromBase64(MF.FileName, MF.Base64);
            TAiChatMessage(ToolCall.ResMsg).AddMediaFile(LClone);
          end;
        end;
        if LExtractedMedia.Count > 0 then
          LExtractedMedia.OwnsObjects := False;
      finally
        LResult.Free;
      end;
    finally
      LExtractedMedia.Free;
    end;
  finally
    FAutoMCPLock.Leave;
  end;
end;

function TAiFunctions.GetAutoMCPSystemPrompt: String;
var
  LSb: TStringBuilder;
  I: Integer;
begin
  LSb := TStringBuilder.Create;
  try
    LSb.AppendLine(
      'Tienes acceso al registry PPM (PascalAI Package Manager) con más de 150 ' +
      'herramientas MCP que cubren GitHub, PostgreSQL, Slack, Jira, Stripe, DuckDB, ' +
      'Kubernetes, AWS S3, Azure, Kafka, GraphQL y muchas más.');
    LSb.AppendLine;
    LSb.AppendLine('Cuando el usuario te pida algo que requiere acceso a servicios externos, ' +
      'actúa de forma AUTÓNOMA siguiendo este flujo sin pedir permiso:');
    LSb.AppendLine('  1. Llama a ppm_search con palabras clave para encontrar la herramienta adecuada.');
    LSb.AppendLine('  2. Llama a ppm_install con el nombre exacto del paquete para instalarlo.');
    LSb.AppendLine('  3. Llama a call_mcp_tool con el nombre del tool, la función y los argumentos JSON.');
    LSb.AppendLine;
    LSb.AppendLine('Convenciones:');
    LSb.AppendLine('- Los nombres de paquetes siguen el patrón mcp-<servicio> (ej: mcp-github, mcp-postgres).');
    LSb.AppendLine('- Los argumentos de call_mcp_tool deben ser un JSON válido como string.');
    LSb.AppendLine('- Responde en el mismo idioma que el usuario.');
    LSb.AppendLine('- Para herramientas de audio/TTS: solicita siempre formato WAV si el tool lo soporta (parámetro format="wav" o similar). Solo usa MP3 u otros formatos si WAV no está disponible.');
    LSb.AppendLine;
    LSb.AppendLine('REGLAS CRÍTICAS DE EJECUCIÓN (obligatorias):');
    LSb.AppendLine('- Ejecuta las herramientas de forma SECUENCIAL, una a la vez. Espera siempre el resultado de call_mcp_tool antes de llamar a cualquier otra herramienta.');
    LSb.AppendLine('- Si ya tienes una herramienta ACTIVA que puede resolver la tarea, úsala DIRECTAMENTE. NO llames a ppm_search ni ppm_install en paralelo mientras call_mcp_tool está en ejecución.');
    LSb.AppendLine('- NUNCA llames ppm_search después de que call_mcp_tool ejecutó la operación (exitosa o con error). ppm_search solo sirve para descubrir herramientas nuevas, no para reintentar.');
    LSb.AppendLine('- Si call_mcp_tool devuelve un error de red, autenticación, credenciales o permisos: reporta el error directamente al usuario. NO busques herramientas alternativas.');
    LSb.AppendLine('- Si call_mcp_tool devuelve "función no encontrada": usa los nombres válidos que indica el error y reintenta UNA vez. Si sigue fallando, reporta al usuario.');

    // Clasificar clientes habilitados: activos (proceso corriendo) vs registrados (proceso no iniciado)
    var LHasActive := False;
    var LHasRegistered := False;
    for I := 0 to FMCPClients.Count - 1 do
      if FMCPClients[I].Enabled then
      begin
        if FMCPClients[I].MCPClient.Available then
          LHasActive := True
        else
          LHasRegistered := True;
      end;

    // Servidores activos: saltar ppm_search y ppm_install; incluir lista exacta de tools con parámetros
    if LHasActive then
    begin
      LSb.AppendLine;
      LSb.AppendLine('HERRAMIENTAS MCP ACTIVAS (proceso ya corriendo — omite ppm_search y ppm_install, ve directo al paso 3):');
      LSb.AppendLine('IMPORTANTE: usa EXCLUSIVAMENTE los nombres de función y parámetros indicados aquí. No inventes nombres.');
      for I := 0 to FMCPClients.Count - 1 do
        if FMCPClients[I].Enabled and FMCPClients[I].MCPClient.Available then
        begin
          // Si el cache de tools está vacío (servidor cargado desde config sin Initialize),
          // llamar Initialize ahora para poblar el cache antes de construir el prompt
          if FMCPClients[I].MCPClient.Tools.Text.IsEmpty then
            FMCPClients[I].MCPClient.Initialize;
          var LToolsJson := FMCPClients[I].MCPClient.Tools.Text;
          var LJObj := TJSONObject.ParseJSONValue(LToolsJson);
          if Assigned(LJObj) then
          try
            var LJTools: TJSONValue;
            if LJObj.TryGetValue('tools', LJTools) and (LJTools is TJSONArray) then
            begin
              var LArr := TJSONArray(LJTools);
              for var J := 0 to LArr.Count - 1 do
              begin
                var LTool := LArr.Items[J];
                if LTool is TJSONObject then
                begin
                  var LName: string;
                  if TJSONObject(LTool).TryGetValue<string>('name', LName) then
                  begin
                    LSb.Append('  ' + FMCPClients[I].Name + ' / ' + LName);
                    // Extraer parámetros del inputSchema
                    var LSchema: TJSONObject;
                    if TJSONObject(LTool).TryGetValue<TJSONObject>('inputSchema', LSchema) then
                    begin
                      var LProps: TJSONObject;
                      var LRequired: TJSONArray;
                      var LReqSet := TStringList.Create;
                      try
                        if LSchema.TryGetValue<TJSONArray>('required', LRequired) then
                          for var R in LRequired do
                            LReqSet.Add(R.Value);
                        if LSchema.TryGetValue<TJSONObject>('properties', LProps) then
                        begin
                          var LParamStr := '';
                          for var P in LProps do
                          begin
                            var LPName := P.JsonString.Value;
                            var LOptional := LReqSet.IndexOf(LPName) < 0;
                            if LParamStr <> '' then LParamStr := LParamStr + ', ';
                            if LOptional then
                              LParamStr := LParamStr + LPName + '?'
                            else
                              LParamStr := LParamStr + LPName;
                          end;
                          if LParamStr <> '' then
                            LSb.Append('(' + LParamStr + ')');
                        end;
                      finally
                        LReqSet.Free;
                      end;
                    end;
                    LSb.AppendLine;
                  end;
                end;
              end;
              LSb.AppendLine;
            end
            else
              LSb.AppendLine('- ' + FMCPClients[I].Name);
          finally
            LJObj.Free;
          end
          else
            LSb.AppendLine('- ' + FMCPClients[I].Name);
        end;
    end;

    // Servidores registrados pero proceso aún no iniciado: saltar ppm_search, llamar ppm_install para arrancar
    if LHasRegistered then
    begin
      LSb.AppendLine;
      LSb.AppendLine('HERRAMIENTAS MCP REGISTRADAS (binario ya instalado — omite ppm_search, llama ppm_install para iniciar el proceso, luego call_mcp_tool):');
      for I := 0 to FMCPClients.Count - 1 do
        if FMCPClients[I].Enabled and not FMCPClients[I].MCPClient.Available then
          LSb.AppendLine('- ' + FMCPClients[I].Name);
    end;

    Result := LSb.ToString;
  finally
    LSb.Free;
  end;
end;

function TAiFunctions.IsAutoMCPAllowed(const APkgName: string): Boolean;
// Evalúa si un paquete puede instalarse según la política AutoMCP.
// Orden: OnAutoMCPRequest → Whitelist (si no vacía) → Blacklist → permitir.
var
  LAllow: Boolean;
begin
  LAllow := True;

  // 1. Callback dinámico (máxima prioridad)
  if Assigned(FOnAutoMCPRequest) then
  begin
    FOnAutoMCPRequest(Self, APkgName, LAllow);
    Result := LAllow;
    Exit;
  end;

  // 2. Whitelist: si tiene entradas, solo esos paquetes se permiten
  if FAutoMCPConfig.FAllowed.Count > 0 then
  begin
    Result := FAutoMCPConfig.FAllowed.IndexOf(APkgName) >= 0;
    Exit;
  end;

  // 3. Blacklist: paquetes siempre bloqueados
  if FAutoMCPConfig.FBlocked.IndexOf(APkgName) >= 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

procedure TAiFunctions.SetAutoMCPConfig(const Value: TAutoMCPConfig);
begin
  FAutoMCPConfig.Assign(Value);
end;

// ---------------------------------------------------------------------------
// TAutoMCPConfig
// ---------------------------------------------------------------------------

constructor TAutoMCPConfig.Create;
begin
  inherited Create;
  FActive := False;
  FRegistryUrl := 'https://registry.pascalai.org';
  FAllowed := TStringList.Create;
  FBlocked := TStringList.Create;
end;

destructor TAutoMCPConfig.Destroy;
begin
  FAllowed.Free;
  FBlocked.Free;
  inherited;
end;

procedure TAutoMCPConfig.Assign(Source: TPersistent);
var
  Src: TAutoMCPConfig;
begin
  if Source is TAutoMCPConfig then
  begin
    Src := TAutoMCPConfig(Source);
    FActive      := Src.FActive;
    FRegistryUrl := Src.FRegistryUrl;
    FWorkingDir  := Src.FWorkingDir;
    FAllowed.Assign(Src.FAllowed);
    FBlocked.Assign(Src.FBlocked);
    FConfigFile  := Src.FConfigFile;
    FAutoLoad    := Src.FAutoLoad;
  end
  else
    inherited Assign(Source);
end;

function TAutoMCPConfig.GetAllowed: TStrings;
begin
  Result := FAllowed;
end;

function TAutoMCPConfig.GetBlocked: TStrings;
begin
  Result := FBlocked;
end;

procedure TAutoMCPConfig.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  FActive := Value;
  // Notificar a TAiFunctions para que cree o limpie las funciones internas
  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self);
end;

procedure TAutoMCPConfig.SetAllowed(const Value: TStrings);
begin
  FAllowed.Assign(Value);
end;

procedure TAutoMCPConfig.SetBlocked(const Value: TStrings);
begin
  FBlocked.Assign(Value);
end;

// ---------------------------------------------------------------------------
// Config file helpers
// ---------------------------------------------------------------------------

function TAiFunctions.GetEffectiveMCPConfigPath: String;
begin
  if FAutoMCPConfig.ConfigFile <> '' then
    Result := FAutoMCPConfig.ConfigFile
  else
    Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'mcp_servers.json');
end;

function TAiFunctions.SaveMCPConfiguration(const AFilePath: string): Boolean;

  // Divide una línea de argumentos respetando comillas dobles.
  // Ej: '-y "path con espacios" --flag' → ['-y', 'path con espacios', '--flag']
  function SplitArgs(const AArgs: string): TArray<string>;
  var
    LList: TList<string>;
    I: Integer;
    LToken: string;
    LInQuote: Boolean;
    C: Char;
  begin
    LList := TList<string>.Create;
    try
      LToken   := '';
      LInQuote := False;
      for I := 1 to Length(AArgs) do
      begin
        C := AArgs[I];
        if C = '"' then
          LInQuote := not LInQuote
        else if (C = ' ') and not LInQuote then
        begin
          if LToken <> '' then
          begin
            LList.Add(LToken);
            LToken := '';
          end;
        end
        else
          LToken := LToken + C;
      end;
      if LToken <> '' then
        LList.Add(LToken);
      Result := LList.ToArray;
    finally
      LList.Free;
    end;
  end;

var
  LPath: String;
  LRoot, LServers, LServerObj, LEnvObj: TJSONObject;
  LArgsArray: TJSONArray;
  LItem: TMCPClientItem;
  LArgs: TArray<string>;
  LArg, LCmd, LUrl, LArgsStr, LDir: string;
  I, J: Integer;
begin
  Result := False;
  LPath := AFilePath;
  if LPath = '' then
    LPath := GetEffectiveMCPConfigPath;

  LRoot := TJSONObject.Create;
  try
    LServers := TJSONObject.Create;
    LRoot.AddPair('mcpServers', LServers);

    for I := 0 to FMCPClients.Count - 1 do
    begin
      LItem := FMCPClients[I];

      LServerObj := TJSONObject.Create;

      case LItem.TransportType of
        tpStdIo:
        begin
          LCmd := LItem.Params.Values['Command'];
          LServerObj.AddPair('command', LCmd);

          LArgsStr := LItem.Params.Values['Arguments'];
          if LArgsStr <> '' then
          begin
            LArgsArray := TJSONArray.Create;
            LArgs := SplitArgs(LArgsStr);
            for J := 0 to High(LArgs) do
              LArgsArray.Add(LArgs[J]);
            LServerObj.AddPair('args', LArgsArray);
          end;

          LDir := LItem.Params.Values['RootDir'];
          if LDir <> '' then
            LServerObj.AddPair('rootDir', LDir);
        end;

        tpSSE, tpHttp:
        begin
          LUrl := LItem.Params.Values['URL'];
          LServerObj.AddPair('url', LUrl);
        end;
      end;

      // Variables de entorno
      if LItem.EnvVars.Count > 0 then
      begin
        LEnvObj := TJSONObject.Create;
        for J := 0 to LItem.EnvVars.Count - 1 do
          LEnvObj.AddPair(LItem.EnvVars.Names[J], LItem.EnvVars.ValueFromIndex[J]);
        LServerObj.AddPair('env', LEnvObj);
      end;

      LServers.AddPair(LItem.Name, LServerObj);
    end;

    // Crear directorio si no existe
    var LDir2 := TPath.GetDirectoryName(LPath);
    if (LDir2 <> '') and not TDirectory.Exists(LDir2) then
      TDirectory.CreateDirectory(LDir2);

    TFile.WriteAllText(LPath, LRoot.Format(2), TEncoding.UTF8);
    Result := True;
    DoLog('SaveMCPConfig: ' + IntToStr(FMCPClients.Count) + ' servidor(es) guardados en ' + LPath);
  except
    on E: Exception do
      DoLog('SaveMCPConfig: Error al guardar: ' + E.Message);
  end;
  LRoot.Free;
end;

// ---------------------------------------------------------------------------

function TAiFunctions.SearchPPMMCP(const AQuery: String; APage, APerPage: Integer;
  const ARegistryUrl: String): TJSONObject;
var
  LUrl, LBody: String;
begin
  Result := nil;
  LUrl := Format('%s/v1/mcp/discover?q=%s&page=%d&per_page=%d',
    [ARegistryUrl,
     TNetEncoding.URL.Encode(AQuery),
     APage,
     APerPage]);
  LBody := PPMHttpGetStr(LUrl);
  if LBody <> '' then
    Result := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
end;

function TAiFunctions.ImportMCPFromPPM(const AName, AVersion, ARegistryUrl: String): TMCPClientItem;
// Registra un stub StdIo sin descargar. Útil si el binario ya está instalado.
// El llamador debe asignar Params['Command'] con la ruta al exe antes de habilitar.
var
  LVersion: String;
  LClientItem: TMCPClientItem;
begin
  Result := nil;

  // Evitar duplicados
  LClientItem := FMCPClients.GetClientByName(AName);
  if Assigned(LClientItem) then
  begin
    Result := LClientItem;
    Exit;
  end;

  LVersion := PPMResolveVersion(ARegistryUrl, AName, AVersion);
  if LVersion = '' then
  begin
    DoLog(Format('ImportPPM: No se pudo resolver la versión de "%s".', [AName]));
    Exit;
  end;

  LClientItem := FMCPClients.Add;
  LClientItem.Name := AName;
  LClientItem.TransportType := tpStdIo;
  LClientItem.Params.Values['Command'] := '';        // El llamador debe asignar la ruta al exe
  LClientItem.Params.Values['Arguments'] := '--protocol stdio';
  LClientItem.Params.Values['RootDir'] := TPath.GetHomePath;
  LClientItem.Enabled := False;
  LClientItem.UpdateClientProperties;

  Result := LClientItem;
  DoLog(Format('ImportPPM: "%s" v%s registrado como stub. Asigne Params[''Command''] antes de habilitar.',
    [AName, LVersion]));
end;

function TAiFunctions.InstallMCPFromPPM(const AName, AVersion, AInstallDir,
  ARegistryUrl: String): TMCPClientItem;
var
  LVersion, LTempFile, LInstallDir, LExePath, LDownloadUrl: String;
  LClientItem: TMCPClientItem;
begin
  Result := nil;

  // Si ya está registrado, retornar el existente sin reinstalar
  LClientItem := FMCPClients.GetClientByName(AName);
  if Assigned(LClientItem) then
  begin
    Result := LClientItem;
    DoLog(Format('InstallPPM: "%s" ya está registrado.', [AName]));
    Exit;
  end;

  // Resolver versión
  LVersion := PPMResolveVersion(ARegistryUrl, AName, AVersion);
  if LVersion = '' then
  begin
    DoLog(Format('InstallPPM: No se pudo resolver la versión de "%s".', [AName]));
    Exit;
  end;

  // Determinar directorio de instalación
  if AInstallDir <> '' then
    LInstallDir := TPath.Combine(AInstallDir, AName)
  else
    LInstallDir := TPath.Combine(TPath.GetHomePath,
      '.ppm' + TPath.DirectorySeparatorChar +
      'mcp' + TPath.DirectorySeparatorChar + AName);

  // Descargar .paipkg a un archivo temporal
  LDownloadUrl := Format('%s/v1/packages/%s/%s/download', [ARegistryUrl, AName, LVersion]);
  LTempFile    := TPath.Combine(TPath.GetTempPath, AName + '-' + LVersion + '.paipkg');

  DoLog(Format('InstallPPM: Descargando "%s" v%s...', [AName, LVersion]));
  if not PPMHttpDownload(LDownloadUrl, LTempFile) then
  begin
    DoLog(Format('InstallPPM: Error al descargar "%s".', [AName]));
    Exit;
  end;

  try
    // Extraer el binario de la plataforma actual
    DoLog(Format('InstallPPM: Extrayendo en "%s"...', [LInstallDir]));
    LExePath := PPMExtractBinary(LTempFile, LInstallDir);
    if LExePath = '' then
    begin
      DoLog(Format('InstallPPM: No se encontró binario compatible en el paquete de "%s".', [AName]));
      Exit;
    end;
  finally
    if TFile.Exists(LTempFile) then
      TFile.Delete(LTempFile);
  end;

  // Registrar como cliente StdIo listo para usar
  LClientItem := FMCPClients.Add;
  LClientItem.Name            := AName;
  LClientItem.TransportType   := tpStdIo;
  LClientItem.Params.Values['Command']   := LExePath;
  LClientItem.Params.Values['Arguments'] := '--protocol stdio';
  LClientItem.Params.Values['RootDir']   := LInstallDir;
  LClientItem.Enabled := True;
  LClientItem.UpdateClientProperties;

  Result := LClientItem;
  DoLog(Format('InstallPPM: "%s" v%s instalado en "%s".', [AName, LVersion, LExePath]));

  // Persistir en el archivo de configuración para que AutoLoad lo cargue en la próxima sesión
  SaveMCPConfiguration;
end;

function TAiFunctions.GetMCPSchema(const AName, AVersion, ARegistryUrl: String): TJSONObject;
var
  LVersion, LUrl, LBody: String;
begin
  Result := nil;
  LVersion := PPMResolveVersion(ARegistryUrl, AName, AVersion);
  if LVersion = '' then
    Exit;
  LUrl  := Format('%s/v1/packages/%s/%s/schema', [ARegistryUrl, AName, LVersion]);
  LBody := PPMHttpGetStr(LUrl);
  if LBody <> '' then
    Result := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
end;

{ MCPClient }

{ TMCPClientItem }

constructor TMCPClientItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FOwned := True;
  FConnected := False;
  FParams := TStringList.Create;
  FEnvVars := TStringList.Create;

  // Por defecto, creamos un cliente StdIo
  FMCPClient := Nil; // TMCPClientStdIo.Create(nil); // Sin Owner para controlarlo nosotros
  FName := 'MCPClient';
  // FMCPClient.Name := 'NewMCPClient';

  // CORRECCI�N: Crear siempre el cliente por defecto (StdIo).
  // Esto asegura que si SetParams se llama antes que SetTransportType,
  // haya un objeto donde guardar los datos.
  FMCPClient := TMCPClientStdIo.Create(nil);
  FMCPClient.Name := FName; //'MCPClient' + IntToStr(ID); // Nombre temporal

  // Sincronizar params iniciales (defaults del StdIo hacia el Wrapper)
  FParams.Assign(FMCPClient.Params);

  // Propagar cambios in-place (ej. Item.Params.Values['k'] := 'v') al cliente real
  TStringList(FParams).OnChange   := OnFParamsChanged;
  TStringList(FEnvVars).OnChange  := OnFEnvVarsChanged;

end;

destructor TMCPClientItem.Destroy;
begin
  FParams.Free;
  FEnvVars.Free;
  if FOwned and Assigned(FMCPClient) then
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
  Result := FName;

  if Result.IsEmpty then
    Result := inherited GetDisplayName;

  {
    if Assigned(FMCPClient) then
    Result := FMCPClient.Name
    else
    Result := inherited GetDisplayName;
  }
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
  // No se necesita hacer nada aqu�. solo debe existir.
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

  // --- L�GICA DE VALIDACI�N EN TIEMPO DE DISE�O ---

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
        // Si ListTools devuelve nil, es un error de conexi�n o protocolo.
        Raise Exception.Create(Format('[ERR] Fallo de conexi�n para "%s".'#13#10#13#10'Revise la configuraci�n (Command, URL, etc.) y los logs del servidor.', [Self.Name]));
      end
      Else
      Begin
        ClientTools.Free;
        FConnected := Value;
        Changed(False);
      End;

    except
      on E: Exception do
      begin
        // Capturamos cualquier otra excepci�n
        Raise Exception.Create(Format('[ERR] Ocurri� una excepci�n al validar "%s".'#13#10#13#10'%s: %s', [Self.Name, E.ClassName, E.Message]));
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
  // OnFEnvVarsChanged se dispara automáticamente por el OnChange de FEnvVars
  Changed(False);
end;

procedure TMCPClientItem.SetName(const Value: string);
begin
  if Pos(MCP_TOOL_SEP, Value) > 0 then
    raise EArgumentException.CreateFmt(
      'El nombre del servidor MCP "%s" no puede contener el separador reservado "%s".',
      [Value, MCP_TOOL_SEP]);

  if FName <> Value then
  begin
    FName := Value;

    // Sincronizamos con el m�todo est�ndar de colecciones para que se vea en el TreeView
    inherited SetDisplayName(Value);

    // Si el cliente interno existe, le pasamos el nombre
    if Assigned(FMCPClient) then
      FMCPClient.Name := Value;

    Changed(False);
  end;

  {
    SetDisplayName(Value);
    if Assigned(FMCPClient) then
    begin
    if FMCPClient.Name <> Value then
    begin
    FMCPClient.Name := Value;
    Changed(False);
    end;
    end;
  }
end;

procedure TMCPClientItem.OnFParamsChanged(Sender: TObject);
begin
  if Assigned(FMCPClient) then
    FMCPClient.Params.Assign(FParams);
end;

procedure TMCPClientItem.OnFEnvVarsChanged(Sender: TObject);
begin
  if Assigned(FMCPClient) then
    FMCPClient.EnvVars.Assign(FEnvVars);
end;

procedure TMCPClientItem.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
  // OnFParamsChanged se dispara automáticamente por el OnChange de FParams
  Changed(False);
end;

procedure TMCPClientItem.SetTransportType(const Value: TToolTransportType);
begin
  // Verificamos si realmente cambi� o si el objeto no existe
  if not Assigned(FMCPClient) or (FMCPClient.TransportType <> Value) then
  begin
    // Liberamos el cliente anterior
    FreeAndNil(FMCPClient);

    // Creamos el nuevo seg�n el tipo seleccionado
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
    // CORRECCI�N: Asignar el tipo expl�citamente AL NUEVO OBJETO
    // antes de sincronizar el resto de propiedades.
    // =========================================================================
    FMCPClient.TransportType := Value;

    // Ahora s�, transferimos nombre, params, envVars, etc.
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
  FMCPClient.Name := FName;  //Self.GetDisplayName;

  // 3. Transferir el estado de 'Enabled'
  FMCPClient.Enabled := Self.FEnabled;

  // 4. Transferir los par�metros (copiar el contenido de nuestra lista local)
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
  // Llama a la funci�n de trabajo recursiva para limpiar el �rbol completo.
  CleanJsonTree(ASchema);
end;

// El verdadero motor: una funci�n recursiva que recorre CUALQUIER �rbol JSON.
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
    // 1. Acci�n: Limpiar las claves no deseadas en el nivel actual.
    LObject.RemovePair('additionalProperties');
    LObject.RemovePair('$schema');

    // 2. Traves�a: Llamarse a s� misma para cada valor hijo del objeto.
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
    // Traves�a: Llamarse a s� misma para cada elemento del array.
    for LItem in LArray do
    begin
      CleanJsonTree(LItem);
    end;
  end;
  // Caso 3: Es un valor simple (string, n�mero, etc.). No se hace nada.
end;

{ TJsonToolUtils }

// ==============================================================================
// NUEVA FUNCI�N DE DETECCI�N
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
    // Diferenciaci�n clave:
    // tfOpenAI (Legacy/Chat): Tiene una clave "function" que contiene los detalles.
    // tfOpenAIResponses (New): Tiene "name" directamente en la ra�z y NO tiene clave "function".

    if AJsonTool.FindValue('function') <> nil then
      Exit(tfOpenAI)
    else if AJsonTool.FindValue('name') <> nil then
      Exit(tfOpenAIResponses);

    // Por defecto si es ambiguo, asumimos el nuevo est�ndar si tiene nombre
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

  // Verificamos si es un objeto (tiene propiedades o es type object expl�cito)
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
// M�TODOS DE NORMALIZACI�N (sin cambios)
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
    LInputSchema := TJSonObject.Create; // Crear schema vac�o si no existe

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
// FUNCI�N DE NORMALIZACI�N DE OPENAI CORREGIDA
// ==============================================================================
class procedure TJsonToolUtils.NormalizeFromOpenAI(AJsonTool: TJSonObject; AToolList: TList<TNormalizedTool>);
var
  LName, LDescription: string;
  LInputSchema: TJSonObject;
  LSchemaValue: TJSONValue;
  LFunctionObject, LDataSource: TJSonObject; // LDataSource apuntar� al objeto correcto
begin
  // Primero, determinamos de d�nde leer los datos.
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
    Exit; // Si no hay nombre, no es una herramienta v�lida.

  LDataSource.TryGetValue<string>('description', LDescription);

  if LDataSource.TryGetValue('parameters', LSchemaValue) and (LSchemaValue is TJSonObject) then
  Begin
    LInputSchema := TJSonObject(LSchemaValue.Clone);
    CleanInputSchema(LInputSchema);
  End
  else
  begin
    // Schema minimo valido: Claude y otros providers exigen type=object incluso sin parametros
    LInputSchema := TJSonObject.Create;
    LInputSchema.AddPair('type', 'object');
    LInputSchema.AddPair('properties', TJSonObject.Create);
  end;

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

    // Guardar el recuento actual para saber qu� herramientas se a�adieron
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

    // Aplicar el prefijo de fuente a las herramientas reci�n a�adidas
    if not ASourceName.IsEmpty then
    begin
      for var J := LInitialCount to ANormalizedList.Count - 1 do
        ANormalizedList[J].FName := Format('%s%s%s', [ASourceName, MCP_TOOL_SEP, ANormalizedList[J].Name]);
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
// M�TODOS DE FORMATEO (sin cambios)
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
// FUNCI�N DE FORMATEO PARA OPENAI (VERSI�N CORREGIDA Y DEFINITIVA)
// ==============================================================================
class function TJsonToolUtils.FormatAsOpenAI(ANormalizedTool: TNormalizedTool): TJSonObject;
var
  LFunctionObject: TJSonObject;
begin
  // 1. Crear el objeto interno "function" que contendr� los detalles.
  LFunctionObject := TJSonObject.Create;
  LFunctionObject.AddPair('name', ANormalizedTool.Name);
  LFunctionObject.AddPair('description', ANormalizedTool.Description);

  // 2. Solo a�adir 'parameters' si el schema tiene contenido.
  if Assigned(ANormalizedTool.InputSchema) and (ANormalizedTool.InputSchema.Count > 0) then
    LFunctionObject.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));

  // 3. Crear el objeto externo principal.
  Result := TJSonObject.Create;
  Result.AddPair('type', 'function');
  Result.AddPair('function', LFunctionObject); // <-- A�adir el objeto interno
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

  // Solo a�adir par�metros si existen
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
    // OpenAI Strict requiere un esquema de par�metros incluso si es vac�o
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
    // --- L�GICA ESPECIAL PARA GEMINI: Agrupar todo en un solo bloque ---
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
    // --- L�GICA EST�NDAR PARA OTROS FORMATOS: Una herramienta por objeto ---
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
  // Crea solo el objeto de la declaraci�n de la funci�n, no la envoltura.
  Result := TJSonObject.Create;
  Result.AddPair('name', ANormalizedTool.Name);
  Result.AddPair('description', ANormalizedTool.Description);

  // Solo a�adir 'parameters' si el schema tiene contenido.
  if Assigned(ANormalizedTool.InputSchema) and (ANormalizedTool.InputSchema.Count > 0) then
    Result.AddPair('parameters', TJSonObject(ANormalizedTool.InputSchema.Clone));
end;

// ==============================================================================
// FUNCIONES P�BLICAS
// ==============================================================================

// SOBRECARGA con detecci�n autom�tica
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

  // 2. Si la detecci�n falla, no podemos continuar.
  if LDetectedFormat = tfUnknown then
  begin
    // Podr�amos lanzar una excepci�n o simplemente devolver el target sin cambios.
    // Devolver el target es m�s seguro.
    // raise EJSON.CreateFmt('Could not detect tool format for source "%s".', [ASourceName]);
    Result := ATargetJson;
    Exit;
  end;

  // 3. Llamar a la funci�n principal con el formato detectado.
  Result := MergeToolLists(ASourceName, ASourceJson, LDetectedFormat, ATargetJson, AOutputFormat);
end;


// VERSI�N EXPL�CITA (l�gica principal corregida y final)

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
  LNormalizedTools := TObjectList<TNormalizedTool>.Create(True);
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

        // Soportar ambos formatos de OpenAI en la entrada (la funci�n NormalizeFromOpenAI detecta la estructura interna)
        tfOpenAI, tfOpenAIResponses:
          NormalizeFromOpenAI(LSourceTool, LNormalizedTools);

        tfGemini:
          NormalizeFromGemini(LSourceTool, LNormalizedTools);
      else
        Continue; // Ignorar formatos no soportados
      end;
    end;

    // Si no se normaliz� ninguna herramienta, no hay nada m�s que hacer.
    if LNormalizedTools.Count = 0 then
    begin
      Result := ATargetJson;
      Exit;
    end;

    // 3. Pre-procesar los nombres de todas las herramientas normalizadas
    for LNormTool in LNormalizedTools do
    begin
      if not ASourceName.IsEmpty then
        LNormTool.FName := Format('%s%s%s', [ASourceName, MCP_TOOL_SEP, LNormTool.Name]);
    end;

    // 4. Formatear y a�adir al destino
    if AOutputFormat = tfGemini then
    begin
      // --- L�GICA ESPECIAL PARA GEMINI: Agrupar todo en un solo bloque ---
      var
      LDeclarationsArray := TJSonArray.Create;
      for LNormTool in LNormalizedTools do
      begin
        // Usar la funci�n de ayuda que formatea una declaraci�n individual
        LDeclarationsArray.Add(FormatAsGeminiFunctionDeclaration(LNormTool));
      end;

      // Crear el �nico objeto contenedor 'tool'
      var
      LGeminiToolWrapper := TJSonObject.Create;
      LGeminiToolWrapper.AddPair('functionDeclarations', LDeclarationsArray);

      // A�adir este �nico objeto al array final de herramientas
      LFinalToolsArray.Add(LGeminiToolWrapper);
    end
    else
    begin
      // --- L�GICA EST�NDAR PARA OTROS FORMATOS: Una herramienta por objeto ---
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
