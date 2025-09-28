unit uMakerAi.MCPClient.Core;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.IOUtils, System.Net.URLClient, System.NetEncoding,
  System.Net.HttpClient, System.Net.Mime, System.Net.HttpClientComponent,
  System.Threading, System.Diagnostics, System.Types,

{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF POSIX}
{$ENDIF}
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Utils.System, uMakerAi.Core; // Dependencia de tu unidad de utilidades de consola

// --- Tipos de Eventos ---
{ type
  //Las siguientes definiciones Se movieron a uMakerAi.Core


  // Evento para notificar mensajes de log
  TMCPLogEvent = procedure(Sender: TObject; const Msg: string) of object;

  // Evento para notificar cambios de estado del servidor (iniciado, detenido, etc.)
  TMCPStatusEvent = procedure(Sender: TObject; const StatusMsg: string) of object;

  TToolFormat = (tfUnknown, tfOpenAI, tfClaude, tfGemini, tfMCP);
  TToolTransportType = (tpStdIo, tpHttp, tpSSE, tpMakerAi);

}

// --- Clase Base Abstracta ---
type

  EMCPClientException = class(Exception);

  TMCPClientCustom = class(TComponent)
  private
    FName: string;
    FOnLog: TMCPLogEvent;
    FOnStatusUpdate: TMCPStatusEvent;
    FTransportType: TToolTransportType;
    FInitialized: Boolean;
    FEnabled: Boolean;
    FTools: TStrings;
    FAvailable: Boolean;
    // FURL: string;
    // FTimeout: Integer;
    FParams: TStrings;
    FEnvVars: TStrings;
    FDisabledFunctions: TStrings;
    FURL: String;
    FServerProcess: TInteractiveProcessInfo;
    FOwnsServerProcess: Boolean;
    procedure SetTransportType(const Value: TToolTransportType);
    procedure SetAvailable(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetInitialized(const Value: Boolean);
    Procedure SetTools(Const Value: TStrings);
    procedure SetParams(const Value: TStrings);
    procedure SetDisabledFunctions(const Value: TStrings);
    function GetParams: TStrings;
    procedure SetCommand(const Value: string);
    function GetEnvVars: TStrings;
    procedure SetEnvVars(const Value: TStrings);
    procedure SetURL(const Value: String);
  protected
    // Propiedades de configuración
    // FCommand: string;
    // FArguments: string;
    // FRootDirectory: string;

    // Métodos para disparar eventos
    FLastError: String;
    FBusy: Boolean;
    procedure DoLog(const Msg: string); virtual;
    procedure DoStatusUpdate(const StatusMsg: string); virtual;
    function IsBinaryContentType(const ContentType: string): Boolean;

    function ProcessAndExtractMedia(const AJsonResult: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
    // procedure ParamsChanged(Sender: TObject);

    function InternalStartLocalServerProcess: Boolean; Virtual;
    procedure InternalStopLocalServerProcess; Virtual;

  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; Override;
    Function GetParamByName(ParamName: String): String;
    Function GetDefaultParams: TStringList; Virtual;

    // La función initialization intentará conectar con el servidor y obtener la lista de herramientas (ListTools) y almacenará
    // la información en la propiedad Tools : TStrings que es de solo lectura.  esto se hace para evitar hacer el llamado
    // a la función cada vez que se necesite la lista de herramientas y se ahorra tiempo y recursos de conexión.
    // También marca la propiedad initialized en true para indicar que ya se probó.
    // si el servidor en el proceso de inicialización falla asigna Enabled = False, de lo

    Function Initialize: Boolean; Virtual;

    // Métodos públicos principales (ciclo de vida completo)
    // Son virtuales y abstractos porque su implementación depende del protocolo
    function ListTools: TJSONObject; virtual; abstract;
    function CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; virtual; abstract;
    function CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; virtual; abstract;

    // Propiedades públicas
    property Name: string read FName write FName;
    // property Command: string read FCommand write SetCommand;
    // property Arguments: string read FArguments write FArguments;
    // property RootDirectory: string read FRootDirectory write FRootDirectory;
    Property TransportType: TToolTransportType read FTransportType write SetTransportType;
    // property URL: string read FURL write FURL;
    // property Timeout: Integer read FTimeout write FTimeout;

    Property Tools: TStrings read FTools; // Propiedad de solo lectura que contiene ListTools después de inicializado
    Property Initialized: Boolean read FInitialized write SetInitialized; // Indica que ya fue inicializado intentando obtener el ListTools
    Property Enabled: Boolean read FEnabled write SetEnabled; // Propiedad que indica que se utilizará para construir el tools final
    // Si está disponible después de inicializar, si falló la inicialización queda en false.
    Property Available: Boolean read FAvailable write SetAvailable;

    Property Params: TStrings read GetParams write SetParams; // Parámetros adicionales en formato ParamName=ParamValue
    Property EnvVars: TStrings read GetEnvVars write SetEnvVars;
    Property URL: String read FURL write SetURL;

    // Property DisabledFunctions: TStrings read FDisabledFunctions write SetDisabledFunctions;
    // Lista de funciones que no se ejecutarán, por defecto todas son válidas   formato (ModuleName,FunctionName)

    // Eventos públicos
    property OnLog: TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate: TMCPStatusEvent read FOnStatusUpdate write FOnStatusUpdate;
  end;

  // --- Implementación del Protocolo STDI/O ---
type
  TMCPClientStdIo = class(TMCPClientCustom)
  private
    FInteractiveProcess: TInteractiveProcessInfo;
    FReadThread: TThread;
    FIncomingMessages: TThreadedQueue<TJSONObject>;
    FIsRunning: Boolean;
    FRequestIDCounter: Integer;

    // Métodos internos que realizan las tareas individuales
    procedure InternalStartServerProcess;
    procedure InternalStopServerProcess;
    function InternalInitialize: TJSONObject;
    procedure InternalSendInitializedNotification;
    function InternalListTools: TJSONObject;
    function InternalCallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;

    // Helpers de comunicación
    procedure ReadProcessOutput;
    procedure InternalSendRawMessage(const AJsonString: string);
    function InternalReceiveJSONResponse(AExpectedID: Integer; ATimeoutMs: Cardinal = 10000): TJSONObject;
    function IsServerRunning: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Implementación de los métodos públicos (orquestan el ciclo de vida)
    function ListTools: TJSONObject; override;
    function CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;
    function CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;
  end;

  TMCPClientHttp = class(TMCPClientCustom)
  private
    FHttpClient: TNetHTTPClient;
    FRequestIDCounter: Integer;
    FServerCapabilities: TJSONObject;

    // Métodos para el handshake MCP
    procedure InternalPerformMCPInitialize;
    procedure InternalSendInitializedNotification;

    // Método principal para enviar solicitudes JSON-RPC por HTTP
    function InternalSendRequest(const AMethod: string; AParams: TJSONObject): TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Initialize: Boolean; override; // Sobrescribimos el Initialize de la base

    function ListTools: TJSONObject; override;
    function CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;
    function CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;
  end;

  TMCPClientMakerAi = class(TMCPClientCustom)
  private
    // Campo privado para gestionar las peticiones HTTP.
    // Será creado en el constructor y liberado automáticamente por el componente.
    FHttpClient: TNetHTTPClient;

    // Método de ayuda privado. Centraliza toda la lógica de comunicación con la API REST:
    // - Construye la URL.
    // - Prepara la autenticación HTTP Basic.
    // - Realiza la petición GET o POST.
    // - Procesa la respuesta, incluyendo el "unwrap" del formato de DataSnap.
    function InternalSendRequest(const AMethodName, AHttpVerb: string; ABodyStream: TStream): TJSONObject;

  public
    // Constructor y destructor estándar del componente.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Implementación de los métodos abstractos heredados de TMCPClientCustom.
    // Estos son los métodos públicos que el resto de la aplicación usará,
    // garantizando que la interfaz sea idéntica a los otros tipos de clientes.

    // Sobrescribe ListTools para llamar al endpoint /listtools de la API.
    function ListTools: TJSONObject; override;

    // Sobrescribe CallTool para llamar a los endpoints /calltool/{toolname} de la API.
    // Acepta argumentos como un TJSONObject.
    function CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;

    // Sobrecarga de CallTool que acepta argumentos como un TStrings para mayor comodidad.
    function CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject; overload; override;
  end;

  TMCPClientSSE = Class(TMCPClientHttp);

implementation

{ TMCPClientCustom }

constructor TMCPClientCustom.Create(AOwner: TComponent);
Var
  List: TStringList;
begin
  inherited Create(AOwner);
  FName := 'MCPClient'; // Valor por defecto
  FTools := TStringList.Create;
  FParams := TStringList.Create;
  FEnvVars := TStringList.Create;
  List := GetDefaultParams;
  FServerProcess := nil;
  Try
    FParams.Assign(List);
  Finally
    List.Free;
  End;
  // TStringList(FParams).OnChange := ParamsChanged;
  FDisabledFunctions := TStringList.Create;
end;

destructor TMCPClientCustom.Destroy;
begin

  if FOwnsServerProcess then
    InternalStopLocalServerProcess;

  FTools.Free;
  FParams.Free;
  FDisabledFunctions.Free;
  FEnvVars.Free;
  inherited;
end;

procedure TMCPClientCustom.DoLog(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Msg);
end;

procedure TMCPClientCustom.DoStatusUpdate(const StatusMsg: string);
begin
  if Assigned(FOnStatusUpdate) then
    FOnStatusUpdate(Self, StatusMsg);
end;

function TMCPClientCustom.GetDefaultParams: TStringList;
begin
  Result := TStringList.Create;
  Try
    Result.Add('Command=npx');
    Result.Add('Arguments=@');
    Result.Add('RootDir=' + TPath.GetHomePath); // Un directorio de inicio más sensato
    Result.Add('PATH=C:\');
    Result.Add('ApiHeaderName=Authorization');
    Result.Add('ApiBearerToken=@MCPBearerToken');
    Result.Add('URL=http://localhost:3001/sse');
    Result.Add('Login=login');
    Result.Add('Password=password');
    Result.Add('OAuthClientId=ClientId');
    Result.Add('OAuthURL=http://localhost:6274/oauth/callback');
    Result.Add('OAuthScope=Scope');
    Result.Add('Timeout=15000');
    Result.Add('InitializeEndpointSuffix='); // Vacío
    Result.Add('NotificationEndpointSuffix='); // Vacío
    Result.Add('RpcEndpointSuffix='); // Vacío
  Finally
  End;
end;

function TMCPClientCustom.GetEnvVars: TStrings;
begin
  Result := FEnvVars;
end;

function TMCPClientCustom.GetParamByName(ParamName: String): String;
begin
  Result := FParams.Values[ParamName];
end;

function TMCPClientCustom.GetParams: TStrings;
begin
  Result := FParams;

  { If FCommand <> '' then
    Result.Values['Command'] := FCommand;

    If FArguments <> '' then
    Result.Values['Arguments'] := FArguments;

    If FRootDirectory <> '' then
    Result.Values['RootDir'] := FRootDirectory;

    If FURL <> '' then
    Result.Values['URL'] := FURL;

    If FTimeout > 0 then
    Result.Values['FTimeout'] := FTimeout.ToString;
  }
end;

function TMCPClientCustom.Initialize: Boolean;
Var
  jTools: TJSONObject;
  jValue: TJSonValue;
begin
  Try
    jTools := ListTools;

    If Assigned(jTools) and (jTools.TryGetValue<TJSonValue>('tools', jValue)) and (jValue is TJSonArray) then
    Begin
      FTools.Text := jTools.Format;
      Initialized := True;
      Enabled := True;
      Available := True;
    End;
  Except
    FTools.Clear;
    Initialized := True;
    Enabled := False;
    Available := False;
  End;
end;

function TMCPClientCustom.InternalStartLocalServerProcess: Boolean;
begin
  Result := False;
  FOwnsServerProcess := False; // Inicialmente false
  // ...
  if Assigned(FServerProcess) and FServerProcess.IsRunning then
  begin
    DoLog('Local server process is already running. This client will just connect.');
    Result := True;
    // No establecer FOwnsServerProcess a true aquí si ya está corriendo.
    // Esto significa que otro cliente (o proceso) ya lo inició.
    Exit;
  end;

  // ... (código para lanzar el proceso si no está corriendo)

  if Assigned(FServerProcess) and FServerProcess.IsRunning then
  begin
    Result := True;
    FOwnsServerProcess := True; // ¡ESTE cliente lo inició!
    DoLog('Local MCP Server started successfully');
    DoStatusUpdate('Local Server running.');
    Sleep(2000);
    DoLog('Brief pause after local server start to allow initialization.');
  end;
  // ... (manejo de errores si el lanzamiento falla)
end;

procedure TMCPClientCustom.InternalStopLocalServerProcess;
begin
  // Solo detener si esta instancia es la propietaria del proceso
  if FOwnsServerProcess and Assigned(FServerProcess) then
  begin
    DoLog('Stopping local MCP server process because this client owns it...');
    // Mensaje más claro
    DoStatusUpdate('Stopping local server...');
    try
      TUtilsSystem.StopInteractiveProcess(FServerProcess);
      FServerProcess := nil; // Es importante ponerlo a nil después de detenerlo
      FOwnsServerProcess := False; // Ya no lo poseemos
      DoLog('Local MCP Server stopped.');
      DoStatusUpdate('Local Server stopped.');
    except
      on E: Exception do
      begin
        FLastError := 'EXCEPTION during local server stop: ' + E.Message;
        DoLog(FLastError);
        DoStatusUpdate('Error stopping local server.');
      end;
    end;
  end
  else if Assigned(FServerProcess) then // Si no lo poseemos pero FServerProcess no es nil (ej. si falla el lanzamiento)
  begin
    // Si FServerProcess no es nil pero FOwnsServerProcess es false,
    // significa que el proceso está corriendo pero esta instancia no lo inició.
    // No lo detengas, solo limpia la referencia local si es necesario.
    FreeAndNil(FServerProcess); // Liberar solo el objeto TInteractiveProcessInfo, no el proceso real
  end;
end;

function TMCPClientCustom.IsBinaryContentType(const ContentType: string): Boolean;
begin
  Result := SameText(ContentType, 'image') or SameText(ContentType, 'audio') or SameText(ContentType, 'video') or SameText(ContentType, 'document') or // Para PDFs, Word, Excel, etc.
    SameText(ContentType, 'archive') or // Para ZIP, RAR, etc.
    SameText(ContentType, 'executable') or // Para EXE, DLL, etc.
    SameText(ContentType, 'font') or // Para TTF, OTF, etc.
    SameText(ContentType, 'model') or // Para archivos 3D
    SameText(ContentType, 'database') or // Para SQLite, etc.
    SameText(ContentType, 'ebook') or // Para EPUB, MOBI, etc.
    SameText(ContentType, 'certificate') or // Para certificados
    SameText(ContentType, 'binary'); // Tipo genérico para binarios
end;

{ procedure TMCPClientCustom.ParamsChanged(Sender: TObject);
  begin
  FCommand := FParams.Values['Command'];
  FArguments := FParams.Values['Arguments'];
  FRootDirectory := FParams.Values['RootDir'];
  FURL := FParams.Values['URL'];
  FTimeout := StrToIntDef(FParams.Values['FTimeout'], 15000);
  end;
}

function TMCPClientCustom.ProcessAndExtractMedia(const AJsonResult: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  LClonedResult: TJSONObject;
  LContentArray: TJSonArray;
  LContentItem: TJSONObject;
  LItemType, LBase64Data, LMimeType, LFileName: string;
  i: Integer;
begin
  Result := nil;
  // 1. Validar entradas
  if not Assigned(AJsonResult) then
    Exit;

  if not Assigned(AExtractedMedia) then // Retorna el objeto si no existe
    AExtractedMedia := TObjectList<TAiMediaFile>.Create;

  // 2. Clonar el objeto JSON de entrada para no modificar el original.
  LClonedResult := TJSONObject(AJsonResult.Clone);
  try
    // 3. Buscar el array 'content' en el JSON clonado.
    if not LClonedResult.TryGetValue<TJSonArray>('content', LContentArray) then
    begin
      Result := LClonedResult;
      Exit;
    end;

    // 4. Iterar sobre cada elemento del array 'content'.
    for i := 0 to LContentArray.Count - 1 do
    begin
      if not(LContentArray.Items[i] is TJSONObject) then
        Continue;

      LContentItem := LContentArray.Items[i] as TJSONObject;

      // Verificar si el item es de un tipo que contiene datos binarios
      if LContentItem.TryGetValue<string>('type', LItemType) and IsBinaryContentType(LItemType) then
      begin
        // Intentar obtener los datos base64 y el tipo mime.
        if LContentItem.TryGetValue<string>('data', LBase64Data) and LContentItem.TryGetValue<string>('mimeType', LMimeType) and not LBase64Data.IsEmpty then
        begin
          // -- Se encontró un binario --
          var
          MediaFile := TAiMediaFile.Create;

          // Generar nombre de archivo con extensión apropiada
          LFileName := Format('mcp-media-%d.%s', [AExtractedMedia.Count + 1, GetFileExtensionFromMimeType(LMimeType)]);

          MediaFile.LoadFromBase64(LFileName, LBase64Data);
          AExtractedMedia.Add(MediaFile);

          // Modificar el JSON clonado
          LContentItem.RemovePair('data');
          LContentItem.AddPair('dataExtracted', TJSONBool.Create(True));
          LContentItem.AddPair('mediaIdentifier', MediaFile.filename);
          LContentItem.AddPair('originalSize', TJSONNumber.Create(Length(LBase64Data))); // Opcional: tamaño original
        end;
      end;
    end;

    Result := LClonedResult;
  except
    LClonedResult.Free;
    Result := nil;
  end;
end;

procedure TMCPClientCustom.SetAvailable(const Value: Boolean);
begin
  FAvailable := Value;
end;

procedure TMCPClientCustom.SetCommand(const Value: string);
begin
  // FCommand := Value;
end;

procedure TMCPClientCustom.SetDisabledFunctions(const Value: TStrings);
begin
  FDisabledFunctions.Assign(Value);
end;

procedure TMCPClientCustom.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TMCPClientCustom.SetEnvVars(const Value: TStrings);
begin
  FEnvVars.Assign(Value);
end;

procedure TMCPClientCustom.SetInitialized(const Value: Boolean);
begin
  FInitialized := Value;
end;

procedure TMCPClientCustom.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);

  { FCommand := FParams.Values['Command'];
    FArguments := FParams.Values['Arguments'];
    FRootDirectory := FParams.Values['RootDir'];
    FURL := FParams.Values['URL'];
    FTimeout := StrToIntDef(FParams.Values['FTimeout'], 15000);
  }
end;

procedure TMCPClientCustom.SetTransportType(const Value: TToolTransportType);
begin
  FTransportType := Value;
end;

procedure TMCPClientCustom.SetURL(const Value: String);
begin
  FURL := Value;
end;

procedure TMCPClientCustom.SetTools(const Value: TStrings);
begin
  FTools.Assign(Value);
end;

{ TMCPClientStdIo }

constructor TMCPClientStdIo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsRunning := False;
  FRequestIDCounter := 0;
  FIncomingMessages := TThreadedQueue<TJSONObject>.Create(1000, INFINITE, 100);
  FInteractiveProcess := nil;
  DoLog('TMCPClientStdIo instance created.');
end;

destructor TMCPClientStdIo.Destroy;
var
  LJson: TJSONObject;
begin
  InternalStopServerProcess; // Asegurarse de que todo esté detenido
  if Assigned(FIncomingMessages) then
  begin
    FIncomingMessages.DoShutDown;
    while FIncomingMessages.PopItem(LJson) = wrSignaled do
    Begin
      If LJson = nil then
        Break;
      LJson.Free; // Vaciar la cola para liberar memoria
    End;
    FreeAndNil(FIncomingMessages);
  end;
  inherited;
end;

function TMCPClientStdIo.IsServerRunning: Boolean;
begin
  Result := FIsRunning and Assigned(FInteractiveProcess) and FInteractiveProcess.IsRunning;
end;

// --- Métodos de Ciclo de Vida Completo ---

function TMCPClientStdIo.ListTools: TJSONObject;
var
  InitResponse: TJSONObject;
begin
  Result := nil;
  DoLog('Executing full cycle for ListTools...');
  InternalStartServerProcess;
  try
    if not IsServerRunning then
    begin
      DoLog('Failed to start server. Aborting ListTools.');
      Exit;
    end;

    // Handshake
    InitResponse := InternalInitialize;
    if not Assigned(InitResponse) then
    begin
      DoLog('Initialization failed. Aborting ListTools.');
      Exit;
    end;
    InitResponse.Free; // No necesitamos la respuesta, solo saber que funcionó
    InternalSendInitializedNotification;

    // CAMBIO: Añadir una pequeña pausa para asegurar que el servidor procese la notificación.
    Sleep(200); // 100ms es un valor seguro.

    // Realizar la llamada real
    Result := InternalListTools;

  finally
    InternalStopServerProcess;
    DoLog('Full cycle for ListTools finished.');
  end;
end;

function TMCPClientStdIo.CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  InitResponse: TJSONObject;
begin
  Result := nil;
  DoLog(Format('Executing full cycle for CallTool: %s', [AToolName]));
  InternalStartServerProcess;
  try
    if not IsServerRunning then
    begin
      DoLog('Failed to start server. Aborting CallTool.');
      FreeAndNil(AArguments); // Liberar los argumentos si no se van a usar
      Exit;
    end;

    // Handshake
    InitResponse := InternalInitialize;
    if not Assigned(InitResponse) then
    begin
      DoLog('Initialization failed. Aborting CallTool.');
      FreeAndNil(AArguments); // Liberar los argumentos si no se van a usar
      Exit;
    end;
    InitResponse.Free;
    InternalSendInitializedNotification;

    Sleep(200);

    // Realizar la llamada real
    Result := InternalCallTool(AToolName, AArguments, AExtractedMedia);

  finally
    InternalStopServerProcess;
    DoLog(Format('Full cycle for CallTool: %s finished.', [AToolName]));
  end;
end;

function TMCPClientStdIo.CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  ArgsObject: TJSONObject;
  i: Integer;
begin
  // Convertir TStrings a TJSONObject
  ArgsObject := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for i := 0 to AArguments.Count - 1 do
    begin
      ArgsObject.AddPair(AArguments.Names[i], AArguments.ValueFromIndex[i]);
    end;
  end;
  // Llamar a la versión principal, que ahora es dueña de ArgsObject
  Result := CallTool(AToolName, ArgsObject, AExtractedMedia);
end;

// En la clase TMCPClientStdIo
procedure TMCPClientStdIo.InternalStartServerProcess;
var
  CommandToExecute: string;
  Command, Arguments, RootDir, Path, CurrentPath, NewPath: String;
  AEnvironment: TStringList;
begin
  if IsServerRunning then
  begin
    DoLog('Server is already running.');
    Exit;
  end;

  DoLog('Starting MCP server process...');
  DoStatusUpdate('Starting server...');
  try
    // Prepara los parámetros principales
    Command := GetParamByName('Command');
    Arguments := GetParamByName('Arguments');
    RootDir := GetParamByName('RootDir');
    Path := Trim(GetParamByName('Path'));

    if Trim(Command) = '' then
    begin
      DoLog('ERROR: "Command" parameter cannot be null.');
      Exit;
    end;

    // Construir el comando correctamente (esto ya está bien)

{$IFDEF MSWINDOWS}
    CommandToExecute := Format('cmd.exe /c "%s %s"', [Command, Arguments]);
{$ENDIF}
{$IFDEF POSIX}
    CommandToExecute := Format('%s %s', [Command, Arguments]);
{$ENDIF}
    DoLog('Executing: ' + CommandToExecute);

    // --- CAMBIO CLAVE: Preparar el entorno COMPLETO ---
    AEnvironment := TUtilsSystem.GetSystemEnvironment;
    try
      // 1. Cargar TODAS las variables de entorno del sistema y del usuario

      // 2. (Opcional) Sobrescribir o añadir variables desde las propiedades del componente si es necesario
      // Por ejemplo, si el usuario define algo en FEnvVars, debería tener prioridad.
      if FEnvVars.Count > 0 then
      begin
        for var s in FEnvVars do
        begin
          var
          Pair := s.Split(['=']);
          if Length(Pair) = 2 then
          begin
            AEnvironment.Values[Pair[0]] := Pair[1];
            DoLog(Format('Overriding/Adding Env Var: %s=%s', [Pair[0], Pair[1]]));
          end;
        end;
      end;

      If Path <> '' then
      Begin
        CurrentPath := System.SysUtils.GetEnvironmentVariable('PATH');
        DoLog(Format('Adding to Path: %s', [Path]));
        NewPath := Path + PathDelim + CurrentPath;
        AEnvironment.Values['PATH'] := NewPath;
      End;

      // Lanzar el proceso con el entorno COMPLETO
      FInteractiveProcess := TUtilsSystem.StartInteractiveProcess(CommandToExecute, RootDir, AEnvironment);

    finally
      AEnvironment.Free;
    end;

    if not Assigned(FInteractiveProcess) or not FInteractiveProcess.IsRunning then
    begin
      DoStatusUpdate('Server failed to start.');
      DoLog('ERROR: MCP server failed to start.');
      if Assigned(FInteractiveProcess) then
      begin
        DoLog('Exit Code (if available): ' + IntToStr(FInteractiveProcess.ExitCode));
        FInteractiveProcess.Free;
        FInteractiveProcess := nil;
      end;
      Exit;
    end;

    FIsRunning := True;

    FReadThread := TThread.CreateAnonymousThread(
      procedure
      begin
        Self.ReadProcessOutput;
      end);
    FReadThread.FreeOnTerminate := True;
    FReadThread.Start;

    DoLog('MCP Server started successfully.');
    DoStatusUpdate('Server running.');
    Sleep(300); // Dar tiempo para que el servidor arranque completamente
  except
    on E: Exception do
    begin
      DoStatusUpdate('Error starting server.');
      DoLog('EXCEPTION during server start: ' + E.Message);
      FIsRunning := False;
    end;
  end;
end;

procedure TMCPClientStdIo.InternalStopServerProcess;
begin
  if not IsServerRunning then
    Exit;

  DoLog('Stopping MCP server process...');
  DoStatusUpdate('Stopping server...');
  try
    FIsRunning := False;
    if Assigned(FReadThread) then
    begin
      // No necesitamos un WaitFor explícito si el hilo es FreeOnTerminate y
      // nuestro bucle de lectura depende de FIsRunning.
      // Solo aseguramos que el proceso se termine, lo que causará que el
      // hilo de lectura también termine su bucle.
    end;

    if Assigned(FInteractiveProcess) then
    begin
      TUtilsSystem.StopInteractiveProcess(FInteractiveProcess);
      FInteractiveProcess := nil;
    end;

    DoLog('MCP Server stopped.');
    DoStatusUpdate('Server stopped.');
  except
    on E: Exception do
    begin
      DoStatusUpdate('Error stopping server.');
      DoLog('EXCEPTION during server stop: ' + E.Message);
    end;
  end;
  FIsRunning := False; // Asegurar el estado
  // No necesitamos liberar FReadThread aquí, es FreeOnTerminate
end;

function TMCPClientStdIo.InternalInitialize: TJSONObject;
var
  RequestObj, Response: TJSONObject;
  ResultPair: TJSonValue;
begin
  Result := nil;
  if not IsServerRunning then
    Exit;

  Inc(FRequestIDCounter);
  RequestObj := TJSONObject.Create;
  try
    RequestObj.AddPair('jsonrpc', '2.0');
    RequestObj.AddPair('id', FRequestIDCounter);
    RequestObj.AddPair('method', 'initialize');
    var
    LParams := TJSONObject.Create;
    RequestObj.AddPair('params', LParams);
    LParams.AddPair('protocolVersion', '2025-06-18');
    var
    LClientInfo := TJSONObject.Create;
    LParams.AddPair('clientInfo', LClientInfo);
    LClientInfo.AddPair('name', 'Delphi Client ' + Self.Name);
    LClientInfo.AddPair('version', '1.1.0');
    var
    LCapabilities := TJSONObject.Create;
    LParams.AddPair('capabilities', LCapabilities);
    // Añadir capacidades vacías
    LCapabilities.AddPair('tools', TJSONObject.Create);
    LCapabilities.AddPair('resources', TJSONObject.Create);
    LCapabilities.AddPair('roots', TJSONObject.Create);

    InternalSendRawMessage(RequestObj.ToJSON);
    Response := InternalReceiveJSONResponse(FRequestIDCounter);

    if Assigned(Response) then
      try
        if (Response is TJSONObject) and TJSONObject(Response).TryGetValue('result', ResultPair) and (ResultPair is TJSONObject) then
        begin
          Result := TJSONObject(ResultPair.Clone); // Clonamos el resultado para que el llamador sea dueño
        end
        else
        begin
          DoLog('Error: Initialize response is invalid. ' + Response.ToJSON);
        end;
      finally
        Response.Free; // Liberamos el contenedor de la respuesta
      end;
  finally
    RequestObj.Free;
  end;
end;

procedure TMCPClientStdIo.InternalSendInitializedNotification;
var
  NotificationObj: TJSONObject;
begin
  if not IsServerRunning then
    Exit;

  NotificationObj := TJSONObject.Create;
  try
    NotificationObj.AddPair('jsonrpc', '2.0');
    NotificationObj.AddPair('method', 'notifications/initialized');
    NotificationObj.AddPair('params', TJSONObject.Create);
    InternalSendRawMessage(NotificationObj.ToJSON);
  finally
    NotificationObj.Free;
  end;
end;

function TMCPClientStdIo.InternalListTools: TJSONObject;
var
  RequestObj, Response: TJSONObject;
  ResultPair: TJSonValue;
begin
  Result := nil;
  if not IsServerRunning then
    Exit;

  Inc(FRequestIDCounter);
  RequestObj := TJSONObject.Create;
  try
    RequestObj.AddPair('jsonrpc', '2.0');
    RequestObj.AddPair('id', FRequestIDCounter);
    RequestObj.AddPair('method', 'tools/list');
    RequestObj.AddPair('params', TJSONObject.Create);

    InternalSendRawMessage(RequestObj.ToJSON);
    Response := InternalReceiveJSONResponse(FRequestIDCounter);

    if Assigned(Response) then
      try
        if (Response is TJSONObject) and TJSONObject(Response).TryGetValue('result', ResultPair) and (ResultPair is TJSONObject) then
        begin
          Result := TJSONObject(ResultPair.Clone); // Clonar para el llamador
        end
        else
        begin
          DoLog('Error: tools/list response is invalid. ' + Response.ToJSON);
        end;
      finally
        Response.Free;
      end;
  finally
    RequestObj.Free;
  end;
end;

function TMCPClientStdIo.InternalCallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  RequestObj, Response: TJSONObject;
  ResultPair: TJSonValue;
begin
  Result := nil;
  if not IsServerRunning then
  begin
    FreeAndNil(AArguments);
    Exit;
  end;
  if not Assigned(AArguments) then
    AArguments := TJSONObject.Create;

  Inc(FRequestIDCounter);
  RequestObj := TJSONObject.Create;
  try
    RequestObj.AddPair('jsonrpc', '2.0');
    RequestObj.AddPair('id', FRequestIDCounter);
    RequestObj.AddPair('method', 'tools/call');

    var
    LParams := TJSONObject.Create;
    RequestObj.AddPair('params', LParams);
    LParams.AddPair('name', TJSONString.Create(AToolName));
    LParams.AddPair('arguments', AArguments); // AArguments es ahora propiedad de LParams

    DoLog(Format('Calling tool "%s"...', [AToolName]));
    InternalSendRawMessage(RequestObj.ToJSON);

    Response := InternalReceiveJSONResponse(FRequestIDCounter);

    if Assigned(Response) then
      try
        if (Response is TJSONObject) and TJSONObject(Response).TryGetValue('result', ResultPair) and (ResultPair is TJSONObject) then
        begin
          // Result := TJSONObject(ResultPair.Clone); // Clonar para el llamador
          // Separa la respuesta de los archivos binarios que pasarán a la respuesta como mediafiles el el mensaje de respuesta
          Result := ProcessAndExtractMedia(TJSONObject(ResultPair), AExtractedMedia);
        end
        else
        begin
          var
            LError: TJSONObject;
          if (Response is TJSONObject) and TJSONObject(Response).TryGetValue<TJSONObject>('error', LError) then
            DoLog(Format('Server error calling "%s": %s', [AToolName, LError.ToJSON]))
          else
            DoLog(Format('Invalid server response for "%s": %s', [AToolName, Response.ToJSON]));
        end;
      finally
        Response.Free;
      end;
  finally
    RequestObj.Free;
  end;
end;

// --- Métodos de bajo nivel (comunicación) ---
// (Estos métodos son esencialmente los mismos que en el demo original,
// solo que ahora usan DoLog para reportar)

procedure TMCPClientStdIo.ReadProcessOutput;
var
  ReadBuffer: array [0 .. 4095] of Byte;
  BytesRead: Integer;
  LineBuffer: TBytes;
  LineFeedPos: Integer;
  CurrentLine: string;
  ParsedJson: TJSONObject;
  TempBytes: TBytes;
begin
  SetLength(LineBuffer, 0);
  DoLog('Hilo de lectura de salida iniciado.');
  try
    while FIsRunning and Assigned(FInteractiveProcess) and FInteractiveProcess.IsRunning do
    begin
      // Leer STDOUT
      BytesRead := FInteractiveProcess.ReadOutput(ReadBuffer, SizeOf(ReadBuffer));
      if BytesRead > 0 then
      begin
        var
        OldLen := Length(LineBuffer);
        SetLength(LineBuffer, OldLen + BytesRead);
        Move(ReadBuffer[0], LineBuffer[OldLen], BytesRead);

        while True do
        begin
          LineFeedPos := -1;
          for var i := 0 to Length(LineBuffer) - 1 do
            if LineBuffer[i] = 10 then
            begin
              LineFeedPos := i;
              Break;
            end;

          if LineFeedPos = -1 then
            Break;

          CurrentLine := TEncoding.UTF8.GetString(LineBuffer, 0, LineFeedPos).Trim;

          if not CurrentLine.IsEmpty then
          begin
            if CurrentLine.StartsWith('{') and CurrentLine.EndsWith('}') then
            begin
              try
                ParsedJson := TJSONObject.ParseJSONValue(CurrentLine) as TJSONObject;
                if Assigned(ParsedJson) then
                begin
                  DoLog('SERVER -> CLIENT (queueing): ' + CurrentLine);
                  // FIX: Usar PushItem en lugar de Enqueue.
                  if FIncomingMessages.PushItem(ParsedJson) <> wrSignaled then
                  begin
                    DoLog('ERROR: La cola de mensajes está llena, se descartó el mensaje.');
                    ParsedJson.Free; // Importante liberar el objeto no encolado.
                  end;
                end;
              except
                on E: Exception do
                  DoLog('Error al parsear JSON (stdout): ' + E.Message + '. Línea: ' + CurrentLine);
              end;
            end
            else
              DoLog('[SERVER RAW STDOUT]: ' + CurrentLine);
          end;
          System.Delete(LineBuffer, 0, LineFeedPos + 1);
        end;
      end;

      // Leer STDERR
      BytesRead := FInteractiveProcess.ReadError(ReadBuffer, SizeOf(ReadBuffer));
      if BytesRead > 0 then
      begin
        // FIX: Uso correcto de GetString para un array de bytes.
        // DoLog('[SERVER RAW STDERR]: ' + TEncoding.UTF8.GetString(ReadBuffer, BytesRead).Trim);

        SetLength(TempBytes, SizeOf(ReadBuffer));
        Move(ReadBuffer[0], TempBytes[0], SizeOf(ReadBuffer));
        DoLog('[SERVER RAW STDERR]: ' + TEncoding.UTF8.GetString(TempBytes, 0, BytesRead).Trim);

      end;
      Sleep(10);
    end;
  finally
    SetLength(LineBuffer, 0);
    DoLog('Hilo de lectura de salida terminado.');
  end;
end;

procedure TMCPClientStdIo.InternalSendRawMessage(const AJsonString: string);
var
  Bytes: TBytes;
begin
  if not IsServerRunning then
  begin
    DoLog('ERROR: Server not running to send message.');
    Exit;
  end;

  DoLog('CLIENT -> SERVER: ' + AJsonString.Trim);
  try
    Bytes := TEncoding.UTF8.GetBytes(AJsonString + #10);
    FInteractiveProcess.WriteInput(Bytes[0], Length(Bytes));
  except
    on E: Exception do
      DoLog('ERROR writing to server STDIN: ' + E.Message);
  end;
end;

function TMCPClientStdIo.InternalReceiveJSONResponse(AExpectedID: Integer; ATimeoutMs: Cardinal): TJSONObject;
var
  ReceivedJson: TJSONObject;
  Stopwatch: TStopwatch;
  Id: Integer;
begin
  Result := nil;
  if not IsServerRunning then
    Exit;

  DoLog(Format('Waiting for JSON-RPC response with ID %d...', [AExpectedID]));
  Stopwatch := TStopwatch.StartNew;

  while Stopwatch.ElapsedMilliseconds < ATimeoutMs do
  begin
    if FIncomingMessages.PopItem(ReceivedJson) = wrSignaled then
    begin
      try
        if ReceivedJson.TryGetValue<Integer>('id', Id) and (Id = AExpectedID) then
        begin
          Result := ReceivedJson; // Success! Return the received object
          Exit;
        end
        else
        begin
          DoLog(Format('DEBUG: Ignoring message with ID %d (expected %d) or unsolicited notification.', [ReceivedJson.GetValue<Integer>('id', -1), AExpectedID]));
          ReceivedJson.Free; // Free the unwanted object
        end;
      except
        on E: Exception do
        begin
          DoLog('ERROR processing queued message: ' + E.Message);
          FreeAndNil(ReceivedJson);
        end;
      end;
    end;

    if not IsServerRunning then
    begin
      DoLog('ERROR: Server stopped while waiting for a response.');
      Break;
    end;
    Sleep(20); // Pequeña pausa para no consumir 100% de CPU
  end;

  DoLog(Format('TIMEOUT: No response received for ID %d in %d ms.', [AExpectedID, ATimeoutMs]));
end;

function MergeToolLists(const ASourceName: string; ASourceJson, ATargetJson: TJSONObject; AFormat: TToolFormat): TJSONObject;
var
  LSourceTools, LTargetTools: TJSonArray;
  LSourceTool, LNewTool, LInputSchema: TJSONObject;
  LInputSchemaValue: TJSonValue;
  LToolName: string;
  i: Integer;
begin
  // 1. Validar las entradas
  if not Assigned(ASourceJson) then
  begin
    // Si la fuente es nula, no hay nada que hacer. Devolvemos el objetivo sin cambios.
    Result := ATargetJson;
    Exit;
  end;

  // Si el objetivo (Target) es nulo, creamos un objeto base con un array de "tools" vacío.
  if not Assigned(ATargetJson) then
  begin
    ATargetJson := TJSONObject.Create;
    ATargetJson.AddPair('tools', TJSonArray.Create);
  end;

  // 2. Obtener los arrays de "tools" de ambos JSON.
  // Si no existen, salimos porque no hay herramientas que procesar.
  if not ASourceJson.TryGetValue<TJSonArray>('tools', LSourceTools) then
  begin
    Result := ATargetJson;
    Exit;
  end;

  if not ATargetJson.TryGetValue<TJSonArray>('tools', LTargetTools) then
    raise Exception.Create('Target JSON object does not contain a "tools" array.');

  // 3. Iterar sobre cada herramienta en el JSON de origen (Source)
  for i := 0 to LSourceTools.Count - 1 do
  begin
    if not(LSourceTools.Items[i] is TJSONObject) then
      Continue; // Saltar si el item no es un objeto

    LSourceTool := LSourceTools.Items[i] as TJSONObject;
    LNewTool := TJSONObject.Create; // Crear un nuevo objeto para la herramienta transformada

    // 4. Transformar el nombre de la herramienta con el prefijo
    if LSourceTool.TryGetValue<string>('name', LToolName) then
    begin
      if ASourceName.IsEmpty then
        LNewTool.AddPair('name', LToolName)
      else
        LNewTool.AddPair('name', TJSONString.Create(Format('%s@%s', [ASourceName, LToolName])));
    end;

    // 5. Copiar la descripción (común a todos los formatos)

    Var
      Description: String;
    If LSourceTool.TryGetValue<String>('description', Description) then
      LNewTool.AddPair('description', Description);

    // 6. Normalizar el esquema de entrada (input schema)
    // El formato MCP usa 'inputSchema', Anthropic usa 'input_schema', OpenAI usa 'parameters'
    if LSourceTool.TryGetValue('inputSchema', LInputSchemaValue) or LSourceTool.TryGetValue('input_schema', LInputSchemaValue) or LSourceTool.TryGetValue('parameters', LInputSchemaValue) then
    begin
      if LInputSchemaValue is TJSONObject then
      begin
        LInputSchema := TJSONObject(LInputSchemaValue.Clone); // Clonamos para no modificar el original

        // El formato de OpenAI tiene una propiedad extra 'type':'function' que debemos añadir.
        if AFormat = tfOpenAI then
        begin
          LNewTool.AddPair('type', 'function');
          LNewTool.AddPair('parameters', LInputSchema); // OpenAI usa el nombre 'parameters'
        end
        else // Anthropic y otros formatos estándar
        begin
          LNewTool.AddPair('input_schema', LInputSchema); // Anthropic usa 'input_schema'
        end;
      end;
    end
    else
    begin
      // Si no hay schema, creamos uno vacío para cumplir con el formato
      LInputSchema := TJSONObject.Create;
      LInputSchema.AddPair('type', 'object');
      LInputSchema.AddPair('properties', TJSONObject.Create);

      if AFormat = tfOpenAI then
      begin
        LNewTool.AddPair('type', 'function');
        LNewTool.AddPair('parameters', LInputSchema);
      end
      else
      begin
        LNewTool.AddPair('input_schema', LInputSchema);
      end;
    end;

    // 7. Añadir la herramienta transformada al array de herramientas del Target
    LTargetTools.Add(LNewTool);
  end;

  // 8. Devolver el objeto Target modificado.
  Result := ATargetJson;
end;

{ TMCPClientHttp }
{ TMCPClientHttp }

constructor TMCPClientHttp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpClient := TNetHTTPClient.Create(Self);
  FRequestIDCounter := 0;
  FreeAndNil(FServerCapabilities);
  DoLog('TMCPClientHttp instance created.');
end;

destructor TMCPClientHttp.Destroy;
begin
  // El proceso del servidor local se detiene en el destructor de TMCPClientCustom
  FreeAndNil(FServerCapabilities);
  inherited;
end;

function TMCPClientHttp.Initialize: Boolean;
const
  MAX_RETRIES_ON_FAIL = 2; // Número de reintentos de conexión si el servidor no responde
var
  jTools: TJSONObject;
  jValue: TJSonValue;
  CurrentAttempt: Integer;
begin
  Result := False;
  FBusy := True;
  FLastError := '';
  Initialized := True;
  Enabled := False;
  Available := False;
  FTools.Clear;

  DoStatusUpdate('Initializing MCP Client (HTTP)...');

  CurrentAttempt := 0;
  while CurrentAttempt < MAX_RETRIES_ON_FAIL do
  begin
    Inc(CurrentAttempt);
    try
      DoLog(Format('Attempting connection to server (Try %d/%d)...', [CurrentAttempt, MAX_RETRIES_ON_FAIL]));

      // 1. Realizar el handshake de inicialización de MCP
      // Las excepciones de conexión se lanzarán aquí y serán capturadas abajo.
      InternalPerformMCPInitialize;
      DoLog('MCP Initialization successful.');

      // 2. Enviar la notificación de inicializado (no lanza excepción, solo logea warnings si falla la POST de la notificación)
      InternalSendInitializedNotification;

      // 3. Obtener la lista de herramientas
      jTools := ListTools;

      If Assigned(jTools) and (jTools.TryGetValue<TJSonValue>('tools', jValue)) and (jValue is TJSonArray) then
      Begin
        FTools.Text := jTools.Format;
        Enabled := True;
        Available := True;
        Result := True; // Éxito
        Break; // Salir del bucle de reintentos
      End
      Else
      Begin
        FLastError := 'Failed to retrieve tools list after successful initialization. Server response incomplete.';
        DoLog(FLastError);
        // Si el problema no es de conexión sino de la respuesta del protocolo, no relanzamos el servidor.
        Break; // No reintentar relanzando el server, ya está conectado.
      End;
    except
      on E: Exception do
      begin
        FLastError := 'MCP HTTP Initialization Failed: ' + E.Message;
        DoLog(FLastError);
        DoStatusUpdate('Initialization Failed: ' + E.Message);

        // Si es un error de conexión (no 200 OK, timeout, etc.) y aún quedan reintentos
        if (CurrentAttempt < MAX_RETRIES_ON_FAIL) then
        begin
          DoLog(Format('Connection failed. Attempting to start local server and retry (Attempt %d).', [CurrentAttempt]));
          InternalStopLocalServerProcess; // Detener cualquier proceso anterior que hayamos podido iniciar y haya fallado
          if InternalStartLocalServerProcess then // Intentar lanzar el servidor local
          begin
            // Éxito al lanzar el server, el bucle reintentará la conexión
            Sleep(2000); // Dar un tiempo extra al server para que esté listo para las conexiones
            Continue; // Volver al inicio del bucle para reintentar la conexión
          end
          else
          begin
            DoLog('Failed to start local server. No more retries for this initialization cycle.');
            Break; // No se pudo lanzar el servidor local, fin de los reintentos
          end;
        end
        else
        begin
          DoLog('Max connection attempts reached. Initialization failed permanently.');
          Break; // Máximo de reintentos alcanzado
        end;
      end;
    end;
  end; // end while

  FBusy := False;
  Exit(Result);
end;

function TMCPClientHttp.ListTools: TJSONObject;
var
  LParams: TJSONObject;
begin
  FLastError := '';
  DoLog('Executing ListTools via HTTP...');
  DoStatusUpdate('Listing tools...');

  LParams := TJSONObject.Create; // Se libera en InternalSendRequest
  try
    Result := InternalSendRequest('tools/list', LParams);
  except
    on E: Exception do
    begin
      FLastError := 'Error listing tools: ' + E.Message;
      DoLog(FLastError);
      Result := nil; // Retornar nil en caso de error
    end;
  end;

  if Assigned(Result) then
    DoStatusUpdate('Tools listed successfully.')
  else
    DoStatusUpdate('Failed to list tools: ' + FLastError);
end;

function TMCPClientHttp.CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  LParams: TJSONObject;
  LResultRaw: TJSONObject;
begin
  Result := nil;
  FLastError := '';

  DoLog(Format('Executing CallTool via HTTP: %s', [AToolName]));
  DoStatusUpdate(Format('Calling tool: %s...', [AToolName]));

  LParams := TJSONObject.Create; // LParams toma la propiedad de AArguments. AArguments no debe ser liberado por el llamador.
  try
    LParams.AddPair('name', TJSONString.Create(AToolName));
    LParams.AddPair('arguments', AArguments); // AArguments es ahora propiedad de LParams

    LResultRaw := InternalSendRequest('tools/call', LParams); // InternalSendRequest libera LParams

    if Assigned(LResultRaw) then
    begin
      Result := ProcessAndExtractMedia(LResultRaw, AExtractedMedia);
      FreeAndNil(LResultRaw); // Liberar el resultado RAW después de procesarlo
    end;
  except
    on E: Exception do
    begin
      FLastError := Format('Error calling tool "%s": %s', [AToolName, E.Message]);
      DoLog(FLastError);
      Result := nil; // Retornar nil en caso de error
    end;
  end;

  if Assigned(Result) then
    DoStatusUpdate(Format('Tool %s called successfully.', [AToolName]))
  else
    DoStatusUpdate(Format('Failed to call tool: %s. Error: %s', [AToolName, FLastError]));
end;

function TMCPClientHttp.CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  ArgsObject: TJSONObject;
  i: Integer;
begin
  ArgsObject := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for i := 0 to AArguments.Count - 1 do
    begin
      ArgsObject.AddPair(AArguments.Names[i], AArguments.ValueFromIndex[i]);
    end;
  end;
  Result := CallTool(AToolName, ArgsObject, AExtractedMedia);
end;

procedure TMCPClientHttp.InternalPerformMCPInitialize;
var
  LRequestObj, LParams, LClientInfo, LCapabilities: TJSONObject;
  LServerInfo, LServerCapabilities: TJSONObject;
  LResultPair: TJSonValue;
  LURL: string;
  LRequestBodyStream: TStringStream;
  LHttpResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
begin
  FBusy := True;
  FLastError := '';
  LRequestObj := TJSONObject.Create;
  try
    Inc(FRequestIDCounter);
    LRequestObj.AddPair('jsonrpc', '2.0');
    LRequestObj.AddPair('id', FRequestIDCounter);
    LRequestObj.AddPair('method', 'initialize');

    LParams := TJSONObject.Create;
    LRequestObj.AddPair('params', LParams);

    LParams.AddPair('protocolVersion', '2025-06-18');

    LClientInfo := TJSONObject.Create;
    LParams.AddPair('clientInfo', LClientInfo);
    LClientInfo.AddPair('name', 'Delphi Client ' + Self.Name);
    LClientInfo.AddPair('version', '1.1.0');

    LCapabilities := TJSONObject.Create;
    LParams.AddPair('capabilities', LCapabilities);
    LCapabilities.AddPair('tools', TJSONObject.Create);
    LCapabilities.AddPair('resources', TJSONObject.Create);
    LCapabilities.AddPair('roots', TJSONObject.Create);
    // Add other client capabilities here if implemented (sampling, elicitation, logging)

    LURL := GetParamByName('URL') + GetParamByName('InitializeEndpointSuffix');

    LRequestBodyStream := TStringStream.Create(LRequestObj.ToJSON, TEncoding.UTF8);
    try
{$IFDEF APIDEBUG}
      LRequestBodyStream.SaveToFile('c:\temp\mcp_init_request.txt');
      LRequestBodyStream.Position := 0;
{$ENDIF}
{$IF CompilerVersion >= 35}
      FHttpClient.SendTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);
{$ENDIF}
      FHttpClient.ResponseTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);
      FHttpClient.ConnectionTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);

      FHttpClient.Accept := 'application/json';
      FHttpClient.ContentType := 'application/json';

      var
      BearerToken := GetParamByName('ApiBearerToken');
      if not BearerToken.IsEmpty and (BearerToken <> '@MCPBearerToken') then
      begin
        var
        HeaderName := GetParamByName('ApiHeaderName');
        if HeaderName.IsEmpty then
          HeaderName := 'Authorization';
        LHeaders := [TNetHeader.Create(HeaderName, 'Bearer ' + BearerToken)];
      end
      else
        LHeaders := [];

      var
        LResponseStream: TMemoryStream; // Usar MemoryStream para la respuesta
      LResponseStream := TMemoryStream.Create;
      try
        LHttpResponse := FHttpClient.Post(LURL, LRequestBodyStream, LResponseStream, LHeaders);
        LResponseStream.Position := 0; // Reset para leer

{$IFDEF APIDEBUG}
        LResponseStream.SaveToFile('c:\temp\mcp_init_response.txt');
        LResponseStream.Position := 0; // Reset para leer
{$ENDIF}
        var
        ResponseContent := LHttpResponse.ContentAsString(TEncoding.UTF8);
        DoLog(Format('SERVER -> CLIENT (HTTP %d): %s', [LHttpResponse.StatusCode, ResponseContent]));

        if LHttpResponse.StatusCode <> 200 then
          raise EMCPClientException.CreateFmt('HTTP initialization failed with status code %d: %s', [LHttpResponse.StatusCode, ResponseContent]);

        var
        LResponseObj := TJSONObject.ParseJSONValue(ResponseContent) as TJSONObject;
        if not Assigned(LResponseObj) then
          raise EMCPClientException.Create('Failed to parse JSON initialization response from server.');
        try
          var
            LResponseID: Integer;
          if not LResponseObj.TryGetValue<Integer>('id', LResponseID) or (LResponseID <> FRequestIDCounter) then
            raise EMCPClientException.CreateFmt('Mismatched response ID during initialize. Expected %d, got %d. Raw: %s', [FRequestIDCounter, LResponseID, LResponseObj.ToJSON]);

          var
            LErrorObj: TJSONObject;
          if LResponseObj.TryGetValue<TJSONObject>('error', LErrorObj) then
            raise EMCPClientException.Create('Server returned a JSON-RPC error during initialization: ' + LErrorObj.ToJSON);

          if LResponseObj.TryGetValue('result', LResultPair) and (LResultPair is TJSONObject) then
          begin
            FreeAndNil(FServerCapabilities);
            if TJSONObject(LResultPair).TryGetValue<TJSONObject>('capabilities', LServerCapabilities) then
              FServerCapabilities := TJSONObject(LServerCapabilities.Clone)
            else
              DoLog('WARNING: Server capabilities not found in initialization response.');

            if TJSONObject(LResultPair).TryGetValue<TJSONObject>('serverInfo', LServerInfo) then
              DoLog(Format('Server Info: Name="%s", Version="%s"', [LServerInfo.GetValue<string>('name', ''), LServerInfo.GetValue<string>('version', '')]));
          end
          else
          begin
            raise EMCPClientException.Create('JSON-RPC initialization response is invalid or does not contain a result object.');
          end;
        finally
          LResponseObj.Free;
        end;
      finally
        LResponseStream.Free;
      end;
    finally
      LRequestBodyStream.Free;
    end;
  finally
    LRequestObj.Free;
    FBusy := False;
  end;
end;

procedure TMCPClientHttp.InternalSendInitializedNotification;
var
  LNotificationObj: TJSONObject;
  LURL: string;
  LRequestBodyStream: TStringStream;
  LHttpResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
begin
  FLastError := '';
  LNotificationObj := TJSONObject.Create;
  try
    DoLog('Sending MCP "notifications/initialized" (HTTP)...');
    LNotificationObj.AddPair('jsonrpc', '2.0');
    LNotificationObj.AddPair('method', 'notifications/initialized');
    LNotificationObj.AddPair('params', TJSONObject.Create);

    LURL := GetParamByName('URL') + GetParamByName('NotificationEndpointSuffix');

    LRequestBodyStream := TStringStream.Create(LNotificationObj.ToJSON, TEncoding.UTF8);
    try
{$IFDEF APIDEBUG}
      LRequestBodyStream.SaveToFile('c:\temp\mcp_initialized_notification.txt');
      LRequestBodyStream.Position := 0;
{$ENDIF}
      FHttpClient.ContentType := 'application/json';

      var
      BearerToken := GetParamByName('ApiBearerToken');
      if not BearerToken.IsEmpty and (BearerToken <> '@MCPBearerToken') then
      begin
        var
        HeaderName := GetParamByName('ApiHeaderName');
        if HeaderName.IsEmpty then
          HeaderName := 'Authorization';
        LHeaders := [TNetHeader.Create(HeaderName, 'Bearer ' + BearerToken)];
      end
      else
        LHeaders := [];

      // Las notificaciones no esperan respuesta, pero el servidor puede responder con un 204 No Content o un 200 OK vacío.
      // Aquí no se espera que lance excepciones al fallar el POST, solo logear un warning.
      var
        LResponseStream: TMemoryStream;
      LResponseStream := TMemoryStream.Create;
      try
        LHttpResponse := FHttpClient.Post(LURL, LRequestBodyStream, LResponseStream, LHeaders);
        LResponseStream.Position := 0; // Reset para leer

{$IFDEF APIDEBUG}
        LResponseStream.SaveToFile('c:\temp\mcp_initialized_notification_response.txt');
{$ENDIF}
        var
        ResponseContent := LHttpResponse.ContentAsString(TEncoding.UTF8);
        DoLog(Format('SERVER -> CLIENT (HTTP %d): %s', [LHttpResponse.StatusCode, ResponseContent]));

        if LHttpResponse.StatusCode <> 200 then
          DoLog(Format('WARNING: HTTP notification failed with status code %d: %s', [LHttpResponse.StatusCode, ResponseContent]));
      finally
        LResponseStream.Free;
      end;
    finally
      LRequestBodyStream.Free;
    end;
  finally
    LNotificationObj.Free;
  end;
end;

// ********************************************************************************
// LA FUNCIÓN SOLICITADA: TMCPClientHttp.InternalSendRequest
// ********************************************************************************
function TMCPClientHttp.InternalSendRequest(const AMethod: string; AParams: TJSONObject): TJSONObject;
var
  LRequestObj: TJSONObject;
  LRequestBodyStream: TStringStream;
  LHttpResponse: IHTTPResponse;
  LResultPair: TJSonValue;
  LURL: string;
  LHeaders: TNetHeaders;
  LResponseContent: string; // Para almacenar el contenido de la respuesta
begin
  Result := nil;
  FBusy := True;
  FLastError := '';
  LRequestObj := TJSONObject.Create;
  try
    // Generar un nuevo ID para la solicitud JSON-RPC
    Inc(FRequestIDCounter);

    // Construir el objeto de solicitud JSON-RPC
    LRequestObj.AddPair('jsonrpc', '2.0');
    LRequestObj.AddPair('id', FRequestIDCounter);
    LRequestObj.AddPair('method', AMethod);
    LRequestObj.AddPair('params', AParams); // AParams se convierte en propiedad de LRequestObj

    // Crear un StringStream para el cuerpo de la solicitud HTTP
    LRequestBodyStream := TStringStream.Create(LRequestObj.ToJSON, TEncoding.UTF8);
    try
      // Obtener la URL completa para la solicitud RPC
      LURL := GetParamByName('URL') + GetParamByName('RpcEndpointSuffix');

      DoLog(Format('CLIENT -> SERVER (HTTP POST to %s): %s', [LURL, LRequestBodyStream.DataString]));

{$IFDEF APIDEBUG}
      // Guardar la petición en un archivo para depuración
      LRequestBodyStream.SaveToFile('c:\temp\mcp_rpc_request_' + AMethod.Replace('/', '_') + '.txt');
      LRequestBodyStream.Position := 0; // Resetear la posición para la lectura por el TNetHTTPClient
{$ENDIF}
      // Configurar los timeouts del cliente HTTP
{$IF CompilerVersion >= 35}
      FHttpClient.SendTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);
{$ENDIF}
      FHttpClient.ResponseTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);
      FHttpClient.ConnectionTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);

      // Configurar los encabezados Accept y Content-Type
      FHttpClient.Accept := 'application/json';
      FHttpClient.ContentType := 'application/json';

      // Preparar los encabezados HTTP personalizados (ej. Authorization)
      var
      BearerToken := GetParamByName('ApiBearerToken');
      if not BearerToken.IsEmpty and (BearerToken <> '@MCPBearerToken') then
      begin
        var
        HeaderName := GetParamByName('ApiHeaderName');
        if HeaderName.IsEmpty then
          HeaderName := 'Authorization';
        LHeaders := [TNetHeader.Create(HeaderName, 'Bearer ' + BearerToken)];
      end
      else
        LHeaders := []; // Array vacío si no hay token

      // Crear un MemoryStream para recibir la respuesta HTTP
      var
        LResponseStream: TMemoryStream;
      LResponseStream := TMemoryStream.Create;
      try
        // Realizar la solicitud HTTP POST
        LHttpResponse := FHttpClient.Post(LURL, LRequestBodyStream, LResponseStream, LHeaders);
        LResponseStream.Position := 0; // Resetear la posición para la lectura del contenido

{$IFDEF APIDEBUG}
        // Guardar la respuesta en un archivo para depuración
        LResponseStream.SaveToFile('c:\temp\mcp_rpc_response_' + AMethod.Replace('/', '_') + '.txt');
        LResponseStream.Position := 0; // Resetear la posición para la lectura del contenido
{$ENDIF}
        // Leer el contenido de la respuesta HTTP
        LResponseContent := (LHttpResponse.ContentAsString(TEncoding.UTF8));
        DoLog(Format('SERVER -> CLIENT (HTTP %d): %s', [LHttpResponse.StatusCode, LResponseContent]));

        // Verificar el código de estado HTTP
        if LHttpResponse.StatusCode <> 200 then
          raise EMCPClientException.CreateFmt('HTTP request failed with status code %d: %s', [LHttpResponse.StatusCode, LResponseContent]);

        // Intentar parsear la respuesta JSON
        var
        LResponseObj := TJSONObject.ParseJSONValue(LResponseContent) as TJSONObject;
        if not Assigned(LResponseObj) then
          raise EMCPClientException.Create('Failed to parse JSON response from server.');
        try
          // Verificar que el ID de la respuesta JSON-RPC coincida con el de la solicitud
          var
            LResponseID: Integer;
          if not LResponseObj.TryGetValue<Integer>('id', LResponseID) or (LResponseID <> FRequestIDCounter) then
            raise EMCPClientException.CreateFmt('Mismatched response ID. Expected %d, got %d. Raw: %s', [FRequestIDCounter, LResponseID, LResponseObj.ToJSON]);

          // Verificar si la respuesta JSON-RPC contiene un error
          var
            LErrorObj: TJSONObject;
          if LResponseObj.TryGetValue<TJSONObject>('error', LErrorObj) then
            raise EMCPClientException.Create('Server returned a JSON-RPC error: ' + LErrorObj.ToJSON);

          // Extraer el resultado de la respuesta JSON-RPC y clonarlo para el llamador
          if LResponseObj.TryGetValue('result', LResultPair) and (LResultPair is TJSONObject) then
            Result := TJSONObject(LResultPair.Clone)
          else
            raise EMCPClientException.Create('JSON-RPC response is invalid or does not contain a result object.');
        finally
          // Liberar el objeto JSON de la respuesta
          LResponseObj.Free;
        end;
      finally
        // Liberar el MemoryStream de la respuesta
        LResponseStream.Free;
      end;
    except
      on E: Exception do
      begin
        // Capturar cualquier excepción durante el proceso HTTP/JSON, logearla y re-lanzarla
        FLastError := 'EXCEPTION during HTTP request: ' + E.Message;
        DoLog(FLastError);
        DoStatusUpdate('Network Error: ' + E.Message);
        raise; // Re-lanzar la excepción para que el método llamador (ej. Initialize) la maneje
      end;
    end;
  finally
    // Liberar el objeto de solicitud JSON-RPC (también liberará AParams, del cual se hizo propietario)
    LRequestObj.Free;
    // Liberar el StringStream del cuerpo de la solicitud
    LRequestBodyStream.Free;
    // Restablecer el estado de ocupado
    FBusy := False;
  end;
end;

{ TMCPClientMakerAi }

constructor TMCPClientMakerAi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Creamos el cliente HTTP y lo asignamos al componente para su gestión automática de memoria.
  FHttpClient := TNetHTTPClient.Create(Self);
  // Asignamos el tipo de transporte para que la factoría y otros mecanismos lo reconozcan.
  TransportType := tpMakerAi;
  DoLog('TMCPClientMakerAi instance created.');
end;

destructor TMCPClientMakerAi.Destroy;
begin
  // FHttpClient es propiedad del componente, por lo que se liberará automáticamente.
  // No se necesita código explícito aquí, pero mantenemos el destructor por si se añade algo en el futuro.
  DoLog('TMCPClientMakerAi instance destroyed.');
  inherited;
end;

function TMCPClientMakerAi.ListTools: TJSONObject;
begin
  FLastError := '';
  DoLog('Executing ListTools via MakerAI REST client...');
  DoStatusUpdate('Listing tools...');
  try
    // La llamada a ListTools no tiene cuerpo y usa el verbo GET.
    Result := InternalSendRequest('listtools', 'GET', nil);

    if Assigned(Result) then
      DoStatusUpdate('Tools listed successfully.')
    else
      DoStatusUpdate('Failed to list tools: ' + FLastError);
  except
    on E: Exception do
    begin
      FLastError := 'Error listing tools: ' + E.Message;
      DoLog(FLastError);
      DoStatusUpdate('Failed to list tools: ' + E.Message);
      Result := nil; // Retornar nil en caso de error
    end;
  end;
end;

function TMCPClientMakerAi.CallTool(const AToolName: string; AArguments: TJSONObject; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  LBodyStream: TStringStream;
  LResultRaw: TJSONObject;
  LMethodName: string;
begin
  Result := nil;
  FLastError := '';
  DoLog(Format('Executing CallTool via MakerAI REST client: %s', [AToolName]));
  DoStatusUpdate(Format('Calling tool: %s...', [AToolName]));

  // Construimos el nombre del método para la URL, como en tu ejemplo: 'calltool/calculate'
  LMethodName := 'calltool/' + AToolName;

  // Creamos el cuerpo de la petición POST a partir de los argumentos JSON.
  LBodyStream := TStringStream.Create(AArguments.ToJSON, TEncoding.UTF8);
  try
    // CallTool usa el verbo POST y envía los argumentos en el cuerpo.
    LResultRaw := InternalSendRequest(LMethodName, 'POST', LBodyStream);

    // Una vez obtenida la respuesta cruda, la procesamos para extraer
    // posibles archivos multimedia, manteniendo la transparencia con el resto del framework.
    if Assigned(LResultRaw) then
    begin
      try
        Result := ProcessAndExtractMedia(LResultRaw, AExtractedMedia);
      finally
        FreeAndNil(LResultRaw); // Liberamos el resultado RAW después de procesarlo.
      end;
    end;

  except
    on E: Exception do
    begin
      FLastError := Format('Error calling tool "%s": %s', [AToolName, E.Message]);
      DoLog(FLastError);
      Result := nil; // Retornar nil en caso de error
    end;
  end;
  LBodyStream.Free; // Liberamos el stream del cuerpo.
  // AArguments es liberado por el llamador original, no lo tocamos.

  if Assigned(Result) then
    DoStatusUpdate(Format('Tool %s called successfully.', [AToolName]))
  else
    DoStatusUpdate(Format('Failed to call tool: %s. Error: %s', [AToolName, FLastError]));
end;

function TMCPClientMakerAi.CallTool(const AToolName: string; AArguments: TStrings; AExtractedMedia: TObjectList<TAiMediaFile>): TJSONObject;
var
  ArgsObject: TJSONObject;
  i: Integer;
begin
  // Esta sobrecarga convierte TStrings en TJSONObject para facilitar su uso.
  ArgsObject := TJSONObject.Create;
  try
    if Assigned(AArguments) then
    begin
      for i := 0 to AArguments.Count - 1 do
      begin
        ArgsObject.AddPair(AArguments.Names[i], TJSONString.Create(AArguments.ValueFromIndex[i]));
      end;
    end;
    // Llama a la versión principal de CallTool.
    Result := CallTool(AToolName, ArgsObject, AExtractedMedia);
  finally
    ArgsObject.Free; // Liberamos el objeto JSON que creamos aquí.
  end;
end;

// --- Método Privado de Ayuda (Corazón de la clase) ---

function TMCPClientMakerAi.InternalSendRequest(const AMethodName, AHttpVerb: string; ABodyStream: TStream): TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LAuthString: string;
  LBase64Auth: string;
  LHeaders: TNetHeaders;
  LDataSnapResultWrapper: TJSONObject;
  LDataSnapResultArray: TJSonArray;
  LResponseContent: string;
  LJsonRpcObject: TJSONObject; // Objeto JSON-RPC anidado
  LResultPair: TJSonValue; // El resultado final del JSON-RPC
begin
  Result := nil;
  FBusy := True;
  FLastError := '';
  LJsonRpcObject := nil;
  LDataSnapResultWrapper := nil;

  // 1. Obtener parámetros de configuración
  LURL := GetParamByName('URL') + '/' + AMethodName;
  var
  LUser := GetParamByName('Login');
  var
  LPass := GetParamByName('Password');
  var
  LTimeout := StrToIntDef(GetParamByName('Timeout'), 15000);

  DoLog(Format('Sending %s request to %s', [AHttpVerb, LURL]));

  // 2. Configurar el cliente HTTP
  FHttpClient.ConnectionTimeout := LTimeout;
  FHttpClient.ResponseTimeout := LTimeout;
{$IF CompilerVersion >= 35}
  FHttpClient.SendTimeout := LTimeout;
{$ENDIF}
  FHttpClient.Accept := 'application/json';
  FHttpClient.ContentType := 'application/json';

  // 3. Preparar la cabecera de autenticación HTTP Basic
  LAuthString := LUser + ':' + LPass;
  LBase64Auth := TNetEncoding.Base64.Encode(LAuthString);
  LHeaders := [TNetHeader.Create('Authorization', 'Basic ' + LBase64Auth)];

  try
    Try
      // 4. Realizar la llamada HTTP (GET o POST)
      if SameText(AHttpVerb, 'GET') then
      begin
        LResponse := FHttpClient.Get(LURL, nil, LHeaders);
      end
      else if SameText(AHttpVerb, 'POST') then
      begin
        if not Assigned(ABodyStream) then
          raise EMCPClientException.Create('POST request requires a body stream.');
        ABodyStream.Position := 0;
        LResponse := FHttpClient.Post(LURL, ABodyStream, nil, LHeaders);
      end
      else
      begin
        raise EMCPClientException.CreateFmt('Unsupported HTTP Verb: %s', [AHttpVerb]);
      end;

      // 5. Procesar la respuesta del servidor
      LResponseContent := LResponse.ContentAsString(TEncoding.UTF8);
      DoLog(Format('SERVER -> CLIENT (HTTP %d): %s', [LResponse.StatusCode, LResponseContent]));

      if LResponse.StatusCode <> 200 then
        raise EMCPClientException.CreateFmt('HTTP request failed. Code: %d. Message: %s', [LResponse.StatusCode, LResponseContent]);

      // 6. Parsear el wrapper DataSnap
      LDataSnapResultWrapper := TJSONObject.ParseJSONValue(LResponseContent) as TJSONObject;
      if not Assigned(LDataSnapResultWrapper) then
        raise EMCPClientException.Create('Failed to parse JSON response (DataSnap wrapper).');

      // 7. Acceder al array "result"
      if not LDataSnapResultWrapper.TryGetValue<TJSonArray>('result', LDataSnapResultArray) or (LDataSnapResultArray.Count = 0) then
        raise EMCPClientException.Create('DataSnap "result" array not found or empty.');

      // 8. CORRECCIÓN: El primer elemento del array es un JSONString que contiene el JSON-RPC.
      if not(LDataSnapResultArray.Items[0] is TJSONString) then
        raise EMCPClientException.Create('First element in DataSnap "result" array is not a JSON string.');

      var
      LJsonRpcString := (LDataSnapResultArray.Items[0] as TJSONString).Value;

      // 9. Parsear el JSON-RPC anidado
      LJsonRpcObject := TJSONObject.ParseJSONValue(LJsonRpcString) as TJSONObject;
      if not Assigned(LJsonRpcObject) then
        raise EMCPClientException.Create('Failed to parse inner JSON-RPC string.');

      // 10. EXTRAER el resultado (el payload real) del JSON-RPC anidado
      if LJsonRpcObject.TryGetValue('result', LResultPair) then
      begin
        if LResultPair is TJSONObject then
        begin
          // Clonamos el objeto resultado para que el llamador sea su dueño.
          Result := TJSONObject(LResultPair.Clone);
        end
        else
        begin
          // Manejar el caso de que el resultado del JSON-RPC sea un valor simple (string, number, boolean)
          // Lo envolvemos en un objeto JSON para mantener la consistencia con el contrato TMCPClientCustom
          Result := TJSONObject.Create;
          Result.AddPair('value', TJSONObject(LResultPair.Clone));
        end;
      end
      else
      begin
        // Verificar si hay un error en el JSON-RPC
        var
          LErrorObj: TJSONObject;
        if LJsonRpcObject.TryGetValue<TJSONObject>('error', LErrorObj) then
          raise EMCPClientException.Create('Server returned a JSON-RPC error: ' + LErrorObj.ToJSON)
        else
          raise EMCPClientException.Create('JSON-RPC object does not contain "result" or "error" keys.');
      end;

    except
      on E: Exception do
      begin
        FLastError := 'EXCEPTION during HTTP request: ' + E.Message;
        DoLog(FLastError);
        DoStatusUpdate('Network Error: ' + E.Message);
        Result := nil;
      end;

    End;
  finally
    // Liberar los objetos JSON que ya no necesitamos
    FreeAndNil(LDataSnapResultWrapper);
    FreeAndNil(LJsonRpcObject);
    FBusy := False;
  end;
end;

end.
