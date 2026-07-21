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

unit uMakerAi.Chat.AiConnection;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding, System.Rtti, System.TypInfo, System.StrUtils,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON,
  uMakerAi.ParamsRegistry, uMakerAi.Tools.Functions, uMakerAi.Core, uMakerAi.Chat,
  uMakerAi.Tools.Shell, uMakerAi.Tools.TextEditor, uMakerAi.Tools.ComputerUse, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages,
  uMakerAi.Memory.Types;

type
  TOnChatModelChangeEvent = procedure(Sender: TObject; const OldChat, NewChat: TAiChat) of object;

  TAiChatConnection = class(TComponent)
  private
    FChat: TAiChat;
    FDriverName: String;
    FModel: String;
    FParams: TStrings;
    FMessages: TAiChatMessages;
    FMessagesOwn: TAiChatMessages; // Instancia de mensajes que poseemos
    // FInitialInstructions: TStrings;
    FMemory: TStrings;
    FAiFunctions: TAiFunctions;
    FPrompt_tokens: integer;
    FCompletion_tokens: integer;
    FTotal_tokens: integer;

    // Eventos
    FOnReceiveData: TAiChatOnDataEvent;
    FOnReceiveDataEnd: TAiChatOnDataEvent;
    FOnAddMessage: TAiChatOnDataEvent;
    FOnCallToolFunction: TOnCallToolFunction;
    FOnBeforeSendMessage: TAiChatOnBeforeSendEvent;
    FOnInitChat: TAiChatOnInitChatEvent;
    FOnProcessMediaFile: TAiChatOnMediaFileEvent;
    FOnError: TAiErrorEvent;
    FOnChatModelChange: TOnChatModelChangeEvent;
    FOnProcessResponse: TAiChatOnProcessResponseEvent;
    FVersion: String;

    FOnReceiveThinking: TAiChatOnDataEvent;
    FChatTools: TAiChatTools;
    FSystemPrompt: TStrings;
    FOnStateChange: TAiStateChangeEvent;
    FChatMode: TAiChatMode;
    FSanitizerActive: Boolean;
    FOnSanitize: TAiSanitizeEvent;

    FTtsParams: TAiTtsParams;
    FTranscriptionParams: TAiTranscriptionParams;
    FImageGenParams: TAiImageGenParams;
    FVideoGenParams: TAiVideoGenParams;
    FWebSearchParams: TAiWebSearchParams;
    FModelConfig: TAiModelConfig;

    FPersistentMemory:  TAiPersistentMemoryBase;
    FMemoryTokenBudget: Integer;
    FAutoStoreMemories: Boolean;
    FInternalParamsUpdate: Boolean; // guard: ediciones internas de FParams no re-disparan ParamsChanged

    // v3.5 — Canal tipado: ModelCaps/SessionCaps/Tool_Active/ThinkingLevel ya no
    // viajan por Params/RTTI; su unico canal es ModelConfig (+ registry via
    // ApplyAutoParams). Estos helpers sostienen esa regla.
    procedure StripModelConfigKeys(AParams: TStrings);
    procedure MigrateModelConfigParams(AParams: TStrings);
    procedure ApplyModelConfigToChat(AChat: TAiChat);

    // Setters y Getters
    procedure SetDriverName(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetParams(const Value: TStrings);
    procedure SetChat(const Value: TAiChat);
    function GetLastError: String;
    function GetBusy: Boolean;
    procedure ParamsChanged(Sender: TObject);
    procedure ModelConfigChanged(Sender: TObject);
    procedure ChatToolsChanged(Sender: TObject);

    procedure SetCompletion_tokens(const Value: integer);
    procedure SetMemory(const Value: TStrings);
    procedure SetOnAddMessage(const Value: TAiChatOnDataEvent);
    procedure SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetOnError(const Value: TAiErrorEvent);
    procedure SetOnInitChat(const Value: TAiChatOnInitChatEvent);
    procedure SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
    procedure SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
    procedure SetOnReceiveData(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
    procedure SetPrompt_tokens(const Value: integer);
    procedure SetTotal_tokens(const Value: integer);
    procedure SetOnReceiveThinking(const Value: TAiChatOnDataEvent);

    procedure SetSystemPrompt(const Value: TStrings);
    procedure SetChatMode(const Value: TAiChatMode);

    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetSanitizerActive(const Value: Boolean);
    procedure SetOnSanitize(const Value: TAiSanitizeEvent);
    procedure SetPersistentMemory(const Value: TAiPersistentMemoryBase);
    procedure SetMemoryTokenBudget(const Value: Integer);
    procedure SetAutoStoreMemories(const Value: Boolean);
    procedure SetTtsParams(const Value: TAiTtsParams);
    procedure SetTranscriptionParams(const Value: TAiTranscriptionParams);
    procedure SetImageGenParams(const Value: TAiImageGenParams);
    procedure SetVideoGenParams(const Value: TAiVideoGenParams);
    procedure SetWebSearchParams(const Value: TAiWebSearchParams);
    procedure SetModelConfig(const Value: TAiModelConfig);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ValideChat;
    procedure UpdateAndApplyParams;
    procedure SetupChatFromDriver;
    procedure ApplyParamsToChat(AChat: TAiChat; AParams: TStrings);
    procedure ApplyEventsToChat(AChat: TAiChat; SetToNil: Boolean = False);
    Procedure OnInternalReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: String);
    // Adiciona los parametros de orgien a destino,  retorna destino si se necesita,  el resultado queda almacenado en destino
    Function MergeParams(Origin, Destination: TStrings): TStrings;
    procedure Loaded; override;

  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;

    Procedure UpdateParamsFromRegistry;
    function AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String; overload;
    function AddMessage(aPrompt, aRole: String): TAiChatMessage;
    function NewMessage(aPrompt, aRole: String): TAiChatMessage;
    function Run(aMsg: TAiChatMessage = nil): String; virtual;
    function GetLastMessage: TAiChatMessage;
    function RemoveMesage(Msg: TAiChatMessage): Boolean; overload;
    function RemoveMesage(IdMsg: integer): Boolean; overload;
    procedure AddToMemory(Key, Value: String);
    procedure RemoveFromMemory(Key: String);
    procedure NewChat;
    procedure Abort;
    // ISSUE #115: control del log de depuracion (opt-in). Delegan en las globales
    // MakerAiDebugLogEnabled / MakerAiDebugLogPath de uMakerAi.Chat.
    procedure EnableDebugLog(const APath: string = '');
    procedure DisableDebugLog;
    function IsDebugLogEnabled: Boolean;
    function GetMessages: TJSonArray; virtual;
    function GetDriversNames: TStringList; virtual;
    function GetAvailableDrivers: TArray<string>;
    function GetModels: TStringList; overload; virtual;
    class function AvailableDrivers: TArray<string>; static;
    class function DriverNames: TStringList; static;
    function IsDriverAvailable(const DriverName: string): Boolean;
    procedure ResetParamsToDefaults;

    procedure RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string); overload;
    procedure RegisterUserParam(const DriverName, ParamName, ParamValue: string); overload;
    procedure ClearRegisterParams(const DriverName: String; ModelName: string = '');
    procedure RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
    procedure ClearCustomModels(const DriverName: string);
    function GetBaseModel(const DriverName, CustomModel: string): string;

    function CreateChatForDriver(const aDriverName, aModel: string): TAiChatConnection;

    function UploadFile(aMediaFile: TAiMediaFile): String;
    function CheckFileState(aMediaFile: TAiMediaFile): String;
    function DeleteFile(aMediaFile: TAiMediaFile): String;
    function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer = 3600): String;

    property Messages: TAiChatMessages read FMessages;
    property LastError: String read GetLastError;
    property Busy: Boolean read GetBusy;
    property AiChat: TAiChat read FChat;

  published
    property DriverName: String read FDriverName write SetDriverName;
    property Model: String read FModel write SetModel;
    property Params: TStrings read FParams write SetParams;
    // property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
    property SystemPrompt: TStrings read FSystemPrompt write SetSystemPrompt;
    property Memory: TStrings read FMemory write SetMemory;
    property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    property Prompt_tokens: integer read FPrompt_tokens write SetPrompt_tokens;
    property Completion_tokens: integer read FCompletion_tokens write SetCompletion_tokens;
    property Total_tokens: integer read FTotal_tokens write SetTotal_tokens;

    Property OnReceiveThinking: TAiChatOnDataEvent read FOnReceiveThinking write SetOnReceiveThinking;
    property OnReceiveData: TAiChatOnDataEvent read FOnReceiveData write SetOnReceiveData;
    property OnReceiveDataEnd: TAiChatOnDataEvent read FOnReceiveDataEnd write SetOnReceiveDataEnd;
    property OnAddMessage: TAiChatOnDataEvent read FOnAddMessage write SetOnAddMessage;
    property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    property OnBeforeSendMessage: TAiChatOnBeforeSendEvent read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    property OnInitChat: TAiChatOnInitChatEvent read FOnInitChat write SetOnInitChat;
    property OnProcessMediaFile: TAiChatOnMediaFileEvent read FOnProcessMediaFile write SetOnProcessMediaFile;
    property OnError: TAiErrorEvent read FOnError write SetOnError;
    property OnChatModelChange: TOnChatModelChangeEvent read FOnChatModelChange write FOnChatModelChange;
    property OnProcessResponse: TAiChatOnProcessResponseEvent read FOnProcessResponse write SetOnProcessResponse;
    Property Version: String Read FVersion;
    property ChatMode: TAiChatMode read FChatMode write SetChatMode default cmConversation;
    property OnStateChange: TAiStateChangeEvent read FOnStateChange write FOnStateChange;
    property SanitizerActive: Boolean read FSanitizerActive write SetSanitizerActive default False;
    property OnSanitize: TAiSanitizeEvent read FOnSanitize write SetOnSanitize;

    property PersistentMemory:  TAiPersistentMemoryBase read FPersistentMemory  write SetPersistentMemory;
    property MemoryTokenBudget: Integer    read FMemoryTokenBudget  write SetMemoryTokenBudget default 1500;
    property AutoStoreMemories: Boolean    read FAutoStoreMemories  write SetAutoStoreMemories default False;

    property TtsParams: TAiTtsParams read FTtsParams write SetTtsParams;
    property TranscriptionParams: TAiTranscriptionParams read FTranscriptionParams write SetTranscriptionParams;
    property ImageParams: TAiImageGenParams read FImageGenParams write SetImageGenParams;
    property VideoParams: TAiVideoGenParams read FVideoGenParams write SetVideoGenParams;
    property WebSearchParams: TAiWebSearchParams read FWebSearchParams write SetWebSearchParams;
    property ModelConfig: TAiModelConfig read FModelConfig write SetModelConfig;

    // v3.5: los atajos raiz (SpeechTool, ImageTool, ...) fueron eliminados.
    // TODAS las herramientas se asignan via ChatTools.XxxTool — misma superficie
    // en TAiChat y TAiChatConnection. El streaming DFM/FMX resuelve las
    // referencias a componentes de sub-objetos TPersistent via fixups.
    property ChatTools: TAiChatTools read FChatTools;

  end;

procedure Register;

implementation

{$I uMakerAi.Version.inc}
{$R ..\Resources\uMakerAiResources.res}

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiChatConnection]);
end;

{ TAiChatConnection }

constructor TAiChatConnection.Create(Sender: TComponent);
begin
  inherited;
  FChat := nil;
  FChatTools := TAiChatTools.Create(Self);
  FChatTools.OnChange := ChatToolsChanged;
  FSystemPrompt := TStringList.Create;
  FMemory := TStringList.Create;
  FMessagesOwn := TAiChatMessages.Create;
  FMessages := FMessagesOwn; // Por defecto, FMessages apunta a nuestra instancia
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := ParamsChanged;
  TStringList(FSystemPrompt).OnChange := ParamsChanged;
  TStringList(FMemory).OnChange := ParamsChanged;
  FVersion := MAKERAI_VERSION_FULL;

  FTtsParams := TAiTtsParams.Create;
  FTranscriptionParams := TAiTranscriptionParams.Create;
  FImageGenParams := TAiImageGenParams.Create;
  FVideoGenParams := TAiVideoGenParams.Create;
  FWebSearchParams := TAiWebSearchParams.Create;
  FModelConfig := TAiModelConfig.Create;
  FModelConfig.OnChange := ModelConfigChanged;

  FPersistentMemory  := nil;
  FMemoryTokenBudget := 1500;
  FAutoStoreMemories := False;
end;

destructor TAiChatConnection.Destroy;
begin
  FreeAndNil(FChat);  // nil before freeing FSystemPrompt/FMemory/FParams (OnChange=ParamsChanged checks FChat)

  FChatTools.Free;
  FSystemPrompt.Free;
  FMemory.Free;

  FTtsParams.Free;
  FTranscriptionParams.Free;
  FImageGenParams.Free;
  FVideoGenParams.Free;
  FWebSearchParams.Free;
  FModelConfig.Free;

  // FMessages es solo una referencia, NO se libera
  FMessagesOwn.Free;
  FParams.Free;
  inherited;
end;

{ procedure TAiChatConnection.SetDriverName(const Value: String);
  begin
  if FDriverName <> Value then
  begin
  FDriverName := Value;
  FModel := ''; // Al cambiar de driver, reseteamos el modelo.
  UpdateAndApplyParams;

  if not(csDesigning in ComponentState) then
  begin
  SetupChatFromDriver;
  end;
  end;
  end;
}

procedure TAiChatConnection.SetDriverName(const Value: String);
begin
  if FDriverName <> Value then
  begin
    FDriverName := Value;
    FModel := '';

    // Solo actualizamos la lista FParams con los defaults del nuevo driver,
    // pero NO los aplicamos al chat viejo si vamos a cambiarlo inmediatamente.

    // Opci�n A (Simple): Dejarlo como est� (funciona, pero aplica params al chat viejo).
    // UpdateAndApplyParams;

    // Opci�n B (Optimizaci�n): Cargar params pero no aplicar al chat viejo.
    if not(csDesigning in ComponentState) then
    begin
      // Cargamos los defaults en FParams sin aplicarlos al FChat actual
      // (Puedes refactorizar UpdateAndApplyParams para aceptar un booleano 'ApplyToChat')
      UpdateAndApplyParams;
      SetupChatFromDriver; // Esto crear� el nuevo chat y le aplicar� los params
    end
    else
    begin
      // En dise�o solo actualizamos params visualmente
      UpdateAndApplyParams;
    end;
  end;
end;

procedure TAiChatConnection.SetModel(const Value: String);
begin
  if FModel <> Value then
  begin
    FModel := Value;
    // Params.Values['model'] := FModel;
    TAiChatFactory.Instance.RegisterUserParam(FDriverName, FModel, 'Model', FModel);

    UpdateAndApplyParams;
  end;
end;

procedure TAiChatConnection.SetParams(const Value: TStrings);
begin
  if Assigned(Value) then
    FParams.Assign(Value);
end;

procedure TAiChatConnection.ParamsChanged(Sender: TObject);
begin
  if FInternalParamsUpdate then
    Exit; // edicion interna (migracion de claves tipadas): no re-aplicar

  if Assigned(FChat) then
  begin
    ApplyParamsToChat(FChat, FParams);
  end;
end;

procedure TAiChatConnection.ModelConfigChanged(Sender: TObject);
begin
  // Propaga cambios hechos via Connection.ModelConfig.Xxx := ... (mutacion directa del
  // sub-objeto) al TAiChat real. SetModelConfig ya cubre la asignacion del objeto completo;
  // esto cubre el camino que antes requeria un SyncModelConfig manual (ver Apps/MakerAIChat).
  if Assigned(FChat) then
    ApplyModelConfigToChat(FChat);
end;

// Copia la configuracion explicita del usuario al chat y completa los campos
// NO fijados con los valores del registry para el driver/modelo actual.
// Este es el UNICO camino por el que ModelCaps/SessionCaps/Tool_Active/
// ThinkingLevel llegan al chat (v3.5: eliminados del canal Params/RTTI).
procedure TAiChatConnection.ApplyModelConfigToChat(AChat: TAiChat);
var
  LRegParams: TStringList;
begin
  if not Assigned(AChat) then
    Exit;

  AChat.ModelConfig.Assign(FModelConfig); // intencion explicita (con marcas UserFields)

  if (FDriverName <> '') and TAiChatFactory.Instance.HasDriver(FDriverName) then
  begin
    LRegParams := TStringList.Create;
    try
      TAiChatFactory.Instance.GetDriverParams(FDriverName, FModel, LRegParams, False);
      AChat.ModelConfig.ApplyAutoParams(LRegParams); // registry rellena lo no fijado
    finally
      LRegParams.Free;
    end;
  end;
end;

// Retira las 4 claves tipadas de una lista de params (p.ej. la vista del registry)
// para que nunca entren a FParams ni pasen por la inyeccion RTTI.
procedure TAiChatConnection.StripModelConfigKeys(AParams: TStrings);
const
  Keys: array [0 .. 3] of string = ('ModelCaps', 'SessionCaps', 'Tool_Active', 'ThinkingLevel');
var
  K: string;
  I: integer;
begin
  if not Assigned(AParams) then
    Exit;
  for K in Keys do
  begin
    I := AParams.IndexOfName(K);
    if I >= 0 then
      AParams.Delete(I);
  end;
end;

// Compatibilidad hacia atras: si el usuario (DFM viejo o codigo) escribio las
// claves tipadas en Params, se interpretan como configuracion EXPLICITA:
// se aplican a ModelConfig, se fijan (UserFields) y se retiran de Params.
// La clave presente fija el campo aunque el valor coincida con el actual
// (ej: agentes escriben SessionCaps=[] para forzar caps vacios).
procedure TAiChatConnection.MigrateModelConfigParams(AParams: TStrings);
var
  I: integer;
  Val: string;
begin
  if not Assigned(AParams) then
    Exit;

  FInternalParamsUpdate := True;
  try
    I := AParams.IndexOfName('ModelCaps');
    if I >= 0 then
    begin
      FModelConfig.ModelCaps := TAiModelConfig.StringToCaps(AParams.ValueFromIndex[I]);
      FModelConfig.UserFields := FModelConfig.UserFields + [mcfModelCaps];
      AParams.Delete(I);
    end;

    I := AParams.IndexOfName('SessionCaps');
    if I >= 0 then
    begin
      FModelConfig.SessionCaps := TAiModelConfig.StringToCaps(AParams.ValueFromIndex[I]);
      FModelConfig.UserFields := FModelConfig.UserFields + [mcfSessionCaps];
      AParams.Delete(I);
    end;

    I := AParams.IndexOfName('Tool_Active');
    if I >= 0 then
    begin
      Val := AParams.ValueFromIndex[I].Trim.ToLower;
      FModelConfig.Tool_Active := (Val = 'true') or (Val = '1') or (Val = 'yes') or (Val = 't');
      FModelConfig.UserFields := FModelConfig.UserFields + [mcfToolActive];
      AParams.Delete(I);
    end;

    I := AParams.IndexOfName('ThinkingLevel');
    if I >= 0 then
    begin
      FModelConfig.ThinkingLevel := TAiModelConfig.StringToThinkingLevel(AParams.ValueFromIndex[I]);
      FModelConfig.UserFields := FModelConfig.UserFields + [mcfThinkingLevel];
      AParams.Delete(I);
    end;
  finally
    FInternalParamsUpdate := False;
  end;
end;

{ procedure TAiChatConnection.SetupChatFromDriver;
  var
  OldChat, NewChat: TAiChat;
  begin

  if csLoading in ComponentState then
  Exit;

  if FDriverName = '' then
  begin
  if Assigned(FChat) then
  FreeAndNil(FChat);
  Exit;
  end;

  OldChat := FChat;
  FChat := nil;

  NewChat := TAiChatFactory.Instance.CreateDriver(FDriverName);
  if not Assigned(NewChat) then
  raise Exception.CreateFmt('Failed to create driver instance for "%s"', [FDriverName]);

  if Assigned(FOnChatModelChange) then
  FOnChatModelChange(Self, OldChat, NewChat);

  ApplyParamsToChat(NewChat, FParams);
  ApplyEventsToChat(NewChat);

  SetChat(NewChat);

  if Assigned(OldChat) then
  OldChat.Free;
  end;
}

procedure TAiChatConnection.SetupChatFromDriver;
var
  OldChat, NewChat: TAiChat;
begin
  if csLoading in ComponentState then
    Exit;

  if FDriverName = '' then
  begin
    if Assigned(FChat) then
      FreeAndNil(FChat);
    Exit;
  end;

  OldChat := FChat;
  FChat := nil; // Desvinculamos temporalmente para evitar efectos secundarios en el Setter

  NewChat := TAiChatFactory.Instance.CreateDriver(FDriverName);
  if not Assigned(NewChat) then
    raise Exception.CreateFmt('Failed to create driver instance for "%s"', [FDriverName]);

  if Assigned(FOnChatModelChange) then
    FOnChatModelChange(Self, OldChat, NewChat);

  // Al llamar a SetChat, este se encargar� de aplicar Params y Eventos
  SetChat(NewChat);

  if Assigned(OldChat) then
    OldChat.Free;
end;

{ procedure TAiChatConnection.UpdateAndApplyParams;
  Var
  LParams: TStringList;
  begin

  if csLoading in ComponentState then
  Exit;

  if FDriverName = '' then
  begin
  FParams.Clear;
  Exit;
  end;

  if TAiChatFactory.Instance.HasDriver(FDriverName) then
  begin
  Var
  ShouldExpand := not(csDesigning in ComponentState);
  LParams := TStringList.Create;
  Try
  TAiChatFactory.Instance.GetDriverParams(FDriverName, FModel, LParams, ShouldExpand);
  MergeParams(LParams, FParams).Text; // Adiciona o actualiza FParams a LParams
  Finally
  LParams.Free;
  End;
  end
  else
  FParams.Clear;

  if Assigned(FChat) then
  begin
  ApplyParamsToChat(FChat, FParams);
  end;
  end;
}

procedure TAiChatConnection.UpdateAndApplyParams;
var
  LRegistryParams: TStringList;
  ShouldExpand: Boolean;
begin
  if csLoading in ComponentState then
    Exit;

  if FDriverName = '' then
  begin
    FParams.Clear;
    Exit;
  end;

  if TAiChatFactory.Instance.HasDriver(FDriverName) then
  begin
    // Seguridad: No expandir claves API en tiempo de dise�o
    ShouldExpand := not(csDesigning in ComponentState);
    LRegistryParams := TStringList.Create;
    try
      // 1. Obtener los par�metros oficiales del registro (Nivel 1, 2 y 3)
      TAiChatFactory.Instance.GetDriverParams(FDriverName, FModel, LRegistryParams, ShouldExpand);

      // v3.5: ModelCaps/SessionCaps/Tool_Active/ThinkingLevel NO viajan por Params.
      // Se retiran de la vista del registro antes del merge; llegan al chat via
      // ApplyModelConfigToChat (canal tipado ModelConfig + ApplyAutoParams).
      StripModelConfigKeys(LRegistryParams);

      // 2. Sincronizaci�n inteligente:
      // En lugar de un Merge simple, vamos a asegurarnos de que FParams refleje
      // la estructura del nuevo modelo.

      FParams.BeginUpdate;
      try
        // OJO: MergeParams actualiza FParams con los valores del registro; las
        // claves que el registro trae para el driver/modelo actual SOBREESCRIBEN
        // lo que hubiera en FParams (necesario para que el cambio de modelo
        // refresque Max_Tokens, ModelCaps, etc. y no queden valores del modelo
        // anterior). Las claves que el registro no conoce se conservan.
        // Para personalizacion durable usar:
        //   - TAiChatFactory.Instance.RegisterUserParam(...)  (nivel usuario del registro)
        //   - Connection.ModelConfig.ModelCaps/SessionCaps    (explicito; ApplyParamsToChat
        //     lo respeta via UserConfigured y el registry ya no lo pisa)
        MergeParams(LRegistryParams, FParams);
      finally
        FParams.EndUpdate;
      end;

    finally
      LRegistryParams.Free;
    end;
  end
  else
    FParams.Clear;

  // 3. Inyectar los par�metros finales en el motor de Chat
  if Assigned(FChat) then
  begin
    ApplyParamsToChat(FChat, FParams);
  end;
end;

procedure TAiChatConnection.UpdateParamsFromRegistry;
begin
  UpdateAndApplyParams;
end;

procedure TAiChatConnection.ValideChat;
begin
  if not Assigned(FChat) and (FDriverName <> '') then
  begin
    SetupChatFromDriver;
  end;

  if not Assigned(FChat) then
    raise Exception.Create('A valid DriverName must be specified to create a Chat instance.');
end;

procedure TAiChatConnection.ApplyParamsToChat(AChat: TAiChat; AParams: TStrings);
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
  I: integer;
  ParamName, ParamValue: string;
  LIntVal: Int64;
  LFloatVal: Double;
begin
  if not Assigned(AChat) then
    Exit;

  // v3.5: si Params trae claves tipadas (DFM viejo o codigo del usuario), se
  // migran a ModelConfig como configuracion explicita y se retiran de Params.
  MigrateModelConfigParams(AParams);

  // 1. ASIGNACIONES DIRECTAS DE ESTRUCTURA (Prioridad v1.5)
  AChat.AiFunctions := Self.AiFunctions;

  // Inyectar el estado del orquestador
  AChat.ChatMode := Self.ChatMode;

  // Inyectar las herramientas (ChatTools)
  AChat.ChatTools.Assign(Self.FChatTools);

  // Inyectar configuración del sanitizador
  AChat.SanitizerActive := Self.FSanitizerActive;

  // Inyectar memoria persistente
  AChat.PersistentMemory  := Self.FPersistentMemory;
  AChat.MemoryTokenBudget := Self.FMemoryTokenBudget;
  AChat.AutoStoreMemories := Self.FAutoStoreMemories;

  // Inyectar sub-objetos de parámetros especiales
  AChat.TtsParams.Assign(Self.FTtsParams);
  AChat.TranscriptionParams.Assign(Self.FTranscriptionParams);
  AChat.ImageParams.Assign(Self.FImageGenParams);
  AChat.VideoParams.Assign(Self.FVideoGenParams);
  AChat.WebSearchParams.Assign(Self.FWebSearchParams);

  // Canal tipado: config explicita del usuario + registry para lo no fijado
  ApplyModelConfigToChat(AChat);

  // Contexto base
  AChat.Memory.Text := Self.Memory.Text;
  AChat.SystemPrompt.Text := Self.SystemPrompt.Text;

  // 2. INYECCI�N DIN�MICA V�A PARAMS (RTTI)
  if not Assigned(AParams) or (AParams.Count <= 0) then
    Exit;

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(AChat.ClassType);

    for I := 0 to AParams.Count - 1 do
    begin
      ParamName := AParams.Names[I];
      ParamValue := AParams.Values[ParamName].Trim;

      if ParamName.IsEmpty then
        Continue;

      // v3.5: las claves tipadas NUNCA se inyectan por RTTI (su unico canal es
      // ModelConfig). Tras StripModelConfigKeys/MigrateModelConfigParams no
      // deberian estar aqui; este skip es la red de seguridad.
      if SameText(ParamName, 'ModelCaps') or SameText(ParamName, 'SessionCaps') or
         SameText(ParamName, 'Tool_Active') or SameText(ParamName, 'ThinkingLevel') then
        Continue;

      // Buscar primero en el objeto principal, luego en sub-objetos TPersistent (2-level RTTI)
      var LTarget: TObject := AChat;
      LProp := LRttiType.GetProperty(ParamName);

      if not Assigned(LProp) then
      begin
        for var LSubPropDef in LRttiType.GetProperties do
        begin
          if LSubPropDef.PropertyType.IsInstance then
          begin
            var LSubObj := LSubPropDef.GetValue(AChat).AsObject;
            if LSubObj is TPersistent then
            begin
              var LSubType := LContext.GetType(LSubObj.ClassType);
              var LSubProp := LSubType.GetProperty(ParamName);
              if Assigned(LSubProp) and LSubProp.IsWritable then
              begin
                LProp := LSubProp;
                LTarget := LSubObj;
                Break;
              end;
            end;
          end;
        end;
      end;

      if Assigned(LProp) and LProp.IsWritable then
      begin
        try
          case LProp.PropertyType.TypeKind of
            tkInteger, tkInt64:
              if TryStrToInt64(ParamValue, LIntVal) then
                LProp.SetValue(LTarget, LIntVal);

            tkFloat:
              if TryStrToFloat(ParamValue, LFloatVal, TFormatSettings.Invariant) or
                 TryStrToFloat(ParamValue, LFloatVal) then
                LProp.SetValue(LTarget, LFloatVal);

            tkString, tkUString, tkWideString:
              LProp.SetValue(LTarget, ParamValue);

            tkEnumeration:
              begin
                if LProp.PropertyType.Handle = TypeInfo(Boolean) then
                  LValue := MatchStr(LowerCase(ParamValue), ['true', '1', 'yes', 't'])
                else
                  LValue := TValue.FromOrdinal(LProp.PropertyType.Handle, GetEnumValue(LProp.PropertyType.Handle, ParamValue));
                LProp.SetValue(LTarget, LValue);
              end;

            tkSet:
              begin
                var
                LSetType := LProp.PropertyType as TRttiSetType;
                if LSetType.ElementType.TypeKind = tkEnumeration then
                begin
                  var
                  LEnumType := LSetType.ElementType;
                  var
                    SetAsInt: NativeInt := 0;

                  if (not ParamValue.IsEmpty) and (ParamValue <> '[]') then
                  begin
                    var
                    CleanValue := ParamValue.Trim(['[', ']', ' ']);
                    var
                    EnumNames := CleanValue.Split([',']);
                    for var EnumName in EnumNames do
                    begin
                      var
                      TrimmedName := Trim(EnumName);
                      if not TrimmedName.IsEmpty then
                      begin
                        // GetEnumValue es sensible a mayúsculas según el Enum definido en uMakerAi.Core
                        var
                        OrdinalValue := GetEnumValue(LEnumType.Handle, TrimmedName);
                        if OrdinalValue >= 0 then
                          SetAsInt := SetAsInt or (1 shl OrdinalValue);
                      end;
                    end;
                  end;
                  TValue.Make(@SetAsInt, LSetType.Handle, LValue);
                  LProp.SetValue(LTarget, LValue);
                end;
              end;

            tkClass:
              begin
                if LProp.PropertyType.QualifiedName.EndsWith('TStrings') then
                begin
                  var
                  LStringsProp := LProp.GetValue(LTarget).AsObject as TStrings;
                  if Assigned(LStringsProp) then
                    LStringsProp.Text := StringReplace(ParamValue, '|', sLineBreak, [rfReplaceAll]);
                end;
              end;
          end;
        except
          // Fallo silencioso por propiedad individual para no detener el resto de la inyección
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TAiChatConnection.ApplyEventsToChat(AChat: TAiChat; SetToNil: Boolean);
begin
  if not Assigned(AChat) then
    Exit;

  if SetToNil then
  begin
    AChat.OnReceiveData := nil;
    AChat.OnReceiveDataEnd := nil;
    AChat.OnReceiveThinking := Nil;
    AChat.OnAddMessage := nil;
    AChat.OnCallToolFunction := nil;
    AChat.OnBeforeSendMessage := nil;
    AChat.OnInitChat := nil;
    AChat.OnProcessMediaFile := nil;
    AChat.OnProcessResponse := nil;
    AChat.OnError := nil;
    AChat.OnStateChange := nil;
    AChat.OnSanitize := nil;

  end
  else
  begin
    AChat.OnReceiveData := Self.OnReceiveData;
    AChat.OnReceiveDataEnd := OnInternalReceiveDataEnd; // Self.OnReceiveDataEnd;
    AChat.OnReceiveThinking := Self.OnReceiveThinking;
    AChat.OnAddMessage := Self.OnAddMessage;
    AChat.OnCallToolFunction := Self.OnCallToolFunction;
    AChat.OnBeforeSendMessage := Self.OnBeforeSendMessage;
    AChat.OnInitChat := Self.OnInitChat;
    AChat.OnProcessMediaFile := Self.OnProcessMediaFile;
    AChat.OnProcessResponse := Self.OnProcessResponse;
    AChat.OnError := Self.OnError;
    AChat.OnStateChange := Self.FOnStateChange;
    AChat.OnSanitize := Self.FOnSanitize;
  end;
end;

procedure TAiChatConnection.ResetParamsToDefaults;
begin
  UpdateAndApplyParams;
end;

procedure TAiChatConnection.RegisterUserParam(const DriverName, ModelName, ParamName, ParamValue: string);
begin
  TAiChatFactory.Instance.RegisterUserParam(DriverName, ModelName, ParamName, ParamValue);
  if (DriverName = FDriverName) and (ModelName = FModel) then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.RegisterUserParam(const DriverName, ParamName, ParamValue: string);
begin
  TAiChatFactory.Instance.RegisterUserParam(DriverName, ParamName, ParamValue);
  if (DriverName = FDriverName) then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.ClearRegisterParams(const DriverName: String; ModelName: string);
begin
  TAiChatFactory.Instance.ClearRegisterParams(DriverName, ModelName);
  if (DriverName = FDriverName) then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.RegisterCustomModel(const DriverName, CustomModelName, ModelBaseName: string);
begin
  TAiChatFactory.Instance.RegisterCustomModel(DriverName, CustomModelName, ModelBaseName);
  // Si estamos usando este driver, refrescamos la lista de modelos disponible
  if DriverName = FDriverName then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.ClearCustomModels(const DriverName: string);
begin
  TAiChatFactory.Instance.ClearCustomModels(DriverName);
  if DriverName = FDriverName then
    UpdateAndApplyParams;
end;

function TAiChatConnection.GetBaseModel(const DriverName, CustomModel: string): string;
begin
  Result := TAiChatFactory.Instance.GetBaseModel(DriverName, CustomModel);
end;

function TAiChatConnection.CreateChatForDriver(const aDriverName, aModel: string): TAiChatConnection;
var
  LChat: TAiChat;
  LModels: TStringList;
begin
  if not IsDriverAvailable(aDriverName) then
    raise Exception.CreateFmt('Driver "%s" not found or is not registered.', [aDriverName]);

  // Valida que el driver y el modelo existan
  LChat := TAiChatFactory.Instance.CreateDriver(aDriverName);
  Try
    if not Assigned(LChat) then
      raise Exception.CreateFmt('Failed to create an instance for driver "%s".', [aDriverName]);

    LModels := LChat.GetModels;
    Try
      LModels.Sort;

      If LModels.IndexOf(aModel) < 0 then
        raise Exception.CreateFmt('Failed to create an instance, model "%s" not found.', [aModel]);

    Finally
      LModels.Free;
    End;

  Finally
    LChat.Free;
  End;
  Result := TAiChatConnection.Create(Self.Owner);
  Result.DriverName := aDriverName;
  Result.Model := aModel;
end;

// --- M�todos de acci�n y fachada ---

procedure TAiChatConnection.Abort;
begin
  ValideChat;
  FChat.Abort;
end;

function TAiChatConnection.AddMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  ValideChat;
  Result := FChat.AddMessage(aPrompt, aRole);
end;

function TAiChatConnection.AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String;
begin
  ValideChat;
  Result := FChat.AddMessageAndRun(aPrompt, aRole, aMediaFiles);
end;

procedure TAiChatConnection.AddToMemory(Key, Value: String);
begin
  ValideChat;
  FMemory.AddPair(Key, Value);
  FChat.AddToMemory(Key, Value);
end;

function TAiChatConnection.CheckFileState(aMediaFile: TAiMediaFile): String;
begin
  ValideChat;
  Result := FChat.CheckFileState(aMediaFile);
end;

function TAiChatConnection.DeleteFile(aMediaFile: TAiMediaFile): String;
begin
  ValideChat;
  Result := FChat.DeleteFile(aMediaFile);
end;

function TAiChatConnection.GetAvailableDrivers: TArray<string>;
begin
  Result := TAiChatFactory.Instance.GetRegisteredDrivers;
end;

class function TAiChatConnection.AvailableDrivers: TArray<string>;
begin
  Result := TAiChatFactory.Instance.GetRegisteredDrivers;
end;

class function TAiChatConnection.DriverNames: TStringList;
var
  D: string;
begin
  Result := TStringList.Create;
  for D in TAiChatFactory.Instance.GetRegisteredDrivers do
    Result.Add(D);
end;

function TAiChatConnection.GetBusy: Boolean;
begin
  if Assigned(FChat) then
    Result := FChat.Busy
  else
    Result := False;
end;

function TAiChatConnection.GetDriversNames: TStringList;
var
  AvailableDrivers: TArray<string>;
  DriverName: string;
begin
  Result := TStringList.Create;
  AvailableDrivers := TAiChatFactory.Instance.GetRegisteredDrivers;
  for DriverName in AvailableDrivers do
    Result.Add(DriverName);
end;

function TAiChatConnection.GetLastError: String;
begin
  if Assigned(FChat) then
    Result := FChat.LastError
  else
    Result := '';
end;

function TAiChatConnection.GetLastMessage: TAiChatMessage;
begin
  ValideChat;
  Result := FChat.GetLastMessage;
end;

function TAiChatConnection.GetMessages: TJSonArray;
begin
  ValideChat;
  Result := FChat.GetMessages;
end;

function TAiChatConnection.GetModels: TStringList;
begin
  ValideChat;
  Result := FChat.GetModels;
end;

function TAiChatConnection.IsDriverAvailable(const DriverName: string): Boolean;
begin
  Result := TAiChatFactory.Instance.HasDriver(DriverName);
end;

procedure TAiChatConnection.Loaded;
begin
  inherited;
  SetupChatFromDriver;
end;

function TAiChatConnection.MergeParams(Origin, Destination: TStrings): TStrings;
var
  I: integer;
begin
  // Origin (registro) manda: actualiza la clave si existe o la agrega si no,
  // sin duplicarla. Destination conserva solo las claves que Origin no trae.
  Result := Destination;
  for I := 0 to Origin.Count - 1 do
  begin
    Destination.Values[Origin.Names[I]] := Origin.ValueFromIndex[I];
  end;
end;

procedure TAiChatConnection.NewChat;
begin
  ValideChat;
  FChat.NewChat;
end;

function TAiChatConnection.NewMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  ValideChat;
  Result := FChat.NewMessage(aPrompt, aRole);
end;

procedure TAiChatConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    // 1. Limpiamos las referencias de las herramientas en la Conexion
    FChatTools.Notification(AComponent, Operation);
    if AComponent = FAiFunctions then
      FAiFunctions := nil;

    // 2. IMPORTANTE: Si hay un chat activo, sincronizamos el 'nil'
    // para evitar que el Chat principal intente usar un objeto destruido.
    if Assigned(FChat) then
    begin
      FChat.ChatTools.Notification(AComponent, Operation);
      if AComponent = FAiFunctions then
        FChat.AiFunctions := nil;
    end;

    if AComponent = FPersistentMemory then
    begin
      FPersistentMemory := nil;
      if Assigned(FChat) then
        FChat.PersistentMemory := nil;
    end;
  end;
end;

procedure TAiChatConnection.OnInternalReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: String);
begin
  if Assigned(aMsg) then
  begin
    Prompt_tokens := Prompt_tokens + aMsg.Prompt_tokens;
    Completion_tokens := Completion_tokens + aMsg.Completion_tokens;
    Total_tokens := Total_tokens + aMsg.Total_tokens;
  end;

  If Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Sender, aMsg, aResponse, aRole, aText);
end;

procedure TAiChatConnection.RemoveFromMemory(Key: String);
begin
  ValideChat;
  FMemory.Values[Key] := '';
  FChat.RemoveFromMemory(Key);
end;

function TAiChatConnection.RemoveMesage(Msg: TAiChatMessage): Boolean;
begin
  ValideChat;
  Result := FChat.RemoveMesage(Msg);
end;

function TAiChatConnection.RemoveMesage(IdMsg: integer): Boolean;
begin
  ValideChat;
  Result := FChat.RemoveMesage(IdMsg);
end;

function TAiChatConnection.Run(aMsg: TAiChatMessage = nil): String;
begin
  ValideChat;
  Result := FChat.Run(aMsg, nil)
end;

function TAiChatConnection.UploadFile(aMediaFile: TAiMediaFile): String;
begin
  ValideChat;
  Result := FChat.UploadFile(aMediaFile);
end;

function TAiChatConnection.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: integer): String;
begin
  ValideChat;
  Result := FChat.UploadFileToCache(aMediaFile, aTTL_Seconds);
end;

// --- Setters de Propiedades y Eventos ---

procedure TAiChatConnection.SetChat(const Value: TAiChat);
begin
  if FChat <> Value then
  begin
    // Si hab�a un chat anterior, aplicar configuraci�n
    if Assigned(FChat) then
    Begin
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);
    End;

    // Asignar el nuevo chat
    FChat := Value;

    if Assigned(FChat) then
    begin
      // Aplicar eventos y par�metros al nuevo chat
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);

      // * CORRECCI�N: Apuntar FMessages a los mensajes del chat
      // (NO creamos ni liberamos nada, solo cambiamos la referencia)
      FMessages := FChat.Messages;
    end
    else
    begin
      // * CORRECCI�N: Si no hay chat, volver a usar nuestra instancia propia
      // (NO creamos una nueva, usamos FMessagesOwn que ya existe)
      FMessages := FMessagesOwn;
    end;
  end;
end;

procedure TAiChatConnection.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChatConnection.SetMemory(const Value: TStrings);
begin
  FMemory.Assign(Value);
  if Assigned(FChat) then
    FChat.Memory.Assign(Value);
end;

procedure TAiChatConnection.SetOnAddMessage(const Value: TAiChatOnDataEvent);
begin
  FOnAddMessage := Value;
  if Assigned(FChat) then
    FChat.OnAddMessage := Value;
end;

procedure TAiChatConnection.SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;
  if Assigned(FChat) then
    FChat.OnBeforeSendMessage := Value;
end;

procedure TAiChatConnection.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
  if Assigned(FChat) then
    FChat.OnCallToolFunction := Value;
end;

procedure TAiChatConnection.SetOnError(const Value: TAiErrorEvent);
begin
  FOnError := Value;
  if Assigned(FChat) then
    FChat.OnError := Value;
end;

procedure TAiChatConnection.SetOnInitChat(const Value: TAiChatOnInitChatEvent);
begin
  FOnInitChat := Value;
  if Assigned(FChat) then
    FChat.OnInitChat := Value;
end;

procedure TAiChatConnection.SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
begin
  FOnProcessMediaFile := Value;
  if Assigned(FChat) then
    FChat.OnProcessMediaFile := Value;
end;

procedure TAiChatConnection.SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
begin
  FOnProcessResponse := Value;
  if Assigned(FChat) then
    FChat.OnProcessResponse := Value;
end;

procedure TAiChatConnection.SetOnReceiveData(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveData := Value;
  if Assigned(FChat) then
    FChat.OnReceiveData := Value;
end;

procedure TAiChatConnection.SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveDataEnd := Value;
  // if Assigned(FChat) then
  // FChat.OnReceiveDataEnd := Value;
end;

procedure TAiChatConnection.SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveThinking := Value;
  if Assigned(FChat) then
    FChat.OnReceiveThinking := Value;
end;

procedure TAiChatConnection.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChatConnection.SetSystemPrompt(const Value: TStrings);
begin
  FSystemPrompt.Assign(Value);
  if Assigned(FChat) then
    FChat.SystemPrompt.Assign(Value);
end;

procedure TAiChatConnection.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
end;

// -------------------------

procedure TAiChatConnection.SetChatMode(const Value: TAiChatMode);
begin
  if FChatMode <> Value then
  begin
    FChatMode := Value;
    if Assigned(FChat) then
      FChat.ChatMode := Value;
  end;
end;

procedure TAiChatConnection.SetAiFunctions(const Value: TAiFunctions);
begin
  if FAiFunctions <> Value then
  begin
    FAiFunctions := Value;
    if FAiFunctions <> nil then
      FAiFunctions.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.AiFunctions := Value;
  end;
end;

procedure TAiChatConnection.SetSanitizerActive(const Value: Boolean);
begin
  FSanitizerActive := Value;
  if Assigned(FChat) then
    FChat.SanitizerActive := Value;
end;

procedure TAiChatConnection.SetOnSanitize(const Value: TAiSanitizeEvent);
begin
  FOnSanitize := Value;
  if Assigned(FChat) then
    FChat.OnSanitize := Value;
end;

procedure TAiChatConnection.SetPersistentMemory(const Value: TAiPersistentMemoryBase);
begin
  if FPersistentMemory = Value then Exit;
  if Assigned(FPersistentMemory) then
    FPersistentMemory.RemoveFreeNotification(Self);
  FPersistentMemory := Value;
  if Assigned(FPersistentMemory) then
    FPersistentMemory.FreeNotification(Self);
  if Assigned(FChat) then
    FChat.PersistentMemory := Value;
end;

procedure TAiChatConnection.SetMemoryTokenBudget(const Value: Integer);
begin
  FMemoryTokenBudget := Value;
  if Assigned(FChat) then
    FChat.MemoryTokenBudget := Value;
end;

procedure TAiChatConnection.SetAutoStoreMemories(const Value: Boolean);
begin
  FAutoStoreMemories := Value;
  if Assigned(FChat) then
    FChat.AutoStoreMemories := Value;
end;

procedure TAiChatConnection.SetTtsParams(const Value: TAiTtsParams);
begin
  FTtsParams.Assign(Value);
  if Assigned(FChat) then
    FChat.TtsParams.Assign(Value);
end;

procedure TAiChatConnection.SetTranscriptionParams(const Value: TAiTranscriptionParams);
begin
  FTranscriptionParams.Assign(Value);
  if Assigned(FChat) then
    FChat.TranscriptionParams.Assign(Value);
end;

procedure TAiChatConnection.SetImageGenParams(const Value: TAiImageGenParams);
begin
  FImageGenParams.Assign(Value);
  if Assigned(FChat) then
    FChat.ImageParams.Assign(Value);
end;

procedure TAiChatConnection.SetVideoGenParams(const Value: TAiVideoGenParams);
begin
  FVideoGenParams.Assign(Value);
  if Assigned(FChat) then
    FChat.VideoParams.Assign(Value);
end;

procedure TAiChatConnection.SetWebSearchParams(const Value: TAiWebSearchParams);
begin
  FWebSearchParams.Assign(Value);
  if Assigned(FChat) then
    FChat.WebSearchParams.Assign(Value);
end;

procedure TAiChatConnection.SetModelConfig(const Value: TAiModelConfig);
begin
  FModelConfig.Assign(Value);
  if Assigned(FChat) then
    FChat.ModelConfig.Assign(Value);
end;

// Propaga mutaciones de Connection.ChatTools.XxxTool al TAiChat vivo
// (mismo patron que ModelConfigChanged). FChatTools.SetXxxTool ya llama
// Value.FreeNotification(FOwner) y Notification gestiona opRemove.
procedure TAiChatConnection.ChatToolsChanged(Sender: TObject);
begin
  if Assigned(FChat) then
    FChat.ChatTools.Assign(FChatTools);
end;

// ISSUE #115: API comoda para el log de depuracion (opt-in).
procedure TAiChatConnection.EnableDebugLog(const APath: string = '');
begin
  MakerAiDebugLogPath := APath;   // vacio = <TEMP>\makerai_debug.log
  MakerAiDebugLogEnabled := True;
end;

procedure TAiChatConnection.DisableDebugLog;
begin
  MakerAiDebugLogEnabled := False;
end;

function TAiChatConnection.IsDebugLogEnabled: Boolean;
begin
  Result := MakerAiDebugLogEnabled;
end;

end.
