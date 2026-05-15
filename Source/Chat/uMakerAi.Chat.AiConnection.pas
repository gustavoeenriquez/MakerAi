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
  uMakerAi.Tools.Shell, uMakerAi.Tools.TextEditor, uMakerAi.Tools.ComputerUse, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages;

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

    // Setters y Getters
    procedure SetDriverName(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetParams(const Value: TStrings);
    procedure SetChat(const Value: TAiChat);
    function GetLastError: String;
    function GetBusy: Boolean;
    procedure ParamsChanged(Sender: TObject);

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
    procedure SetTtsParams(const Value: TAiTtsParams);
    procedure SetTranscriptionParams(const Value: TAiTranscriptionParams);
    procedure SetImageGenParams(const Value: TAiImageGenParams);
    procedure SetVideoGenParams(const Value: TAiVideoGenParams);
    procedure SetWebSearchParams(const Value: TAiWebSearchParams);
    procedure SetModelConfig(const Value: TAiModelConfig);

    // Atajos directos para ChatTools — permiten asignar en el IDE sin necesidad
    // de código en el formulario. Las propiedades directas de TComponent resuelven
    // referencias forward en el DFM/FMX correctamente; ChatTools.XxxTool (sub-objeto
    // TPersistent) no garantiza la resolución de referencias forward.
    procedure SetSpeechTool(const Value: TAiSpeechToolBase);
    function  GetSpeechTool: TAiSpeechToolBase;
    procedure SetImageTool(const Value: TAiImageToolBase);
    function  GetImageTool: TAiImageToolBase;
    procedure SetVisionTool(const Value: TAiVisionToolBase);
    function  GetVisionTool: TAiVisionToolBase;
    procedure SetVideoTool(const Value: TAiVideoToolBase);
    function  GetVideoTool: TAiVideoToolBase;
    procedure SetPdfTool(const Value: TAiPdfToolBase);
    function  GetPdfTool: TAiPdfToolBase;
    procedure SetWebSearchTool(const Value: TAiWebSearchToolBase);
    function  GetWebSearchTool: TAiWebSearchToolBase;
    procedure SetReportTool(const Value: TAiReportToolBase);
    function  GetReportTool: TAiReportToolBase;
    procedure SetShellTool(const Value: TAiShell);
    function  GetShellTool: TAiShell;
    procedure SetTextEditorTool(const Value: TAiTextEditorTool);
    function  GetTextEditorTool: TAiTextEditorTool;
    procedure SetComputerUseTool(const Value: TAiComputerUseTool);
    function  GetComputerUseTool: TAiComputerUseTool;

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
    function GetMessages: TJSonArray; virtual;
    function GetDriversNames: TStringList; virtual;
    function GetAvailableDrivers: TArray<string>;
    function GetModels: TStringList; overload; virtual;
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

    property TtsParams: TAiTtsParams read FTtsParams write SetTtsParams;
    property TranscriptionParams: TAiTranscriptionParams read FTranscriptionParams write SetTranscriptionParams;
    property ImageParams: TAiImageGenParams read FImageGenParams write SetImageGenParams;
    property VideoParams: TAiVideoGenParams read FVideoGenParams write SetVideoGenParams;
    property WebSearchParams: TAiWebSearchParams read FWebSearchParams write SetWebSearchParams;
    property ModelConfig: TAiModelConfig read FModelConfig write SetModelConfig;

    // Atajos directos para ChatTools. Equivalentes a ChatTools.XxxTool pero
    // resuelven referencias forward en DFM/FMX — basta asignarlos en el IDE.
    property SpeechTool:       TAiSpeechToolBase    read GetSpeechTool      write SetSpeechTool;
    property ImageTool:        TAiImageToolBase     read GetImageTool       write SetImageTool;
    property VisionTool:       TAiVisionToolBase    read GetVisionTool      write SetVisionTool;
    property VideoTool:        TAiVideoToolBase     read GetVideoTool       write SetVideoTool;
    property PdfTool:          TAiPdfToolBase       read GetPdfTool         write SetPdfTool;
    property WebSearchTool:    TAiWebSearchToolBase read GetWebSearchTool   write SetWebSearchTool;
    property ReportTool:       TAiReportToolBase    read GetReportTool      write SetReportTool;
    property ShellTool:        TAiShell             read GetShellTool       write SetShellTool;
    property TextEditorTool:   TAiTextEditorTool    read GetTextEditorTool  write SetTextEditorTool;
    property ComputerUseTool:  TAiComputerUseTool   read GetComputerUseTool write SetComputerUseTool;

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
  if Assigned(FChat) then
  begin
    ApplyParamsToChat(FChat, FParams);
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

      // 2. Sincronizaci�n inteligente:
      // En lugar de un Merge simple, vamos a asegurarnos de que FParams refleje
      // la estructura del nuevo modelo.

      FParams.BeginUpdate;
      try
        // Si quieres que el Registro sea la fuente de verdad absoluta al cambiar de modelo:
        // FParams.Assign(LRegistryParams);

        // Si prefieres mantener lo que el usuario escribi� en el Object Inspector
        // pero inyectar lo nuevo del registro:
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

  // 1. ASIGNACIONES DIRECTAS DE ESTRUCTURA (Prioridad v1.5)
  AChat.AiFunctions := Self.AiFunctions;

  // Inyectar el estado del orquestador
  AChat.ChatMode := Self.ChatMode;

  // Inyectar las herramientas (ChatTools)
  AChat.ChatTools.Assign(Self.FChatTools);

  // Inyectar configuración del sanitizador
  AChat.SanitizerActive := Self.FSanitizerActive;

  // Inyectar sub-objetos de parámetros especiales
  AChat.TtsParams.Assign(Self.FTtsParams);
  AChat.TranscriptionParams.Assign(Self.FTranscriptionParams);
  AChat.ImageParams.Assign(Self.FImageGenParams);
  AChat.VideoParams.Assign(Self.FVideoGenParams);
  AChat.WebSearchParams.Assign(Self.FWebSearchParams);
  AChat.ModelConfig.Assign(Self.FModelConfig);

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
              if TryStrToFloat(ParamValue, LFloatVal) then
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
  Result := Destination;
  for I := 0 to Origin.Count - 1 do
  begin
    // Esto actualiza si existe o a�ade si no existe, sin duplicar la clave
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
  end;
end;

procedure TAiChatConnection.OnInternalReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: String);
begin

  Prompt_tokens := Prompt_tokens + aMsg.Prompt_tokens;
  Completion_tokens := Completion_tokens + aMsg.Completion_tokens;
  Total_tokens := Total_tokens + aMsg.Total_tokens;

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

// ---------------------------------------------------------------------------
// Atajos directos para ChatTools
// FChatTools.SetXxxTool ya llama Value.FreeNotification(FOwner) internamente,
// y TAiChatConnection.Notification ya gestiona opRemove sobre FChatTools.
// ---------------------------------------------------------------------------

function TAiChatConnection.GetSpeechTool: TAiSpeechToolBase;
begin Result := FChatTools.SpeechTool; end;

procedure TAiChatConnection.SetSpeechTool(const Value: TAiSpeechToolBase);
begin
  FChatTools.SpeechTool := Value;
  if Assigned(FChat) then FChat.ChatTools.SpeechTool := Value;
end;

function TAiChatConnection.GetImageTool: TAiImageToolBase;
begin Result := FChatTools.ImageTool; end;

procedure TAiChatConnection.SetImageTool(const Value: TAiImageToolBase);
begin
  FChatTools.ImageTool := Value;
  if Assigned(FChat) then FChat.ChatTools.ImageTool := Value;
end;

function TAiChatConnection.GetVisionTool: TAiVisionToolBase;
begin Result := FChatTools.VisionTool; end;

procedure TAiChatConnection.SetVisionTool(const Value: TAiVisionToolBase);
begin
  FChatTools.VisionTool := Value;
  if Assigned(FChat) then FChat.ChatTools.VisionTool := Value;
end;

function TAiChatConnection.GetVideoTool: TAiVideoToolBase;
begin Result := FChatTools.VideoTool; end;

procedure TAiChatConnection.SetVideoTool(const Value: TAiVideoToolBase);
begin
  FChatTools.VideoTool := Value;
  if Assigned(FChat) then FChat.ChatTools.VideoTool := Value;
end;

function TAiChatConnection.GetPdfTool: TAiPdfToolBase;
begin Result := FChatTools.PdfTool; end;

procedure TAiChatConnection.SetPdfTool(const Value: TAiPdfToolBase);
begin
  FChatTools.PdfTool := Value;
  if Assigned(FChat) then FChat.ChatTools.PdfTool := Value;
end;

function TAiChatConnection.GetWebSearchTool: TAiWebSearchToolBase;
begin Result := FChatTools.WebSearchTool; end;

procedure TAiChatConnection.SetWebSearchTool(const Value: TAiWebSearchToolBase);
begin
  FChatTools.WebSearchTool := Value;
  if Assigned(FChat) then FChat.ChatTools.WebSearchTool := Value;
end;

function TAiChatConnection.GetReportTool: TAiReportToolBase;
begin Result := FChatTools.ReportTool; end;

procedure TAiChatConnection.SetReportTool(const Value: TAiReportToolBase);
begin
  FChatTools.ReportTool := Value;
  if Assigned(FChat) then FChat.ChatTools.ReportTool := Value;
end;

function TAiChatConnection.GetShellTool: TAiShell;
begin Result := FChatTools.ShellTool; end;

procedure TAiChatConnection.SetShellTool(const Value: TAiShell);
begin
  FChatTools.ShellTool := Value;
  if Assigned(FChat) then FChat.ChatTools.ShellTool := Value;
end;

function TAiChatConnection.GetTextEditorTool: TAiTextEditorTool;
begin Result := FChatTools.TextEditorTool; end;

procedure TAiChatConnection.SetTextEditorTool(const Value: TAiTextEditorTool);
begin
  FChatTools.TextEditorTool := Value;
  if Assigned(FChat) then FChat.ChatTools.TextEditorTool := Value;
end;

function TAiChatConnection.GetComputerUseTool: TAiComputerUseTool;
begin Result := FChatTools.ComputerUseTool; end;

procedure TAiChatConnection.SetComputerUseTool(const Value: TAiComputerUseTool);
begin
  FChatTools.ComputerUseTool := Value;
  if Assigned(FChat) then FChat.ChatTools.ComputerUseTool := Value;
end;

end.
