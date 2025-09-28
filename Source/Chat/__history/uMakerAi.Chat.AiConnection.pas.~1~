unit uMakerAi.Chat.AiConnection;

// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

interface



uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding, System.Rtti, System.TypInfo, System.StrUtils,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON,
  uMakerAi.ParamsRegistry, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Chat, uMakerAi.Chat.Initializations;

type
  TOnChatModelChangeEvent = procedure(Sender: TObject; const OldChat, NewChat: TAiChat) of object;

  TAiChatConnection = class(TComponent)
  private
    FChat: TAiChat;
    FDriverName: String;
    FModel: String;
    FParams: TStrings;
    FMessages: TAiChatMessages;
    FInitialInstructions: TStrings;
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

    // Setters y Getters
    procedure SetDriverName(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetParams(const Value: TStrings);
    procedure SetChat(const Value: TAiChat);
    function GetLastError: String;
    function GetBusy: Boolean;
    procedure ParamsChanged(Sender: TObject);

    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetCompletion_tokens(const Value: integer);
    procedure SetInitialInstructions(const Value: TStrings);
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

  protected
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
    function AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String; overload;
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
    property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
    property Memory: TStrings read FMemory write SetMemory;
    property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    property Prompt_tokens: integer read FPrompt_tokens write SetPrompt_tokens;
    property Completion_tokens: integer read FCompletion_tokens write SetCompletion_tokens;
    property Total_tokens: integer read FTotal_tokens write SetTotal_tokens;

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
    Property Version : String Read FVersion;
  end;

procedure Register;

implementation

{$I uMakerAi.Version.inc}


procedure Register;
begin
  RegisterComponents('MakerAI', [TAiChatConnection]);
end;

{ TAiChatConnection }

constructor TAiChatConnection.Create(Sender: TComponent);
begin
  inherited;
  FChat := nil;
  FInitialInstructions := TStringList.Create;
  FMemory := TStringList.Create;
  FMessages := TAiChatMessages.Create;
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := ParamsChanged;
  TStringList(FInitialInstructions).OnChange := ParamsChanged;
  TStringList(FMemory).OnChange := ParamsChanged;
  FVersion := MAKERAI_VERSION_FULL;
end;

destructor TAiChatConnection.Destroy;
begin
  if Assigned(FChat) then
    FChat.Free;
  FInitialInstructions.Free;
  FMemory.Free;
  FMessages.Free;
  FParams.Free;
  inherited;
end;

procedure TAiChatConnection.SetDriverName(const Value: String);
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

procedure TAiChatConnection.SetupChatFromDriver;
var
  OldChat, NewChat: TAiChat;
  LParams: TStringList;
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

procedure TAiChatConnection.UpdateAndApplyParams;
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
begin
  if not Assigned(AChat) then
    Exit;

  // Asignaciones directas primero
  AChat.AiFunctions := Self.AiFunctions;
  AChat.Memory.Text := Self.Memory.Text;
  AChat.InitialInstructions.Text := Self.InitialInstructions.Text;

  if not Assigned(AParams) or (AParams.Count <= 0) then
    Exit;

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(AChat.ClassType);

    for I := 0 to AParams.Count - 1 do
    begin
      ParamName := AParams.Names[I];
      ParamValue := AParams.Values[ParamName];

      LProp := LRttiType.GetProperty(ParamName);

      if Assigned(LProp) and LProp.IsWritable then
      begin
        try
          // --- ESTRUCTURA REFACTORIZADA ---
          // Cada rama ahora es completamente responsable de su lógica,
          // incluyendo la llamada a SetValue.
          case LProp.PropertyType.TypeKind of
            tkInteger, tkInt64:
              begin
                LValue := StrToInt64(ParamValue);
                LProp.SetValue(AChat, LValue);
              end;
            tkFloat:
              begin
                LValue := TValue.From<Double>(StrToFloat(ParamValue));
                LProp.SetValue(AChat, LValue);
              end;
            tkString, tkUString, tkWideString:
              begin
                LValue := ParamValue;
                LProp.SetValue(AChat, LValue);
              end;
            tkEnumeration:
              begin
                if LProp.PropertyType.Handle = TypeInfo(Boolean) then
                  LValue := AnsiSameText(ParamValue, 'true') or (ParamValue = '1')
                else
                  LValue := TValue.FromOrdinal(LProp.PropertyType.Handle, GetEnumValue(LProp.PropertyType.Handle, ParamValue));
                LProp.SetValue(AChat, LValue);
              end;
            tkSet:
              begin
                // La lógica para 'tkSet' ya era auto-contenida y no necesita SetValue.
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
                        try
                          var
                          OrdinalValue := GetEnumValue(LEnumType.Handle, TrimmedName);
                          if OrdinalValue >= 0 then
                            SetAsInt := SetAsInt or (1 shl OrdinalValue);
                        except
                          // Ignorar valores inválidos
                        end;
                      end;
                    end;
                  end;
                  TValue.Make(@SetAsInt, LSetType.Handle, LValue);
                  // La llamada a SetValue se hace con el TValue recién creado.
                  LProp.SetValue(AChat, LValue);
                end;
              end;
            tkClass:
              begin
                // La lógica para 'tkClass' es especial. Modifica el objeto existente.
                // Usamos EndsText para ser más flexibles (acepta TStrings, TStringList, etc.)
                if LProp.PropertyType.QualifiedName.EndsWith('TStrings') then
                begin
                  var
                  LStringsProp := LProp.GetValue(AChat).AsObject as TStrings;
                  if Assigned(LStringsProp) then
                  begin
                    // Modificamos el contenido del objeto directamente. No se necesita SetValue.
                    LStringsProp.Text := StringReplace(ParamValue, '|', sLineBreak, [rfReplaceAll]);
                  end;
                end;
              end;
          end; // Fin del case
        except
          // on E: Exception do // Log error si es necesario
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
    AChat.OnAddMessage := nil;
    AChat.OnCallToolFunction := nil;
    AChat.OnBeforeSendMessage := nil;
    AChat.OnInitChat := nil;
    AChat.OnProcessMediaFile := nil;
    AChat.OnProcessResponse := nil;
    AChat.OnError := nil;
  end
  else
  begin
    AChat.OnReceiveData := Self.OnReceiveData;
    AChat.OnReceiveDataEnd := OnInternalReceiveDataEnd; // Self.OnReceiveDataEnd;
    AChat.OnAddMessage := Self.OnAddMessage;
    AChat.OnCallToolFunction := Self.OnCallToolFunction;
    AChat.OnBeforeSendMessage := Self.OnBeforeSendMessage;
    AChat.OnInitChat := Self.OnInitChat;
    AChat.OnProcessMediaFile := Self.OnProcessMediaFile;
    AChat.OnProcessResponse := Self.OnProcessResponse;
    AChat.OnError := Self.OnError;
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

function TAiChatConnection.CreateChatForDriver(const aDriverName, aModel: string): TAiChatConnection;
var
  LParams: TStringList;
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

// --- Métodos de acción y fachada ---

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

function TAiChatConnection.AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
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
Var
  Name, Value: String;
  I: integer;
begin
  Result := Destination;

  For I := 0 to Origin.Count - 1 do
  Begin
    Name := Origin.Names[I];
    Value := Origin.Values[Name];
    Result.Values[Name] := Value;
  End;

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

procedure TAiChatConnection.OnInternalReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject;
  aRole, aText: String);
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

procedure TAiChatConnection.SetAiFunctions(const Value: TAiFunctions);
begin
  FAiFunctions := Value;
  if Assigned(FChat) then
    FChat.AiFunctions := Value;
end;

procedure TAiChatConnection.SetChat(const Value: TAiChat);
begin
  if FChat <> Value then
  begin
    if Assigned(FChat) then
    Begin
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);
    End;

    FChat := Value;

    if Assigned(FChat) then
    begin
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);
      FMessages := FChat.Messages; // Sincronizar la lista de mensajes
    end
    else
    begin
      FMessages := TAiChatMessages.Create; // Crear una lista vacía si no hay chat
    end;
  end;
end;

procedure TAiChatConnection.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChatConnection.SetInitialInstructions(const Value: TStrings);
begin
  FInitialInstructions.Assign(Value);
  if Assigned(FChat) then
    FChat.InitialInstructions.Assign(Value);
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

procedure TAiChatConnection.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChatConnection.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
end;

end.
