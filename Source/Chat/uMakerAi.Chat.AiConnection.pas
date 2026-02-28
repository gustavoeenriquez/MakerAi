// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Adaptaciones respecto a la version Delphi:
//   - System.Rtti (TRttiContext, TRttiType, TRttiProperty, TValue) →
//       TypInfo (GetPropInfo, SetOrdProp, SetFloatProp, SetStrProp, SetInt64Prop)
//   - tkBool separado de tkEnumeration en FPC (Delphi pone Boolean como tkEnumeration)
//   - tkAString para AnsiString (Delphi usa tkString/tkUString)
//   - TArray<string> → TAiStringArray = array of string (de UMakerAi.ParamsRegistry)
//   - var x := ... (inline vars) → declaraciones en var block
//   - string.IsEmpty / string.Trim / etc → Length(), Trim() de SysUtils
//   - TJSonArray → TJSONArray (nomenclatura fpjson)
//   - {$R ..\Resources\...res} → eliminado (no hay recursos en FPC port)
//   - Register (componente IDE) → omitido (FPC port sin componentes VCL/FMX)
//   - FChat.RemoveMesage (typo Delphi, 1 's') → FChat.RemoveMessage (2 's')

unit uMakerAi.Chat.AiConnection;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo,
  fpjson,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Tools.ComputerUse,
  uMakerAi.Chat.Initializations,
  UMakerAi.ParamsRegistry;

type
  TOnChatModelChangeEvent = procedure(Sender: TObject;
      const OldChat, NewChat: TAiChat) of object;

  TAiChatConnection = class(TComponent)
  private
    FChat              : TAiChat;
    FDriverName        : string;
    FModel             : string;
    FParams            : TStrings;
    FMessages          : TAiChatMessages;
    FMessagesOwn       : TAiChatMessages;
    FMemory            : TStrings;
    FSystemPrompt      : TStrings;
    FAiFunctions       : TAiFunctions;
    FPrompt_tokens     : Integer;
    FCompletion_tokens : Integer;
    FTotal_tokens      : Integer;
    FVersion           : string;
    FChatMode          : TAiChatMode;

    // Tool references
    FShellTool         : TAiShell;
    FTextEditorTool    : TAiTextEditorTool;
    FComputerUseTool   : TAiComputerUseTool;
    FSpeechTool        : TAiSpeechToolBase;
    FImageTool         : TAiImageToolBase;
    FVideoTool         : TAiVideoToolBase;
    FWebSearchTool     : TAiWebSearchToolBase;
    FVisionTool        : TAiVisionToolBase;
    FPdfTool           : TAiPdfToolBase;
    FReportTool        : TAiReportToolBase;

    // Eventos
    FOnReceiveData      : TAiChatOnDataEvent;
    FOnReceiveDataEnd   : TAiChatOnDataEvent;
    FOnAddMessage       : TAiChatOnDataEvent;
    FOnReceiveThinking  : TAiChatOnDataEvent;
    FOnCallToolFunction : TOnCallToolFunction;
    FOnBeforeSendMessage: TAiChatOnBeforeSendEvent;
    FOnInitChat         : TAiChatOnInitChatEvent;
    FOnProcessMediaFile : TAiChatOnMediaFileEvent;
    FOnProcessResponse  : TAiChatOnProcessResponseEvent;
    FOnError            : TAiErrorEvent;
    FOnChatModelChange  : TOnChatModelChangeEvent;
    FOnStateChange      : TAiStateChangeEvent;

    // Setters / Getters
    procedure SetDriverName(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetParams(const Value: TStrings);
    procedure SetChat(const Value: TAiChat);
    function  GetLastError: string;
    function  GetBusy: Boolean;
    procedure ParamsChanged(Sender: TObject);
    procedure SetPrompt_tokens(const Value: Integer);
    procedure SetCompletion_tokens(const Value: Integer);
    procedure SetTotal_tokens(const Value: Integer);
    procedure SetMemory(const Value: TStrings);
    procedure SetSystemPrompt(const Value: TStrings);
    procedure SetChatMode(const Value: TAiChatMode);
    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetShellTool(const Value: TAiShell);
    procedure SetTextEditorTool(const Value: TAiTextEditorTool);
    procedure SetComputerUseTool(const Value: TAiComputerUseTool);
    procedure SetSpeechTool(const Value: TAiSpeechToolBase);
    procedure SetImageTool(const Value: TAiImageToolBase);
    procedure SetVideoTool(const Value: TAiVideoToolBase);
    procedure SetWebSearchTool(const Value: TAiWebSearchToolBase);
    procedure SetVisionTool(const Value: TAiVisionToolBase);
    procedure SetPdfTool(const Value: TAiPdfToolBase);
    procedure SetReportTool(const Value: TAiReportToolBase);
    procedure SetOnReceiveData(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
    procedure SetOnAddMessage(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
    procedure SetOnInitChat(const Value: TAiChatOnInitChatEvent);
    procedure SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
    procedure SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
    procedure SetOnError(const Value: TAiErrorEvent);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure ValideChat;
    procedure UpdateAndApplyParams;
    procedure SetupChatFromDriver;
    procedure ApplyParamsToChat(AChat: TAiChat; AParams: TStrings);
    procedure ApplyEventsToChat(AChat: TAiChat; SetToNil: Boolean = False);
    procedure OnInternalReceiveDataEnd(const Sender: TObject;
        aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    function  MergeParams(Origin, Destination: TStrings): TStrings;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    procedure UpdateParamsFromRegistry;
    function  AddMessageAndRun(aPrompt, aRole: string;
        aMediaFiles: TAiMediaFilesArray = nil): string;
    function  AddMessage(aPrompt, aRole: string): TAiChatMessage;
    function  NewMessage(aPrompt, aRole: string): TAiChatMessage;
    function  Run(aMsg: TAiChatMessage = nil): string; virtual;
    function  GetLastMessage: TAiChatMessage;
    // RemoveMesage con typo (1 's') para compatibilidad con API Delphi original
    function  RemoveMesage(Msg: TAiChatMessage): Boolean; overload;
    function  RemoveMesage(IdMsg: Integer): Boolean; overload;
    procedure AddToMemory(Key, Value: string);
    procedure RemoveFromMemory(Key: string);
    procedure NewChat;
    procedure Abort;
    function  GetMessages: TJSONArray; virtual;
    function  GetDriversNames: TStringList; virtual;
    function  GetAvailableDrivers: TAiStringArray;
    function  GetModels: TStringList; overload; virtual;
    function  IsDriverAvailable(const DriverName: string): Boolean;
    procedure ResetParamsToDefaults;

    procedure RegisterUserParam(const DriverName, ModelName, ParamName,
        ParamValue: string); overload;
    procedure RegisterUserParam(const DriverName, ParamName,
        ParamValue: string); overload;
    procedure ClearRegisterParams(const ADriverName: string; ModelName: string = '');
    procedure RegisterCustomModel(const ADriverName, CustomModelName,
        ModelBaseName: string);
    procedure ClearCustomModels(const ADriverName: string);
    function  GetBaseModel(const ADriverName, CustomModel: string): string;
    function  CreateChatForDriver(const aDriverName, aModel: string): TAiChatConnection;

    function  UploadFile(aMediaFile: TAiMediaFile): string;
    function  CheckFileState(aMediaFile: TAiMediaFile): string;
    function  DeleteFile(aMediaFile: TAiMediaFile): string;
    function  UploadFileToCache(aMediaFile: TAiMediaFile;
        aTTL_Seconds: Integer = 3600): string;

    property Messages  : TAiChatMessages read FMessages;
    property LastError : string          read GetLastError;
    property Busy      : Boolean         read GetBusy;
    property AiChat    : TAiChat         read FChat;

  published
    property DriverName        : string          read FDriverName   write SetDriverName;
    property Model             : string          read FModel        write SetModel;
    property Params            : TStrings        read FParams       write SetParams;
    property SystemPrompt      : TStrings        read FSystemPrompt write SetSystemPrompt;
    property Memory            : TStrings        read FMemory       write SetMemory;
    property AiFunctions       : TAiFunctions    read FAiFunctions  write SetAiFunctions;
    property Prompt_tokens     : Integer         read FPrompt_tokens     write SetPrompt_tokens;
    property Completion_tokens : Integer         read FCompletion_tokens write SetCompletion_tokens;
    property Total_tokens      : Integer         read FTotal_tokens      write SetTotal_tokens;
    property ChatMode          : TAiChatMode     read FChatMode     write SetChatMode
        default cmConversation;
    property Version           : string          read FVersion;

    property ShellTool         : TAiShell           read FShellTool      write SetShellTool;
    property TextEditorTool    : TAiTextEditorTool  read FTextEditorTool write SetTextEditorTool;
    property ComputerUseTool   : TAiComputerUseTool read FComputerUseTool write SetComputerUseTool;
    property SpeechTool        : TAiSpeechToolBase  read FSpeechTool     write SetSpeechTool;
    property ImageTool         : TAiImageToolBase   read FImageTool      write SetImageTool;
    property VideoTool         : TAiVideoToolBase   read FVideoTool      write SetVideoTool;
    property WebSearchTool     : TAiWebSearchToolBase read FWebSearchTool write SetWebSearchTool;
    property VisionTool        : TAiVisionToolBase  read FVisionTool     write SetVisionTool;
    property PdfTool           : TAiPdfToolBase     read FPdfTool        write SetPdfTool;
    property ReportTool        : TAiReportToolBase  read FReportTool     write SetReportTool;

    property OnReceiveData      : TAiChatOnDataEvent
        read FOnReceiveData      write SetOnReceiveData;
    property OnReceiveDataEnd   : TAiChatOnDataEvent
        read FOnReceiveDataEnd   write SetOnReceiveDataEnd;
    property OnReceiveThinking  : TAiChatOnDataEvent
        read FOnReceiveThinking  write SetOnReceiveThinking;
    property OnAddMessage       : TAiChatOnDataEvent
        read FOnAddMessage       write SetOnAddMessage;
    property OnCallToolFunction : TOnCallToolFunction
        read FOnCallToolFunction write SetOnCallToolFunction;
    property OnBeforeSendMessage: TAiChatOnBeforeSendEvent
        read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    property OnInitChat         : TAiChatOnInitChatEvent
        read FOnInitChat         write SetOnInitChat;
    property OnProcessMediaFile : TAiChatOnMediaFileEvent
        read FOnProcessMediaFile write SetOnProcessMediaFile;
    property OnProcessResponse  : TAiChatOnProcessResponseEvent
        read FOnProcessResponse  write SetOnProcessResponse;
    property OnError            : TAiErrorEvent
        read FOnError            write SetOnError;
    property OnChatModelChange  : TOnChatModelChangeEvent
        read FOnChatModelChange  write FOnChatModelChange;
    property OnStateChange      : TAiStateChangeEvent
        read FOnStateChange      write FOnStateChange;
  end;

implementation

{$I uMakerAi.Version.inc}

// ===========================================================================
//  Helpers locales
// ===========================================================================

// ---------------------------------------------------------------------------
//  BoolFromStr — convierte 'true'/'1'/'yes'/'t' a True
// ---------------------------------------------------------------------------
function BoolFromStr(const S: string): Boolean;
var
  L: string;
begin
  L := LowerCase(S);
  Result := (L = 'true') or (L = '1') or (L = 'yes') or (L = 't');
end;

// ---------------------------------------------------------------------------
//  ParseSetValue — convierte '[EnumName1, EnumName2]' en bitmask Integer
//  Usa el TypeInfo del tipo elemento del set para GetEnumValue.
// ---------------------------------------------------------------------------
function ParseSetValue(PropInfo: PPropInfo; const ParamValue: string): Integer;
var
  CleanValue  : string;
  EnumParts   : TStringList;
  EI          : Integer;
  TrimmedName : string;
  OrdVal      : Integer;
  CompType    : PTypeInfo;
begin
  Result := 0;
  CleanValue := Trim(ParamValue);
  // Quitar corchetes [ ]
  if (Length(CleanValue) >= 2) and (CleanValue[1] = '[') then
  begin
    Delete(CleanValue, 1, 1);
    if (Length(CleanValue) >= 1) and (CleanValue[Length(CleanValue)] = ']') then
      Delete(CleanValue, Length(CleanValue), 1);
    CleanValue := Trim(CleanValue);
  end;
  if CleanValue = '' then
    Exit;
  // Obtener el tipo del elemento del set.
  // En FPC, PropInfo^.PropType es PTypeInfo (NO PPTypeInfo como en Delphi).
  // GetTypeData(PTypeInfo)^.CompType es PTypeInfo del tipo elemento.
  try
    CompType := GetTypeData(PropInfo^.PropType)^.CompType;
  except
    Exit;
  end;
  if CompType = nil then Exit;
  // Dividir por comas y obtener el ordinal de cada nombre
  EnumParts := TStringList.Create;
  try
    EnumParts.Delimiter := ',';
    EnumParts.StrictDelimiter := True;
    EnumParts.DelimitedText := CleanValue;
    for EI := 0 to EnumParts.Count - 1 do
    begin
      TrimmedName := Trim(EnumParts[EI]);
      if TrimmedName = '' then Continue;
      OrdVal := GetEnumValue(CompType, TrimmedName);
      if OrdVal >= 0 then
        Result := Result or (1 shl OrdVal);
    end;
  finally
    EnumParts.Free;
  end;
end;

// ===========================================================================
//  TAiChatConnection
// ===========================================================================

constructor TAiChatConnection.Create(Sender: TComponent);
begin
  inherited;
  FChat        := nil;
  FSystemPrompt := TStringList.Create;
  FMemory      := TStringList.Create;
  FMessagesOwn := TAiChatMessages.Create;
  FMessages    := FMessagesOwn;
  FParams      := TStringList.Create;
  TStringList(FParams).OnChange      := @ParamsChanged;
  TStringList(FSystemPrompt).OnChange := @ParamsChanged;
  TStringList(FMemory).OnChange      := @ParamsChanged;
  FChatMode    := cmConversation;
  FVersion     := MAKERAI_VERSION_FULL;
end;

destructor TAiChatConnection.Destroy;
begin
  if Assigned(FChat) then
    FChat.Free;
  FSystemPrompt.Free;
  FMemory.Free;
  FMessagesOwn.Free;   // FMessages es solo referencia, no se libera por separado
  FParams.Free;
  inherited;
end;

// ---------------------------------------------------------------------------
//  SetDriverName
// ---------------------------------------------------------------------------
procedure TAiChatConnection.SetDriverName(const Value: string);
begin
  if FDriverName <> Value then
  begin
    FDriverName := Value;
    FModel := '';
    if not (csDesigning in ComponentState) then
    begin
      UpdateAndApplyParams;
      SetupChatFromDriver;
    end
    else
      UpdateAndApplyParams;
  end;
end;

// ---------------------------------------------------------------------------
//  SetModel
// ---------------------------------------------------------------------------
procedure TAiChatConnection.SetModel(const Value: string);
begin
  if FModel <> Value then
  begin
    FModel := Value;
    TAiChatFactory.Instance.RegisterUserParam(FDriverName, FModel, 'Model', FModel);
    UpdateAndApplyParams;
  end;
end;

// ---------------------------------------------------------------------------
//  SetParams
// ---------------------------------------------------------------------------
procedure TAiChatConnection.SetParams(const Value: TStrings);
begin
  if Assigned(Value) then
    FParams.Assign(Value);
end;

// ---------------------------------------------------------------------------
//  ParamsChanged — llamado cuando FParams/FMemory/FSystemPrompt cambian
// ---------------------------------------------------------------------------
procedure TAiChatConnection.ParamsChanged(Sender: TObject);
begin
  if Assigned(FChat) then
    ApplyParamsToChat(FChat, FParams);
end;

// ---------------------------------------------------------------------------
//  SetChat — conecta el FChat y apunta FMessages al historial del driver
// ---------------------------------------------------------------------------
procedure TAiChatConnection.SetChat(const Value: TAiChat);
begin
  if FChat <> Value then
  begin
    // Si había un chat anterior aplica configuración (edge case: SetChat directo)
    if Assigned(FChat) then
    begin
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);
    end;

    FChat := Value;

    if Assigned(FChat) then
    begin
      ApplyEventsToChat(FChat);
      ApplyParamsToChat(FChat, FParams);
      FMessages := FChat.Messages;
    end
    else
      FMessages := FMessagesOwn;
  end;
end;

// ---------------------------------------------------------------------------
//  SetupChatFromDriver — crea el driver vía factory y lo conecta
// ---------------------------------------------------------------------------
procedure TAiChatConnection.SetupChatFromDriver;
var
  OldChat, LNewChat: TAiChat;
begin
  if csLoading in ComponentState then
    Exit;

  if FDriverName = '' then
  begin
    if Assigned(FChat) then
      FreeAndNil(FChat);
    Exit;
  end;

  OldChat  := FChat;
  FChat    := nil;  // desvinculamos temporalmente

  LNewChat := TAiChatFactory.Instance.CreateDriver(FDriverName);
  if not Assigned(LNewChat) then
    raise Exception.CreateFmt('Failed to create driver instance for "%s"', [FDriverName]);

  if Assigned(FOnChatModelChange) then
    FOnChatModelChange(Self, OldChat, LNewChat);

  SetChat(LNewChat);

  if Assigned(OldChat) then
    OldChat.Free;
end;

// ---------------------------------------------------------------------------
//  UpdateAndApplyParams — obtiene defaults del registry, fusiona con FParams
//  y los aplica al FChat si existe
// ---------------------------------------------------------------------------
procedure TAiChatConnection.UpdateAndApplyParams;
var
  LRegistryParams: TStringList;
  ShouldExpand   : Boolean;
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
    ShouldExpand := not (csDesigning in ComponentState);
    LRegistryParams := TStringList.Create;
    try
      TAiChatFactory.Instance.GetDriverParams(FDriverName, FModel,
          LRegistryParams, ShouldExpand);
      FParams.BeginUpdate;
      try
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

  if Assigned(FChat) then
    ApplyParamsToChat(FChat, FParams);
end;

procedure TAiChatConnection.UpdateParamsFromRegistry;
begin
  UpdateAndApplyParams;
end;

// ---------------------------------------------------------------------------
//  MergeParams — agrega claves de Origin a Destination sin duplicar
//  Retorna Destination. Si la clave existe en Destination, se actualiza.
// ---------------------------------------------------------------------------
function TAiChatConnection.MergeParams(Origin, Destination: TStrings): TStrings;
var
  I: Integer;
begin
  Result := Destination;
  for I := 0 to Origin.Count - 1 do
    Destination.Values[Origin.Names[I]] := Origin.ValueFromIndex[I];
end;

// ---------------------------------------------------------------------------
//  ApplyParamsToChat — inyecta parámetros al driver usando TypInfo (FPC RTTI)
//
//  Delphi usaba TRttiContext.GetType + TRttiProperty.SetValue.
//  FPC usa GetPropInfo + SetOrdProp/SetFloatProp/SetStrProp/SetInt64Prop.
//
//  Diferencias FPC vs Delphi en TypeKind:
//    - tkBool  (FPC) — Boolean; en Delphi Boolean es tkEnumeration
//    - tkAString (FPC) — AnsiString; en Delphi tkString/tkUString
//    - tkSet CompType es PPTypeInfo en FPC (doble puntero, dereferenciar con ^)
// ---------------------------------------------------------------------------
procedure TAiChatConnection.ApplyParamsToChat(AChat: TAiChat; AParams: TStrings);
var
  PropInfo  : PPropInfo;
  I         : Integer;
  ParamName : string;
  ParamValue: string;
  LIntVal   : Int64;
  LFloatVal : Double;
  LEnumVal  : Integer;
  LSetVal   : Integer;
  StrObj    : TObject;
begin
  if not Assigned(AChat) then Exit;

  // 1. Asignaciones directas de estructuras (herramientas, modo, contexto)
  AChat.AiFunctions    := Self.FAiFunctions;
  AChat.ShellTool      := Self.FShellTool;
  AChat.TextEditorTool := Self.FTextEditorTool;
  AChat.ComputerUseTool:= Self.FComputerUseTool;
  AChat.ChatMode       := Self.FChatMode;
  AChat.SpeechTool     := Self.FSpeechTool;
  AChat.ImageTool      := Self.FImageTool;
  AChat.VideoTool      := Self.FVideoTool;
  AChat.WebSearchTool  := Self.FWebSearchTool;
  AChat.VisionTool     := Self.FVisionTool;
  AChat.PdfTool        := Self.FPdfTool;
  AChat.ReportTool     := Self.FReportTool;
  AChat.Memory.Text    := Self.FMemory.Text;
  AChat.SystemPrompt.Text := Self.FSystemPrompt.Text;

  // 2. Inyección dinámica vía TypInfo (RTTI)
  if not Assigned(AParams) or (AParams.Count <= 0) then Exit;

  for I := 0 to AParams.Count - 1 do
  begin
    ParamName  := AParams.Names[I];
    ParamValue := Trim(AParams.Values[ParamName]);
    if ParamName = '' then Continue;

    PropInfo := GetPropInfo(AChat, ParamName);
    if PropInfo = nil then Continue;
    if PropInfo^.SetProc = nil then Continue;  // solo escritura

    try
      case PropInfo^.PropType^.Kind of

        // Enteros (incluye Integer, Word, Byte, etc.)
        tkInteger:
          if TryStrToInt(ParamValue, LEnumVal) then
            SetOrdProp(AChat, PropInfo, LEnumVal);

        // Int64 y QWord
        tkInt64, tkQWord:
          if TryStrToInt64(ParamValue, LIntVal) then
            SetInt64Prop(AChat, PropInfo, LIntVal);

        // Flotantes (Double, Single, Extended, Currency)
        tkFloat:
          if TryStrToFloat(ParamValue, LFloatVal) then
            SetFloatProp(AChat, PropInfo, LFloatVal);

        // Strings — FPC: tkAString=AnsiString, tkSString=ShortString, tkUString=UnicodeString
        tkAString, tkSString, tkUString, tkLString, tkWString:
          SetStrProp(AChat, PropInfo, ParamValue);

        // Boolean — FPC tiene tkBool separado (Delphi usa tkEnumeration para Boolean)
        tkBool:
          SetOrdProp(AChat, PropInfo, Ord(BoolFromStr(ParamValue)));

        // Enumeraciones (no Boolean en FPC)
        // PropInfo^.PropType es PTypeInfo en FPC (no PPTypeInfo)
        tkEnumeration:
          begin
            LEnumVal := GetEnumValue(PropInfo^.PropType, ParamValue);
            if LEnumVal >= 0 then
              SetOrdProp(AChat, PropInfo, LEnumVal);
          end;

        // Sets — parsear '[EnumName1, EnumName2]' → bitmask
        tkSet:
          begin
            LSetVal := ParseSetValue(PropInfo, ParamValue);
            SetOrdProp(AChat, PropInfo, LSetVal);
          end;

        // Clases — si es TStrings, asignar texto (separador '|' → LineEnding)
        tkClass:
          begin
            StrObj := GetObjectProp(AChat, PropInfo);
            if Assigned(StrObj) and (StrObj is TStrings) then
              TStrings(StrObj).Text :=
                  StringReplace(ParamValue, '|', LineEnding, [rfReplaceAll]);
          end;

      end; // case
    except
      // Fallo silencioso por propiedad individual — no detiene el resto
    end;
  end;
end;

// ---------------------------------------------------------------------------
//  ApplyEventsToChat — conecta o desconecta eventos entre Connection y Chat
//  NOTA: OnReceiveDataEnd se conecta a OnInternalReceiveDataEnd (interception)
//        no directamente al FOnReceiveDataEnd del usuario.
// ---------------------------------------------------------------------------
procedure TAiChatConnection.ApplyEventsToChat(AChat: TAiChat; SetToNil: Boolean);
begin
  if not Assigned(AChat) then Exit;

  if SetToNil then
  begin
    AChat.OnReceiveData      := nil;
    AChat.OnReceiveDataEnd   := nil;
    AChat.OnReceiveThinking  := nil;
    AChat.OnAddMessage       := nil;
    AChat.OnCallToolFunction := nil;
    AChat.OnBeforeSendMessage:= nil;
    AChat.OnInitChat         := nil;
    AChat.OnProcessMediaFile := nil;
    AChat.OnProcessResponse  := nil;
    AChat.OnError            := nil;
    AChat.OnStateChange      := nil;
  end
  else
  begin
    AChat.OnReceiveData      := Self.FOnReceiveData;
    AChat.OnReceiveDataEnd   := @Self.OnInternalReceiveDataEnd;
    AChat.OnReceiveThinking  := Self.FOnReceiveThinking;
    AChat.OnAddMessage       := Self.FOnAddMessage;
    AChat.OnCallToolFunction := Self.FOnCallToolFunction;
    AChat.OnBeforeSendMessage:= Self.FOnBeforeSendMessage;
    AChat.OnInitChat         := Self.FOnInitChat;
    AChat.OnProcessMediaFile := Self.FOnProcessMediaFile;
    AChat.OnProcessResponse  := Self.FOnProcessResponse;
    AChat.OnError            := Self.FOnError;
    AChat.OnStateChange      := Self.FOnStateChange;
  end;
end;

// ---------------------------------------------------------------------------
//  OnInternalReceiveDataEnd — intercepta el fin de la respuesta para
//  acumular tokens antes de reenviar al handler del usuario
// ---------------------------------------------------------------------------
procedure TAiChatConnection.OnInternalReceiveDataEnd(const Sender: TObject;
    aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  Prompt_tokens     := Prompt_tokens     + aMsg.Prompt_tokens;
  Completion_tokens := Completion_tokens + aMsg.Completion_tokens;
  Total_tokens      := Total_tokens      + aMsg.Total_tokens;

  // Pasamos Self (TAiChatConnection) como Sender, no el driver interno,
  // para que el codigo consumidor pueda hacer "if Sender is TAiChatConnection"
  // y acceder a Conn.Prompt_tokens acumulados directamente.
  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, aMsg, aResponse, aRole, aText);
end;

// ---------------------------------------------------------------------------
//  ValideChat — garantiza que FChat existe antes de delegar
// ---------------------------------------------------------------------------
procedure TAiChatConnection.ValideChat;
begin
  if not Assigned(FChat) and (FDriverName <> '') then
    SetupChatFromDriver;

  if not Assigned(FChat) then
    raise Exception.Create(
        'A valid DriverName must be specified to create a Chat instance.');
end;

// ---------------------------------------------------------------------------
//  Notification — gestión del ciclo de vida de herramientas componente
// ---------------------------------------------------------------------------
procedure TAiChatConnection.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    // Limpiar referencias en la Connection
    if AComponent = FSpeechTool    then FSpeechTool    := nil;
    if AComponent = FImageTool     then FImageTool     := nil;
    if AComponent = FVideoTool     then FVideoTool     := nil;
    if AComponent = FWebSearchTool then FWebSearchTool := nil;
    if AComponent = FVisionTool    then FVisionTool    := nil;
    if AComponent = FShellTool     then FShellTool     := nil;
    if AComponent = FTextEditorTool  then FTextEditorTool  := nil;
    if AComponent = FComputerUseTool then FComputerUseTool := nil;
    if AComponent = FAiFunctions   then FAiFunctions   := nil;
    if AComponent = FPdfTool       then FPdfTool       := nil;
    if AComponent = FReportTool    then FReportTool    := nil;

    // Sincronizar 'nil' al Chat activo para evitar referencias colgantes
    if Assigned(FChat) then
    begin
      if AComponent = FSpeechTool    then FChat.SpeechTool    := nil;
      if AComponent = FImageTool     then FChat.ImageTool     := nil;
      if AComponent = FVideoTool     then FChat.VideoTool     := nil;
      if AComponent = FWebSearchTool then FChat.WebSearchTool := nil;
      if AComponent = FVisionTool    then FChat.VisionTool    := nil;
      if AComponent = FShellTool     then FChat.ShellTool     := nil;
      if AComponent = FTextEditorTool  then FChat.TextEditorTool  := nil;
      if AComponent = FComputerUseTool then FChat.ComputerUseTool := nil;
      if AComponent = FAiFunctions   then FChat.AiFunctions   := nil;
      if AComponent = FPdfTool       then FChat.PdfTool       := nil;
      if AComponent = FReportTool    then FChat.ReportTool    := nil;
    end;
  end;
end;

// ---------------------------------------------------------------------------
//  Loaded — después de cargar un DFM/LFM, configura el driver
// ---------------------------------------------------------------------------
procedure TAiChatConnection.Loaded;
begin
  inherited;
  SetupChatFromDriver;
end;

// ===========================================================================
//  Métodos de acción y fachada (delegan a FChat)
// ===========================================================================

procedure TAiChatConnection.Abort;
begin
  ValideChat;
  FChat.Abort;
end;

function TAiChatConnection.AddMessage(aPrompt, aRole: string): TAiChatMessage;
begin
  ValideChat;
  Result := FChat.AddMessage(aPrompt, aRole);
end;

function TAiChatConnection.AddMessageAndRun(aPrompt, aRole: string;
    aMediaFiles: TAiMediaFilesArray): string;
begin
  ValideChat;
  Result := FChat.AddMessageAndRun(aPrompt, aRole, aMediaFiles);
end;

procedure TAiChatConnection.AddToMemory(Key, Value: string);
begin
  ValideChat;
  FMemory.Values[Key] := Value;
  FChat.AddToMemory(Key, Value);
end;

function TAiChatConnection.CheckFileState(aMediaFile: TAiMediaFile): string;
begin
  ValideChat;
  Result := FChat.CheckFileState(aMediaFile);
end;

function TAiChatConnection.DeleteFile(aMediaFile: TAiMediaFile): string;
begin
  ValideChat;
  Result := FChat.DeleteFile(aMediaFile);
end;

function TAiChatConnection.GetAvailableDrivers: TAiStringArray;
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
  AvailableDrivers: TAiStringArray;
  ADrv            : string;
begin
  Result := TStringList.Create;
  AvailableDrivers := TAiChatFactory.Instance.GetRegisteredDrivers;
  for ADrv in AvailableDrivers do
    Result.Add(ADrv);
end;

function TAiChatConnection.GetLastError: string;
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

function TAiChatConnection.GetMessages: TJSONArray;
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
var
  ADrv: string;
begin
  ADrv := DriverName;
  Result := TAiChatFactory.Instance.HasDriver(ADrv);
end;

procedure TAiChatConnection.NewChat;
begin
  ValideChat;
  FChat.NewChat;
end;

function TAiChatConnection.NewMessage(aPrompt, aRole: string): TAiChatMessage;
begin
  ValideChat;
  Result := FChat.NewMessage(aPrompt, aRole);
end;

procedure TAiChatConnection.RemoveFromMemory(Key: string);
begin
  ValideChat;
  FMemory.Values[Key] := '';
  FChat.RemoveFromMemory(Key);
end;

// RemoveMesage con typo (1 's') — compatibilidad con API Delphi original
// Internamente llama RemoveMessage (2 's') del driver FPC
function TAiChatConnection.RemoveMesage(Msg: TAiChatMessage): Boolean;
begin
  ValideChat;
  Result := FChat.RemoveMessage(Msg);
end;

function TAiChatConnection.RemoveMesage(IdMsg: Integer): Boolean;
begin
  ValideChat;
  Result := FChat.RemoveMessage(IdMsg);
end;

procedure TAiChatConnection.ResetParamsToDefaults;
begin
  UpdateAndApplyParams;
end;

function TAiChatConnection.Run(aMsg: TAiChatMessage): string;
begin
  ValideChat;
  Result := FChat.Run(aMsg, nil);
end;

function TAiChatConnection.UploadFile(aMediaFile: TAiMediaFile): string;
begin
  ValideChat;
  Result := FChat.UploadFile(aMediaFile);
end;

function TAiChatConnection.UploadFileToCache(aMediaFile: TAiMediaFile;
    aTTL_Seconds: Integer): string;
begin
  ValideChat;
  Result := FChat.UploadFileToCache(aMediaFile, aTTL_Seconds);
end;

// ===========================================================================
//  Métodos de registro (delegan a TAiChatFactory)
// ===========================================================================

procedure TAiChatConnection.RegisterUserParam(const DriverName, ModelName,
    ParamName, ParamValue: string);
var
  ADrv: string;
begin
  ADrv := DriverName;
  TAiChatFactory.Instance.RegisterUserParam(ADrv, ModelName, ParamName, ParamValue);
  if (ADrv = FDriverName) and (ModelName = FModel) then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.RegisterUserParam(const DriverName, ParamName,
    ParamValue: string);
var
  ADrv: string;
begin
  ADrv := DriverName;
  TAiChatFactory.Instance.RegisterUserParam(ADrv, ParamName, ParamValue);
  if ADrv = FDriverName then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.ClearRegisterParams(const ADriverName: string;
    ModelName: string);
begin
  TAiChatFactory.Instance.ClearRegisterParams(ADriverName, ModelName);
  if ADriverName = FDriverName then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.RegisterCustomModel(const ADriverName,
    CustomModelName, ModelBaseName: string);
begin
  TAiChatFactory.Instance.RegisterCustomModel(ADriverName, CustomModelName,
      ModelBaseName);
  if ADriverName = FDriverName then
    UpdateAndApplyParams;
end;

procedure TAiChatConnection.ClearCustomModels(const ADriverName: string);
begin
  TAiChatFactory.Instance.ClearCustomModels(ADriverName);
  if ADriverName = FDriverName then
    UpdateAndApplyParams;
end;

function TAiChatConnection.GetBaseModel(const ADriverName, CustomModel: string): string;
begin
  Result := TAiChatFactory.Instance.GetBaseModel(ADriverName, CustomModel);
end;

// ---------------------------------------------------------------------------
//  CreateChatForDriver — crea una nueva TAiChatConnection para el driver/modelo
//  Valida que el driver exista y que el modelo esté disponible.
// ---------------------------------------------------------------------------
function TAiChatConnection.CreateChatForDriver(const aDriverName,
    aModel: string): TAiChatConnection;
var
  LChat  : TAiChat;
  LModels: TStringList;
begin
  if not IsDriverAvailable(aDriverName) then
    raise Exception.CreateFmt(
        'Driver "%s" not found or is not registered.', [aDriverName]);

  LChat := TAiChatFactory.Instance.CreateDriver(aDriverName);
  try
    if not Assigned(LChat) then
      raise Exception.CreateFmt(
          'Failed to create an instance for driver "%s".', [aDriverName]);

    LModels := LChat.GetModels;
    try
      LModels.Sort;
      if LModels.IndexOf(aModel) < 0 then
        raise Exception.CreateFmt(
            'Model "%s" not found for driver "%s".', [aModel, aDriverName]);
    finally
      LModels.Free;
    end;
  finally
    LChat.Free;
  end;

  Result := TAiChatConnection.Create(Self.Owner);
  Result.DriverName := aDriverName;
  Result.Model      := aModel;
end;

// ===========================================================================
//  Setters de propiedades simples
// ===========================================================================

procedure TAiChatConnection.SetPrompt_tokens(const Value: Integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChatConnection.SetCompletion_tokens(const Value: Integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChatConnection.SetTotal_tokens(const Value: Integer);
begin
  FTotal_tokens := Value;
end;

procedure TAiChatConnection.SetMemory(const Value: TStrings);
begin
  FMemory.Assign(Value);
  if Assigned(FChat) then
    FChat.Memory.Assign(Value);
end;

procedure TAiChatConnection.SetSystemPrompt(const Value: TStrings);
begin
  FSystemPrompt.Assign(Value);
  if Assigned(FChat) then
    FChat.SystemPrompt.Assign(Value);
end;

procedure TAiChatConnection.SetChatMode(const Value: TAiChatMode);
begin
  if FChatMode <> Value then
  begin
    FChatMode := Value;
    if Assigned(FChat) then
      FChat.ChatMode := Value;
  end;
end;

// ===========================================================================
//  Setters de herramientas — patrón FreeNotification + propagación al Chat
// ===========================================================================

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

procedure TAiChatConnection.SetShellTool(const Value: TAiShell);
begin
  if FShellTool <> Value then
  begin
    FShellTool := Value;
    if FShellTool <> nil then
      FShellTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.ShellTool := Value;
  end;
end;

procedure TAiChatConnection.SetTextEditorTool(const Value: TAiTextEditorTool);
begin
  if FTextEditorTool <> Value then
  begin
    FTextEditorTool := Value;
    if FTextEditorTool <> nil then
      FTextEditorTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.TextEditorTool := Value;
  end;
end;

procedure TAiChatConnection.SetComputerUseTool(const Value: TAiComputerUseTool);
begin
  if FComputerUseTool <> Value then
  begin
    FComputerUseTool := Value;
    if FComputerUseTool <> nil then
      FComputerUseTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.ComputerUseTool := Value;
  end;
end;

procedure TAiChatConnection.SetSpeechTool(const Value: TAiSpeechToolBase);
begin
  if FSpeechTool <> Value then
  begin
    FSpeechTool := Value;
    if FSpeechTool <> nil then
      FSpeechTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.SpeechTool := Value;
  end;
end;

procedure TAiChatConnection.SetImageTool(const Value: TAiImageToolBase);
begin
  if FImageTool <> Value then
  begin
    FImageTool := Value;
    if FImageTool <> nil then
      FImageTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.ImageTool := Value;
  end;
end;

procedure TAiChatConnection.SetVideoTool(const Value: TAiVideoToolBase);
begin
  if FVideoTool <> Value then
  begin
    FVideoTool := Value;
    if FVideoTool <> nil then
      FVideoTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.VideoTool := Value;
  end;
end;

procedure TAiChatConnection.SetWebSearchTool(const Value: TAiWebSearchToolBase);
begin
  if FWebSearchTool <> Value then
  begin
    FWebSearchTool := Value;
    if FWebSearchTool <> nil then
      FWebSearchTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.WebSearchTool := Value;
  end;
end;

procedure TAiChatConnection.SetVisionTool(const Value: TAiVisionToolBase);
begin
  if FVisionTool <> Value then
  begin
    FVisionTool := Value;
    if FVisionTool <> nil then
      FVisionTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.VisionTool := Value;
  end;
end;

procedure TAiChatConnection.SetPdfTool(const Value: TAiPdfToolBase);
begin
  if FPdfTool <> Value then
  begin
    FPdfTool := Value;
    if FPdfTool <> nil then
      FPdfTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.PdfTool := Value;
  end;
end;

procedure TAiChatConnection.SetReportTool(const Value: TAiReportToolBase);
begin
  if FReportTool <> Value then
  begin
    FReportTool := Value;
    if FReportTool <> nil then
      FReportTool.FreeNotification(Self);
    if Assigned(FChat) then
      FChat.ReportTool := Value;
  end;
end;

// ===========================================================================
//  Setters de eventos — almacena localmente y propaga al Chat activo
//  EXCEPCIÓN: OnReceiveDataEnd NO se propaga directamente (interception).
// ===========================================================================

procedure TAiChatConnection.SetOnReceiveData(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveData := Value;
  if Assigned(FChat) then
    FChat.OnReceiveData := Value;
end;

procedure TAiChatConnection.SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveDataEnd := Value;
  // NO se propaga directamente a FChat.OnReceiveDataEnd:
  // FChat siempre tiene OnReceiveDataEnd = OnInternalReceiveDataEnd
  // que acumula tokens antes de llamar a FOnReceiveDataEnd.
end;

procedure TAiChatConnection.SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveThinking := Value;
  if Assigned(FChat) then
    FChat.OnReceiveThinking := Value;
end;

procedure TAiChatConnection.SetOnAddMessage(const Value: TAiChatOnDataEvent);
begin
  FOnAddMessage := Value;
  if Assigned(FChat) then
    FChat.OnAddMessage := Value;
end;

procedure TAiChatConnection.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
  if Assigned(FChat) then
    FChat.OnCallToolFunction := Value;
end;

procedure TAiChatConnection.SetOnBeforeSendMessage(
    const Value: TAiChatOnBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;
  if Assigned(FChat) then
    FChat.OnBeforeSendMessage := Value;
end;

procedure TAiChatConnection.SetOnInitChat(const Value: TAiChatOnInitChatEvent);
begin
  FOnInitChat := Value;
  if Assigned(FChat) then
    FChat.OnInitChat := Value;
end;

procedure TAiChatConnection.SetOnProcessMediaFile(
    const Value: TAiChatOnMediaFileEvent);
begin
  FOnProcessMediaFile := Value;
  if Assigned(FChat) then
    FChat.OnProcessMediaFile := Value;
end;

procedure TAiChatConnection.SetOnProcessResponse(
    const Value: TAiChatOnProcessResponseEvent);
begin
  FOnProcessResponse := Value;
  if Assigned(FChat) then
    FChat.OnProcessResponse := Value;
end;

procedure TAiChatConnection.SetOnError(const Value: TAiErrorEvent);
begin
  FOnError := Value;
  if Assigned(FChat) then
    FChat.OnError := Value;
end;

end.
