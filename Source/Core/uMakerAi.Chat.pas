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
//   - TNetHTTPClient (Delphi)          → TFPHTTPClient (fphttpclient)
//   - FClient.Asynchronous := True     → TAiHttpThread (thread manual)
//   - OnInternalReceiveData(...)        → OnSSELine(ALine: string)
//                                         llamado desde TAiSSEStream.Write()
//   - IHTTPResponse                    → FClient.ResponseStatusCode (Integer)
//   - TJSonObject.ParseJSONValue(s)    → TJSONObject(GetJSON(s))
//   - JObj.GetValue<T>('k')            → JGetStr/JGetInt/etc. (helpers locales)
//   - JObj.TryGetValue<T>('k', v)      → JTryGetStr/JTryGetObj/etc.
//   - JObj.AddPair('k', v)             → JObj.Add('k', v)
//   - JArr.ToJSON / JObj.Format        → JArr.AsJSON / JObj.FormatJSON
//   - TTask.Create(proc).Start         → loop secuencial en Fase 1
//   - TTask.WaitForAll(tasks)          → eliminado (secuencial en Fase 1)
//   - UTF8ToString(UTF8Encode(s))      → eliminado (FPC nativo UTF-8)
//   - sLineBreak                       → LineEnding
//   - TDictionary<Integer, TJSonObject> → specialize TObjectDictionary<Integer, TJSONObject>

unit uMakerAi.Chat;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Tools.ComputerUse,
  uMakerAi.Utils.CodeExtractor;

// ---------------------------------------------------------------------------
//  Tipos de eventos y enumeraciones del modulo Chat
// ---------------------------------------------------------------------------
type
  TAiChatResponseFormat = (tiaChatRfText, tiaChatRfJson, tiaChatRfJsonSchema);

  TAiChatMode = (
    cmConversation,        // Modo dialogo (Orquestacion Inteligente)
    cmImageGeneration,     // Forzar Generacion de Imagen
    cmVideoGeneration,     // Forzar Generacion de Video
    cmSpeechGeneration,    // Forzar Texto a Voz (TTS)
    cmTranscription,       // Forzar Transcripcion
    cmWebSearch,           // Forzar Busqueda Web
    cmReportGeneration     // Forzar Generacion de Reporte
  );

  TAiChat = class;   // forward

  TAiChatOnDataEvent = procedure(const Sender: TObject; aMsg: TAiChatMessage;
      aResponse: TJSONObject; aRole, aText: string) of object;

  TAiChatOnBeforeSendEvent = procedure(const Sender: TObject;
      var aMsg: TAiChatMessage) of object;

  TAiChatOnInitChatEvent = procedure(const Sender: TObject; aRole: string;
      var aText: string; var aMemory: TJSONObject) of object;

  TAiChatOnMediaFileEvent = procedure(const Sender: TObject; Prompt: string;
      MediaFile: TAiMediaFile; aNativeInputFiles: TAiFileCategories;
      var Respuesta: string; var aProcesado: Boolean) of object;

  TAiChatOnProcessResponseEvent = procedure(const Sender: TObject;
      LastMsg, ResMsg: TAiChatMessage; var Response: string) of object;

  TAiModelProgressEvent = procedure(Sender: TObject; Status: string;
      Completed, Total: Int64) of object;

  TOnCallToolFunction = procedure(Sender: TObject;
      AiToolCall: TAiToolsFunction) of object;

// ---------------------------------------------------------------------------
//  TAiSSEStream — stream de escritura que procesa lineas SSE del modelo
//  Recibe los chunks del HTTP thread y los pasa linea a linea a TAiChat
// ---------------------------------------------------------------------------
  TAiSSEStream = class(TStream)
  private
    FBuffer: string;
    FChat  : TAiChat;
  public
    constructor Create(AChat: TAiChat);
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

// ---------------------------------------------------------------------------
//  TAiHttpThread — ejecuta el POST HTTP en un thread secundario (modo async)
// ---------------------------------------------------------------------------
  TAiHttpThread = class(TThread)
  private
    FUrl    : string;
    FBody   : string;
    FHeaders: array of string;   // pares [name, value, name, value...]
    FChat   : TAiChat;
    FAborted: Boolean;
    FClient : TFPHTTPClient;
    FLock   : TCriticalSection;
  public
    constructor Create(const AUrl, ABody: string; const AHeaders: array of string;
                       AChat: TAiChat);
    destructor Destroy; override;
    procedure Execute; override;
    procedure RequestAbort;
  end;

// ---------------------------------------------------------------------------
//  TAiChat — clase base abstracta para todos los drivers LLM
// ---------------------------------------------------------------------------
  TAiChat = class(TComponent, IAiToolContext)
  private
    // HTTP
    FHttpThread      : TAiHttpThread;
    FLastStatusCode  : Integer;

    // Configuracion basica
    FApiKey          : string;
    FModel           : string;
    FUrl             : string;
    FUser            : string;
    FStop            : string;
    FTool_choice     : string;
    FSeed            : Integer;
    FN               : Integer;
    FTop_p           : Double;
    FTemperature     : Double;
    FFrequency_penalty: Double;
    FPresence_penalty : Double;
    FLogit_bias      : string;
    FTop_logprobs    : string;
    FLogprobs        : Boolean;
    FMax_tokens      : Integer;
    FResponseTimeOut : Integer;
    FVoice           : string;
    FVoice_format    : string;
    FLanguage        : string;
    FTranscription_ResponseFormat : string;
    FTranscription_TimestampGranularities: string;
    FReasoningFormat : string;
    FK               : Integer;
    FStream_Usage    : Boolean;
    FAsynchronous    : Boolean;
    FTool_Active     : Boolean;
    FChatMode        : TAiChatMode;
    FResponse_format : TAiChatResponseFormat;
    FThinkingLevel   : TAiThinkingLevel;
    FMediaResolution : TAiMediaResolution;

    // Capacidades
    FNativeInputFiles : TAiFileCategories;
    FNativeOutputFiles: TAiFileCategories;
    FChatMediaSupports: TAiChatMediaSupports;
    FEnabledFeatures  : TAiChatMediaSupports;
    FModelCaps        : TAiCapabilities;
    FSessionCaps      : TAiCapabilities;
    FNewSystemConfigured: Boolean;

    // Herramientas
    FAiFunctions      : TAiFunctions;
    FShellTool        : TAiShell;
    FTextEditorTool   : TAiTextEditorTool;
    FComputerUseTool  : TAiComputerUseTool;
    FSpeechTool       : TAiSpeechToolBase;
    FImageTool        : TAiImageToolBase;
    FVideoTool        : TAiVideoToolBase;
    FWebSearchTool    : TAiWebSearchToolBase;
    FVisionTool       : TAiVisionToolBase;
    FPdfTool          : TAiPdfToolBase;
    FReportTool       : TAiReportToolBase;

    // Parametros adicionales
    FSystemPrompt     : TStrings;
    FMemory           : TStrings;
    FJsonSchema       : TStrings;
    FImageParams      : TStrings;
    FVideoParams      : TStrings;
    FWebSearchParams  : TStrings;

    // Eventos
    FOnReceiveDataEvent  : TAiChatOnDataEvent;
    FOnReceiveDataEnd    : TAiChatOnDataEvent;
    FOnAddMessage        : TAiChatOnDataEvent;
    FOnReceiveThinking   : TAiChatOnDataEvent;
    FOnCallToolFunction  : TOnCallToolFunction;
    FOnBeforeSendMessage : TAiChatOnBeforeSendEvent;
    FOnInitChat          : TAiChatOnInitChatEvent;
    FOnProcessMediaFile  : TAiChatOnMediaFileEvent;
    FOnProcessResponse   : TAiChatOnProcessResponseEvent;
    FOnProgressEvent     : TAiModelProgressEvent;
    FOnError             : TAiErrorEvent;
    FOnStateChange       : TAiStateChangeEvent;

    // Setters
    procedure SetApiKey(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetUrl(const Value: string);
    procedure SetUser(const Value: string);
    procedure SetStop(const Value: string);
    procedure SetTool_choice(const Value: string);
    procedure SetSeed(const Value: Integer);
    procedure SetN(const Value: Integer);
    procedure SetTop_p(const Value: Double);
    procedure SetTemperature(const Value: Double);
    procedure SetFrequency_penalty(const Value: Double);
    procedure SetPresence_penalty(const Value: Double);
    procedure SetLogit_bias(const Value: string);
    procedure SetTop_logprobs(const Value: string);
    procedure SetLogprobs(const Value: Boolean);
    procedure SetMax_tokens(const Value: Integer);
    procedure SetResponseTimeOut(const Value: Integer);
    procedure SetVoice(const Value: string);
    procedure SetVoice_format(const Value: string);
    procedure SetLanguage(const Value: string);
    procedure SetTranscription_ResponseFormat(const Value: string);
    procedure SetTranscription_TimestampGranularities(const Value: string);
    procedure SetReasoningFormat(const Value: string);
    procedure SetK(const Value: Integer);
    procedure SetStream_Usage(const Value: Boolean);
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetTool_Active(const Value: Boolean);
    procedure SetChatMode_prop(const Value: TAiChatMode);
    procedure SetResponse_format(const Value: TAiChatResponseFormat);
    procedure SetThinkingLevel(const Value: TAiThinkingLevel);
    procedure SetMediaResolution(const Value: TAiMediaResolution);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
    procedure SetNativeOutputFiles(const Value: TAiFileCategories);
    procedure SetChatMediaSupports(const Value: TAiChatMediaSupports);
    procedure SetEnabledFeatures(const Value: TAiChatMediaSupports);
    procedure SetModelCaps(const Value: TAiCapabilities);
    procedure SetSessionCaps(const Value: TAiCapabilities);
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
    procedure SetSystemPrompt(const Value: TStrings);
    procedure SetMemory(const Value: TStrings);
    procedure SetJsonSchema(const Value: TStrings);
    procedure SetImageParams(const Value: TStrings);
    procedure SetVideoParams(const Value: TStrings);
    procedure SetWebSearchParams(const Value: TStrings);
    procedure SetOnReceiveDataEvent(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
    procedure SetOnAddMessage(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
    procedure SetOnInitChat(const Value: TAiChatOnInitChatEvent);
    procedure SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
    procedure SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
    procedure SetOnProgressEvent(const Value: TAiModelProgressEvent);
    procedure SetOnError(const Value: TAiErrorEvent);
    procedure SetCompletion_tokens(const Value: Integer);
    procedure SetPrompt_tokens(const Value: Integer);
    procedure SetTotal_tokens(const Value: Integer);
    procedure SetThinking_tokens(const Value: Integer);
    procedure SetLastError(const Value: string);

    function  GetApiKey: string;

    // Nuevo sistema de orquestacion (v3.3)
    procedure EnsureNewSystemConfig;
    function  LegacyToModelCaps: TAiCapabilities;
    function  LegacyToSessionCaps: TAiCapabilities;
    function  FileTypeInModelCaps(ACategory: TAiFileCategory): Boolean;
    procedure SyncLegacyFromModelCaps;
    procedure SyncLegacyFromSessionCaps;
    function  RunNew(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): string;

  protected
    // Estado de streaming SSE
    FTmpResponseText  : string;
    FTmpToolCallBuffer: specialize TObjectDictionary<Integer, TJSONObject>;
    FTmpRole          : string;
    FLastContent      : string;
    FLastReasoning    : string;
    FLastPrompt       : string;
    FLastError        : string;
    FAbort            : Boolean;
    FBusy             : Boolean;
    FMessages         : TAiChatMessages;
    FTools            : TStrings;
    FCompletion_tokens: Integer;
    FTotal_tokens     : Integer;
    FPrompt_tokens    : Integer;
    FThinking_tokens  : Integer;

    // Manejo de mensajes
    function  InternalAddMessage(aPrompt, aRole: string; aToolCallId: string;
        aFunctionName: string): TAiChatMessage; overload; virtual;
    function  InternalAddMessage(aPrompt, aRole: string;
        aMediaFiles: TAiMediaFilesArray): TAiChatMessage; overload; virtual;
    function  InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
        overload; virtual;

    // Metodos virtuales — implementacion base OpenAI-compatible
    function  InitChatCompletions: string; virtual;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); virtual;
    function  ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions; virtual;
    function  PrepareSystemMsg: string; virtual;

    // Procesamiento SSE: reemplaza OnInternalReceiveData de Delphi
    procedure OnSSELine(const ALine: string); virtual;
    procedure ProcessSSELine(const ALine: string); virtual;

    // Hook para drivers que reciben delta.content como array tipado (ej: Mistral magistral).
    // La base no hace nada; los drivers con formato no-estandar lo sobreescriben.
    procedure ParseDeltaContentArray(AContentArr: TJSONArray; jObj: TJSONObject); virtual;

    // Stubs para Fase 2 (retornan '' o no hacen nada)
    function  InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunReport(ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunTranscription(aMediaFile: TAiMediaFile;
        ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunImageDescription(aMediaFile: TAiMediaFile;
        ResMsg, AskMsg: TAiChatMessage): string; virtual;
    function  InternalRunPDFDescription(aMediaFile: TAiMediaFile;
        ResMsg, AskMsg: TAiChatMessage): string; virtual;

    // Herramientas / callbacks
    procedure DoCallFunction(ToolCall: TAiToolsFunction); virtual;
    function  GetToolsStr(aToolFormat: TToolFormat): string;

    // Hooks de error de red — virtuales para que subclases puedan sobreescribir
    // Equivalen a OnRequestErrorEvent / OnRequestExceptionEvent del Delphi original
    procedure OnRequestErrorEvent(const AError: string); virtual;
    procedure OnRequestExceptionEvent(const AError: Exception); virtual;
    procedure DoProcessMediaFile(aPrompt: string; aAiMediaFile: TAiMediaFile;
        var Respuesta: string; var Procesado: Boolean);
    // Sobrecarga protected para respuestas de tool calls (compatibilidad Delphi)
    function  AddMessageAndRun(aPrompt, aRole: string; aToolCallId: string;
        aFunctionName: string): string; overload;
    function  AddMessageAndRunTool(aPrompt, aRole: string; aToolCallId: string;
        aFunctionName: string): string;

    // Implementacion de IAiToolContext
    procedure DoData(Msg: TAiChatMessage; const Role, Text: string;
        aResponse: TJSONObject = nil);
    procedure DoDataEnd(Msg: TAiChatMessage; const Role, Text: string;
        aResponse: TJSONObject = nil);
    procedure DoThinking(Msg: TAiChatMessage; const Role, Text: string;
        aResponse: TJSONObject = nil);
    procedure DoProgress(const AStatus: string; ACompleted, ATotal: Int64);
    procedure DoError(const ErrorMsg: string; E: Exception); virtual;
    procedure DoProcessResponse(aLastMsg, aResMsg: TAiChatMessage;
        var aResponse: string);
    procedure DoStateChange(State: TAiChatState; const Description: string = '');
    function  GetAsynchronous: Boolean;

    // Arrancar thread HTTP async (necesario para subclases con URL propia)
    procedure StartHttpThread(const sUrl, ABody: string;
        const AHeaders: array of string);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    // Metodos abstractos — cada driver debe implementarlos
    class function GetDriverName: string; virtual; abstract;
    class procedure RegisterDefaultParams(Params: TStrings); virtual; abstract;
    class function CreateInstance(Sender: TComponent): TAiChat; virtual; abstract;

    // API publica
    function  AddMessageAndRun(aPrompt, aRole: string;
        aMediaFiles: TAiMediaFilesArray = nil): string; overload;
    function  AddMessageAndRunMsg(aPrompt, aRole: string;
        aMediaFiles: TAiMediaFilesArray = nil): TAiChatMessage;
    function  AddMessage(aPrompt, aRole: string): TAiChatMessage;
    function  NewMessage(aPrompt, aRole: string): TAiChatMessage;
    function  Run(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage = nil): string; virtual;
    function  GetLastMessage: TAiChatMessage;
    function  RemoveMessage(Msg: TAiChatMessage): Boolean; overload;
    function  RemoveMessage(IdMsg: Integer): Boolean; overload;
    procedure NewChat; virtual;
    procedure Abort;
    procedure AddToMemory(Key, Value: string);
    procedure RemoveFromMemory(Key: string);
    function  PublicChatToSend: string;
    function  GetMessages: TJSONArray; virtual;

    class function GetModels(aApiKey, aUrl: string): TStringList; overload; virtual;
    function  GetModels: TStringList; overload; virtual;

    // Stubs de gestion de archivos remotos (Fase 2)
    function  UploadFile(aMediaFile: TAiMediaFile): string; virtual;
    function  DownloadFile(aMediaFile: TAiMediaFile): string; virtual;
    function  CheckFileState(aMediaFile: TAiMediaFile): string; virtual;
    function  DeleteFile(aMediaFile: TAiMediaFile): string; virtual;
    function  RetrieveFile(aFileId: string): TAiMediaFile; virtual;
    function  RetrieveFileList: TAiMediaFiles; virtual;
    function  UploadFileToCache(aMediaFile: TAiMediaFile;
        aTTL_Seconds: Integer = 3600): string; virtual;

    // Utilidades de serializacion de capacidades
    function  FileCategoriesToString(const ACategories: TAiFileCategories): string;
    function  StringToFileCategories(const AValue: string): TAiFileCategories;

    property Messages   : TAiChatMessages read FMessages;
    property LastError  : string          read FLastError write SetLastError;
    property LastContent: string          read FLastContent;
    property LastPrompt : string          read FLastPrompt;
    property Busy       : Boolean         read FBusy;

  published
    property ApiKey      : string                read GetApiKey             write SetApiKey;
    property Model       : string                read FModel                write SetModel;
    property Url         : string                read FUrl                  write SetUrl;
    property User        : string                read FUser                 write SetUser;
    property Stop        : string                read FStop                 write SetStop;
    property Tool_choice : string                read FTool_choice          write SetTool_choice;
    property Seed        : Integer               read FSeed                 write SetSeed;
    property N           : Integer               read FN                    write SetN;
    property Top_p       : Double                read FTop_p                write SetTop_p;
    property Temperature : Double                read FTemperature          write SetTemperature;
    property Frequency_penalty: Double           read FFrequency_penalty    write SetFrequency_penalty;
    property Presence_penalty : Double           read FPresence_penalty     write SetPresence_penalty;
    property Logit_bias  : string                read FLogit_bias           write SetLogit_bias;
    property Top_logprobs: string                read FTop_logprobs         write SetTop_logprobs;
    property Logprobs    : Boolean               read FLogprobs             write SetLogprobs;
    property Max_tokens  : Integer               read FMax_tokens           write SetMax_tokens;
    property ResponseTimeOut: Integer            read FResponseTimeOut      write SetResponseTimeOut;
    property Voice       : string                read FVoice                write SetVoice;
    property Voice_format: string                read FVoice_format         write SetVoice_format;
    property Language    : string                read FLanguage             write SetLanguage;
    property Transcription_ResponseFormat: string
        read FTranscription_ResponseFormat write SetTranscription_ResponseFormat;
    property Transcription_TimestampGranularities: string
        read FTranscription_TimestampGranularities write SetTranscription_TimestampGranularities;
    property ReasoningFormat: string             read FReasoningFormat      write SetReasoningFormat;
    property K           : Integer               read FK                    write SetK;
    property Stream_Usage: Boolean               read FStream_Usage         write SetStream_Usage;
    property Asynchronous: Boolean               read FAsynchronous         write SetAsynchronous;
    property Tool_Active : Boolean               read FTool_Active          write SetTool_Active;
    property ChatMode    : TAiChatMode           read FChatMode             write SetChatMode_prop
        default cmConversation;
    property Response_format: TAiChatResponseFormat
        read FResponse_format write SetResponse_format;
    property ThinkingLevel  : TAiThinkingLevel   read FThinkingLevel        write SetThinkingLevel;
    property MediaResolution: TAiMediaResolution read FMediaResolution      write SetMediaResolution;
    property NativeInputFiles : TAiFileCategories
        read FNativeInputFiles write SetNativeInputFiles;
    property NativeOutputFiles: TAiFileCategories
        read FNativeOutputFiles write SetNativeOutputFiles;
    property ChatMediaSupports: TAiChatMediaSupports
        read FChatMediaSupports write SetChatMediaSupports;
    property EnabledFeatures  : TAiChatMediaSupports
        read FEnabledFeatures write SetEnabledFeatures;
    property ModelCaps  : TAiCapabilities        read FModelCaps            write SetModelCaps;
    property SessionCaps: TAiCapabilities        read FSessionCaps          write SetSessionCaps;
    property AiFunctions: TAiFunctions           read FAiFunctions          write SetAiFunctions;
    property ShellTool  : TAiShell               read FShellTool            write SetShellTool;
    property TextEditorTool: TAiTextEditorTool   read FTextEditorTool       write SetTextEditorTool;
    property ComputerUseTool: TAiComputerUseTool read FComputerUseTool      write SetComputerUseTool;
    property SpeechTool  : TAiSpeechToolBase     read FSpeechTool           write SetSpeechTool;
    property ImageTool   : TAiImageToolBase      read FImageTool            write SetImageTool;
    property VideoTool   : TAiVideoToolBase      read FVideoTool            write SetVideoTool;
    property WebSearchTool: TAiWebSearchToolBase read FWebSearchTool        write SetWebSearchTool;
    property VisionTool  : TAiVisionToolBase     read FVisionTool           write SetVisionTool;
    property PdfTool     : TAiPdfToolBase        read FPdfTool              write SetPdfTool;
    property ReportTool  : TAiReportToolBase     read FReportTool           write SetReportTool;
    property SystemPrompt: TStrings              read FSystemPrompt         write SetSystemPrompt;
    property Memory      : TStrings              read FMemory               write SetMemory;
    property JsonSchema  : TStrings              read FJsonSchema           write SetJsonSchema;
    property ImageParams : TStrings              read FImageParams          write SetImageParams;
    property VideoParams : TStrings              read FVideoParams          write SetVideoParams;
    property WebSearchParams: TStrings           read FWebSearchParams      write SetWebSearchParams;
    property Prompt_tokens    : Integer          read FPrompt_tokens        write SetPrompt_tokens;
    property Completion_tokens: Integer          read FCompletion_tokens    write SetCompletion_tokens;
    property Total_tokens     : Integer          read FTotal_tokens         write SetTotal_tokens;
    property Thinking_tokens  : Integer          read FThinking_tokens      write SetThinking_tokens;

    property OnReceiveData   : TAiChatOnDataEvent    read FOnReceiveDataEvent  write SetOnReceiveDataEvent;
    property OnReceiveDataEnd: TAiChatOnDataEvent    read FOnReceiveDataEnd    write SetOnReceiveDataEnd;
    property OnAddMessage    : TAiChatOnDataEvent    read FOnAddMessage        write SetOnAddMessage;
    property OnReceiveThinking: TAiChatOnDataEvent   read FOnReceiveThinking   write SetOnReceiveThinking;
    property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction  write SetOnCallToolFunction;
    property OnBeforeSendMessage: TAiChatOnBeforeSendEvent
        read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    property OnInitChat     : TAiChatOnInitChatEvent read FOnInitChat          write SetOnInitChat;
    property OnProcessMediaFile: TAiChatOnMediaFileEvent
        read FOnProcessMediaFile write SetOnProcessMediaFile;
    property OnProcessResponse: TAiChatOnProcessResponseEvent
        read FOnProcessResponse write SetOnProcessResponse;
    property OnProgressEvent: TAiModelProgressEvent  read FOnProgressEvent     write SetOnProgressEvent;
    property OnError        : TAiErrorEvent          read FOnError             write SetOnError;
    property OnStateChange  : TAiStateChangeEvent    read FOnStateChange       write FOnStateChange;
  end;

procedure LogDebug(const Mensaje: string);

// ---------------------------------------------------------------------------
//  JSON helpers — visibles a units driver (fpjson no tiene GetValue<T>)
// ---------------------------------------------------------------------------
function JGetStr(J: TJSONObject; const Key: string; const Def: string = ''): string;
function JGetInt(J: TJSONObject; const Key: string; Def: Integer = 0): Integer;
function JGetInt64(J: TJSONObject; const Key: string; Def: Int64 = 0): Int64;
function JGetBool(J: TJSONObject; const Key: string; Def: Boolean = False): Boolean;
function JGetObj(J: TJSONObject; const Key: string): TJSONObject;
function JGetArr(J: TJSONObject; const Key: string): TJSONArray;
function JTryGetStr(J: TJSONObject; const Key: string; out Val: string): Boolean;
function JTryGetObj(J: TJSONObject; const Key: string; out Val: TJSONObject): Boolean;
function JTryGetArr(J: TJSONObject; const Key: string; out Val: TJSONArray): Boolean;
function JTryGetInt(J: TJSONObject; const Key: string; out Val: Integer): Boolean;

implementation

uses UMakerAi.ParamsRegistry, TypInfo;

// ===========================================================================
//  TToolCallThread — ejecuta una tool call en un thread independiente
//  Permite paralelizar varias tool calls (equivalente a TTask de Delphi)
// ===========================================================================
type
  TToolCallThread = class(TThread)
  private
    FChat    : TAiChat;
    FToolCall: TAiToolsFunction;
  public
    constructor Create(AChat: TAiChat; AToolCall: TAiToolsFunction);
    procedure Execute; override;
  end;

constructor TToolCallThread.Create(AChat: TAiChat; AToolCall: TAiToolsFunction);
begin
  inherited Create(True);   // suspendido hasta Start
  FChat     := AChat;
  FToolCall := AToolCall;
  FreeOnTerminate := False;
end;

procedure TToolCallThread.Execute;
begin
  try
    FChat.DoCallFunction(FToolCall);
  except
    on E: Exception do
      FChat.OnRequestExceptionEvent(E);
  end;
end;

// ===========================================================================
//  Helpers locales para JSON (fpjson no tiene GetValue<T>/TryGetValue<T>)
// ===========================================================================

function JGetStr(J: TJSONObject; const Key: string; const Def: string = ''): string;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  if Idx >= 0 then Result := J.Items[Idx].AsString
  else Result := Def;
end;

function JGetInt(J: TJSONObject; const Key: string; Def: Integer = 0): Integer;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  if Idx >= 0 then
    try Result := J.Items[Idx].AsInteger; except Result := Def; end
  else Result := Def;
end;

function JGetBool(J: TJSONObject; const Key: string; Def: Boolean = False): Boolean;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  if Idx >= 0 then Result := J.Items[Idx].AsBoolean
  else Result := Def;
end;

function JGetObj(J: TJSONObject; const Key: string): TJSONObject;
var D: TJSONData;
begin
  D := J.Find(Key);
  if (D <> nil) and (D is TJSONObject) then Result := TJSONObject(D)
  else Result := nil;
end;

function JGetArr(J: TJSONObject; const Key: string): TJSONArray;
var D: TJSONData;
begin
  D := J.Find(Key);
  if (D <> nil) and (D is TJSONArray) then Result := TJSONArray(D)
  else Result := nil;
end;

function JTryGetStr(J: TJSONObject; const Key: string; out Val: string): Boolean;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  Result := Idx >= 0;
  if Result then Val := J.Items[Idx].AsString;
end;

function JTryGetObj(J: TJSONObject; const Key: string; out Val: TJSONObject): Boolean;
var D: TJSONData;
begin
  D := J.Find(Key);
  Result := (D <> nil) and (D is TJSONObject);
  if Result then Val := TJSONObject(D);
end;

function JTryGetArr(J: TJSONObject; const Key: string; out Val: TJSONArray): Boolean;
var D: TJSONData;
begin
  D := J.Find(Key);
  Result := (D <> nil) and (D is TJSONArray);
  if Result then Val := TJSONArray(D);
end;

function JTryGetInt(J: TJSONObject; const Key: string; out Val: Integer): Boolean;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  Result := Idx >= 0;
  if Result then
    try Val := J.Items[Idx].AsInteger; except Result := False; end;
end;

function JGetInt64(J: TJSONObject; const Key: string; Def: Int64 = 0): Int64;
var Idx: Integer;
begin
  Idx := J.IndexOfName(Key);
  if Idx >= 0 then
    try Result := J.Items[Idx].AsInt64; except Result := Def; end
  else Result := Def;
end;

// ===========================================================================
//  LogDebug
// ===========================================================================

procedure LogDebug(const Mensaje: string);
begin
  // Solo activar para depuracion
end;

// ===========================================================================
//  TAiSSEStream
// ===========================================================================

constructor TAiSSEStream.Create(AChat: TAiChat);
begin
  inherited Create;
  FChat   := AChat;
  FBuffer := '';
end;

function TAiSSEStream.Write(const Buffer; Count: Longint): Longint;
var
  Chunk : string;
  P     : Integer;
  Line  : string;
begin
  Result := Count;
  SetString(Chunk, PChar(@Buffer), Count);
  FBuffer := FBuffer + Chunk;

  // Procesar todas las lineas completas (terminadas en #10)
  while Pos(#10, FBuffer) > 0 do
  begin
    P    := Pos(#10, FBuffer);
    Line := Trim(Copy(FBuffer, 1, P - 1));
    Delete(FBuffer, 1, P);
    if Assigned(FChat) then
      FChat.OnSSELine(Line);
  end;

  // Caso borde: si el buffer restante es exactamente [DONE]
  Chunk := Trim(FBuffer);
  if (Chunk = '[DONE]') or (Chunk = 'data: [DONE]') then
  begin
    if Assigned(FChat) then
      FChat.OnSSELine(Chunk);
    FBuffer := '';
  end;
end;

function TAiSSEStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TAiSSEStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

// ===========================================================================
//  TAiHttpThread
// ===========================================================================

constructor TAiHttpThread.Create(const AUrl, ABody: string;
    const AHeaders: array of string; AChat: TAiChat);
var
  I: Integer;
begin
  inherited Create(True);  // creado suspendido
  FUrl     := AUrl;
  FBody    := ABody;
  FChat    := AChat;
  FAborted := False;
  FClient  := nil;
  FLock    := TCriticalSection.Create;
  FreeOnTerminate := False;

  SetLength(FHeaders, Length(AHeaders));
  for I := 0 to High(AHeaders) do
    FHeaders[I] := AHeaders[I];
end;

destructor TAiHttpThread.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TAiHttpThread.Execute;
var
  BodyStream: TStringStream;
  SseStream : TAiSSEStream;
  I         : Integer;
begin
  BodyStream := TStringStream.Create(FBody);
  SseStream  := TAiSSEStream.Create(FChat);
  FLock.Enter;
  FClient := TFPHTTPClient.Create(nil);
  FLock.Leave;
  try
    // Agregar headers (pares [name, value, ...])
    I := 0;
    while I + 1 < Length(FHeaders) do
    begin
      FClient.AddHeader(FHeaders[I], FHeaders[I + 1]);
      Inc(I, 2);
    end;
    FClient.AddHeader('Content-Type', 'application/json');

    if FChat.ResponseTimeOut > 0 then
      FClient.IOTimeout := FChat.ResponseTimeOut;

    try
      FClient.RequestBody := BodyStream;
      FClient.HTTPMethod('POST', FUrl, SseStream, [200]);
    except
      on E: Exception do
      begin
        if not FAborted then
          FChat.OnRequestExceptionEvent(E);
      end;
    end;
  finally
    FLock.Enter;
    FreeAndNil(FClient);
    FLock.Leave;
    BodyStream.Free;
    SseStream.Free;
  end;
end;

procedure TAiHttpThread.RequestAbort;
begin
  FAborted := True;
  FLock.Enter;
  try
    { FPC: TFPHTTPClient no tiene propiedad Active.
      FAborted=True es suficiente — el stream ignorara los datos restantes. }
  finally
    FLock.Leave;
  end;
end;

// ===========================================================================
//  TAiChat — constructor / destructor
// ===========================================================================

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

constructor TAiChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  FMessages          := TAiChatMessages.Create;
  FTools             := TStringList.Create;
  FMemory            := TStringList.Create;
  FJsonSchema        := TStringList.Create;
  FSystemPrompt      := TStringList.Create;
  FImageParams       := TStringList.Create;
  FVideoParams       := TStringList.Create;
  FWebSearchParams   := TStringList.Create;
  FTmpToolCallBuffer := specialize TObjectDictionary<Integer, TJSONObject>.Create([doOwnsValues]);
  FHttpThread        := nil;
  FShellTool         := nil;
  FTextEditorTool    := nil;

  FNativeInputFiles  := [];
  FNativeOutputFiles := [];
  FModelCaps         := [];
  FSessionCaps       := [];
  FNewSystemConfigured := False;

  FModel           := 'gpt-4o';
  FN               := 1;
  FResponse_format := tiaChatRfText;
  FTemperature     := 1;
  FUser            := 'user';
  FSystemPrompt.Text := 'Eres un asistente muy util y servicial';
  FMax_tokens      := 3000;
  FUrl             := GlOpenAIUrl;
  FTop_p           := 1;
  FResponseTimeOut := 120000;
  FStream_Usage    := False;
  FTool_choice     := 'auto';
  FChatMode        := cmConversation;
  FMediaResolution := mrDefault;
end;

destructor TAiChat.Destroy;
begin
  // Terminar thread HTTP si esta activo
  if Assigned(FHttpThread) then
  begin
    FHttpThread.RequestAbort;
    FHttpThread.WaitFor;
    FreeAndNil(FHttpThread);
  end;
  FTools.Free;
  FMemory.Free;
  FJsonSchema.Free;
  FImageParams.Free;
  FVideoParams.Free;
  FWebSearchParams.Free;
  FTmpToolCallBuffer.Free;
  FSystemPrompt.Free;
  NewChat;
  FMessages.Free;
  inherited Destroy;
end;

// ===========================================================================
//  Gestion de mensajes internos
// ===========================================================================

function TAiChat.InternalAddMessage(aPrompt, aRole: string; aToolCallId: string;
    aFunctionName: string): TAiChatMessage;
var Msg: TAiChatMessage;
begin
  Msg    := TAiChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
  Msg.id := FMessages.Count + 1;
  FMessages.Add(Msg);
  FLastPrompt := aPrompt;
  Result := Msg;
end;

function TAiChat.InternalAddMessage(aPrompt, aRole: string;
    aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
var
  Msg: TAiChatMessage;
  MF : TAiMediaFile;
begin
  Msg := TAiChatMessage.Create(aPrompt, aRole);
  if Length(aMediaFiles) > 0 then
    for MF in aMediaFiles do
      if Assigned(MF) then
        Msg.AddMediaFile(MF);
  Result := InternalAddMessage(Msg);
end;

function TAiChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
var
  TmpMsg       : TAiChatMessage;
  MF           : TAiMediaFile;
  MensajeInicial: string;
  Respuesta    : string;
  Procesado    : Boolean;
begin
  if not Assigned(aMsg) then
    raise Exception.Create('El parametro aMsg debe estar instanciado');

  MensajeInicial := Trim(PrepareSystemMsg);

  // Inserta el system prompt la primera vez
  if (FMessages.Count = 0) and (MensajeInicial <> '') then
  begin
    TmpMsg    := TAiChatMessage.Create(MensajeInicial, 'system');
    TmpMsg.id := FMessages.Count + 1;
    FMessages.Add(TmpMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, TmpMsg, nil, 'system', MensajeInicial);
  end;

  if Assigned(FMessages) and Assigned(aMsg) then
  begin
    aMsg.id := FMessages.Count + 1;
    FMessages.Add(aMsg);
  end;

  if Assigned(FOnAddMessage) then
    FOnAddMessage(Self, aMsg, nil, aMsg.Role, aMsg.Prompt);

  // Procesa cada archivo de media del mensaje
  for MF in aMsg.MediaFiles do
  begin
    if Assigned(MF) then
    begin
      Procesado := False;
      DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado);
      MF.Procesado     := Procesado;
      MF.Transcription := Respuesta;
    end;
  end;

  FLastPrompt := aMsg.Prompt;

  if Assigned(FOnBeforeSendMessage) then
    FOnBeforeSendMessage(Self, aMsg);

  Result := aMsg;
end;

// ===========================================================================
//  PrepareSystemMsg
// ===========================================================================

function TAiChat.PrepareSystemMsg: string;
var
  S, Key, Val, MensajeInicial: string;
  I      : Integer;
  JMemory: TJSONObject;
begin
  if FResponse_format = tiaChatRfJson then
    S := 'Responde en formato json'
  else
    S := '';

  MensajeInicial := FSystemPrompt.Text + LineEnding + S;

  JMemory := TJSONObject.Create;
  try
    try
      for I := 0 to FMemory.Count - 1 do
      begin
        Key := FMemory.Names[I];
        Val := FMemory.Values[Key];
        JMemory.Add(Key, Val);
      end;
    except
      on E: Exception do
        raise Exception.Create(
          'El formato de memoria debe ser Key=Value, no esta bien configurado');
    end;

    if Assigned(FOnInitChat) then
      FOnInitChat(Self, 'system', MensajeInicial, JMemory);

    if Trim(MensajeInicial) <> '' then
      Result := MensajeInicial;

    if Length(Trim(JMemory.FormatJSON)) > 10 then
      Result := Result + LineEnding + 'Para Recordar= ' + JMemory.FormatJSON;
  finally
    JMemory.Free;
  end;
end;

// ===========================================================================
//  GetMessages — serializa el historial al formato del provider (base: OpenAI)
// ===========================================================================

function TAiChat.GetMessages: TJSONArray;
begin
  FMessages.NativeInputFiles := FNativeInputFiles;
  Result := FMessages.ToJson;
end;

// ===========================================================================
//  InitChatCompletions — construye el JSON del request (formato OpenAI)
// ===========================================================================

function TAiChat.InitChatCompletions: string;
var
  AJSONObject, jToolChoice, jeffort: TJSONObject;
  JArr  : TJSONArray;
  JStop : TJSONArray;
  Lista : TStringList;
  I     : Integer;
  LModel, sEffort, sTools, Res: string;
begin
  if FUser = '' then
    FUser := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, FModel);
  if LModel = '' then
    LModel := FModel;
  if LModel = '' then
    LModel := 'gpt-4o';

  AJSONObject := TJSONObject.Create;
  Lista       := TStringList.Create;
  try
    AJSONObject.Add('stream', TJSONBoolean.Create(FAsynchronous));

    // Tools
    if FTool_Active then
    begin
      sTools := GetToolsStr(tfOpenAI);
      if Trim(sTools) <> '' then
      begin
        JArr := TJSONArray(GetJSON(sTools));
        if not Assigned(JArr) then
          raise Exception.Create(
            'La propiedad Tools esta mal definida, debe ser un JsonArray');
        AJSONObject.Add('tools', JArr);

        if Trim(FTool_choice) <> '' then
        begin
          jToolChoice := TJSONObject(GetJSON(FTool_choice));
          if Assigned(jToolChoice) then
            AJSONObject.Add('tool_choice', jToolChoice);
        end;
      end;
    end;

    AJSONObject.Add('messages', GetMessages);
    AJSONObject.Add('model', LModel);
    AJSONObject.Add('user', FUser);

    // response_format
    case FResponse_format of
      tiaChatRfJsonSchema:
        AJSONObject.Add('response_format',
            TJSONObject.Create(['type', 'json_schema']));
      tiaChatRfJson:
        AJSONObject.Add('response_format',
            TJSONObject.Create(['type', 'json_object']));
      else
        AJSONObject.Add('response_format',
            TJSONObject.Create(['type', 'text']));
    end;

    // Thinking / reasoning
    sEffort := '';
    case FThinkingLevel of
      tlLow:    sEffort := 'low';
      tlMedium: sEffort := 'medium';
      tlHigh:   sEffort := 'high';
    end;

    if sEffort <> '' then
    begin
      jeffort := TJSONObject.Create;
      jeffort.Add('effort', sEffort);
      AJSONObject.Add('reasoning', jeffort);
      AJSONObject.Add('max_completion_tokens', TJSONIntegerNumber.Create(FMax_tokens));
    end
    else
      AJSONObject.Add('max_tokens', TJSONIntegerNumber.Create(FMax_tokens));

    // Stop words
    Lista.CommaText := FStop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.Add('stop', JStop);
    end;

    // Logprobs
    if FLogprobs then
    begin
      if FLogit_bias <> '' then
        AJSONObject.Add('logit_bias', TJSONIntegerNumber.Create(StrToIntDef(FLogit_bias, 0)));
      AJSONObject.Add('logprobs', TJSONBoolean.Create(FLogprobs));
      if FTop_logprobs <> '' then
        AJSONObject.Add('top_logprobs', TJSONIntegerNumber.Create(StrToIntDef(FTop_logprobs, 0)));
    end;

    if FSeed > 0 then
      AJSONObject.Add('seed', TJSONIntegerNumber.Create(FSeed));

    // WebSearch nativo
    if tcm_WebSearch in FChatMediaSupports then
      AJSONObject.Add('web_search_options', TJSONObject.Create)
    else
    begin
      if FTop_p <> 0 then
        AJSONObject.Add('top_p', TJSONFloatNumber.Create(FTop_p));
      AJSONObject.Add('temperature',
          TJSONFloatNumber.Create(Trunc(FTemperature * 100) / 100));
      AJSONObject.Add('frequency_penalty',
          TJSONFloatNumber.Create(Trunc(FFrequency_penalty * 100) / 100));
      AJSONObject.Add('presence_penalty',
          TJSONFloatNumber.Create(Trunc(FPresence_penalty * 100) / 100));
      AJSONObject.Add('n', TJSONIntegerNumber.Create(FN));
    end;

    Res := AJSONObject.AsJSON;
    // Normalizar: fpjson ya escapa / correctamente; solo limpiar \r\n literales
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — ejecuta el POST (sync o async)
// ===========================================================================

function TAiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody    : string;
  sUrl     : string;
  BodyStream: TStringStream;
  RespStream: TStringStream;
  Client   : TFPHTTPClient;
  jData    : TJSONData;
  jObj     : TJSONObject;
  Headers  : array of string;
begin
  Result := '';
  FBusy      := True;
  FAbort     := False;
  FLastError := '';
  FLastContent := '';
  FLastReasoning := '';
  FLastPrompt := '';

  if Assigned(FTmpToolCallBuffer) then
    FTmpToolCallBuffer.Clear;

  sUrl  := FUrl;
  // Normalizar URL: debe terminar en /
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'chat/completions';

  ABody := InitChatCompletions;

  DoStateChange(acsConnecting, sUrl);

  if FAsynchronous then
  begin
    // Modo async: lanzar TAiHttpThread
    if Assigned(FHttpThread) then
    begin
      FHttpThread.RequestAbort;
      FHttpThread.WaitFor;
      FreeAndNil(FHttpThread);
    end;

    Headers := ['Authorization', 'Bearer ' + ApiKey];
    FHttpThread := TAiHttpThread.Create(sUrl, ABody, Headers, Self);
    FHttpThread.Start;
    // El resultado vendrá por OnSSELine -> eventos
    Exit;
  end;

  // Modo sync
  BodyStream := TStringStream.Create(ABody);
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    if FResponseTimeOut > 0 then
      Client.IOTimeout := FResponseTimeOut;

    try
      Client.RequestBody := BodyStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
    except
      on E: Exception do
      begin
        FBusy := False;
        FLastError := E.Message;
        DoStateChange(acsError, E.Message);
        DoError(E.Message, E);
        Exit;
      end;
    end;

    FLastStatusCode := Client.ResponseStatusCode;

    if FLastStatusCode = 200 then
    begin
      jData := GetJSON(RespStream.DataString);
      if not Assigned(jData) or not (jData is TJSONObject) then
      begin
        FreeAndNil(jData);
        FBusy := False;
        raise Exception.CreateFmt('Error: Respuesta JSON invalida: %s',
            [Copy(RespStream.DataString, 1, 200)]);
      end;
      jObj := TJSONObject(jData);
      try
        FBusy := False;
        ParseChat(jObj, ResMsg);
        Result := FLastContent;
      finally
        FreeAndNil(jObj);
      end;
    end
    else
    begin
      FBusy := False;
      DoStateChange(acsError, RespStream.DataString);
      raise Exception.CreateFmt('Error Received: %d, %s',
          [FLastStatusCode, RespStream.DataString]);
    end;
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  ParseChat — parsea la respuesta (formato OpenAI choices[])
// ===========================================================================

procedure TAiChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  choices  : TJSONArray;
  JItem    : TJSONObject;
  jMessage : TJSONObject;
  uso      : TJSONObject;
  JToolCallsValue: TJSONData;
  JVal     : TJSONData;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta, sReasoning, sRes, sToolCalls: string;
  AskMsg   : TAiChatMessage;
  Msg      : TAiChatMessage;
  LFunciones  : TAiToolsFunctions;
  ToolCall    : TAiToolsFunction;
  ToolMsg     : TAiChatMessage;
  Clave       : string;
  I, NumTasks : Integer;
  ToolThreads : array of TToolCallThread;
  Code        : TMarkdownCodeExtractor;
  CodeFile : TCodeFile;
  CodeFiles: TCodeFileList;
  MF       : TAiMediaFile;
  St       : TStringStream;
  ModelVersion: string;
begin
  aPrompt_tokens     := 0;
  aCompletion_tokens := 0;
  aTotal_tokens      := 0;

  if JObj.IndexOfName('model') >= 0 then
    ModelVersion := JObj.Items[JObj.IndexOfName('model')].AsString;

  uso := JGetObj(jObj, 'usage');
  if Assigned(uso) then
  begin
    aPrompt_tokens     := JGetInt(uso, 'prompt_tokens');
    aCompletion_tokens := JGetInt(uso, 'completion_tokens');
    aTotal_tokens      := JGetInt(uso, 'total_tokens');
  end;

  AskMsg := GetLastMessage;

  choices := JGetArr(jObj, 'choices');
  if not Assigned(choices) then
  begin
    FBusy := False;
    Exit;
  end;

  Respuesta  := '';
  sReasoning := '';
  sToolCalls := '';

  for I := 0 to choices.Count - 1 do
  begin
    if not (choices.Items[I] is TJSONObject) then
      Continue;
    JItem    := TJSONObject(choices.Items[I]);
    jMessage := JGetObj(JItem, 'message');
    if not Assigned(jMessage) then
      Continue;

    Role := JGetStr(jMessage, 'role');

    // Reasoning (DeepSeek, Groq, etc.)
    if not JTryGetStr(jMessage, 'reasoning', sReasoning) then
      JTryGetStr(jMessage, 'reasoning_content', sReasoning);
    if sReasoning <> '' then
      ResMsg.ReasoningContent := sReasoning;

    sRes := '';
    JTryGetStr(jMessage, 'content', sRes);

    if Trim(sRes) <> '' then
      Respuesta := Trim(Respuesta + LineEnding + sRes)
    else
      Respuesta := Trim(Respuesta + LineEnding + sReasoning);

    // tool_calls
    JVal := jMessage.Find('tool_calls');
    if Assigned(JVal) and (JVal is TJSONArray) then
      sToolCalls := TJSONArray(JVal).AsJSON;
  end;

  Respuesta := Trim(Respuesta);

  FLastContent      := Trim(FLastContent + LineEnding + Respuesta);
  FPrompt_tokens    := FPrompt_tokens    + aPrompt_tokens;
  FCompletion_tokens:= FCompletion_tokens+ aCompletion_tokens;
  FTotal_tokens     := FTotal_tokens     + aTotal_tokens;

  if sToolCalls = '' then
  begin
    // Respuesta normal
    ResMsg.Role              := Role;
    ResMsg.Model             := ModelVersion;
    ResMsg.Tool_calls        := sToolCalls;
    ResMsg.Prompt            := ResMsg.Prompt + Respuesta;
    ResMsg.Prompt_tokens     := ResMsg.Prompt_tokens     + aPrompt_tokens;
    ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
    ResMsg.Total_tokens      := ResMsg.Total_tokens      + aTotal_tokens;
    DoProcessResponse(AskMsg, ResMsg, Respuesta);
  end
  else
  begin
    // Respuesta con tool calls — crear mensaje en el historial
    Msg := TAiChatMessage.Create(Respuesta, Role);
    Msg.Tool_calls := sToolCalls;
    if sReasoning <> '' then
      Msg.ReasoningContent := sReasoning;
    Msg.id := FMessages.Count + 1;
    FMessages.Add(Msg);
  end;

  LFunciones := ExtractToolCallFromJson(choices);
  try
    if LFunciones.Count > 0 then
    begin
      // Ejecucion paralela: cada tool call corre en su propio thread
      // (equivalente a TTask.WaitForAll de Delphi)
      SetLength(ToolThreads, LFunciones.Count);
      NumTasks := 0;
      for Clave in LFunciones.Keys do
      begin
        ToolCall        := LFunciones[Clave];
        ToolCall.ResMsg := ResMsg;
        ToolCall.AskMsg := AskMsg;
        ToolThreads[NumTasks] := TToolCallThread.Create(Self, ToolCall);
        ToolThreads[NumTasks].Start;
        Inc(NumTasks);
      end;
      // Barrera: esperar a que todos los threads terminen
      for I := 0 to NumTasks - 1 do
      begin
        ToolThreads[I].WaitFor;
        ToolThreads[I].Free;
      end;

      // Agregar respuestas de herramientas al historial
      for Clave in LFunciones.Keys do
      begin
        ToolCall := LFunciones[Clave];
        ToolMsg  := TAiChatMessage.Create(ToolCall.Response, 'tool',
            ToolCall.id, ToolCall.name);
        ToolMsg.id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      end;

      // Re-ejecutar para obtener respuesta final
      Self.Run(nil, ResMsg);
      ResMsg.Content := '';
    end
    else
    begin
      FBusy := False;

      // Extraer bloques de codigo si esta configurado
      if tfc_ExtractTextFile in FNativeOutputFiles then
      begin
        Code := TMarkdownCodeExtractor.Create;
        try
          CodeFiles := Code.ExtractCodeFiles(Respuesta);
          for CodeFile in CodeFiles do
          begin
            St := TStringStream.Create(CodeFile.Code);
            try
              St.Position := 0;
              MF := TAiMediaFile.Create;
              MF.LoadFromStream('file.' + CodeFile.FileType, St);
              ResMsg.MediaFiles.Add(MF);
            finally
              St.Free;
            end;
          end;
          CodeFiles.Free;
        finally
          Code.Free;
        end;
      end;

      DoProcessResponse(AskMsg, ResMsg, Respuesta);
      ResMsg.Prompt := Respuesta;

      // En modo sync: ParseChat dispara el evento final aqui.
      // En modo async: ProcessSSELine lo dispara via DoDataEnd — evitar double-fire.
      if not FAsynchronous then
      begin
        DoStateChange(acsFinished, 'Done');
        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
      end;
    end;
  finally
    LFunciones.Free;
  end;
end;

// ===========================================================================
//  ExtractToolCallFromJson
// ===========================================================================

function TAiChat.ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions;
var
  JItem, Msg, jFunc, BufferFunc: TJSONObject;
  JToolCalls: TJSONArray;
  Fun        : TAiToolsFunction;
  Arg        : TJSONObject;
  JData      : TJSONData;
  Nom, Valor, sType, sId, sName, sArgs: string;
  I, J, ArgIdx : Integer;
begin
  Result := TAiToolsFunctions.Create;

  for I := 0 to jChoices.Count - 1 do
  begin
    if not (jChoices.Items[I] is TJSONObject) then
      Continue;
    JItem := TJSONObject(jChoices.Items[I]);
    Msg   := JGetObj(JItem, 'message');
    if not Assigned(Msg) then
      Continue;

    JToolCalls := JGetArr(Msg, 'tool_calls');
    if not Assigned(JToolCalls) then
      Continue;

    for J := 0 to JToolCalls.Count - 1 do
    begin
      if not (JToolCalls.Items[J] is TJSONObject) then
        Continue;
      jFunc := TJSONObject(JToolCalls.Items[J]);

      sType := JGetStr(jFunc, 'type', 'function');
      if sType <> 'function' then
        Continue;

      Fun      := TAiToolsFunction.Create;
      Fun.id   := JGetStr(jFunc, 'id');
      Fun.Tipo := sType;

      BufferFunc := JGetObj(jFunc, 'function');
      if Assigned(BufferFunc) then
      begin
        Fun.name      := JGetStr(BufferFunc, 'name');
        Fun.Arguments := JGetStr(BufferFunc, 'arguments');
      end;

      // Parsear argumentos JSON
      if (Fun.Arguments <> '') and (Fun.Arguments <> '{}') then
      try
        JData := GetJSON(Fun.Arguments);
        if Assigned(JData) and (JData is TJSONObject) then
        begin
          Arg := TJSONObject(JData);
          try
            for ArgIdx := 0 to Arg.Count - 1 do
            begin
              Nom   := Arg.Names[ArgIdx];
              Valor := Arg.Items[ArgIdx].AsString;
              Fun.Params.Values[Nom] := Valor;
            end;
          finally
            Arg.Free;
          end;
        end
        else
          FreeAndNil(JData);
      except
        // Sin parametros → no error
      end;

      Result.Add(Fun.id, Fun);
    end;
  end;
end;

// ===========================================================================
//  OnSSELine — entrada principal del parser SSE (llamado desde TAiSSEStream)
// ===========================================================================

procedure TAiChat.OnSSELine(const ALine: string);
begin
  ProcessSSELine(ALine);
end;

procedure TAiChat.ProcessSSELine(const ALine: string);
var
  Line         : string;
  jData        : TJSONData;
  jObj         : TJSONObject;
  jArrChoices  : TJSONArray;
  Delta        : TJSONObject;
  JToolCalls   : TJSONArray;
  JTC, BufferObj, BufFunc, FakeResponseObj, FakeChoice, FakeMsg,
  FakeUsage, NewFunc, BufFuncObj : TJSONObject;
  FakeChoicesArr, CombinedTools: TJSONArray;
  JVal         : TJSONData;
  ToolIndex    : Integer;
  Value, Role1, OldVal, sToolCallsStr: string;
  SortedKeys   : specialize TList<Integer>;
  aKey         : Integer;
  TempMsg      : TAiChatMessage;
  I            : Integer;
begin
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallBuffer.Clear;
    FLastReasoning := '';
    DoDataEnd(nil, 'system', 'abort', nil);
    Exit;
  end;

  Line := ALine;
  if Line = '' then
    Exit;

  // Limpiar prefijo "data:"
  if (Length(Line) >= 5) and (Copy(Line, 1, 5) = 'data:') then
    Line := Trim(Copy(Line, 6, MaxInt));

  if Line = '' then
    Exit;

  // -----------------------------------------------------------------------
  // CASO [DONE]
  // -----------------------------------------------------------------------
  if Line = '[DONE]' then
  begin
    sToolCallsStr := '';

    if FTmpToolCallBuffer.Count > 0 then
    begin
      CombinedTools := TJSONArray.Create;
      SortedKeys    := specialize TList<Integer>.Create;
      try
        for aKey in FTmpToolCallBuffer.Keys do
          SortedKeys.Add(aKey);
        SortedKeys.Sort;
        for aKey in SortedKeys do
          CombinedTools.Add(TJSONObject(FTmpToolCallBuffer[aKey].Clone));
        sToolCallsStr := CombinedTools.AsJSON;
      finally
        SortedKeys.Free;
        CombinedTools.Free;
        FTmpToolCallBuffer.Clear;
      end;
    end;

    // Construir Fake JSON para ParseChat
    FakeResponseObj := TJSONObject.Create;
    try
      FakeResponseObj.Add('id',    'stream-' + IntToStr(GetTickCount64));
      FakeResponseObj.Add('model', FModel);

      FakeUsage := TJSONObject.Create;
      FakeUsage.Add('prompt_tokens',     TJSONIntegerNumber.Create(0));
      FakeUsage.Add('completion_tokens', TJSONIntegerNumber.Create(0));
      FakeUsage.Add('total_tokens',      TJSONIntegerNumber.Create(0));
      FakeResponseObj.Add('usage', FakeUsage);

      FakeChoicesArr := TJSONArray.Create;
      FakeChoice     := TJSONObject.Create;
      FakeMsg        := TJSONObject.Create;

      FakeMsg.Add('role', FTmpRole);
      if FLastContent <> '' then
        FakeMsg.Add('content', FLastContent);
      if FLastReasoning <> '' then
        FakeMsg.Add('reasoning_content', FLastReasoning);
      if sToolCallsStr <> '' then
      begin
        jData := GetJSON(sToolCallsStr);
        if Assigned(jData) and (jData is TJSONArray) then
          FakeMsg.Add('tool_calls', TJSONArray(jData))
        else
          FreeAndNil(jData);
      end;

      FakeChoice.Add('message', FakeMsg);
      FakeChoice.Add('finish_reason', 'stop');
      FakeChoicesArr.Add(FakeChoice);
      FakeResponseObj.Add('choices', FakeChoicesArr);

      TempMsg := TAiChatMessage.Create('', FTmpRole);
      try
        ParseChat(FakeResponseObj, TempMsg);
        if sToolCallsStr = '' then
        begin
          TempMsg.id := FMessages.Count + 1;
          FMessages.Add(TempMsg);
          DoStateChange(acsFinished, 'Done');
          DoDataEnd(TempMsg, FTmpRole, FLastContent, nil);
          TempMsg := nil;
        end
        else
        begin
          DoStateChange(acsFinished, 'Done');
          DoDataEnd(GetLastMessage, FTmpRole, '', nil);
        end;
      finally
        if Assigned(TempMsg) then
          TempMsg.Free;
      end;
    finally
      FakeResponseObj.Free;
    end;

    FLastReasoning := '';
    FBusy := False;
    Exit;
  end;

  // -----------------------------------------------------------------------
  // CASO JSON standard (chunk delta)
  // -----------------------------------------------------------------------
  jData := GetJSON(Line);
  if not Assigned(jData) or not (jData is TJSONObject) then
  begin
    FreeAndNil(jData);
    Exit;
  end;
  jObj := TJSONObject(jData);
  try
    if not JTryGetArr(jObj, 'choices', jArrChoices) then
      Exit;
    if (jArrChoices = nil) or (jArrChoices.Count = 0) then
      Exit;

    if not (jArrChoices.Items[0] is TJSONObject) then
      Exit;
    Delta := JGetObj(TJSONObject(jArrChoices.Items[0]), 'delta');
    if not Assigned(Delta) then
      Exit;

    // Role
    if JTryGetStr(Delta, 'role', Role1) then
      FTmpRole := Role1;

    // Thinking / reasoning
    Value := '';
    if not JTryGetStr(Delta, 'reasoning', Value) then
      JTryGetStr(Delta, 'reasoning_content', Value);
    if Value <> '' then
    begin
      FLastReasoning := FLastReasoning + Value;
      if Assigned(FOnReceiveThinking) then
      begin
        Value := StringReplace(Value, #10, LineEnding, [rfReplaceAll]);
        FOnReceiveThinking(Self, nil, jObj, FTmpRole, Value);
      end;
    end;

    // Content
    JVal := Delta.Find('content');
    if Assigned(JVal) and (JVal is TJSONString) then
    begin
      Value := JVal.AsString;
      FLastContent := FLastContent + Value;
      if Assigned(FOnReceiveDataEvent) then
      begin
        Value := StringReplace(Value, #10, LineEnding, [rfReplaceAll]);
        FOnReceiveDataEvent(Self, nil, jObj, FTmpRole, Value);
      end;
    end
    else if Assigned(JVal) and (JVal is TJSONArray) then
      ParseDeltaContentArray(TJSONArray(JVal), jObj);

    // Tool calls (streaming)
    if JTryGetArr(Delta, 'tool_calls', JToolCalls) then
    begin
      for I := 0 to JToolCalls.Count - 1 do
      begin
        if not (JToolCalls.Items[I] is TJSONObject) then
          Continue;
        JTC := TJSONObject(JToolCalls.Items[I]);

        if not JTryGetInt(JTC, 'index', ToolIndex) then
          Continue;

        if not FTmpToolCallBuffer.TryGetValue(ToolIndex, BufferObj) then
        begin
          BufferObj := TJSONObject.Create;
          FTmpToolCallBuffer.Add(ToolIndex, BufferObj);
        end;

        if JTryGetStr(JTC, 'id', Value) and (BufferObj.Find('id') = nil) then
          BufferObj.Add('id', Value);
        if JTryGetStr(JTC, 'type', Value) and (BufferObj.Find('type') = nil) then
          BufferObj.Add('type', Value);

        BufFunc := JGetObj(JTC, 'function');
        if Assigned(BufFunc) then
        begin
          if BufferObj.Find('function') = nil then
          begin
            NewFunc := TJSONObject.Create;
            BufferObj.Add('function', NewFunc);
          end;
          BufFuncObj := JGetObj(BufferObj, 'function');
          if Assigned(BufFuncObj) then
          begin
            if JTryGetStr(BufFunc, 'name', Value) then
            begin
              OldVal := JGetStr(BufFuncObj, 'name');
              if BufFuncObj.IndexOfName('name') >= 0 then
                BufFuncObj.Delete(BufFuncObj.IndexOfName('name'));
              BufFuncObj.Add('name', OldVal + Value);
            end;
            if JTryGetStr(BufFunc, 'arguments', Value) then
            begin
              OldVal := JGetStr(BufFuncObj, 'arguments');
              if BufFuncObj.IndexOfName('arguments') >= 0 then
                BufFuncObj.Delete(BufFuncObj.IndexOfName('arguments'));
              BufFuncObj.Add('arguments', OldVal + Value);
            end;
          end;
        end;
      end;
    end;

  finally
    jObj.Free;
  end;
end;

// ===========================================================================
//  Run / RunNew — orquestacion central
// ===========================================================================

function TAiChat.Run(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): string;
begin
  EnsureNewSystemConfig;
  Result := RunNew(AskMsg, ResMsg);
end;

function TAiChat.RunNew(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): string;
var
  MF          : TAiMediaFile;
  LOwnsResMsg : Boolean;
  LRes        : string;
  LProc       : Boolean;
  Gap         : TAiCapabilities;
begin
  Result      := '';
  LOwnsResMsg := False;
  Gap         := FSessionCaps - FModelCaps;

  if not Assigned(ResMsg) then
  begin
    ResMsg      := TAiChatMessage.Create('', 'assistant');
    LOwnsResMsg := True;
  end;

  try
    if Assigned(AskMsg) then
      InternalAddMessage(AskMsg);

    if not Assigned(AskMsg) then
      AskMsg := GetLastMessage;

    if not Assigned(AskMsg) then
      raise Exception.Create('No hay un mensaje valido para procesar en TAiChat.Run');

    // --- FASE 1: BRIDGE DE ENTRADA (solo cmConversation) ---
    if FChatMode = cmConversation then
    begin
      for MF in AskMsg.MediaFiles do
      begin
        if not FileTypeInModelCaps(MF.FileCategory) and not MF.Procesado then
        begin
          // Prioridad 1: evento manual
          if Assigned(FOnProcessMediaFile) then
          begin
            LRes  := '';
            LProc := False;
            FOnProcessMediaFile(Self, AskMsg.Prompt, MF, FNativeInputFiles, LRes, LProc);
            MF.Procesado     := LProc;
            MF.Transcription := LRes;
          end;
          // Prioridad 2: bridge automatico segun gap
          if not MF.Procesado then
          begin
            case MF.FileCategory of
              Tfc_Audio:
                if cap_Audio in Gap then
                  InternalRunTranscription(MF, ResMsg, AskMsg);
              Tfc_Image:
                if cap_Image in Gap then
                  InternalRunImageDescription(MF, ResMsg, AskMsg);
              Tfc_Pdf:
                if cap_Pdf in Gap then
                  InternalRunPDFDescription(MF, ResMsg, AskMsg);
            end;
          end;
        end;
      end;
    end;

    // --- FASE 2: GROUNDING ---
    if cap_WebSearch in Gap then
    begin
      DoStateChange(acsReasoning, 'Ejecutando Bridge de Busqueda Web...');
      InternalRunWebSearch(ResMsg, AskMsg);
    end;

    // --- FASE 3: ORQUESTACION DE SALIDA ---
    case FChatMode of
      cmConversation:
        begin
          if cap_GenVideo in Gap then
            InternalRunImageVideoGeneration(ResMsg, AskMsg)
          else if cap_GenImage in Gap then
            InternalRunImageGeneration(ResMsg, AskMsg)
          else if cap_GenAudio in Gap then
            InternalRunSpeechGeneration(ResMsg, AskMsg)
          else if cap_GenReport in Gap then
            InternalRunReport(ResMsg, AskMsg)
          else
            InternalRunCompletions(ResMsg, AskMsg);
        end;
      cmImageGeneration:
        InternalRunImageGeneration(ResMsg, AskMsg);
      cmVideoGeneration:
        InternalRunImageVideoGeneration(ResMsg, AskMsg);
      cmSpeechGeneration:
        InternalRunSpeechGeneration(ResMsg, AskMsg);
      cmWebSearch:
        InternalRunWebSearch(ResMsg, AskMsg);
      cmReportGeneration:
        InternalRunReport(ResMsg, AskMsg);
      cmTranscription:
        begin
          for MF in AskMsg.MediaFiles do
            if (MF.FileCategory = Tfc_Audio) and not MF.Procesado then
            begin
              InternalRunTranscription(MF, ResMsg, AskMsg);
              Break;
            end;
        end;
    end;

    // --- GESTION ASYNC/SYNC ---
    if FAsynchronous then
    begin
      if LOwnsResMsg then
      begin
        FreeAndNil(ResMsg);
      end;
      Exit;
    end;

    if Assigned(ResMsg) then
    begin
      Result := ResMsg.Prompt;
      if (AskMsg.Role <> 'tool') and (AskMsg.ToolCallId = '') then
      begin
        if FMessages.IndexOf(ResMsg) = -1 then
        begin
          ResMsg.id := FMessages.Count + 1;
          FMessages.Add(ResMsg);
        end;
      end
      else if LOwnsResMsg then
        FreeAndNil(ResMsg);
    end;

  except
    on E: Exception do
    begin
      if LOwnsResMsg and Assigned(ResMsg) then
        if FMessages.IndexOf(ResMsg) = -1 then
          ResMsg.Free;
      DoError(E.Message, E);
    end;
  end;
end;

// ===========================================================================
//  Nuevo sistema de orquestacion v3.3
// ===========================================================================

procedure TAiChat.EnsureNewSystemConfig;
begin
  if FNewSystemConfigured then
    Exit;
  FModelCaps  := LegacyToModelCaps;
  FSessionCaps:= LegacyToSessionCaps;
end;

function TAiChat.LegacyToModelCaps: TAiCapabilities;
begin
  Result := [];
  if Tcm_Image          in FChatMediaSupports then Include(Result, cap_Image);
  if Tcm_Audio          in FChatMediaSupports then Include(Result, cap_Audio);
  if Tcm_Video          in FChatMediaSupports then Include(Result, cap_Video);
  if Tcm_Pdf            in FChatMediaSupports then Include(Result, cap_Pdf);
  if tcm_WebSearch      in FChatMediaSupports then Include(Result, cap_WebSearch);
  if Tcm_Reasoning      in FChatMediaSupports then Include(Result, cap_Reasoning);
  if Tcm_CodeInterpreter in FChatMediaSupports then Include(Result, cap_CodeInterpreter);
  if Tcm_Memory         in FChatMediaSupports then Include(Result, cap_Memory);
  if Tcm_TextEditor     in FChatMediaSupports then Include(Result, cap_TextEditor);
  if Tcm_ComputerUse    in FChatMediaSupports then Include(Result, cap_ComputerUse);
  if Tcm_Shell          in FChatMediaSupports then Include(Result, cap_Shell);
  if Tcm_ReportGeneration in FChatMediaSupports then Include(Result, cap_GenReport);
  if (Tfc_Audio in FNativeOutputFiles) and (Tcm_Audio in FChatMediaSupports) then
    Include(Result, cap_GenAudio);
  if (Tfc_Image in FNativeOutputFiles) and (Tcm_Image in FChatMediaSupports) then
    Include(Result, cap_GenImage);
  if (Tfc_Video in FNativeOutputFiles) and (Tcm_Video in FChatMediaSupports) then
    Include(Result, cap_GenVideo);
  if tfc_ExtractTextFile in FNativeOutputFiles then
    Include(Result, cap_ExtractCode);
end;

function TAiChat.LegacyToSessionCaps: TAiCapabilities;
begin
  Result := LegacyToModelCaps;
  if Tcm_Image          in FEnabledFeatures then Include(Result, cap_Image);
  if Tcm_Audio          in FEnabledFeatures then Include(Result, cap_Audio);
  if Tcm_Video          in FEnabledFeatures then Include(Result, cap_Video);
  if Tcm_Pdf            in FEnabledFeatures then Include(Result, cap_Pdf);
  if tcm_WebSearch      in FEnabledFeatures then Include(Result, cap_WebSearch);
  if Tcm_Reasoning      in FEnabledFeatures then Include(Result, cap_Reasoning);
  if Tcm_CodeInterpreter in FEnabledFeatures then Include(Result, cap_CodeInterpreter);
  if Tcm_Memory         in FEnabledFeatures then Include(Result, cap_Memory);
  if Tcm_TextEditor     in FEnabledFeatures then Include(Result, cap_TextEditor);
  if Tcm_ComputerUse    in FEnabledFeatures then Include(Result, cap_ComputerUse);
  if Tcm_Shell          in FEnabledFeatures then Include(Result, cap_Shell);
  if Tfc_Audio   in FNativeOutputFiles then Include(Result, cap_GenAudio);
  if Tfc_Image   in FNativeOutputFiles then Include(Result, cap_GenImage);
  if Tfc_Video   in FNativeOutputFiles then Include(Result, cap_GenVideo);
  if Tfc_Report  in FNativeOutputFiles then Include(Result, cap_GenReport);
  if tfc_ExtractTextFile in FNativeOutputFiles then Include(Result, cap_ExtractCode);
end;

function TAiChat.FileTypeInModelCaps(ACategory: TAiFileCategory): Boolean;
begin
  case ACategory of
    Tfc_Image: Result := cap_Image in FModelCaps;
    Tfc_Audio: Result := cap_Audio in FModelCaps;
    Tfc_Video: Result := cap_Video in FModelCaps;
    Tfc_Pdf:   Result := cap_Pdf   in FModelCaps;
  else
    Result := False;
  end;
end;

procedure TAiChat.SyncLegacyFromModelCaps;
var
  LNewInputFiles: TAiFileCategories;
  LNewSupports  : TAiChatMediaSupports;
begin
  LNewInputFiles := [];
  if cap_Image in FModelCaps then Include(LNewInputFiles, Tfc_Image);
  if cap_Audio in FModelCaps then Include(LNewInputFiles, Tfc_Audio);
  if cap_Video in FModelCaps then Include(LNewInputFiles, Tfc_Video);
  if cap_Pdf   in FModelCaps then Include(LNewInputFiles, Tfc_Pdf);
  SetNativeInputFiles(LNewInputFiles);

  LNewSupports := [];
  if cap_Image          in FModelCaps then Include(LNewSupports, Tcm_Image);
  if cap_Audio          in FModelCaps then Include(LNewSupports, Tcm_Audio);
  if cap_Video          in FModelCaps then Include(LNewSupports, Tcm_Video);
  if cap_Pdf            in FModelCaps then Include(LNewSupports, Tcm_Pdf);
  if cap_WebSearch      in FModelCaps then Include(LNewSupports, tcm_WebSearch);
  if cap_Reasoning      in FModelCaps then Include(LNewSupports, Tcm_Reasoning);
  if cap_CodeInterpreter in FModelCaps then Include(LNewSupports, Tcm_CodeInterpreter);
  if cap_Memory         in FModelCaps then Include(LNewSupports, Tcm_Memory);
  if cap_TextEditor     in FModelCaps then Include(LNewSupports, Tcm_TextEditor);
  if cap_ComputerUse    in FModelCaps then Include(LNewSupports, Tcm_ComputerUse);
  if cap_Shell          in FModelCaps then Include(LNewSupports, Tcm_Shell);
  if cap_GenReport      in FModelCaps then Include(LNewSupports, Tcm_ReportGeneration);
  FChatMediaSupports := LNewSupports;
end;

procedure TAiChat.SyncLegacyFromSessionCaps;
var
  LNewOutput : TAiFileCategories;
  LNewEnabled: TAiChatMediaSupports;
begin
  LNewOutput := [];
  if cap_GenAudio   in FSessionCaps then Include(LNewOutput, Tfc_Audio);
  if cap_GenImage   in FSessionCaps then Include(LNewOutput, Tfc_Image);
  if cap_GenVideo   in FSessionCaps then Include(LNewOutput, Tfc_Video);
  if cap_GenReport  in FSessionCaps then Include(LNewOutput, Tfc_Report);
  if cap_ExtractCode in FSessionCaps then Include(LNewOutput, tfc_ExtractTextFile);
  FNativeOutputFiles := LNewOutput;

  LNewEnabled := [];
  if cap_Image          in FSessionCaps then Include(LNewEnabled, Tcm_Image);
  if cap_Audio          in FSessionCaps then Include(LNewEnabled, Tcm_Audio);
  if cap_Video          in FSessionCaps then Include(LNewEnabled, Tcm_Video);
  if cap_Pdf            in FSessionCaps then Include(LNewEnabled, Tcm_Pdf);
  if cap_WebSearch      in FSessionCaps then Include(LNewEnabled, tcm_WebSearch);
  if cap_Reasoning      in FSessionCaps then Include(LNewEnabled, Tcm_Reasoning);
  if cap_CodeInterpreter in FSessionCaps then Include(LNewEnabled, Tcm_CodeInterpreter);
  if cap_Memory         in FSessionCaps then Include(LNewEnabled, Tcm_Memory);
  if cap_TextEditor     in FSessionCaps then Include(LNewEnabled, Tcm_TextEditor);
  if cap_ComputerUse    in FSessionCaps then Include(LNewEnabled, Tcm_ComputerUse);
  if cap_Shell          in FSessionCaps then Include(LNewEnabled, Tcm_Shell);
  FEnabledFeatures := LNewEnabled;
end;

// ===========================================================================
//  Stubs de InternalRun* para Fase 2
// ===========================================================================

function TAiChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiSpeechTool;
begin
  Result := '';
  if Assigned(FSpeechTool) and Supports(FSpeechTool, IAiSpeechTool, LTool) then
  begin
    if FSpeechTool is TAiCustomTool then
      TAiCustomTool(FSpeechTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Generando audio...');
    LTool.ExecuteSpeechGeneration(AskMsg.Prompt, ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de audio (SpeechTool).');
end;

function TAiChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiImageTool;
begin
  Result := '';
  if Assigned(FImageTool) and Supports(FImageTool, IAiImageTool, LTool) then
  begin
    if FImageTool is TAiCustomTool then
      TAiCustomTool(FImageTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Generando imagen...');
    LTool.ExecuteImageGeneration(AskMsg.Prompt, ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de imagen (ImageTool).');
end;

function TAiChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiVideoTool;
begin
  Result := '';
  if Assigned(FVideoTool) and Supports(FVideoTool, IAiVideoTool, LTool) then
  begin
    if FVideoTool is TAiCustomTool then
      TAiCustomTool(FVideoTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Generando video...');
    LTool.ExecuteVideoGeneration(ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de video (VideoTool).');
end;

function TAiChat.InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiWebSearchTool;
begin
  Result := '';
  if Assigned(FWebSearchTool) and Supports(FWebSearchTool, IAiWebSearchTool, LTool) then
  begin
    if FWebSearchTool is TAiCustomTool then
      TAiCustomTool(FWebSearchTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Buscando en la web...');
    LTool.ExecuteSearch(AskMsg.Prompt, ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de busqueda (WebSearchTool).');
end;

function TAiChat.InternalRunReport(ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiReportTool;
begin
  Result := '';
  if Assigned(FReportTool) and Supports(FReportTool, IAiReportTool, LTool) then
  begin
    if FReportTool is TAiCustomTool then
      TAiCustomTool(FReportTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Generando reporte...');
    LTool.ExecuteReport(ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    InternalRunCompletions(ResMsg, AskMsg);
end;

function TAiChat.InternalRunTranscription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiSpeechTool;
begin
  Result := '';
  if Assigned(FSpeechTool) and Supports(FSpeechTool, IAiSpeechTool, LTool) then
  begin
    if FSpeechTool is TAiCustomTool then
      TAiCustomTool(FSpeechTool).SetContext(Self);
    LTool.ExecuteTranscription(aMediaFile, ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de transcripcion (SpeechTool).');
end;

function TAiChat.InternalRunImageDescription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiVisionTool;
begin
  Result := '';
  if Assigned(FVisionTool) and Supports(FVisionTool, IAiVisionTool, LTool) then
  begin
    if FVisionTool is TAiCustomTool then
      TAiCustomTool(FVisionTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Analizando imagen...');
    LTool.ExecuteImageDescription(aMediaFile, ResMsg, AskMsg);
    if not FAsynchronous then Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de vision (VisionTool).');
end;

function TAiChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
var LTool: IAiPdfTool;
begin
  Result := '';
  if Assigned(FPdfTool) and Supports(FPdfTool, IAiPdfTool, LTool) then
  begin
    if FPdfTool is TAiCustomTool then
      TAiCustomTool(FPdfTool).SetContext(Self);
    DoStateChange(acsToolExecuting, 'Analizando PDF...');
    LTool.ExecutePdfAnalysis(aMediaFile, ResMsg, AskMsg);
    if not FAsynchronous then Result := aMediaFile.Transcription;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de PDF (PdfTool).');
end;

// ===========================================================================
//  API publica
// ===========================================================================

function TAiChat.AddMessageAndRun(aPrompt, aRole: string;
    aMediaFiles: TAiMediaFilesArray): string;
begin
  if aMediaFiles = nil then
    InternalAddMessage(aPrompt, aRole, TAiMediaFilesArray(nil))
  else
    InternalAddMessage(aPrompt, aRole, aMediaFiles);
  Result := Run(nil, nil);
end;

function TAiChat.AddMessageAndRunMsg(aPrompt, aRole: string;
    aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
begin
  if FAsynchronous then
    raise Exception.Create('Esta funcion no es compatible con el modo asincrono');
  AddMessageAndRun(aPrompt, aRole, aMediaFiles);
  Result := GetLastMessage;
end;

// Sobrecarga protected — compatibilidad con Delphi (AddMessageAndRun de 4 params)
function TAiChat.AddMessageAndRun(aPrompt, aRole: string; aToolCallId: string;
    aFunctionName: string): string;
begin
  Result := AddMessageAndRunTool(aPrompt, aRole, aToolCallId, aFunctionName);
end;

function TAiChat.AddMessageAndRunTool(aPrompt, aRole: string; aToolCallId: string;
    aFunctionName: string): string;
begin
  InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName);
  Result := Run(nil, nil);
end;

function TAiChat.AddMessage(aPrompt, aRole: string): TAiChatMessage;
begin
  Result := InternalAddMessage(aPrompt, aRole, TAiMediaFilesArray(nil));
end;

function TAiChat.NewMessage(aPrompt, aRole: string): TAiChatMessage;
begin
  Result := TAiChatMessage.Create(aPrompt, aRole);
end;

function TAiChat.GetLastMessage: TAiChatMessage;
begin
  Result := nil;
  if FMessages.Count > 0 then
    Result := FMessages[FMessages.Count - 1];
end;

function TAiChat.RemoveMessage(Msg: TAiChatMessage): Boolean;
begin
  Result := FMessages.Remove(Msg) >= 0;
end;

function TAiChat.RemoveMessage(IdMsg: Integer): Boolean;
var I: Integer;
begin
  for I := 0 to FMessages.Count - 1 do
    if FMessages[I].id = IdMsg then
    begin
      FMessages.Delete(I);
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TAiChat.NewChat;
var I: Integer;
begin
  for I := FMessages.Count - 1 downto 0 do
  begin
    FMessages[I].Free;
    FMessages.Delete(I);
  end;
  FMessages.Clear;
end;

procedure TAiChat.Abort;
begin
  FAbort := True;
  if Assigned(FHttpThread) then
    FHttpThread.RequestAbort;
end;

procedure TAiChat.AddToMemory(Key, Value: string);
begin
  FMemory.Values[Key] := Value;
end;

procedure TAiChat.RemoveFromMemory(Key: string);
begin
  FMemory.Values[Key] := '';
end;

function TAiChat.PublicChatToSend: string;
begin
  Result := InitChatCompletions;
end;

// ===========================================================================
//  GetModels — obtiene lista de modelos del endpoint (formato OpenAI)
// ===========================================================================

class function TAiChat.GetModels(aApiKey, aUrl: string): TStringList;
var
  Client       : TFPHTTPClient;
  RespStream   : TStringStream;
  sUrl         : string;
  jData        : TJSONData;
  jRes         : TJSONObject;
  JArr         : TJSONArray;
  sModel       : string;
  CustomModels : TAiStringArray;
  I            : Integer;
begin
  Result := TStringList.Create;

  if aUrl <> '' then
    sUrl := aUrl
  else
    sUrl := GlOpenAIUrl;

  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';
  sUrl := sUrl + 'models';

  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + aApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    try
      Client.Get(sUrl, RespStream);
    except
      on E: Exception do
      begin
        Result.Add('Error: ' + E.Message);
        Exit;
      end;
    end;

    if Client.ResponseStatusCode = 200 then
    begin
      jData := GetJSON(RespStream.DataString);
      if Assigned(jData) and (jData is TJSONObject) then
      begin
        jRes := TJSONObject(jData);
        try
          JArr := JGetArr(jRes, 'data');
          if Assigned(JArr) then
            for I := 0 to JArr.Count - 1 do
            begin
              if JArr.Items[I] is TJSONObject then
              begin
                sModel := JGetStr(TJSONObject(JArr.Items[I]), 'id');
                if sModel <> '' then
                  Result.Add(sModel);
              end;
            end;
        finally
          jRes.Free;
        end;
      end
      else
        FreeAndNil(jData);
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s',
          [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Client.Free;
    RespStream.Free;
  end;

  // Agregar modelos personalizados registrados en el factory
  CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);
  for I := Low(CustomModels) to High(CustomModels) do
    if Result.IndexOf(CustomModels[I]) = -1 then
      Result.Add(CustomModels[I]);
end;

function TAiChat.GetModels: TStringList;
begin
  Result := GetModels(ApiKey, FUrl);
end;

// ===========================================================================
//  Stubs de gestion de archivos remotos
// ===========================================================================

function TAiChat.UploadFile(aMediaFile: TAiMediaFile): string;      begin Result := ''; end;
function TAiChat.DownloadFile(aMediaFile: TAiMediaFile): string;    begin Result := ''; end;
function TAiChat.CheckFileState(aMediaFile: TAiMediaFile): string;  begin Result := ''; end;
function TAiChat.DeleteFile(aMediaFile: TAiMediaFile): string;      begin Result := ''; end;

function TAiChat.RetrieveFile(aFileId: string): TAiMediaFile;
begin
  Result := nil; // Clase base: los drivers (Gemini, OpenAI) sobreescriben
end;

function TAiChat.RetrieveFileList: TAiMediaFiles;
begin
  Result := nil; // Clase base: los drivers sobreescriben
end;

function TAiChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): string;
begin
  Result := ''; // Clase base: implementado en TAiGeminiChat (Context Caching)
end;

// ===========================================================================
//  ParseDeltaContentArray — hook para drivers con delta.content como array
// ===========================================================================

procedure TAiChat.ParseDeltaContentArray(AContentArr: TJSONArray; jObj: TJSONObject);
begin
  // Implementacion base vacia.
  // Drivers que reciben delta.content como array tipado (ej: Mistral magistral)
  // sobreescriben este metodo para procesar los bloques {"type":"text"/"thinking",...}.
end;

// ===========================================================================
//  FileCategoriesToString / StringToFileCategories — serializacion de TAiFileCategories
// ===========================================================================

function TAiChat.FileCategoriesToString(const ACategories: TAiFileCategories): string;
var
  Category: TAiFileCategory;
begin
  Result := '';
  if ACategories = [] then
    Exit;
  for Category := Low(TAiFileCategory) to High(TAiFileCategory) do
    if Category in ACategories then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo(TAiFileCategory), Ord(Category));
    end;
end;

function TAiChat.StringToFileCategories(const AValue: string): TAiFileCategories;
var
  Remaining    : string;
  CategoryName : string;
  EnumValue    : Integer;
  P            : Integer;
begin
  Result := [];
  if AValue = '' then
    Exit;
  Remaining := AValue;
  while Remaining <> '' do
  begin
    P := Pos(',', Remaining);
    if P > 0 then
    begin
      CategoryName := Trim(Copy(Remaining, 1, P - 1));
      Delete(Remaining, 1, P);
    end
    else
    begin
      CategoryName := Trim(Remaining);
      Remaining := '';
    end;
    if CategoryName <> '' then
    begin
      EnumValue := GetEnumValue(TypeInfo(TAiFileCategory), CategoryName);
      if EnumValue >= 0 then
        Include(Result, TAiFileCategory(EnumValue));
    end;
  end;
end;

// ===========================================================================
//  Implementacion de IAiToolContext
// ===========================================================================

procedure TAiChat.DoData(Msg: TAiChatMessage; const Role, Text: string;
    aResponse: TJSONObject);
begin
  if Assigned(FOnReceiveDataEvent) then
    FOnReceiveDataEvent(Self, Msg, aResponse, Role, Text);
end;

procedure TAiChat.DoDataEnd(Msg: TAiChatMessage; const Role, Text: string;
    aResponse: TJSONObject);
begin
  FBusy := False;
  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, Msg, aResponse, Role, Text);
end;

procedure TAiChat.DoThinking(Msg: TAiChatMessage; const Role, Text: string;
    aResponse: TJSONObject);
begin
  if Assigned(FOnReceiveThinking) then
    FOnReceiveThinking(Self, Msg, aResponse, Role, Text);
end;

procedure TAiChat.DoProgress(const AStatus: string; ACompleted, ATotal: Int64);
begin
  if Assigned(FOnProgressEvent) then
    FOnProgressEvent(Self, AStatus, ACompleted, ATotal);
end;

procedure TAiChat.StartHttpThread(const sUrl, ABody: string;
    const AHeaders: array of string);
begin
  FHttpThread := TAiHttpThread.Create(sUrl, ABody, AHeaders, Self);
  FHttpThread.Start;
end;

procedure TAiChat.OnRequestErrorEvent(const AError: string);
begin
  DoError(AError, nil);
end;

procedure TAiChat.OnRequestExceptionEvent(const AError: Exception);
begin
  DoError(AError.Message, AError);
end;

procedure TAiChat.DoError(const ErrorMsg: string; E: Exception);
begin
  DoStateChange(acsError, ErrorMsg);
  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg, E, nil);
end;

procedure TAiChat.DoProcessResponse(aLastMsg, aResMsg: TAiChatMessage;
    var aResponse: string);
begin
  if Assigned(FOnProcessResponse) then
    FOnProcessResponse(Self, aLastMsg, aResMsg, aResponse);
end;

procedure TAiChat.DoStateChange(State: TAiChatState; const Description: string);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, State, Description);
end;

function TAiChat.GetAsynchronous: Boolean;
begin
  Result := FAsynchronous;
end;

procedure TAiChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin
  if Assigned(FAiFunctions) and FAiFunctions.DoCallFunction(ToolCall) then
    // la funcion fue ejecutada por FAiFunctions
  else if Assigned(FOnCallToolFunction) then
    FOnCallToolFunction(Self, ToolCall);
end;

function TAiChat.GetToolsStr(aToolFormat: TToolFormat): string;
begin
  if Assigned(FAiFunctions) and FTool_Active then
    Result := FAiFunctions.GetTools(aToolFormat)
  else
    Result := '';
end;

procedure TAiChat.DoProcessMediaFile(aPrompt: string; aAiMediaFile: TAiMediaFile;
    var Respuesta: string; var Procesado: Boolean);
begin
  Procesado := False;
  if Assigned(FOnProcessMediaFile) then
  begin
    Respuesta := '';
    FOnProcessMediaFile(Self, aPrompt, aAiMediaFile, FNativeInputFiles,
        Respuesta, Procesado);
    aAiMediaFile.Procesado     := Procesado;
    aAiMediaFile.Transcription := Respuesta;
  end;
end;

// ===========================================================================
//  GetApiKey — resolucion @ENV_VAR
// ===========================================================================

function TAiChat.GetApiKey: string;
begin
  if (Length(FApiKey) > 1) and (FApiKey[1] = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

// ===========================================================================
//  Notification (liberacion de referencias a componentes)
// ===========================================================================

procedure TAiChat.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FSpeechTool    then FSpeechTool    := nil;
    if AComponent = FImageTool     then FImageTool     := nil;
    if AComponent = FComputerUseTool then FComputerUseTool := nil;
    if AComponent = FVideoTool     then FVideoTool     := nil;
    if AComponent = FWebSearchTool then FWebSearchTool := nil;
    if AComponent = FVisionTool    then FVisionTool    := nil;
    if AComponent = FPdfTool       then FPdfTool       := nil;
    if AComponent = FReportTool    then FReportTool    := nil;
    if AComponent = FShellTool     then FShellTool     := nil;
    if AComponent = FTextEditorTool then FTextEditorTool := nil;
  end;
end;

// ===========================================================================
//  Setters
// ===========================================================================

procedure TAiChat.SetApiKey(const Value: string);         begin FApiKey          := Value; end;
procedure TAiChat.SetModel(const Value: string);          begin FModel           := Value; end;
procedure TAiChat.SetUrl(const Value: string);
begin
  if Value <> '' then
    FUrl := Value
  else
    FUrl := GlOpenAIUrl;
end;
procedure TAiChat.SetUser(const Value: string);           begin FUser            := Value; end;
procedure TAiChat.SetStop(const Value: string);           begin FStop            := Value; end;
procedure TAiChat.SetTool_choice(const Value: string);    begin FTool_choice     := Value; end;
procedure TAiChat.SetSeed(const Value: Integer);          begin FSeed            := Value; end;
procedure TAiChat.SetN(const Value: Integer);             begin FN               := Value; end;
procedure TAiChat.SetTop_p(const Value: Double);
var TmpVal: Double;
begin
  if Value > 1 then
    TmpVal := 1
  else if Value < 0.1 then
    TmpVal := 0.1
  else
    TmpVal := Value;
  FTop_p := TmpVal;
end;
procedure TAiChat.SetTemperature(const Value: Double);    begin FTemperature     := Value; end;
procedure TAiChat.SetFrequency_penalty(const Value: Double); begin FFrequency_penalty := Value; end;
procedure TAiChat.SetPresence_penalty(const Value: Double);  begin FPresence_penalty  := Value; end;
procedure TAiChat.SetLogit_bias(const Value: string);     begin FLogit_bias      := Value; end;
procedure TAiChat.SetTop_logprobs(const Value: string);   begin FTop_logprobs    := Value; end;
procedure TAiChat.SetLogprobs(const Value: Boolean);      begin FLogprobs        := Value; end;
procedure TAiChat.SetMax_tokens(const Value: Integer);    begin FMax_tokens      := Value; end;
procedure TAiChat.SetResponseTimeOut(const Value: Integer);
begin
  if Value < 61000 then
    FResponseTimeOut := 61000
  else
    FResponseTimeOut := Value;
end;
procedure TAiChat.SetVoice(const Value: string);          begin FVoice           := Value; end;
procedure TAiChat.SetVoice_format(const Value: string);   begin FVoice_format    := Value; end;
procedure TAiChat.SetLanguage(const Value: string);       begin FLanguage        := Value; end;
procedure TAiChat.SetTranscription_ResponseFormat(const Value: string);
  begin FTranscription_ResponseFormat := Value; end;
procedure TAiChat.SetTranscription_TimestampGranularities(const Value: string);
  begin FTranscription_TimestampGranularities := Value; end;
procedure TAiChat.SetReasoningFormat(const Value: string); begin FReasoningFormat := Value; end;
procedure TAiChat.SetK(const Value: Integer);              begin FK               := Value; end;
procedure TAiChat.SetStream_Usage(const Value: Boolean);   begin FStream_Usage    := Value; end;
procedure TAiChat.SetAsynchronous(const Value: Boolean);   begin FAsynchronous    := Value; end;
procedure TAiChat.SetTool_Active(const Value: Boolean);    begin FTool_Active     := Value; end;
procedure TAiChat.SetChatMode_prop(const Value: TAiChatMode); begin FChatMode     := Value; end;
procedure TAiChat.SetResponse_format(const Value: TAiChatResponseFormat);
  begin FResponse_format := Value; end;
procedure TAiChat.SetThinkingLevel(const Value: TAiThinkingLevel);
  begin FThinkingLevel := Value; end;
procedure TAiChat.SetMediaResolution(const Value: TAiMediaResolution);
  begin FMediaResolution := Value; end;
procedure TAiChat.SetNativeInputFiles(const Value: TAiFileCategories);
begin
  FNativeInputFiles := Value;
  if Assigned(FMessages) then
    FMessages.NativeInputFiles := Value;
end;
procedure TAiChat.SetNativeOutputFiles(const Value: TAiFileCategories);
  begin FNativeOutputFiles := Value; end;
procedure TAiChat.SetChatMediaSupports(const Value: TAiChatMediaSupports);
  begin FChatMediaSupports := Value; end;
procedure TAiChat.SetEnabledFeatures(const Value: TAiChatMediaSupports);
  begin FEnabledFeatures := Value; end;
procedure TAiChat.SetModelCaps(const Value: TAiCapabilities);
begin
  FModelCaps := Value;
  FNewSystemConfigured := True;
  SyncLegacyFromModelCaps;
end;
procedure TAiChat.SetSessionCaps(const Value: TAiCapabilities);
begin
  FSessionCaps := Value;
  FNewSystemConfigured := True;
  SyncLegacyFromSessionCaps;
end;
procedure TAiChat.SetAiFunctions(const Value: TAiFunctions);
  begin FAiFunctions := Value; end;
procedure TAiChat.SetShellTool(const Value: TAiShell);
begin
  if FShellTool <> Value then
  begin
    FShellTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetTextEditorTool(const Value: TAiTextEditorTool);
begin
  if FTextEditorTool <> Value then
  begin
    FTextEditorTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetComputerUseTool(const Value: TAiComputerUseTool);
begin
  if FComputerUseTool <> Value then
  begin
    FComputerUseTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetSpeechTool(const Value: TAiSpeechToolBase);
begin
  if FSpeechTool <> Value then
  begin
    FSpeechTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetImageTool(const Value: TAiImageToolBase);
begin
  if FImageTool <> Value then
  begin
    FImageTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetVideoTool(const Value: TAiVideoToolBase);
begin
  if FVideoTool <> Value then
  begin
    FVideoTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetWebSearchTool(const Value: TAiWebSearchToolBase);
begin
  if FWebSearchTool <> Value then
  begin
    FWebSearchTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetVisionTool(const Value: TAiVisionToolBase);
begin
  if FVisionTool <> Value then
  begin
    FVisionTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetPdfTool(const Value: TAiPdfToolBase);
begin
  if FPdfTool <> Value then
  begin
    FPdfTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetReportTool(const Value: TAiReportToolBase);
begin
  if FReportTool <> Value then
  begin
    FReportTool := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;
procedure TAiChat.SetSystemPrompt(const Value: TStrings);
  begin FSystemPrompt.Assign(Value); end;
procedure TAiChat.SetMemory(const Value: TStrings);
  begin FMemory.Assign(Value); end;
procedure TAiChat.SetJsonSchema(const Value: TStrings);
  begin FJsonSchema.Assign(Value); end;
procedure TAiChat.SetImageParams(const Value: TStrings);
  begin FImageParams.Assign(Value); end;
procedure TAiChat.SetVideoParams(const Value: TStrings);
  begin FVideoParams.Assign(Value); end;
procedure TAiChat.SetWebSearchParams(const Value: TStrings);
  begin FWebSearchParams.Assign(Value); end;
procedure TAiChat.SetOnReceiveDataEvent(const Value: TAiChatOnDataEvent);
  begin FOnReceiveDataEvent := Value; end;
procedure TAiChat.SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
  begin FOnReceiveDataEnd := Value; end;
procedure TAiChat.SetOnAddMessage(const Value: TAiChatOnDataEvent);
  begin FOnAddMessage := Value; end;
procedure TAiChat.SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
  begin FOnReceiveThinking := Value; end;
procedure TAiChat.SetOnCallToolFunction(const Value: TOnCallToolFunction);
  begin FOnCallToolFunction := Value; end;
procedure TAiChat.SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
  begin FOnBeforeSendMessage := Value; end;
procedure TAiChat.SetOnInitChat(const Value: TAiChatOnInitChatEvent);
  begin FOnInitChat := Value; end;
procedure TAiChat.SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
  begin FOnProcessMediaFile := Value; end;
procedure TAiChat.SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
  begin FOnProcessResponse := Value; end;
procedure TAiChat.SetOnProgressEvent(const Value: TAiModelProgressEvent);
  begin FOnProgressEvent := Value; end;
procedure TAiChat.SetOnError(const Value: TAiErrorEvent);
  begin FOnError := Value; end;
procedure TAiChat.SetCompletion_tokens(const Value: Integer);
  begin FCompletion_tokens := Value; end;
procedure TAiChat.SetPrompt_tokens(const Value: Integer);
  begin FPrompt_tokens := Value; end;
procedure TAiChat.SetTotal_tokens(const Value: Integer);
  begin FTotal_tokens := Value; end;
procedure TAiChat.SetThinking_tokens(const Value: Integer);
  begin FThinking_tokens := Value; end;
procedure TAiChat.SetLastError(const Value: string);
  begin FLastError := Value; end;

end.
