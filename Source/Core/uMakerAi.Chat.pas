// MIT License
//
// Copyright (c) 2013 Gustavo Enr?quez - CimaMaker
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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// --------- CAMBIOS --------------------
// 29/08/2024 - Se adiciona el manejo de response_format = json_schema
// 04/11/2024 - adiciona el manejo de detail como propiedad en mediafile
// 04/11/2024 - adiciona el manejo de TAiChat.Stream_Usage - Estadistica de uso en modo stream OpenAi



// https://platform.openai.com/docs/guides/tools-code-interpreter

unit uMakerAi.Chat;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.StrUtils,
  System.Threading, System.TypInfo, System.Types, System.Net.Mime,
  System.NetConsts, System.NetEncoding, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.JSON, Rest.JSON, uMakerAi.Chat.Messages,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Tools.Functions, uMakerAi.Core, uMakerAi.Utils.CodeExtractor, uMakerAi.Tools.Shell, uMakerAi.Tools.TextEditor, uMakerAi.Tools.ComputerUse, uMakerAi.Chat.Tools;

type

  TAiChatResponseFormat = (tiaChatRfText, tiaChatRfJson, tiaChatRfJsonSchema);

  TAiChatMode = (cmConversation, // Modo di?logo (Orquestaci?n Inteligente)
    cmImageGeneration, // Forzar Generaci?n de Imagen
    cmVideoGeneration, // Forzar Generaci?n de Video
    cmSpeechGeneration, // Forzar Texto a Voz (TTS)
    cmTranscription, // Forzar Transcripci?n
    cmWebSearch, // Forzar B?squeda Web
    cmReportGeneration // Forzar Generaci?n de Reporte (PDF, HTML, XLSX, etc.)
    );

  TAiChatOnDataEvent = procedure(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: String) of object;
  TAiChatOnBeforeSendEvent = procedure(const Sender: TObject; var aMsg: TAiChatMessage) of object;
  TAiChatOnInitChatEvent = procedure(const Sender: TObject; aRole: String; Var aText: String; Var aMemory: TJSonObject) of object;
  TAiChatOnMediaFileEvent = Procedure(Const Sender: TObject; Prompt: String; MediaFile: TAiMediaFile; aNativeInputFiles: TAiFileCategories; Var Respuesta: String; Var aProcesado: Boolean) of object;
  TAiChatOnProcessResponseEvent = procedure(const Sender: TObject; LastMsg, ResMsg: TAiChatMessage; var Response: String) of object;
  TAiModelProgressEvent = procedure(Sender: TObject; Status: string; Completed, Total: Int64) of object; // Se dispara cuando hay procesos largos

  // Evento callback cuando se utiliza la herramienta tools del chat
  TOnCallToolFunction = Procedure(Sender: TObject; AiToolCall: TAiToolsFunction) of object;

  TAiChat = class(TComponent, IAiToolContext)
  Private
    FOwner: TObject;
    FApiKey: String;
    FSeed: Integer;
    FTool_choice: string;
    FN: Integer;
    FTop_p: Double;
    FLogprobs: Boolean;
    FFrequency_penalty: Double;
    FStop: string;
    FLogit_bias: String;
    FTemperature: Double;
    FPresence_penalty: Double;
    FUser: String;
    FMax_tokens: Integer;
    FAsynchronous: Boolean;
    FTop_logprobs: String;
    FModel: String;
    FSystemPrompt: TStrings;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;
    FPrompt_tokens: Integer;
    FTool_Active: Boolean;
    FUrl: String;
    FResponseTimeOut: Integer;
    FOnInitChat: TAiChatOnInitChatEvent;
    FMemory: TStrings;

    FAiFunctions: TAiFunctions;
    FOnProcessMediaFile: TAiChatOnMediaFileEvent;
    FJsonSchema: TStrings;
    FStream_Usage: Boolean;
    FNativeInputFiles: TAiFileCategories;
    FNativeOutputFiles: TAiFileCategories;
    FOnError: TAiErrorEvent;
    FOnProcessResponse: TAiChatOnProcessResponseEvent;
    FVoice: String;
    Fvoice_format: String;
    FLanguage: string;
    FTranscription_ResponseFormat: string;
    FTranscription_TimestampGranularities: string;
    FChatMediaSupports: TAiChatMediaSupports;
    FReasoningFormat: String;
    FK: Integer;
    FOnProgressEvent: TAiModelProgressEvent;
    FOnReceiveThinking: TAiChatOnDataEvent;
    FThinkingLevel: TAiThinkingLevel;
    FMediaResolution: TAiMediaResolution;
    FImageParams: TStrings;
    FVideoParams: TStrings;
    FWebSearchParams: TStrings;
    FOnStateChange: TAiStateChangeEvent;
    FComputerUseTool: TAiComputerUseTool;
    FSpeechTool: TAiSpeechToolBase;
    FImageTool: TAiImageToolBase;
    FVideoTool: TAiVideoToolBase;
    FWebSearchTool: TAiWebSearchToolBase;
    FVisionTool: TAiVisionToolBase;
    FChatMode: TAiChatMode;
    FEnabledFeatures: TAiChatMediaSupports;
    FPdfTool: TAiPdfToolBase;
    FReportTool: TAiReportToolBase;
    // Nuevo sistema de orquestación (v3.3)
    FModelCaps: TAiCapabilities; // capacidades nativas del modelo
    FSessionCaps: TAiCapabilities; // capacidades deseadas en la sesión
    FNewSystemConfigured: Boolean; // True si ModelCaps/SessionCaps fueron asignados explícitamente

    procedure SetApiKey(const Value: String);
    procedure SetFrequency_penalty(const Value: Double);
    procedure SetLogit_bias(const Value: String);
    procedure SetLogprobs(const Value: Boolean);
    procedure SetMax_tokens(const Value: Integer);
    procedure SetN(const Value: Integer);
    procedure SetPresence_penalty(const Value: Double);
    procedure SetResponse_format(const Value: TAiChatResponseFormat);
    procedure SetSeed(const Value: Integer);
    procedure SetStop(const Value: string);
    procedure SetTemperature(const Value: Double);
    procedure SetTool_choice(const Value: string);
    procedure SetTop_p(const Value: Double);
    procedure SetUser(const Value: String);
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetTop_logprobs(const Value: String);
    procedure SetOnReceiveDataEvent(const Value: TAiChatOnDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
    procedure SetModel(const Value: String);
    procedure SetSystemPrompt(const Value: TStrings);
    procedure SetOnAddMessage(const Value: TAiChatOnDataEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetTool_Active(const Value: Boolean);
    procedure SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
    procedure SetCompletion_tokens(const Value: Integer);
    procedure SetPrompt_tokens(const Value: Integer);
    procedure SetTotal_tokens(const Value: Integer);
    procedure SetUrl(const Value: String);
    procedure SetLastError(const Value: String);
    procedure SetResponseTimeOut(const Value: Integer);
    procedure SetOnInitChat(const Value: TAiChatOnInitChatEvent);
    procedure SetMemory(const Value: TStrings);
    procedure SetJsonSchema(const Value: TStrings);

    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
    procedure SetStream_Usage(const Value: Boolean);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
    procedure SetNativeOutputFiles(const Value: TAiFileCategories);
    procedure SetOnError(const Value: TAiErrorEvent);
    procedure SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
    procedure SetVoice(const Value: String);
    procedure Setvoice_format(const Value: String);
    procedure SetLanguage(const Value: string);
    procedure SetTranscription_ResponseFormat(const Value: string);
    procedure SetTranscription_TimestampGranularities(const Value: string);
    procedure SetChatMediaSupports(const Value: TAiChatMediaSupports);
    function GetApiKey: String;
    procedure SetReasoningFormat(const Value: String);
    procedure SetK(const Value: Integer);
    procedure SetOnProgressEvent(const Value: TAiModelProgressEvent);
    procedure SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
    procedure SetThinkingLevel(const Value: TAiThinkingLevel);
    procedure SetMediaResolution(const Value: TAiMediaResolution);
    procedure SetImageParams(const Value: TStrings);
    procedure SetVideoParams(const Value: TStrings);
    procedure SetWebSearchParams(const Value: TStrings);
    procedure SetShellTool(const Value: TAiShell);
    procedure SetThinking_tokens(const Value: Integer);
    procedure SetTextEditorTool(const Value: TAiTextEditorTool);
    procedure SetComputerUseTool(const Value: TAiComputerUseTool);
    procedure SetSpeechTool(const Value: TAiSpeechToolBase);
    procedure SetImageTool(const Value: TAiImageToolBase);
    procedure SetVideoTool(const Value: TAiVideoToolBase);
    procedure SetWebSearchTool(const Value: TAiWebSearchToolBase);
    procedure SetVisionTool(const Value: TAiVisionToolBase);
    procedure SetEnabledFeatures(const Value: TAiChatMediaSupports);
    procedure SetPdfTool(const Value: TAiPdfToolBase);
    procedure SetReportTool(const Value: TAiReportToolBase);
    // Nuevo sistema de orquestación (v3.3)
    procedure SetModelCaps(const Value: TAiCapabilities);
    procedure SetSessionCaps(const Value: TAiCapabilities);
    procedure EnsureNewSystemConfig;
    function LegacyToModelCaps: TAiCapabilities;
    function LegacyToSessionCaps: TAiCapabilities;
    function RunLegacy(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
    function RunNew(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
    function FileTypeInModelCaps(ACategory: TAiFileCategory): Boolean;
    procedure SyncLegacyFromModelCaps;
    procedure SyncLegacyFromSessionCaps;

  Protected
    FClient: TNetHTTPClient;
    FTmpRole: String;
    FTmpResponseText: String;
    FTmpResponseText1: String;
    FAbort: Boolean;
    FBusy: Boolean;
    FTools: TStrings;
    FLastContent: String;
    FLastReasoning: String;  // Acumula reasoning_content durante streaming SSE
    FLastPrompt: String;
    FLastError: String;
    FMessages: TAiChatMessages;
    FResponse_format: TAiChatResponseFormat;
    FResponse: TStringStream;
    FOnReceiveDataEvent: TAiChatOnDataEvent;
    FOnReceiveDataEnd: TAiChatOnDataEvent;
    FOnAddMessage: TAiChatOnDataEvent;
    FOnCallToolFunction: TOnCallToolFunction;
    FOnBeforeSendMessage: TAiChatOnBeforeSendEvent;
    FTmpToolCallBuffer: TObjectDictionary<Integer, TJSonObject>;
    FShellTool: TAiShell;
    FThinking_tokens: Integer;
    FTextEditorTool: TAiTextEditorTool;

    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Virtual;

    // Extensión para drivers con delta.content como array tipado (ej: Mistral magistral).
    // La base no hace nada; los drivers con formato no-estándar lo sobreescriben.
    procedure ParseDeltaContentArray(AContentArr: TJSonArray; jObj: TJSonObject); Virtual;

    Procedure OnRequestErrorEvent(const Sender: TObject; const AError: string); Virtual;
    Procedure OnRequestExceptionEvent(const Sender: TObject; const AError: Exception); Virtual;
    Procedure OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse); Virtual;

    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): TAiChatMessage; Overload; Virtual;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage; Overload; Virtual;
    Function InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage; Overload; Virtual;

    function InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunReport(ResMsg, AskMsg: TAiChatMessage): String; Virtual;

    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Virtual;
    function InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Virtual;

    // Inicializa el json de completions, se saca apaarte porque es complejo
    Function InitChatCompletions: String; Virtual;

    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Virtual;
    procedure ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);

    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Virtual; // Obtiene la lista de funciones a partir del json de respuesta en modo sincrono
    Procedure DoCallFunction(ToolCall: TAiToolsFunction); Virtual;
    function GetTools(aToolFormat: TToolFormat): TStrings; virtual;
    Function PrepareSystemMsg: String; Virtual; // Crea el primer mensaje del chat para system, para configurar el asistente
    Procedure DoProcessMediaFile(aPrompt: String; aAiMediaFile: TAiMediaFile; Var Respuesta: String; Var Procesado: Boolean);
    Function AddMessageAndRun(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String; Overload;
    // Implementaci?n de IAiToolContext
    procedure DoData(Msg: TAiChatMessage; const Role, Text: string; aResponse: TJSonObject = nil);
    procedure DoDataEnd(Msg: TAiChatMessage; const Role, Text: string; aResponse: TJSonObject = nil);
    function GetAsynchronous: Boolean;

    procedure DoError(const ErrorMsg: string; E: Exception); virtual;
    Procedure DoProcessResponse(aLastMsg, aResMsg: TAiChatMessage; var aResponse: String);
    procedure DoStateChange(State: TAiChatState; const Description: string = '');

    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String; Overload;
    Function AddMessageAndRunMsg(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage; Overload;

    Function AddMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje y lo adiciona a la conversaci?n
    Function NewMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje pero no lo adiciona a la conversaci?n
    Function Run(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage = Nil): String; Virtual;
    Function GetLastMessage: TAiChatMessage;
    Function RemoveMesage(Msg: TAiChatMessage): Boolean; Overload;
    Function RemoveMesage(IdMsg: Integer): Boolean; Overload;
    Procedure AddToMemory(Key, Value: String);
    Procedure RemoveFromMemory(Key: String);
    Procedure NewChat; Virtual;
    Procedure Abort;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Overload; virtual;
    Function GetModels: TStringList; Overload; Virtual;
    Function GetMessages: TJSonArray; Virtual;

    Function PublicChatToSend: String;

    Function FileCategoriesToString(const ACategories: TAiFileCategories): string;
    Function StringToFileCategories(const AValue: string): TAiFileCategories;

    Function UploadFile(aMediaFile: TAiMediaFile): String; Virtual;
    Function DownLoadFile(aMediaFile: TAiMediaFile): String; Virtual;
    Function CheckFileState(aMediaFile: TAiMediaFile): String; Virtual;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Virtual;
    function RetrieveFile(aFileId: string): TAiMediaFile; virtual;
    function RetrieveFileList: TAiMediaFiles; virtual;
    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Virtual;

    class function GetDriverName: string; virtual; abstract;
    class procedure RegisterDefaultParams(Params: TStrings); virtual; abstract;
    class function CreateInstance(Sender: TComponent): TAiChat; virtual; abstract;

    Property Messages: TAiChatMessages read FMessages;
    Property LastError: String read FLastError write SetLastError;

  Published
    Property ApiKey: String read GetApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property Frequency_penalty: Double read FFrequency_penalty write SetFrequency_penalty; // -2 to 2
    Property Logit_bias: String read FLogit_bias write SetLogit_bias;
    // Vac?o o entre -100 y 100
    Property Logprobs: Boolean read FLogprobs write SetLogprobs;
    Property Top_logprobs: String read FTop_logprobs write SetTop_logprobs;
    // vacio o between 0 and 5
    Property Max_tokens: Integer read FMax_tokens write SetMax_tokens;
    // 0 = null o el m?ximo
    Property N: Integer read FN write SetN;
    // Defaults to 1 How many Chat completion choices to generate for each input message.Note that you will be charged based on the number of generated tokens across all of the choices.Keep n as 1 to minimize costs.
    Property Presence_penalty: Double read FPresence_penalty write SetPresence_penalty; // Defaults to 0 number between - 2.0 and 2.0
    Property Response_format: TAiChatResponseFormat read FResponse_format write SetResponse_format;
    // object Optional an object specifying the format that the model must output.Compatible with gpt - 4 - 1106 - preview and gpt - 3.5 - turbo - 1106.
    Property Seed: Integer read FSeed write SetSeed; // 0 no se env?a
    Property Stop: string read FStop write SetStop;
    // Array de palabras separado por comas
    Property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    Property Temperature: Double read FTemperature write SetTemperature;
    // Defaults to 1  between 0 and 2.
    Property Top_p: Double read FTop_p write SetTop_p;
    Property K: Integer read FK write SetK;
    // Defaults to 0 si es 0 no se env?a,  entre 0 y 1
    // Property Tools: TStrings read GetTools;
    Property Tool_choice: string read FTool_choice write SetTool_choice;
    Property Tool_Active: Boolean read FTool_Active write SetTool_Active;
    Property User: String read FUser write SetUser;
    Property SystemPrompt: TStrings read FSystemPrompt write SetSystemPrompt;
    Property Prompt_tokens: Integer read FPrompt_tokens write SetPrompt_tokens;
    Property Completion_tokens: Integer read FCompletion_tokens write SetCompletion_tokens;
    Property Total_tokens: Integer read FTotal_tokens write SetTotal_tokens;
    Property Thinking_tokens: Integer read FThinking_tokens write SetThinking_tokens;
    Property LastContent: String Read FLastContent;
    Property LastPrompt: String Read FLastPrompt;
    Property Busy: Boolean Read FBusy;
    Property OnReceiveThinking: TAiChatOnDataEvent read FOnReceiveThinking write SetOnReceiveThinking;
    Property OnReceiveData: TAiChatOnDataEvent read FOnReceiveDataEvent write SetOnReceiveDataEvent;
    Property OnReceiveDataEnd: TAiChatOnDataEvent read FOnReceiveDataEnd write SetOnReceiveDataEnd;
    Property OnAddMessage: TAiChatOnDataEvent read FOnAddMessage write SetOnAddMessage;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property OnBeforeSendMessage: TAiChatOnBeforeSendEvent read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    Property OnInitChat: TAiChatOnInitChatEvent read FOnInitChat write SetOnInitChat;
    Property OnProcessResponse: TAiChatOnProcessResponseEvent read FOnProcessResponse write SetOnProcessResponse;
    Property OnProgressEvent: TAiModelProgressEvent read FOnProgressEvent write SetOnProgressEvent;
    Property OnProcessMediaFile: TAiChatOnMediaFileEvent read FOnProcessMediaFile write SetOnProcessMediaFile;

    Property Url: String read FUrl write SetUrl;
    Property ResponseTimeOut: Integer read FResponseTimeOut write SetResponseTimeOut;
    Property Memory: TStrings read FMemory Write SetMemory;
    Property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    // ------ chat tools ------
    property SpeechTool: TAiSpeechToolBase read FSpeechTool write SetSpeechTool;
    property ImageTool: TAiImageToolBase read FImageTool write SetImageTool;
    property VideoTool: TAiVideoToolBase read FVideoTool write SetVideoTool;
    property WebSearchTool: TAiWebSearchToolBase read FWebSearchTool write SetWebSearchTool;
    property VisionTool: TAiVisionToolBase read FVisionTool write SetVisionTool;
    property PdfTool: TAiPdfToolBase read FPdfTool write SetPdfTool;
    property ReportTool: TAiReportToolBase read FReportTool write SetReportTool;

    Property JsonSchema: TStrings read FJsonSchema write SetJsonSchema;
    Property Stream_Usage: Boolean read FStream_Usage write SetStream_Usage;
    property ChatMode: TAiChatMode read FChatMode write FChatMode default cmConversation;
    Property NativeInputFiles: TAiFileCategories read FNativeInputFiles write SetNativeInputFiles; // Archivos esperados de entrada
    Property NativeOutputFiles: TAiFileCategories read FNativeOutputFiles write SetNativeOutputFiles; // Archivos esperados de salida
    Property ChatMediaSupports: TAiChatMediaSupports read FChatMediaSupports write SetChatMediaSupports; // Capacidades del modelo seleccionado
    property EnabledFeatures: TAiChatMediaSupports read FEnabledFeatures write SetEnabledFeatures; // Capacidades del Chat esperados si el modelo no los tiene los delega

    Property Voice: String read FVoice write SetVoice;
    Property voice_format: String read Fvoice_format write Setvoice_format;
    property OnError: TAiErrorEvent read FOnError write SetOnError;
    property Language: string read FLanguage write SetLanguage; // e.g., 'es', 'en', 'es-419'
    property Transcription_ResponseFormat: string read FTranscription_ResponseFormat write SetTranscription_ResponseFormat;
    // 'json', 'text', 'verbose_json', etc.
    property Transcription_TimestampGranularities: string read FTranscription_TimestampGranularities write SetTranscription_TimestampGranularities; // 'word', 'segment', 'word,segment'
    Property ReasoningFormat: String read FReasoningFormat write SetReasoningFormat;
    property ThinkingLevel: TAiThinkingLevel read FThinkingLevel write SetThinkingLevel;
    Property MediaResolution: TAiMediaResolution read FMediaResolution write SetMediaResolution;
    Property VideoParams: TStrings read FVideoParams write SetVideoParams;
    Property ImageParams: TStrings read FImageParams write SetImageParams;
    Property WebSearchParams: TStrings read FWebSearchParams write SetWebSearchParams;
    property OnStateChange: TAiStateChangeEvent read FOnStateChange write FOnStateChange;
    property ShellTool: TAiShell read FShellTool write SetShellTool;
    Property TextEditorTool: TAiTextEditorTool read FTextEditorTool write SetTextEditorTool;
    property ComputerUseTool: TAiComputerUseTool read FComputerUseTool write SetComputerUseTool;
    // Nuevo sistema de orquestación (v3.3)
    property ModelCaps: TAiCapabilities read FModelCaps write SetModelCaps; // capacidades nativas del modelo
    property SessionCaps: TAiCapabilities read FSessionCaps write SetSessionCaps; // capacidades deseadas en la sesión
  end;

  // procedure Register;

procedure LogDebug(const Mensaje: string);

implementation

uses uMakerAi.ParamsRegistry;

{ TAiChat }

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure LogDebug(const Mensaje: string);
var
  Archivo: TextFile;
  RutaLog: string;
begin
  // ---------------------------------------------------------------------------------
  // -------- OPCI?N DESHABILITADA ES SOLO UN LOG DE PRUEBAS--------------------------
  // ---------------------------------------------------------------------------------
  RutaLog := 'c:\temp\ialog.txt';

  try
    AssignFile(Archivo, RutaLog);

    // Si el archivo existe, lo abre para agregar; si no, lo crea
    if FileExists(RutaLog) then
      Append(Archivo)
    else
      Rewrite(Archivo);

    // Escribe la l?nea con fecha/hora
    // WriteLn(Archivo, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Mensaje);
    WriteLn(Archivo, Mensaje);

  finally
    CloseFile(Archivo);
  end;

end;

procedure TAiChat.Abort;
begin
  FAbort := True;
end;

// --- Este mensaje se env?a cuando es una funci?n toll, es indepensiente a los otros dos
function TAiChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): TAiChatMessage;
Var
  Msg: TAiChatMessage;
begin
  Try
    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    Result := Msg;
  Finally
  End;
end;

// ------- Metodo para el manejo de mensajes del chat normal
function TAiChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
Var
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
begin

  Try
    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole);

    If Length(aMediaFiles) > 0 then
    Begin
      For MF in aMediaFiles do
      Begin
        If Assigned(MF) then
          Msg.AddMediaFile(MF);
      End;
    End;

    Result := InternalAddMessage(Msg);

  Finally
  End;
end;

// ------- Metodo para el manejo de mensajes del chat normal
function TAiChat.InternalAddMessage(aMsg: TAiChatMessage): TAiChatMessage;
Var
  TmpMsg: TAiChatMessage;
  MF: TAiMediaFile;
  MensajeInicial: String;
  Respuesta: String;
  Procesado: Boolean;
begin

  If Not Assigned(aMsg) then
    Raise Exception.Create('El par?metro aMsg debe estar instanciado');

  Try

    MensajeInicial := Trim(Self.PrepareSystemMsg);

    // Comienza con las instrucciones iniciales, en cada modelo es diferente
    If (FMessages.Count = 0) and (MensajeInicial <> '') then
    Begin

      TmpMsg := TAiChatMessage.Create(MensajeInicial, 'system');
      TmpMsg.Id := FMessages.Count + 1;
      FMessages.Add(TmpMsg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, TmpMsg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    If Assigned(FMessages) and Assigned(aMsg) then
    Begin
      aMsg.Id := FMessages.Count + 1;
      FMessages.Add(aMsg);
    End;

    If Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);
    End;

    For MF in aMsg.MediaFiles do
    Begin
      If Assigned(MF) then
      Begin
        Procesado := False;
        DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Env?a el archivo por si lo quiere procesar otra AI especializada, Ej.
        MF.Procesado := Procesado;
        MF.Transcription := Respuesta;
        // Guarda las transcripciones en los MediaFile,  luego construye la respuesta definitiva con todos los mediafiles
      End;
    End;

    FLastPrompt := aMsg.Prompt; // aqui lleva el Prompt Inicial + la conversi?n de los MediaFiles a texto si el usuario lo permite

    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, aMsg, Nil, aMsg.Role, aMsg.Prompt);

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, aMsg);

    Result := aMsg;

  Finally
  End;
end;

function TAiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
  St: TStringStream;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastReasoning := '';
  FLastPrompt := '';

  // Variable del componente para mantener vivo el stream en llamados asincronos
  St := TStringStream.Create('', TEncoding.UTF8);

  if Assigned(FTmpToolCallBuffer) then
    FTmpToolCallBuffer.Clear;

  sUrl := FUrl + 'chat/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FResponse.Position := 0;

    FLastContent := '';
    FLastReasoning := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin

        var
        LParsed := TJSonObject.ParseJSONValue(Res.ContentAsString);
        if not(LParsed is TJSonObject) then
        begin
          LParsed.Free;
          raise Exception.CreateFmt('Error: Respuesta JSON inv?lida: %s', [Res.ContentAsString]);
        end;
        jObj := TJSonObject(LParsed);
        Try
          FBusy := False;
          ParseChat(jObj, ResMsg);
          Result := FLastContent;

        Finally
          FreeAndNil(jObj);
        End;
      End
      else
      begin
        FBusy := False;
        DoStateChange(acsError, Res.ContentAsString);
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      FreeAndNil(St); // Esto no funciona en multiarea, as? que se libera cuando no lo es.
  End;
end;

function TAiChat.InternalRunImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiVisionTool;
begin
  Result := '';

  // 1. Intentamos usar la herramienta externa de visi?n
  if Assigned(FVisionTool) and Supports(FVisionTool, IAiVisionTool, LTool) then
  begin
    if FVisionTool is TAiCustomTool then
      TAiCustomTool(FVisionTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Analizando imagen con herramienta de visi?n...');

    // 2. Ejecutamos el an?lisis (la Tool se encarga de llamar a ReportDataEnd con el texto)
    LTool.ExecuteImageDescription(aMediaFile, ResMsg, AskMsg);

    if Asynchronous then
      Exit;

    // Si es s?ncrono, la Tool debe haber actualizado el ResMsg.Prompt
    Result := ResMsg.Prompt;
  end
  else
  begin
    // 3. Si no hay herramienta, lanzamos el error que ten?as originalmente
    raise Exception.Create('El modelo principal no soporta visi?n nativa y no se ha asignado un VisionTool externo.');
  end;
end;

function TAiChat.InternalRunImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiImageTool;
begin
  Result := '';
  if Assigned(FImageTool) and Supports(FImageTool, IAiImageTool, LTool) then
  begin
    if FImageTool is TAiCustomTool then
      TAiCustomTool(FImageTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Generando imagen...');
    LTool.ExecuteImageGeneration(AskMsg.Prompt, ResMsg, AskMsg);

    if Asynchronous then
      Exit;
    Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de imagen (ImageTool).');
end;

function TAiChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiVideoTool;
begin
  Result := '';
  if Assigned(FVideoTool) and Supports(FVideoTool, IAiVideoTool, LTool) then
  begin
    if FVideoTool is TAiCustomTool then
      TAiCustomTool(FVideoTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Generando video...');
    LTool.ExecuteVideoGeneration(ResMsg, AskMsg);

    if Asynchronous then
      Exit;
    Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de video (VideoTool).');
end;

function TAiChat.InternalRunPDFDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiPdfTool;
begin
  Result := '';

  // Verificamos si hay una herramienta de PDF asignada
  if Assigned(FPdfTool) and Supports(FPdfTool, IAiPdfTool, LTool) then
  begin
    // Le pasamos el contexto (Self) para que reporte estados (acsToolExecuting, etc.)
    if FPdfTool is TAiCustomTool then
      TAiCustomTool(FPdfTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Analizando documento PDF (Rasterizaci?n/OCR)...');

    // Ejecutamos la l?gica de la herramienta
    LTool.ExecutePdfAnalysis(aMediaFile, ResMsg, AskMsg);

    if Asynchronous then
      Exit;

    // Si es s?ncrono, la herramienta deber?a haber llenado la transcripci?n
    Result := aMediaFile.Transcription;

  end
  else
    raise Exception.Create('El modelo no soporta PDF nativo y no se ha asignado un PdfTool externo.');
end;

function TAiChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModel, LVoice, LResponseFormat: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders; // <--- Variable ya declarada, ahora la usaremos
  LResponse: IHTTPResponse;
  LJsonObject: TJSonObject;
  S, LErrorResponse: string;
  LResponseMsg: TAiChatMessage;
  LNewAudioFile: TAiMediaFile;
  LTool: IAiSpeechTool;
begin
  Result := '';
  if Assigned(FSpeechTool) and Supports(FSpeechTool, IAiSpeechTool, LTool) then
  begin
    if FSpeechTool is TAiCustomTool then
      TAiCustomTool(FSpeechTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Generando audio con herramienta externa...');
    LTool.ExecuteSpeechGeneration(AskMsg.Prompt, ResMsg, AskMsg);

    if Asynchronous then
      Exit;

    Result := ResMsg.Prompt;
  end
  else
  begin
    FBusy := True;
    FLastError := '';
    FLastContent := '';
    FLastPrompt := AskMsg.Prompt;

    // 2. Preparar par?metros para la API de TTS
    LUrl := Url + 'audio/speech';
    LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
    LVoice := Self.Voice; // Usamos la propiedad del componente
    LResponseFormat := Self.voice_format; // Usamos la propiedad del componente

    // 3. Construir y ejecutar la petici?n
    LJsonObject := TJSonObject.Create;
    LBodyStream := nil;
    LResponseStream := TMemoryStream.Create;
    try
      LJsonObject.AddPair('model', LModel);
      LJsonObject.AddPair('input', AskMsg.Prompt);
      LJsonObject.AddPair('voice', LVoice);

      If voice_format.IsEmpty then
        voice_format := 'wav';

      LJsonObject.AddPair('response_format', voice_format);

      S := LJsonObject.ToString;

      LBodyStream := TStringStream.Create;
      LBodyStream.WriteString(S);

      LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
      FClient.ContentType := 'application/json';

      LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
      LBodyStream.SaveToFile('c:\temp\peticionAudio.json.txt');
      LBodyStream.Position := 0;
{$ENDIF}
      LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);
      // LResponse := FClient.Post(LUrl, LBodyStream, Nil, LHeaders);

      // 4. Procesar la respuesta
      if LResponse.StatusCode = 200 then
      begin
        // ... (el resto del c?digo de procesamiento de la respuesta es correcto)
        LNewAudioFile := TAiMediaFile.Create;
        try
          LResponseStream.Position := 0;

{$IFDEF APIDEBUG}
          LResponseStream.SaveToFile('c:\temp\respuestavoice.txt');
          LResponseStream.Position := 0;
{$ENDIF}
          LNewAudioFile.LoadFromStream('generated_audio.' + LResponseFormat, LResponseStream);
          LResponseMsg := TAiChatMessage.Create(AskMsg.Prompt, 'assistant');
          LResponseMsg.MediaFiles.Add(LNewAudioFile);
          LResponseMsg.Id := FMessages.Count + 1;
          FMessages.Add(LResponseMsg);

          DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO

          if Assigned(FOnReceiveDataEnd) then
            FOnReceiveDataEnd(Self, LResponseMsg, nil, 'assistant', '');
        except
          LNewAudioFile.Free;
          raise;
        end;
      end
      else
      begin
        LResponseStream.Position := 0;
        var
        LReader := TStreamReader.Create(LResponseStream);
        try
          LErrorResponse := LReader.ReadToEnd;
        finally
          LReader.Free;
        end;
        FLastError := Format('Error generando audio: %d, %s', [LResponse.StatusCode, LErrorResponse]);
        DoStateChange(acsError, FLastError);
        DoError(FLastError, nil);
      end;
    finally
      LJsonObject.Free;
      LBodyStream.Free;
      LResponseStream.Free;
    end;

    FBusy := False;
  end;
end;

function TAiChat.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList; // Para procesar las granularidades
  I: Integer;
  LModel: String;
  LTool: IAiSpeechTool;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripci?n.');

  // Si hay una Tool asignada que soporte Audio
  if Assigned(FSpeechTool) and Supports(FSpeechTool, IAiSpeechTool, LTool) then
  begin
    // Le pasamos el contexto (Self) para que sepa a qui?n reportar eventos
    if FSpeechTool is TAiCustomTool then
      TAiCustomTool(FSpeechTool).SetContext(Self);

    LTool.ExecuteTranscription(aMediaFile, ResMsg, AskMsg);

    // Si es as?ncrono, no devolvemos nada ahora; esperamos el evento de la Tool
    if Asynchronous then
      Exit;

    // Si es s?ncrono, la Tool deber?a haber llenado ResMsg.Prompt
    Result := ResMsg.Prompt;
  end
  else
  begin

    sUrl := Url + 'audio/transcriptions';

    Client := TNetHTTPClient.Create(Self);

{$IF CompilerVersion >= 35}
    Client.SynchronizeEvents := False;
{$ENDIF}
    LResponseStream := TMemoryStream.Create;
    Body := TMultipartFormData.Create;
    Granularities := TStringList.Create;
    LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
    LTempStream := TMemoryStream.Create;
    try
      Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

      // Crear un stream temporal para pasarlo al formulario multipart
      aMediaFile.Content.Position := 0;
      LTempStream.LoadFromStream(aMediaFile.Content);
      LTempStream.Position := 0;

      // --- 1. CONSTRUCCI?N DEL BODY MULTIPART CON PAR?METROS GEN?RICOS ---

{$IF CompilerVersion >= 35}
      Body.AddStream('file', LTempStream, False, aMediaFile.FileName, aMediaFile.MimeType);
{$ELSE}
      Body.AddStream('file', LTempStream, aMediaFile.FileName, aMediaFile.MimeType);
{$ENDIF}
      // Modelo: usa el modelo principal si es de transcripci?n, si no, usa un default.
      Body.AddField('model', LModel); // Default seguro

      if not AskMsg.Prompt.IsEmpty then
        Body.AddField('prompt', AskMsg.Prompt);

      // Formato de respuesta (gen?rico, como string)
      if not Self.Transcription_ResponseFormat.IsEmpty then
        Body.AddField('response_format', Self.Transcription_ResponseFormat)
      else
        Body.AddField('response_format', 'json'); // Default a JSON si no se especifica

      // Par?metros opcionales
      if not Self.Language.IsEmpty then
        Body.AddField('language', Self.Language);

      // Usamos la propiedad de Temperatura existente en el componente
      if Self.Temperature > 0 then
        Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

      // Timestamps Granularities (procesamos la cadena)
      if not Self.Transcription_TimestampGranularities.IsEmpty then
      begin
        // Dividimos la cadena por comas
        Granularities.CommaText := Self.Transcription_TimestampGranularities;
        for I := 0 to Granularities.Count - 1 do
        begin
          // A?adimos cada granularidad como un campo separado con '[]'
          Body.AddField('timestamp_granularities[]', Trim(Granularities[I]));
        end;
      end;

      // TODO : TODAV?A NO EST? LISTO PARA UTILIZAR LA TRANSCRIPCI?N EN MODO ASCINCR?NICO, Falta implementar a futuro
      // Streaming
      // if Self.Asynchronous then
      // Body.AddField('stream', 'true');

      // --- 2. EJECUCI?N DE LA PETICI?N POST ---

      // (La l?gica de streaming/s?ncrono se mantiene igual)
      begin
        Res := Client.Post(sUrl, Body, LResponseStream, Headers);

        if Res.StatusCode = 200 then
        begin

          var
          LParsedTranscript := TJSonObject.ParseJSONValue(Res.ContentAsString);
          if not(LParsedTranscript is TJSonObject) then
          begin
            LParsedTranscript.Free;
            LResponseObj := TJSonObject.Create(TJSonPair.Create('text', Res.ContentAsString));
          end
          else
            LResponseObj := TJSonObject(LParsedTranscript);

          try
            // Aqu? llamas al procedimiento de parseo de transcripciones
            ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
          finally
            LResponseObj.Free;
          end;

          Result := ResMsg.Prompt;
        end
        else
        begin
          DoStateChange(acsError, Res.ContentAsString);
          Raise Exception.CreateFmt('Error en la transcripci?n: %d, %s', [Res.StatusCode, Res.ContentAsString]);
        end;
      end;

    finally
      Body.Free;
      LTempStream.Free;
      Client.Free;
      LResponseStream.Free;
    end;
  end;
end;

function TAiChat.InternalRunWebSearch(ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiWebSearchTool;
begin
  Result := '';
  if Assigned(FWebSearchTool) and Supports(FWebSearchTool, IAiWebSearchTool, LTool) then
  begin
    if FWebSearchTool is TAiCustomTool then
      TAiCustomTool(FWebSearchTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Buscando en la web...');
    LTool.ExecuteSearch(AskMsg.Prompt, ResMsg, AskMsg);

    if Asynchronous then
      Exit;
    Result := ResMsg.Prompt;
  end
  else
    raise Exception.Create('No se ha asignado una herramienta de b?squeda web (WebSearchTool).');
end;

function TAiChat.InternalRunReport(ResMsg, AskMsg: TAiChatMessage): String;
var
  LTool: IAiReportTool;
begin
  Result := '';
  if Assigned(FReportTool) and Supports(FReportTool, IAiReportTool, LTool) then
  begin
    if FReportTool is TAiCustomTool then
      TAiCustomTool(FReportTool).SetContext(Self);

    DoStateChange(acsToolExecuting, 'Generando reporte...');
    LTool.ExecuteReport(ResMsg, AskMsg);

    if Asynchronous then
      Exit;
    Result := ResMsg.Prompt;
  end
  else
    // Sin herramienta asignada: genera el reporte v?a LLM (ej. code_interpreter de Gemini)
    InternalRunCompletions(ResMsg, AskMsg);
end;

function TAiChat.AddMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  Result := InternalAddMessage(aPrompt, aRole, []);
end;

function TAiChat.AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): String;
begin
  InternalAddMessage(aPrompt, aRole, aMediaFiles);
  Result := Run(Nil, Nil);
end;

function TAiChat.AddMessageAndRunMsg(aPrompt, aRole: String; aMediaFiles: TAiMediaFilesArray): TAiChatMessage;
begin
  if FAsynchronous = False then
  Begin
    AddMessageAndRun(aPrompt, aRole, aMediaFiles);
    Result := Self.GetLastMessage; // Retorna el mensaje
  End
  Else
    Raise Exception.Create('Esta funci?n no es compatible con el modo asincr?nico')
end;

function TAiChat.AddMessageAndRun(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String;
begin
  InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName);
  Result := Run(Nil, Nil);
end;

procedure TAiChat.AddToMemory(Key, Value: String);
begin
  FMemory.AddPair(Key, Value);
end;

function TAiChat.CheckFileState(aMediaFile: TAiMediaFile): String;
begin
  // Revisa el estado de un archivo
end;

constructor TAiChat.Create(Sender: TComponent);
begin
  inherited;
  FOwner := Sender;
  FMessages := TAiChatMessages.Create;
  FTools := TStringList.Create;
  FMemory := TStringList.Create;
  FJsonSchema := TStringList.Create;
  FSystemPrompt := TStringList.Create;
  FWebSearchParams := TStringList.Create;
  FTmpToolCallBuffer := TObjectDictionary<Integer, TJSonObject>.Create([doOwnsValues]);
  FTextEditorTool := Nil;
  FShellTool := Nil;

  FResponse := TStringStream.Create('', TEncoding.UTF8);
  FClient := TNetHTTPClient.Create(Self);
{$IF CompilerVersion >= 35}
  FClient.SynchronizeEvents := False;
  FClient.OnRequestException := Self.OnRequestExceptionEvent;
{$ENDIF}
  FClient.OnReceiveData := Self.OnInternalReceiveData;
  FClient.OnRequestError := Self.OnRequestErrorEvent;
  FClient.OnRequestCompleted := Self.OnRequestCompletedEvent;
  FClient.ResponseTimeOut := 120000;

  FNativeInputFiles := [];
  FNativeOutputFiles := [];
  FModelCaps := [];
  FSessionCaps := [];
  FNewSystemConfigured := False;

  FModel := 'gpt-5';
  FN := 1;
  FResponse_format := TAiChatResponseFormat.tiaChatRfText;
  FTemperature := 1;
  FUser := 'user';
  FSystemPrompt.Text := 'Eres un asistente muy ?til y servicial';
  FMax_tokens := 3000;
  FUrl := GlOpenAIUrl;
  FTop_p := 1;
  FResponseTimeOut := 120000;
  FStream_Usage := False; // Envia la estadistica de uso por token
  FTool_choice := 'auto';
  FVideoParams := TStringList.Create;
  FImageParams := TStringList.Create;
  FMediaResolution := mrDefault;
end;

function TAiChat.DeleteFile(aMediaFile: TAiMediaFile): String;
begin
  // Permite borrar un archivo previamente subido al llm
end;

destructor TAiChat.Destroy;
begin
  FResponse.Free;
  FTools.Free;
  FClient.Free;
  FMemory.Free;
  FJsonSchema.Free;
  FTmpToolCallBuffer.Free;
  FVideoParams.Free;
  FImageParams.Free;
  FWebSearchParams.Free;
  FSystemPrompt.Free;
  NewChat;
  FMessages.Free;

  inherited;
end;

procedure TAiChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin

  If Assigned(AiFunctions) and AiFunctions.DoCallFunction(ToolCall) then
  Begin
    // Si ejecut? la funci?n
  End
  Else
  Begin // Ejecuta la funci?n por defecto en el componente TAiChat
    If Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall)
  End;
end;

procedure TAiChat.DoData(Msg: TAiChatMessage; const Role, Text: string; aResponse: TJSonObject);
begin
  if Assigned(FOnReceiveDataEvent) then
    FOnReceiveDataEvent(Self, TAiChatMessage(Msg), aResponse, Role, Text);
end;

procedure TAiChat.DoDataEnd(Msg: TAiChatMessage; const Role, Text: string; aResponse: TJSonObject);
begin
  FBusy := False;
  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, TAiChatMessage(Msg), aResponse, Role, Text);
end;

procedure TAiChat.DoError(const ErrorMsg: string; E: Exception);
begin
  DoStateChange(acsError, ErrorMsg);

  if Assigned(FOnError) then
    FOnError(Self, ErrorMsg, E, Nil);
end;

procedure TAiChat.DoProcessMediaFile(aPrompt: String; aAiMediaFile: TAiMediaFile; Var Respuesta: String; Var Procesado: Boolean);
begin
  Procesado := False;

  If Assigned(FOnProcessMediaFile) then
  Begin
    Respuesta := '';
    FOnProcessMediaFile(Self, aPrompt, aAiMediaFile, Self.NativeInputFiles, Respuesta, Procesado);
    aAiMediaFile.Procesado := Procesado;
    aAiMediaFile.Transcription := Respuesta;
  End;
end;

procedure TAiChat.DoProcessResponse(aLastMsg, aResMsg: TAiChatMessage; var aResponse: String);
begin
  If Assigned(FOnProcessResponse) then
    FOnProcessResponse(Self, aLastMsg, aResMsg, aResponse);
end;

procedure TAiChat.DoStateChange(State: TAiChatState; const Description: string);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, State, Description);
end;

function TAiChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
begin

end;

function TAiChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSonObject;
  JVal, JVal1, jValToolCall: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  Nom, Valor, sType: String;
  I: Integer;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

    If Msg.TryGetValue<TJSonValue>('tool_calls', jValToolCall) and (jValToolCall is TJSonArray) and Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        jObj := TJSonObject(JVal);
        // Algunos providers (Mistral) omiten 'type'; en ese caso se asume 'function'
        if not jObj.TryGetValue<String>('type', sType) then
          sType := 'function';
        If sType = 'function' then
        Begin
          jObj := TJSonObject(JVal);
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          Fun.Tipo := sType;

          If jObj.TryGetValue<TJSonObject>('function', jFunc) then
          Begin
            // Fun.Name := jObj.GetValue<TJSonObject>('function').GetValue<String>('name');
            Fun.Name := jFunc.GetValue<String>('name');

            Fun.Arguments := jObj.GetValue<TJSonObject>('function').GetValue<String>('arguments');
          End;

          Try
            If (Fun.Arguments <> '') and (Fun.Arguments <> '{}') then
            Begin
              Arg := TJSonObject(TJSonObject.ParseJSONValue(Fun.Arguments));
              Try
                If Assigned(Arg) then
                Begin
                  For I := 0 to Arg.Count - 1 do
                  Begin
                    Nom := Arg.Pairs[I].JsonString.Value;
                    Valor := Arg.Pairs[I].JsonValue.Value;
                    Fun.Params.Values[Nom] := Valor;
                  End;
                End;

              Finally
                Arg.Free;
              End;
            End;
          Except
            // Si no hay par?metros no marca error
          End;
          Result.Add(Fun.Id, Fun);
        End;
      End;
    End;
  End;
end;

function TAiChat.GetApiKey: String;
begin
  // Si est? en modo de dise?o, simplemente retorna el valor tal cual
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
  begin
    Result := FApiKey;
    Exit;
  end;

  // En modo de ejecuci?n
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    // Retorna el valor de la variable de entorno, quitando el '@'
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

function TAiChat.GetAsynchronous: Boolean;
begin
  Result := Self.Asynchronous;
end;

function TAiChat.GetLastMessage: TAiChatMessage;
begin
  Result := Nil;
  If FMessages.Count > 0 then
    Result := FMessages[FMessages.Count - 1];
end;

function TAiChat.GetMessages: TJSonArray;
begin
  Result := FMessages.ToJSon;
end;

function TAiChat.GetModels: TStringList;
begin
  Result := GetModels(ApiKey, Url);
end;

class function TAiChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  Client := TNetHTTPClient.Create(Nil);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'models';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      var
      LParsedModels := TJSonObject.ParseJSONValue(Res.ContentAsString);
      if not(LParsedModels is TJSonObject) then
      begin
        LParsedModels.Free;
        raise Exception.CreateFmt('Error: Respuesta JSON inv?lida: %s', [Res.ContentAsString]);
      end;
      jRes := TJSonObject(LParsedModels);
      Try
        If jRes.TryGetValue<TJSonArray>('data', JArr) then
        Begin
          For JVal in JArr do
          Begin
            sModel := JVal.GetValue<String>('id');
            If sModel <> '' then
              Result.Add(sModel);
          End;
        End;

        // Agregar modelos personalizados
        CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

        for I := Low(CustomModels) to High(CustomModels) do
        begin
{$IF CompilerVersion <= 35.0}
          if Result.IndexOf(CustomModels[I]) = -1 then
            Result.Add(CustomModels[I]);
{$ELSE}
          if not Result.Contains(CustomModels[I]) then
            Result.Add(CustomModels[I]);
{$ENDIF}
        end;

      Finally
        jRes.Free;
      End;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiChat.GetTools(aToolFormat: TToolFormat): TStrings;
begin
  If Assigned(FAiFunctions) and FTool_Active then // Si est? asignado el componente lo obtiene del componente
  Begin
    FTools.Text := FAiFunctions.GetTools(aToolFormat);
    Result := FTools;
  End
  Else
  Begin
    FTools.Text := '';
    Result := FTools;
  End;
end;

function TAiChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel, sEffort: String;
begin

  If FUser = '' then
    FUser := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'gpt-4o';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous; // and (not Self.Tool_Active);

  // FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin

{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools est?n mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(FTool_choice) <> '') then
      Begin

{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(FTool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(FTool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('user', User);

    If (FResponse_format = tiaChatRfJsonSchema) then
    Begin
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_schema'))
    End
    Else If { LAsincronico or } (FResponse_format = tiaChatRfJson) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'))
    Else If (FResponse_format = tiaChatRfText) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'))
    Else
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'));

    sEffort := '';

    case FThinkingLevel of
      tlLow:
        sEffort := 'low';
      tlMedium:
        sEffort := 'medium';
      tlHigh:
        sEffort := 'high';
    end;

    if sEffort <> '' then
    begin
      var
      jeffort := TJSonObject.Create;
      jeffort.AddPair('effort', sEffort);
      AJSONObject.AddPair('reasoning', jeffort);

      AJSONObject.AddPair('max_completion_tokens', TJSONNumber.Create(FMax_tokens));
    end
    else
      AJSONObject.AddPair('max_tokens', FMax_tokens);

    Lista.CommaText := FStop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If FLogprobs = True then
    Begin
      If FLogit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(FLogit_bias));

      AJSONObject.AddPair('logprobs', TJSONBool.Create(FLogprobs));

      If FTop_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(FTop_logprobs));
    End;

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(FSeed));

    if tcm_WebSearch in FChatMediaSupports then
    begin
      // La API de OpenAI espera un objeto para las opciones, incluso si est? vac?o.
      Var
      jWebSearchOptions := TJSonObject.Create;
      AJSONObject.AddPair('web_search_options', jWebSearchOptions);
    end
    Else
    Begin
      If FTop_p <> 0 then
        AJSONObject.AddPair('top_p', TJSONNumber.Create(FTop_p));

      AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(FTemperature * 100) / 100));
      AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(FFrequency_penalty * 100) / 100));
      AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(FPresence_penalty * 100) / 100));
      AJSONObject.AddPair('n', TJSONNumber.Create(FN));

    End;

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSon));
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

procedure TAiChat.NewChat;
Var
  I: Integer;
begin
  For I := FMessages.Count - 1 downto 0 do
  Begin
    FMessages[I].Free;
    FMessages.Delete(I);
  End;
  FMessages.Clear;
end;

function TAiChat.NewMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  Result := TAiChatMessage.Create(aPrompt, aRole);
end;

procedure TAiChat.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FSpeechTool then
      FSpeechTool := nil;
    if AComponent = FImageTool then
      FImageTool := nil;
    if AComponent = FComputerUseTool then
      FComputerUseTool := nil;
    if AComponent = FVideoTool then
      FVideoTool := nil;
    if AComponent = FWebSearchTool then
      FWebSearchTool := nil;
    if AComponent = FVisionTool then
      FVisionTool := nil;
    if AComponent = FPdfTool then
      FPdfTool := nil;
    if AComponent = FReportTool then
      FReportTool := nil;
    if AComponent = FShellTool then
      FShellTool := nil;
    if AComponent = FTextEditorTool then
      FTextEditorTool := nil;
  end;

end;

procedure TAiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  jObj, Delta: TJSonObject;
  JToolCalls: TJSonArray;
  sJson, Value, Role1: String;
  P: Integer;

  // Variables para manejo de Tools
  jFunc: TJSonValue;

  ToolIndex: Integer;
  BufferObj, BufferFunc: TJSonObject;

  // Variables para reconstrucci?n y Fin de Stream
  CombinedTools: TJSonArray;
  sToolCallsStr: String;
  SortedKeys: TList<Integer>;

  // Variables para Fake JSON
  FakeResponseObj: TJSonObject;
  FakeChoice: TJSonObject;
  FakeMsg: TJSonObject;
  FakeChoicesArr: TJSonArray;
  FakeUsage: TJSonObject;
  TempMsg: TAiChatMessage;

  // Variables para compatibilidad y parsing
  jArrChoices: TJSonArray;

  // Procedimiento local para procesar una l?nea de JSON o Comando
  // Esto evita duplicar c?digo para el caso del buffer remanente
  Procedure ProcessLine(ALine: String);
  Begin
    if ALine = '' then
      Exit;

    // Limpiar prefijo "data:" si existe
    if StartsStr('data:', ALine) then
      ALine := Trim(Copy(ALine, 6, Length(ALine)));

    if ALine = '' then
      Exit;

    // -----------------------------------------------------------------------
    // CASO 1: [DONE]
    // -----------------------------------------------------------------------
    if ALine = '[DONE]' then
    Begin
      // L?gica de Finalizaci?n y Reconstrucci?n de Tools
      sToolCallsStr := '';

      if FTmpToolCallBuffer.Count > 0 then
      begin
        CombinedTools := TJSonArray.Create;
        try
          SortedKeys := TList<Integer>.Create;
          try
            Var
              aKey: Integer;

            for aKey in FTmpToolCallBuffer.Keys do
              SortedKeys.Add(aKey);
            SortedKeys.Sort;
            for aKey in SortedKeys do
            begin
              CombinedTools.Add(FTmpToolCallBuffer[aKey].Clone as TJSonObject);
            end;
          finally
            SortedKeys.Free;
          end;
          sToolCallsStr := CombinedTools.ToJSon;
        finally
          CombinedTools.Free;
          FTmpToolCallBuffer.Clear;
        end;
      end;

      // Fake JSON para enga?ar a ParseChat
      FakeResponseObj := TJSonObject.Create;
      try
        FakeResponseObj.AddPair('id', 'stream-' + IntToStr(TThread.GetTickCount));
        FakeResponseObj.AddPair('model', FModel);

        FakeUsage := TJSonObject.Create;
        FakeUsage.AddPair('prompt_tokens', TJSONNumber.Create(0));
        FakeUsage.AddPair('completion_tokens', TJSONNumber.Create(0));
        FakeUsage.AddPair('total_tokens', TJSONNumber.Create(0));
        FakeResponseObj.AddPair('usage', FakeUsage);

        FakeChoicesArr := TJSonArray.Create;
        FakeChoice := TJSonObject.Create;
        FakeMsg := TJSonObject.Create;

        FakeMsg.AddPair('role', FTmpRole);
        if FLastContent <> '' then
          FakeMsg.AddPair('content', FLastContent);
        if FLastReasoning <> '' then
          FakeMsg.AddPair('reasoning_content', FLastReasoning); // Requerido por APIs con thinking (DeepSeek, Kimi, etc.)
        if sToolCallsStr <> '' then
          FakeMsg.AddPair('tool_calls', TJSonArray(TJSonObject.ParseJSONValue(sToolCallsStr)));

        FakeChoice.AddPair('message', FakeMsg);
        FakeChoice.AddPair('finish_reason', 'stop');
        FakeChoicesArr.Add(FakeChoice);
        FakeResponseObj.AddPair('choices', FakeChoicesArr);

        TempMsg := TAiChatMessage.Create('', FTmpRole);
        try
          ParseChat(FakeResponseObj, TempMsg);
          if sToolCallsStr = '' then
          begin
            TempMsg.Id := FMessages.Count + 1;
            FMessages.Add(TempMsg);

            DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO

            if Assigned(FOnReceiveDataEnd) then
              FOnReceiveDataEnd(Self, TempMsg, Nil, FTmpRole, FLastContent);
            TempMsg := nil;
          end
          else
          begin
            DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO

            if Assigned(FOnReceiveDataEnd) then
              FOnReceiveDataEnd(Self, GetLastMessage, Nil, FTmpRole, '');
          end;
        finally
          if Assigned(TempMsg) then
            TempMsg.Free;
        end;
      finally
        FakeResponseObj.Free;
      end;

      FLastReasoning := ''; // Limpiar para el próximo round o próxima petición
      FBusy := False;
      Exit;
    End;

    // -----------------------------------------------------------------------
    // CASO 2: JSON STANDARD
    // -----------------------------------------------------------------------
    var
    LParsedLine := TJSonObject.ParseJSONValue(ALine);
    if not(LParsedLine is TJSonObject) then
    begin
      LParsedLine.Free;
      Exit; // JSON inv?lido o incompleto
    end;
    jObj := TJSonObject(LParsedLine);

    Try
      jArrChoices := jObj.GetValue<TJSonArray>('choices');
      if (jArrChoices <> nil) and (jArrChoices.Count > 0) then
      begin
        Delta := jArrChoices.Items[0].GetValue<TJSonObject>('delta');
        if Assigned(Delta) then
        begin
          if Delta.TryGetValue<String>('role', Role1) then
            FTmpRole := Role1;

          // Thinking
          Value := '';
          if Delta.TryGetValue<String>('reasoning', Value) or Delta.TryGetValue<String>('reasoning_content', Value) then
          begin
            FLastReasoning := FLastReasoning + Value; // Acumular para incluir en historial
            if (Value <> '') and Assigned(FOnReceiveThinking) then
            begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveThinking(Self, Nil, jObj, FTmpRole, Value);
            end;
          end;

          // Content
          {if (Delta.GetValue('content') <> nil) and not(Delta.GetValue('content') is TJSONNull) then
          begin
            Value := Delta.GetValue<String>('content');
            FLastContent := FLastContent + Value;
            if Assigned(FOnReceiveDataEvent) then
            begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            end;
          end;
          }

          var
            JVal: TJSonValue;
          JVal := Delta.GetValue('content');
          if Assigned(JVal) and (JVal is TJSONString) then
          begin
            Value := JVal.Value;
            FLastContent := FLastContent + Value;
            if Assigned(FOnReceiveDataEvent) then
            begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            end;
          end
          else if Assigned(JVal) and (JVal is TJSonArray) then
            ParseDeltaContentArray(TJSonArray(JVal), jObj);

          // Tools
          if Delta.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
          begin
            Var
              JToolCall: TJSonValue;

            for JToolCall in JToolCalls do
            begin
              if JToolCall is TJSonObject then
              begin
                var
                JTC := TJSonObject(JToolCall);
                if JTC.TryGetValue<Integer>('index', ToolIndex) then
                begin
                  if not FTmpToolCallBuffer.TryGetValue(ToolIndex, BufferObj) then
                  begin
                    BufferObj := TJSonObject.Create;
                    FTmpToolCallBuffer.Add(ToolIndex, BufferObj);
                  end;

                  if JTC.TryGetValue<String>('id', Value) then
                    if BufferObj.GetValue('id') = nil then
                      BufferObj.AddPair('id', Value);

                  if JTC.TryGetValue<String>('type', Value) then
                    if BufferObj.GetValue('type') = nil then
                      BufferObj.AddPair('type', Value);

                  if JTC.TryGetValue<TJSonObject>('function', TJSonObject(jFunc)) then
                  begin
                    if not BufferObj.TryGetValue<TJSonObject>('function', BufferFunc) then
                    begin
                      BufferFunc := TJSonObject.Create;
                      BufferObj.AddPair('function', BufferFunc);
                    end;

                    if jFunc.TryGetValue<String>('name', Value) then
                    begin
                      var
                        OldName: string := '';
                      if BufferFunc.TryGetValue<String>('name', OldName) then
                        BufferFunc.RemovePair('name');
                      BufferFunc.AddPair('name', OldName + Value);
                    end;

                    if jFunc.TryGetValue<String>('arguments', Value) then
                    begin
                      var
                        OldArgs: string := '';
                      if BufferFunc.TryGetValue<String>('arguments', OldArgs) then
                        BufferFunc.RemovePair('arguments');
                      BufferFunc.AddPair('arguments', OldArgs + Value);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    Finally
      jObj.Free;
    End;
  End;

begin
  LogDebug('--OnInternalReceiveData--');
  if Length(FResponse.DataString) > 500 then
    LogDebug(Copy(FResponse.DataString, 1, 500) + '...[truncado]')
  else
    LogDebug(FResponse.DataString);

  If FClient.Asynchronous = False then
    Exit;

  AAbort := FAbort;
  If FAbort = True then
  Begin
    FBusy := False;
    FTmpToolCallBuffer.Clear;
    FLastReasoning := '';
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
    Exit;
  End;

  Try
    FTmpResponseText := FTmpResponseText + FResponse.DataString;
    FResponse.Clear;

    // 1. Bucle principal: Procesa todo lo que tenga salto de l?nea seguro
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      P := Pos(#10, FTmpResponseText);
      sJson := Trim(Copy(FTmpResponseText, 1, P - 1)); // Trim elimina espacios iniciales como en tu error
      Delete(FTmpResponseText, 1, P);

      ProcessLine(sJson); // Procesamos la l?nea limpia
    end;

    // 2. Borde de Seguridad: ?ltima l?nea sin salto
    // Si lo que queda en el buffer es EXACTAMENTE '[DONE]' (o con data:), lo procesamos.
    // Si es JSON incompleto (ej: '{"id":'), NO lo tocamos, esperamos el siguiente paquete.
    sJson := Trim(FTmpResponseText);
    if (sJson = '[DONE]') or (sJson = 'data: [DONE]') then
    begin
      ProcessLine(sJson); // Procesamos el DONE final
      FTmpResponseText := ''; // Limpiamos el buffer
    end;

  Except
    // Manejo de errores silencioso para stream
  End;
end;

procedure TAiChat.ParseDeltaContentArray(AContentArr: TJSonArray; jObj: TJSonObject);
begin
  // Implementación base vacía.
  // Drivers que reciben delta.content como array tipado (ej: Mistral magistral)
  // sobreescriben este método para procesar los bloques {"type":"text"/"thinking",...}.
end;

procedure TAiChat.OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse);
begin


  // OJO Activar
  // if FAsynchronous and Assigned(FCurrentPostStream) then
  // FreeAndNil(FCurrentPostStream);

  If Assigned(FOnError) then
  Begin
    If Assigned(aResponse) and ((aResponse.StatusCode < 200) or (aResponse.StatusCode > 299)) then
      FOnError(Self, aResponse.ContentAsString, Nil, aResponse);
  End;
end;

procedure TAiChat.OnRequestErrorEvent(const Sender: TObject; const AError: string);
begin

  If Assigned(FOnError) then
    FOnError(Self, AError, Nil, Nil);
end;

procedure TAiChat.OnRequestExceptionEvent(const Sender: TObject; const AError: Exception);
begin
  If Assigned(FOnError) then
    FOnError(Self, AError.Message, AError, Nil);

end;

procedure TAiChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
Var
  choices: TJSonArray;
  JItem: TJSonObject;
  JVal: TJSonValue;
  JToolCallsValue: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta, sReasoning: String;
  ToolMsg, AskMsg: TAiChatMessage;
  // Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave, sToolCalls, sRes: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;
  ModelVersion: String;

  // Subrutina local: garantiza captura independiente por valor en Delphi 10.4+
  // (la asignaci?n LCaptura:=ToolCall dentro del anon-proc corre cuando el task
  // ejecuta, no cuando se crea, por lo que ToolCall pudo haber cambiado)
  procedure _CreateTask(TC: TAiToolsFunction; AIdx: Integer);
  begin
    TaskList[AIdx] := TTask.Create(
      procedure
      begin
        try
          DoCallFunction(TC);
        except
          on E: Exception do
          begin
            var LErrorMsg := 'Error in "' + TC.Name + '": ' + E.Message;
            TThread.Queue(nil,
              procedure
              begin
                DoError(LErrorMsg, nil);
              end);
          end;
        end;
      end);
    TaskList[AIdx].Start;
  end;

begin

  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  aTotal_tokens := 0;
  if Assigned(jObj.GetValue('model')) then
    ModelVersion := jObj.GetValue('model').Value;
  uso := jObj.GetValue('usage') as TJSonObject;
  if Assigned(uso) then
  begin
    aPrompt_tokens := uso.GetValue<Integer>('prompt_tokens');
    aCompletion_tokens := uso.GetValue<Integer>('completion_tokens');
    aTotal_tokens := uso.GetValue<Integer>('total_tokens');
  end;

  AskMsg := GetLastMessage; // Obtiene la pregunta, ya que ResMsg se adiciona a la lista si no hay errores.

  jObj.TryGetValue<TJSonArray>('choices', choices);

  For JVal in choices do
  Begin
    JItem := TJSonObject(JVal);
    jMessage := JItem.GetValue<TJSonObject>('message');

    Role := jMessage.GetValue<String>('role');

    if jMessage.TryGetValue<String>('reasoning', sReasoning) then // Groq
    begin
      ResMsg.ReasoningContent := sReasoning;
    end
    Else if jMessage.TryGetValue<String>('reasoning_content', sReasoning) then // DeepSeek
    begin
      ResMsg.ReasoningContent := sReasoning;
    end;

    jMessage.TryGetValue<String>('content', sRes);

    If Not Trim(sRes).IsEmpty then
      Respuesta := Trim(Respuesta + sLineBreak + sRes)
    Else
      Respuesta := Trim(Respuesta + sLineBreak + sReasoning);

    sToolCalls := '';
    If jMessage.TryGetValue<TJSonValue>('tool_calls', JToolCallsValue) and Assigned(JToolCallsValue) and (JToolCallsValue is TJSonArray) then
    Begin
      sToolCalls := TJSonArray(JToolCallsValue).Format;
    End;
  End;

  Respuesta := Trim(Respuesta);

  Self.FLastContent := Trim(Self.FLastContent + sLineBreak + Respuesta);
  FPrompt_tokens := FPrompt_tokens + aPrompt_tokens;
  FCompletion_tokens := FCompletion_tokens + aCompletion_tokens;
  FTotal_tokens := FTotal_tokens + aTotal_tokens;

  if sToolCalls.IsEmpty then // Si es una respuesta normal adiciona los datos al ResMsg
  Begin
    ResMsg.Role := Role;
    ResMsg.Model := ModelVersion;
    ResMsg.Tool_calls := sToolCalls;
    ResMsg.Prompt := ResMsg.Prompt + Respuesta;
    ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
    ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
    ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
    DoProcessResponse(AskMsg, ResMsg, Respuesta);
  End
  Else // Si tiene toolcall lo adiciona y ejecuta nuevamente el run para obtener la respuesta
  Begin
    Var
    Msg := TAiChatMessage.Create(Respuesta, Role);
    Msg.Tool_calls := sToolCalls;
    if sReasoning <> '' then
      Msg.ReasoningContent := sReasoning;  // Requerido por deepseek-reasoner y otros modelos con reasoning
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
  End;

  LFunciones := ExtractToolCallFromJson(choices);

  Try
    If LFunciones.Count > 0 then
    Begin

      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tama?o del array para el n?mero de tareas

      I := 0;
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        ToolCall.ResMsg := ResMsg; // Se pasan los mensajes por si desean procesarlos
        ToolCall.AskMsg := AskMsg;

        _CreateTask(ToolCall, I); // subrutina local garantiza captura por valor
        Inc(I);

      End;
      TTask.WaitForAll(TaskList);

      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      End;

      Self.Run(Nil, ResMsg);
      ResMsg.Content := '';

    End
    Else
    Begin
      FBusy := False;

      // Si est? marcado que genere archivos tomar? el resultado y extraer? los archivos y los adicona como mediafiles a ResMsg

      If tfc_ExtracttextFile in NativeOutputFiles then
      Begin
        Code := TMarkdownCodeExtractor.Create;
        Try

          CodeFiles := Code.ExtractCodeFiles(Respuesta);
          For CodeFile in CodeFiles do
          Begin
            St := TStringStream.Create(CodeFile.Code);
            Try
              St.Position := 0;

              MF := TAiMediaFile.Create;
              MF.LoadFromStream('file.' + CodeFile.FileType, St);
              ResMsg.MediaFiles.Add(MF);
            Finally
              St.Free;
            End;

          End;
        Finally
          Code.Free;
        End;
      End;

      DoProcessResponse(AskMsg, ResMsg, Respuesta);

      ResMsg.Prompt := Respuesta;

      DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO

      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, jObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

{
  procedure TAiChat.ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);
  var
  // JSON parsing
  jUsage, jInputTokenDetails: TJSonObject;
  jArrWords, jArrSegments: TJSonArray;

  // Datos extra?dos
  sTextoTranscrito, sTextoWords, sTextoSegments: String;
  aTotal_tokens, aInput_tokens, aOutput_tokens: integer;
  aText_tokens, aAudio_tokens: integer; // Tokens detallados del input

  begin
  sTextoTranscrito := '';
  aTotal_tokens := 0;
  aInput_tokens := 0;
  aOutput_tokens := 0;
  aText_tokens := 0;
  aAudio_tokens := 0;

  if not jObj.TryGetValue<string>('text', sTextoTranscrito) then
  begin
  FLastError := 'La respuesta de la API no contiene el campo "text" con la transcripci?n.';
  DoError(FLastError, nil);
  FBusy := False;
  Exit;
  end;

  If jObj.TryGetValue<TJSonArray>('words', jArrWords) then
  sTextoWords := jArrWords.Format;

  If jObj.TryGetValue<TJSonArray>('segments', jArrSegments) then
  sTextoSegments := jArrSegments.Format;

  // --- 3. EXTRACCI?N DE DATOS DE USO (TOKENS) ---
  // El objeto 'usage' podr?a no venir, as? que lo manejamos de forma segura.
  if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
  begin
  // Extraemos los tokens principales
  jUsage.TryGetValue<integer>('total_tokens', aTotal_tokens);
  jUsage.TryGetValue<integer>('input_tokens', aInput_tokens); // Costo del audio (y prompt si hubo)
  jUsage.TryGetValue<integer>('output_tokens', aOutput_tokens); // Costo del texto generado

  // Extraemos los detalles de los tokens de entrada (sub-objeto)
  // Esto nos dice cu?ntos tokens correspondieron al audio y cu?ntos a un posible prompt de texto.
  if jUsage.TryGetValue<TJSonObject>('input_token_details', jInputTokenDetails) then
  begin
  jInputTokenDetails.TryGetValue<integer>('text_tokens', aText_tokens);
  jInputTokenDetails.TryGetValue<integer>('audio_tokens', aAudio_tokens);
  end;
  end;

  // --- 4. ACTUALIZACI?N DEL ESTADO DEL COMPONENTE ---
  // Actualizamos los contadores de tokens globales, sumando los de esta llamada.
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
  Self.Prompt_tokens := Self.Prompt_tokens + aInput_tokens; // 'input' equivale a 'prompt'
  Self.Completion_tokens := Self.Completion_tokens + aOutput_tokens; // 'output' equivale a 'completion'

  If Trim(sTextoWords + sLineBreak + sTextoSegments) <> '' then
  Begin
  aMediaFile.Transcription := Trim(sTextoWords + sLineBreak + sTextoSegments);
  aMediaFile.Detail := Trim(sTextoTranscrito);
  ResMsg.Prompt := Trim(ResMsg.Prompt + aMediaFile.Transcription);
  ResMsg.Content := ResMsg.Content + sLineBreak + aMediaFile.Detail;
  End
  Else
  Begin
  aMediaFile.Transcription := sTextoTranscrito;
  ResMsg.Prompt := Trim(ResMsg.Prompt + sLineBreak + sTextoTranscrito);
  ResMsg.Content := Trim(ResMsg.Content + sLineBreak + sTextoTranscrito);
  End;

  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aInput_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aOutput_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
  end;
}

procedure TAiChat.ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);
var
  // JSON parsing
  jUsage, jInputTokenDetails: TJSonObject;
  jArrWords, jArrSegments: TJSonArray;

  // Datos extra?dos
  sTextoTranscrito, sTextoWords, sTextoSegments: String;
  aTotal_tokens, aInput_tokens, aOutput_tokens: Integer;
  aText_tokens, aAudio_tokens: Integer; // Tokens detallados del input

  // Variable auxiliar para el evento
  AskMsg: TAiChatMessage;

begin
  sTextoTranscrito := '';
  aTotal_tokens := 0;
  aInput_tokens := 0;
  aOutput_tokens := 0;
  aText_tokens := 0;
  aAudio_tokens := 0;

  // 1. Validar texto principal
  if not jObj.TryGetValue<string>('text', sTextoTranscrito) then
  begin
    FLastError := 'La respuesta de la API no contiene el campo "text" con la transcripci?n.';
    DoError(FLastError, nil);
    FBusy := False;
    Exit;
  end;

  // 2. Extraer detalles (Granularidad)
  If jObj.TryGetValue<TJSonArray>('words', jArrWords) then
    sTextoWords := jArrWords.Format;

  If jObj.TryGetValue<TJSonArray>('segments', jArrSegments) then
    sTextoSegments := jArrSegments.Format;

  // 3. EXTRACCI?N DE DATOS DE USO (TOKENS)
  if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
  begin
    jUsage.TryGetValue<Integer>('total_tokens', aTotal_tokens);
    jUsage.TryGetValue<Integer>('input_tokens', aInput_tokens);
    jUsage.TryGetValue<Integer>('output_tokens', aOutput_tokens);

    if jUsage.TryGetValue<TJSonObject>('input_token_details', jInputTokenDetails) then
    begin
      jInputTokenDetails.TryGetValue<Integer>('text_tokens', aText_tokens);
      jInputTokenDetails.TryGetValue<Integer>('audio_tokens', aAudio_tokens);
    end;
  end;

  // 4. ACTUALIZACI?N DEL ESTADO DEL COMPONENTE Y MENSAJES
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
  Self.Prompt_tokens := Self.Prompt_tokens + aInput_tokens;
  Self.Completion_tokens := Self.Completion_tokens + aOutput_tokens;

  // Asignar al archivo de medios
  if Trim(sTextoWords + sLineBreak + sTextoSegments) <> '' then
  begin
    aMediaFile.Transcription := Trim(sTextoWords + sLineBreak + sTextoSegments);
    aMediaFile.Detail := Trim(sTextoTranscrito);
  end
  else
  begin
    aMediaFile.Transcription := sTextoTranscrito;
  end;

  // Marcar como procesado para que no se reenv?e infinitamente en un loop
  aMediaFile.Procesado := True;

  // Actualizar el mensaje de respuesta (ResMsg)
  if ResMsg.Prompt <> '' then
    ResMsg.Prompt := ResMsg.Prompt + sLineBreak + sTextoTranscrito
  else
    ResMsg.Prompt := sTextoTranscrito;

  ResMsg.Content := ResMsg.Prompt; // Sincronizar Content con Prompt

  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aInput_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aOutput_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;

  // Asignar rol si no lo tiene
  if ResMsg.Role = '' then
    ResMsg.Role := 'assistant';

  // --- 5. DISPARAR EVENTOS (NUEVO BLOQUE) ---

  // A) Obtener el mensaje original (Usuario) para pasar como contexto
  AskMsg := GetLastMessage;

  // B) Notificar procesamiento interno (?til para logs o hooks)
  DoProcessResponse(AskMsg, ResMsg, sTextoTranscrito);

  // C) Notificar cambio de estado a la UI
  DoStateChange(acsFinished, 'Transcription Completed');

  If Self.Asynchronous and Assigned(FOnReceiveDataEvent) then
    FOnReceiveDataEvent(Self, ResMsg, jObj, 'assistant', sTextoTranscrito);

  // D) Disparar evento final de datos (Crucial para que la UI reciba el texto)
  if Assigned(FOnReceiveDataEnd) then
    FOnReceiveDataEnd(Self, ResMsg, jObj, 'assistant', sTextoTranscrito);

  // Liberar el estado de ocupado
  FBusy := False;
end;

function TAiChat.PrepareSystemMsg: String;
Var
  S, Key, Val, MensajeInicial: String;
  I: Integer;
  JMemory: TJSonObject;
begin
  // Si el formato de respuesta es Json, siempre debe llevar en la instrucci?n que el formato sea json
  If Self.Response_format = TAiChatResponseFormat.tiaChatRfJson then
    S := 'Responde en formato json'
  Else
    S := '';

  MensajeInicial := FSystemPrompt.Text + sLineBreak + S;

  JMemory := TJSonObject.Create;
  Try
    Try
      For I := 0 to FMemory.Count - 1 do
      Begin
        Key := FMemory.KeyNames[I];
        Val := FMemory.Values[Key];
        JMemory.AddPair(Key, Val);
      End;
    Except
      ON E: Exception do
      Begin
        Raise Exception.Create('El formato de memoria debe ser Key=Value, no est? bien configurado');
      End;
    End;

    If Assigned(FOnInitChat) then // Da la oportunidad de inicializar el chat con parametros adicionales como la memoria
      FOnInitChat(Self, 'system', MensajeInicial, JMemory);

    If Trim(MensajeInicial) <> '' then
      Result := MensajeInicial;

    If Length(Trim(JMemory.Format)) > 10 then
      Result := Result + sLineBreak + 'Para Recordar= ' + JMemory.Format;
  Finally
    JMemory.Free;
  End;
end;

function TAiChat.PublicChatToSend: String;
begin
  Result := InitChatCompletions;
end;

procedure TAiChat.RemoveFromMemory(Key: String);
begin
  FMemory.Values[Key] := '';
end;

function TAiChat.RemoveMesage(IdMsg: Integer): Boolean;
Var
  I: Integer;
  Msg: TAiChatMessage;
begin
  For I := 0 to FMessages.Count - 1 do
  Begin
    Msg := FMessages[I];
    If Msg.Id = IdMsg then
    Begin
      FMessages.Remove(Msg);
      Break;
    End;
  End;
  Result := True;
end;

function TAiChat.RetrieveFile(aFileId: string): TAiMediaFile;
begin
  Result := Nil; // Clase base no hace nada
end;

function TAiChat.RetrieveFileList: TAiMediaFiles;
begin
  Result := Nil; // Clase base no hace nada
end;

{ //original validado y probado
  Function TAiChat.Run(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
  Var
  MF: TAiMediaFile;
  LOwnsResMsg: Boolean; // Bandera para controlar la propiedad del objeto
  begin
  Result := '';
  LOwnsResMsg := False;

  // Si no se pasa un objeto ResMsg, creamos uno localmente.
  // En este caso, Run es el "due?o" y responsable de su ciclo de vida si no se a?ade a la lista.
  If Not Assigned(ResMsg) then
  begin
  ResMsg := TAiChatMessage.Create('', 'assistant');
  LOwnsResMsg := True;
  end;

  Try
  // Si el mensaje se pasa al run directamente lo adiciona a la lista de mensajes
  If Assigned(AskMsg) then
  InternalAddMessage(AskMsg);

  Try
  // Obtiene el ?ltimo mensaje que corresponde a la solicitud
  if not Assigned(AskMsg) then
  AskMsg := GetLastMessage;

  // Si existen mensajes sin procesar intentar? procesarlos antes de pasarlo al chat
  if Assigned(AskMsg) and (AskMsg.HasUnprocessedItems) then
  begin
  for MF in AskMsg.MediaFiles do
  begin
  If MF.FileCategory in NativeInputFiles then
  Begin
  // L?gica de procesamiento de archivos existentes...
  if (not(Tcm_Audio in ChatMediaSupports)) and (MF.FileCategory = Tfc_Audio) and (not MF.Procesado) then
  InternalRunTranscription(MF, ResMsg, AskMsg)
  else if (not(Tcm_pdf in ChatMediaSupports)) and (MF.FileCategory = Tfc_pdf) and (not MF.Procesado) then
  InternalRunPDFDescription(MF, ResMsg, AskMsg)
  Else if (not(Tcm_Image in ChatMediaSupports)) and (MF.FileCategory = Tfc_Image) and (not MF.Procesado) and (Not(Tfc_Video in NativeOutputFiles)) then
  InternalRunImageDescription(MF, ResMsg, AskMsg)
  Else if (Not(Tcm_Video in ChatMediaSupports)) and (Tfc_Video in NativeOutputFiles) then
  InternalRunImageVideoGeneration(ResMsg, AskMsg)
  Else
  Begin
  InternalRunCompletions(ResMsg, AskMsg);

  // --- CORRECCI?N 1: Manejo Async dentro del bucle de archivos ---
  If FClient.Asynchronous = True then
  Begin
  if LOwnsResMsg then // Solo liberar si nosotros lo creamos
  begin
  ResMsg.Free;
  ResMsg := Nil;
  end;
  Exit; // Salir para evitar a?adir a la lista abajo
  End;
  End;
  end
  Else
  Begin
  InternalRunCompletions(ResMsg, AskMsg);

  // --- CORRECCI?N 2: Manejo Async else nativo ---
  If FClient.Asynchronous = True then
  Begin
  if LOwnsResMsg then
  begin
  ResMsg.Free;
  ResMsg := Nil;
  end;
  Exit;
  End;
  End;
  end;
  end
  Else
  Begin
  // L?gica principal sin archivos pendientes de procesar
  if (Not(Tcm_Image in ChatMediaSupports)) and (Tfc_Image in NativeOutputFiles) then
  InternalRunImageGeneration(ResMsg, AskMsg)
  Else if (Not(Tcm_Audio in ChatMediaSupports)) and (Tfc_Audio in NativeOutputFiles) then
  InternalRunSpeechGeneration(ResMsg, AskMsg)
  Else if (Not(Tcm_Video in ChatMediaSupports)) and (Tfc_Video in NativeOutputFiles) then
  InternalRunImageVideoGeneration(ResMsg, AskMsg)
  Else if (Not(tcm_WebSearch in ChatMediaSupports)) and (Tfc_WebSearch in NativeInputFiles) then
  InternalRunWebSearch(ResMsg, AskMsg)
  Else
  Begin
  InternalRunCompletions(ResMsg, AskMsg);

  // --- CORRECCI?N 3: Manejo Async Principal (Donde te daba el error) ---
  If FClient.Asynchronous = True then
  Begin
  // CORREGIDO: Solo liberamos si Run cre? el objeto.
  // Si viene de ParseChat (Tool Call), LOwnsResMsg es False, no se libera aqu?,
  // evitando el Double Free en OnInternalReceiveData.
  if LOwnsResMsg then
  begin
  ResMsg.Free;
  ResMsg := Nil;
  end;
  Exit; // Salimos inmediatamente.
  End;

  End;
  End;

  // Solo si llega hasta aqu? (Modo S?ncrono)
  If Assigned(ResMsg) then
  Result := ResMsg.Prompt;

  If (AskMsg.Role <> 'tool') and (AskMsg.ToolCallId = '') then
  Begin
  If Assigned(ResMsg) then
  Begin
  ResMsg.Id := FMessages.Count + 1;
  FMessages.Add(ResMsg);
  End;
  End;

  Except
  On E: Exception do
  Begin
  // Limpieza segura en caso de error
  If LOwnsResMsg and Assigned(ResMsg) then
  begin
  // Verificamos que no est? en la lista antes de liberar
  if FMessages.IndexOf(ResMsg) = -1 then
  ResMsg.Free;
  end;
  DoError(E.Message, E);
  End;
  End;

  Finally
  End;
  end;
}

function TAiChat.RunLegacy(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
var
  MF: TAiMediaFile;
  LOwnsResMsg: Boolean;
  LRes: String;
  LProc: Boolean;
begin
  Result := '';
  LOwnsResMsg := False;

  // --- PREPARACI?N ---
  // 1. Gesti?n de mensaje de respuesta: Si no se provee uno, se crea localmente
  if not Assigned(ResMsg) then
  begin
    ResMsg := TAiChatMessage.Create('', 'assistant');
    LOwnsResMsg := True;
  end;

  try
    // 2. Registro del mensaje de entrada en el historial
    if Assigned(AskMsg) then
      InternalAddMessage(AskMsg);

    // 3. Referencia al mensaje que dispar? la acci?n
    if not Assigned(AskMsg) then
      AskMsg := GetLastMessage;

    if not Assigned(AskMsg) then
      raise Exception.Create('No hay un mensaje v?lido para procesar en TAiChat.Run');

    // --- FASE 1: BRIDGE DE ENTRADA (Interpretaci?n / Transcripci?n) ---
    // Solo se ejecuta en modo Conversaci?n para preparar el contexto del LLM
    if (FChatMode = cmConversation) then
    begin
      for MF in AskMsg.MediaFiles do
      begin
        // Regla: Si la categor?a del archivo NO es aceptada nativamente por el driver
        // y el archivo a?n no ha sido marcado como procesado.
        if not(MF.FileCategory in NativeInputFiles) and not MF.Procesado then
        begin
          // A. Intercepci?n manual por evento (Prioridad 1)
          if Assigned(FOnProcessMediaFile) then
          begin
            LRes := '';
            LProc := False;
            FOnProcessMediaFile(Self, AskMsg.Prompt, MF, FNativeInputFiles, LRes, LProc);
            MF.Procesado := LProc;
            MF.Transcription := LRes;
          end;

          // B. Delegaci?n a Bridges Autom?ticos (Prioridad 2)
          if not MF.Procesado then
          begin
            case MF.FileCategory of
              Tfc_Audio:
                // Si el modelo no tiene "o?do" nativo (Tcm_Audio), transcribimos
                if not(Tcm_Audio in ChatMediaSupports) then
                  InternalRunTranscription(MF, ResMsg, AskMsg);

              Tfc_Image:
                // Si el modelo no tiene "visi?n" nativa (Tcm_Image), describimos
                if not(Tcm_Image in ChatMediaSupports) then
                  InternalRunImageDescription(MF, ResMsg, AskMsg);

              Tfc_Pdf:
                // Si el modelo no entiende PDF nativo (Tcm_Pdf), extraemos texto
                if not(Tcm_Pdf in ChatMediaSupports) then
                  InternalRunPDFDescription(MF, ResMsg, AskMsg);
            end;
          end;
        end;
      end;
    end;

    // --- FASE 2: GROUNDING (Conexi?n a Datos Externos) ---
    // Si la b?squeda web est? habilitada por el usuario (EnabledFeatures)
    // PERO el modelo actual no sabe navegar nativamente (ChatMediaSupports).
    if (tcm_WebSearch in EnabledFeatures) and not(tcm_WebSearch in ChatMediaSupports) then
    begin
      DoStateChange(acsReasoning, 'Ejecutando Bridge de B?squeda Web...');
      InternalRunWebSearch(ResMsg, AskMsg);
      // El bridge inyecta el conocimiento en el prompt o en el objeto ResMsg
    end;

    // --- FASE 3: ORQUESTACI?N DE SALIDA (Generaci?n seg?n Capacidad) ---
    case FChatMode of
      cmConversation:
        begin
          // El orquestador decide bas?ndose en los Gaps de capacidad:

          // ?El usuario quiere video y el modelo no puede? -> Delegar a Tool
          if (Tfc_Video in NativeOutputFiles) and not(Tcm_Video in ChatMediaSupports) then
            InternalRunImageVideoGeneration(ResMsg, AskMsg)

            // ?El usuario quiere imagen y el modelo no puede? -> Delegar a Tool
          else if (Tfc_Image in NativeOutputFiles) and not(Tcm_Image in ChatMediaSupports) then
            InternalRunImageGeneration(ResMsg, AskMsg)

            // ?El usuario quiere voz (speech) y el modelo no puede? -> Delegar a Tool
          else if (Tfc_Audio in NativeOutputFiles) and not(Tcm_Audio in ChatMediaSupports) then
            InternalRunSpeechGeneration(ResMsg, AskMsg)

            // ?El usuario quiere reporte y el modelo no lo genera nativamente? -> Delegar a Tool
          else if (Tfc_Report in NativeOutputFiles) and not(Tcm_ReportGeneration in ChatMediaSupports) then
            InternalRunReport(ResMsg, AskMsg)

            // Si no hay gaps o no se requieren medios especiales, usamos el LLM nativo
          else
            InternalRunCompletions(ResMsg, AskMsg);
        end;

      // MODOS ESPEC?FICOS (Forzados): Saltan la l?gica de gaps y ejecutan la herramienta directamente
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

    // --- GESTI?N DE RESULTADO Y ASINCRON?A ---
    if FClient.Asynchronous then
    begin
      // Si es as?ncrono, el resultado vendr? por eventos.
      // Liberamos el mensaje temporal si el Run lo cre?.
      if LOwnsResMsg then
      begin
        ResMsg.Free;
        ResMsg := Nil;
      end;
      Exit;
    end;

    // Modo S?ncrono: Finalizaci?n
    if Assigned(ResMsg) then
    begin
      Result := ResMsg.Prompt;
      // Solo guardamos en el historial si no es un mensaje intermedio de herramienta (Tool)
      if (AskMsg.Role <> 'tool') and (AskMsg.ToolCallId = '') then
      begin
        if FMessages.IndexOf(ResMsg) = -1 then // guard: evitar doble-add (ej: Cohere ParseChat ya lo agregó)
        begin
          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);
        end;
      end
      else if LOwnsResMsg then
        FreeAndNil(ResMsg); // respuesta de tool no se agrega al historial: liberar si somos dueños
    end;

  except
    on E: Exception do
    begin
      // Limpieza segura de memoria en caso de error
      if LOwnsResMsg and Assigned(ResMsg) then
      begin
        if FMessages.IndexOf(ResMsg) = -1 then
          ResMsg.Free;
      end;
      DoError(E.Message, E);
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Nuevo sistema de orquestacion v3.3 - TAiCapabilities
// ---------------------------------------------------------------------------

procedure TAiChat.EnsureNewSystemConfig;
begin
  if FNewSystemConfigured then
    Exit; // ya usa el nuevo sistema, no sobreescribir
  FModelCaps := LegacyToModelCaps;
  FSessionCaps := LegacyToSessionCaps;
end;

function TAiChat.LegacyToModelCaps: TAiCapabilities;
begin
  Result := [];
  // Capacidades de comprension (lo que completions maneja nativamente)
  if Tcm_Image in FChatMediaSupports then
    Include(Result, cap_Image);
  if Tcm_Audio in FChatMediaSupports then
    Include(Result, cap_Audio);
  if Tcm_Video in FChatMediaSupports then
    Include(Result, cap_Video);
  if Tcm_Pdf in FChatMediaSupports then
    Include(Result, cap_Pdf);
  if tcm_WebSearch in FChatMediaSupports then
    Include(Result, cap_WebSearch);
  if Tcm_Reasoning in FChatMediaSupports then
    Include(Result, cap_Reasoning);
  if Tcm_CodeInterpreter in FChatMediaSupports then
    Include(Result, cap_CodeInterpreter);
  if Tcm_Memory in FChatMediaSupports then
    Include(Result, cap_Memory);
  if Tcm_TextEditor in FChatMediaSupports then
    Include(Result, cap_TextEditor);
  if Tcm_ComputerUse in FChatMediaSupports then
    Include(Result, cap_ComputerUse);
  if Tcm_Shell in FChatMediaSupports then
    Include(Result, cap_Shell);
  if Tcm_ReportGeneration in FChatMediaSupports then
    Include(Result, cap_GenReport);
  // Generacion nativa via completions (NativeOutputFiles + ChatMediaSupports confirman el output)
  if (Tfc_Audio in FNativeOutputFiles) and (Tcm_Audio in FChatMediaSupports) then
    Include(Result, cap_GenAudio);
  if (Tfc_Image in FNativeOutputFiles) and (Tcm_Image in FChatMediaSupports) then
    Include(Result, cap_GenImage);
  if (Tfc_Video in FNativeOutputFiles) and (Tcm_Video in FChatMediaSupports) then
    Include(Result, cap_GenVideo);
  // Post-procesamiento: siempre es "nativo" (hecho por InternalRunCompletions)
  if tfc_ExtracttextFile in FNativeOutputFiles then
    Include(Result, cap_ExtractCode);
end;

function TAiChat.LegacyToSessionCaps: TAiCapabilities;
begin
  Result := LegacyToModelCaps; // parte de la base nativa
  // Agregar desde EnabledFeatures (lo que el usuario quiere, puede crear gaps)
  if Tcm_Image in FEnabledFeatures then
    Include(Result, cap_Image);
  if Tcm_Audio in FEnabledFeatures then
    Include(Result, cap_Audio);
  if Tcm_Video in FEnabledFeatures then
    Include(Result, cap_Video);
  if Tcm_Pdf in FEnabledFeatures then
    Include(Result, cap_Pdf);
  if tcm_WebSearch in FEnabledFeatures then
    Include(Result, cap_WebSearch);
  if Tcm_Reasoning in FEnabledFeatures then
    Include(Result, cap_Reasoning);
  if Tcm_CodeInterpreter in FEnabledFeatures then
    Include(Result, cap_CodeInterpreter);
  if Tcm_Memory in FEnabledFeatures then
    Include(Result, cap_Memory);
  if Tcm_TextEditor in FEnabledFeatures then
    Include(Result, cap_TextEditor);
  if Tcm_ComputerUse in FEnabledFeatures then
    Include(Result, cap_ComputerUse);
  if Tcm_Shell in FEnabledFeatures then
    Include(Result, cap_Shell);
  // Agregar desde NativeOutputFiles (formato de salida deseado)
  if Tfc_Audio in FNativeOutputFiles then
    Include(Result, cap_GenAudio);
  if Tfc_Image in FNativeOutputFiles then
    Include(Result, cap_GenImage);
  if Tfc_Video in FNativeOutputFiles then
    Include(Result, cap_GenVideo);
  if Tfc_Report in FNativeOutputFiles then
    Include(Result, cap_GenReport);
  if tfc_ExtracttextFile in FNativeOutputFiles then
    Include(Result, cap_ExtractCode);
end;

procedure TAiChat.SetModelCaps(const Value: TAiCapabilities);
begin
  FModelCaps := Value;
  FNewSystemConfigured := True;
  SyncLegacyFromModelCaps;
end;

procedure TAiChat.SyncLegacyFromModelCaps;
var
  LNewInputFiles: TAiFileCategories;
  LNewSupports: TAiChatMediaSupports;
begin
  LNewInputFiles := [];
  if cap_Image in FModelCaps then
    Include(LNewInputFiles, Tfc_Image);
  if cap_Audio in FModelCaps then
    Include(LNewInputFiles, Tfc_Audio);
  if cap_Video in FModelCaps then
    Include(LNewInputFiles, Tfc_Video);
  if cap_Pdf in FModelCaps then
    Include(LNewInputFiles, Tfc_Pdf);
  SetNativeInputFiles(LNewInputFiles); // usa el setter existente (tambien actualiza FMessages)

  LNewSupports := [];
  if cap_Image in FModelCaps then
    Include(LNewSupports, Tcm_Image);
  if cap_Audio in FModelCaps then
    Include(LNewSupports, Tcm_Audio);
  if cap_Video in FModelCaps then
    Include(LNewSupports, Tcm_Video);
  if cap_Pdf in FModelCaps then
    Include(LNewSupports, Tcm_Pdf);
  if cap_WebSearch in FModelCaps then
    Include(LNewSupports, tcm_WebSearch);
  if cap_Reasoning in FModelCaps then
    Include(LNewSupports, Tcm_Reasoning);
  if cap_CodeInterpreter in FModelCaps then
    Include(LNewSupports, Tcm_CodeInterpreter);
  if cap_Memory in FModelCaps then
    Include(LNewSupports, Tcm_Memory);
  if cap_TextEditor in FModelCaps then
    Include(LNewSupports, Tcm_TextEditor);
  if cap_ComputerUse in FModelCaps then
    Include(LNewSupports, Tcm_ComputerUse);
  if cap_Shell in FModelCaps then
    Include(LNewSupports, Tcm_Shell);
  if cap_GenReport in FModelCaps then
    Include(LNewSupports, Tcm_ReportGeneration);
  FChatMediaSupports := LNewSupports;
end;

procedure TAiChat.SetSessionCaps(const Value: TAiCapabilities);
begin
  FSessionCaps := Value;
  FNewSystemConfigured := True;
  SyncLegacyFromSessionCaps;
end;

procedure TAiChat.SyncLegacyFromSessionCaps;
var
  LNewOutput: TAiFileCategories;
  LNewEnabled: TAiChatMediaSupports;
begin
  LNewOutput := [];
  if cap_GenAudio in FSessionCaps then
    Include(LNewOutput, Tfc_Audio);
  if cap_GenImage in FSessionCaps then
    Include(LNewOutput, Tfc_Image);
  if cap_GenVideo in FSessionCaps then
    Include(LNewOutput, Tfc_Video);
  if cap_GenReport in FSessionCaps then
    Include(LNewOutput, Tfc_Report);
  if cap_ExtractCode in FSessionCaps then
    Include(LNewOutput, tfc_ExtracttextFile);
  FNativeOutputFiles := LNewOutput;

  LNewEnabled := [];
  if cap_Image in FSessionCaps then
    Include(LNewEnabled, Tcm_Image);
  if cap_Audio in FSessionCaps then
    Include(LNewEnabled, Tcm_Audio);
  if cap_Video in FSessionCaps then
    Include(LNewEnabled, Tcm_Video);
  if cap_Pdf in FSessionCaps then
    Include(LNewEnabled, Tcm_Pdf);
  if cap_WebSearch in FSessionCaps then
    Include(LNewEnabled, tcm_WebSearch);
  if cap_Reasoning in FSessionCaps then
    Include(LNewEnabled, Tcm_Reasoning);
  if cap_CodeInterpreter in FSessionCaps then
    Include(LNewEnabled, Tcm_CodeInterpreter);
  if cap_Memory in FSessionCaps then
    Include(LNewEnabled, Tcm_Memory);
  if cap_TextEditor in FSessionCaps then
    Include(LNewEnabled, Tcm_TextEditor);
  if cap_ComputerUse in FSessionCaps then
    Include(LNewEnabled, Tcm_ComputerUse);
  if cap_Shell in FSessionCaps then
    Include(LNewEnabled, Tcm_Shell);
  FEnabledFeatures := LNewEnabled;
end;

function TAiChat.FileTypeInModelCaps(ACategory: TAiFileCategory): Boolean;
begin
  case ACategory of
    Tfc_Image:
      Result := cap_Image in FModelCaps;
    Tfc_Audio:
      Result := cap_Audio in FModelCaps;
    Tfc_Video:
      Result := cap_Video in FModelCaps;
    Tfc_Pdf:
      Result := cap_Pdf in FModelCaps;
  else
    Result := False; // tipos desconocidos siempre necesitan bridge (OnProcessMediaFile)
  end;
end;

function TAiChat.RunNew(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
var
  MF: TAiMediaFile;
  LOwnsResMsg: Boolean;
  LRes: String;
  LProc: Boolean;
  Gap: TAiCapabilities;
begin
  Result := '';
  LOwnsResMsg := False;
  Gap := FSessionCaps - FModelCaps; // Calculo central del gap

  // --- PREPARACION ---
  if not Assigned(ResMsg) then
  begin
    ResMsg := TAiChatMessage.Create('', 'assistant');
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
            LRes := '';
            LProc := False;
            FOnProcessMediaFile(Self, AskMsg.Prompt, MF, FNativeInputFiles, LRes, LProc);
            MF.Procesado := LProc;
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

    // --- FASE 2: GROUNDING (sin guarda de modo -- siempre se ejecuta) ---
    if cap_WebSearch in Gap then
    begin
      DoStateChange(acsReasoning, 'Ejecutando Bridge de Busqueda Web...');
      InternalRunWebSearch(ResMsg, AskMsg);
    end;

    // --- FASE 3: ORQUESTACION DE SALIDA ---
    case FChatMode of
      cmConversation:
        begin
          // Gap analysis unificado
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
          // cap_ExtractCode lo maneja InternalRunCompletions internamente
          // via Tfc_ExtractTextFile in NativeOutputFiles (sincronizado por SyncLegacyFromSessionCaps)
        end;
      // Modos forzados: saltan gap analysis, llamada directa
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
    if FClient.Asynchronous then
    begin
      if LOwnsResMsg then
      begin
        ResMsg.Free;
        ResMsg := Nil;
      end;
      Exit;
    end;

    if Assigned(ResMsg) then
    begin
      Result := ResMsg.Prompt;
      if (AskMsg.Role <> 'tool') and (AskMsg.ToolCallId = '') then
      begin
        if FMessages.IndexOf(ResMsg) = -1 then // guard: evitar doble-add (ej: Cohere ParseChat ya lo agregó)
        begin
          ResMsg.Id := FMessages.Count + 1;
          FMessages.Add(ResMsg);
        end;
      end
      else if LOwnsResMsg then
        FreeAndNil(ResMsg); // respuesta de tool no se agrega al historial: liberar si somos dueños
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

function TAiChat.Run(AskMsg: TAiChatMessage; ResMsg: TAiChatMessage): String;
begin
  EnsureNewSystemConfig; // traduce legacy -> nuevo si aun no se configuro
  Result := RunNew(AskMsg, ResMsg);
end;

function TAiChat.RemoveMesage(Msg: TAiChatMessage): Boolean;
begin
  // Remove devuelve el?ndice del elemento eliminado. Si es >= 0, fue exitoso.
  Result := FMessages.Remove(Msg) >= 0;
end;

procedure TAiChat.SetAiFunctions(const Value: TAiFunctions);
begin
  FAiFunctions := Value;
end;

procedure TAiChat.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiChat.SetAsynchronous(const Value: Boolean);
begin
  FAsynchronous := Value;
  FClient.Asynchronous := Value;
end;

procedure TAiChat.SetSpeechTool(const Value: TAiSpeechToolBase);
begin
  if FSpeechTool <> Value then
  begin
    FSpeechTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetChatMediaSupports(const Value: TAiChatMediaSupports);
begin
  FChatMediaSupports := Value;
end;

procedure TAiChat.SetCompletion_tokens(const Value: Integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChat.SetComputerUseTool(const Value: TAiComputerUseTool);
begin
  if FComputerUseTool <> Value then
  begin
    FComputerUseTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetEnabledFeatures(const Value: TAiChatMediaSupports);
begin
  FEnabledFeatures := Value;
end;

procedure TAiChat.SetFrequency_penalty(const Value: Double);
begin
  FFrequency_penalty := Value;
end;

procedure TAiChat.SetImageParams(const Value: TStrings);
begin
  FImageParams.Assign(Value);
end;

procedure TAiChat.SetImageTool(const Value: TAiImageToolBase);
begin
  if FImageTool <> Value then
  begin
    FImageTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetSystemPrompt(const Value: TStrings);
begin
  FSystemPrompt.Assign(Value);
end;

procedure TAiChat.SetJsonSchema(const Value: TStrings);
begin
  FJsonSchema.Assign(Value);
end;

procedure TAiChat.SetK(const Value: Integer);
begin
  FK := Value;
end;

procedure TAiChat.SetLanguage(const Value: string);
begin
  FLanguage := Value;
end;

procedure TAiChat.SetLastError(const Value: String);
begin
  FLastError := Value;
end;

procedure TAiChat.SetLogit_bias(const Value: String);
begin
  FLogit_bias := Value;
end;

procedure TAiChat.SetLogprobs(const Value: Boolean);
begin
  FLogprobs := Value;
end;

procedure TAiChat.SetMax_tokens(const Value: Integer);
begin
  FMax_tokens := Value;
end;

procedure TAiChat.SetMediaResolution(const Value: TAiMediaResolution);
begin
  FMediaResolution := Value;
end;

procedure TAiChat.SetMemory(const Value: TStrings);
begin
  FMemory.Text := Value.Text;
end;

procedure TAiChat.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiChat.SetN(const Value: Integer);
begin
  FN := Value;
end;

procedure TAiChat.SetNativeInputFiles(const Value: TAiFileCategories);
begin
  FNativeInputFiles := Value;
  FMessages.NativeInputFiles := Value;
end;

procedure TAiChat.SetNativeOutputFiles(const Value: TAiFileCategories);
begin
  FNativeOutputFiles := Value;
end;

procedure TAiChat.SetOnAddMessage(const Value: TAiChatOnDataEvent);
begin
  FOnAddMessage := Value;
end;

procedure TAiChat.SetOnBeforeSendMessage(const Value: TAiChatOnBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;
end;

procedure TAiChat.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
end;

procedure TAiChat.SetOnError(const Value: TAiErrorEvent);
begin
  FOnError := Value;
end;

procedure TAiChat.SetOnInitChat(const Value: TAiChatOnInitChatEvent);
begin
  FOnInitChat := Value;
end;

procedure TAiChat.SetOnProcessMediaFile(const Value: TAiChatOnMediaFileEvent);
begin
  FOnProcessMediaFile := Value;
end;

procedure TAiChat.SetOnProcessResponse(const Value: TAiChatOnProcessResponseEvent);
begin
  FOnProcessResponse := Value;
end;

procedure TAiChat.SetOnProgressEvent(const Value: TAiModelProgressEvent);
begin
  FOnProgressEvent := Value;
end;

procedure TAiChat.SetOnReceiveDataEnd(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveDataEnd := Value;
end;

procedure TAiChat.SetOnReceiveDataEvent(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveDataEvent := Value;
end;

procedure TAiChat.SetOnReceiveThinking(const Value: TAiChatOnDataEvent);
begin
  FOnReceiveThinking := Value;
end;

procedure TAiChat.SetPdfTool(const Value: TAiPdfToolBase);
begin
  If FPdfTool <> Value then
  Begin
    FPdfTool := Value;
    If Value <> Nil then
      Value.FreeNotification(Self);
  End;
end;

procedure TAiChat.SetReportTool(const Value: TAiReportToolBase);
begin
  If FReportTool <> Value then
  Begin
    FReportTool := Value;
    If Value <> Nil then
      Value.FreeNotification(Self);
  End;
end;

procedure TAiChat.SetPresence_penalty(const Value: Double);
begin
  FPresence_penalty := Value;
end;

procedure TAiChat.SetPrompt_tokens(const Value: Integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChat.SetReasoningFormat(const Value: String);
begin
  FReasoningFormat := Value;
end;

procedure TAiChat.SetResponseTimeOut(const Value: Integer);
begin
  If Value < 61000 then
    FResponseTimeOut := 61000
  Else
    FResponseTimeOut := Value;

  FClient.ResponseTimeOut := FResponseTimeOut;
end;

procedure TAiChat.SetResponse_format(const Value: TAiChatResponseFormat);
begin
  FResponse_format := Value;
end;

procedure TAiChat.SetSeed(const Value: Integer);
begin
  FSeed := Value;
end;

procedure TAiChat.SetShellTool(const Value: TAiShell);
begin
  if FShellTool <> Value then
  begin
    FShellTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetStop(const Value: string);
begin
  FStop := Value;
end;

procedure TAiChat.SetStream_Usage(const Value: Boolean);
begin
  FStream_Usage := Value;
end;

procedure TAiChat.SetTemperature(const Value: Double);
begin
  FTemperature := Value;
end;

procedure TAiChat.SetTextEditorTool(const Value: TAiTextEditorTool);
begin
  if FTextEditorTool <> Value then
  begin
    FTextEditorTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetThinkingLevel(const Value: TAiThinkingLevel);
begin
  FThinkingLevel := Value;
end;

procedure TAiChat.SetThinking_tokens(const Value: Integer);
begin
  FThinking_tokens := Value;
end;

procedure TAiChat.SetTool_Active(const Value: Boolean);
begin
  FTool_Active := Value;
end;

procedure TAiChat.SetTool_choice(const Value: string);
begin
  FTool_choice := Value;
end;

procedure TAiChat.SetTop_logprobs(const Value: String);
begin
  FTop_logprobs := Value;
end;

procedure TAiChat.SetTop_p(const Value: Double);
Var
  TmpVal: Double;
begin
  If Value > 1 then
    TmpVal := 1
  Else If Value < 0.1 then
    TmpVal := 0.1
  Else
    TmpVal := Value;

  FTop_p := TmpVal;
end;

procedure TAiChat.SetTotal_tokens(const Value: Integer);
begin
  FTotal_tokens := Value;
end;

procedure TAiChat.SetTranscription_ResponseFormat(const Value: string);
begin
  FTranscription_ResponseFormat := Value;
end;

procedure TAiChat.SetTranscription_TimestampGranularities(const Value: string);
begin
  FTranscription_TimestampGranularities := Value;
end;

procedure TAiChat.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiChat.SetUser(const Value: String);
begin
  FUser := Value;
end;

procedure TAiChat.SetVideoParams(const Value: TStrings);
begin
  FVideoParams.Assign(Value);
end;

procedure TAiChat.SetVideoTool(const Value: TAiVideoToolBase);
begin
  If FVideoTool <> Value then
  Begin
    FVideoTool := Value;
    If Value <> Nil then
      Value.FreeNotification(Self);
  End;
end;

procedure TAiChat.SetVisionTool(const Value: TAiVisionToolBase);
begin
  if FVisionTool <> Value then
  begin
    FVisionTool := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiChat.SetVoice(const Value: String);
begin
  FVoice := Value;
end;

procedure TAiChat.Setvoice_format(const Value: String);
begin
  Fvoice_format := Value;
end;

procedure TAiChat.SetWebSearchParams(const Value: TStrings);
begin
  FWebSearchParams.Assign(Value);
end;

procedure TAiChat.SetWebSearchTool(const Value: TAiWebSearchToolBase);
begin
  If FWebSearchTool <> Value then
  Begin
    FWebSearchTool := Value;
    If Value <> Nil then
      Value.FreeNotification(Self);
  End;
end;

Function TAiChat.FileCategoriesToString(const ACategories: TAiFileCategories): string;
var
  Category: TAiFileCategory;
  Parts: TStringDynArray;
begin
  SetLength(Parts, 0);
  if ACategories = [] then
    Exit('');

  for Category := Low(TAiFileCategory) to High(TAiFileCategory) do
  begin
    if Category in ACategories then
      Parts := Parts + [GetEnumName(TypeInfo(TAiFileCategory), Ord(Category))];
  end;
  Result := String.Join(',', Parts);
End;

Function TAiChat.StringToFileCategories(const AValue: string): TAiFileCategories;
var
  CategoryName: string;
  EnumValue: Integer;
begin
  Result := []; // Empezamos con un set vac?o
  if AValue.IsEmpty then
    Exit;

  // Usamos Split de TStringHelper para dividir por comas
  for CategoryName in AValue.Split([',']) do
  begin
    try
      // Obtenemos el valor ordinal del enum a partir de su nombre
      EnumValue := GetEnumValue(TypeInfo(TAiFileCategory), Trim(CategoryName));
      // Incluimos el valor en el set resultante
      Include(Result, TAiFileCategory(EnumValue));
    except
      // Opcional: Ignorar nombres de categor?a no v?lidos o lanzar una excepci?n
      // Aqu? simplemente los ignoramos.
    end;
  end;
End;

function TAiChat.UploadFile(aMediaFile: TAiMediaFile): String;
begin
  // Se reescribe para subir un archivo al modelo y retornar la URI del archivo en el modelo
end;

function TAiChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String;
begin
  // Permite subir archivos en cach?,  este se puede trasladar entre diferentes peticiones del mismo chat
end;

end.
