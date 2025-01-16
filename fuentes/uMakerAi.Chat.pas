// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// --------- CAMBIOS --------------------
// 29/08/2024 - Se adiciona el manejo de response_format = json_schema
// 04/11/2024 - adiciona el manejo de detail como propiedad en mediafile
// 04/11/2024 - adiciona el manejo de TAiChat.Stream_Usage - Estadistica de uso en modo stream OpenAi

unit uMakerAi.Chat;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON, uMakerAi.ToolFunctions, uMakerAi.Core;

type
  TAiChatMessage = Class(TObject)
  Private
    procedure SetContent(const Value: String);
    procedure SetRole(const Value: String);
    procedure SetPrompt(const Value: String);
    procedure SetFunctionName(const Value: String);
    procedure SetTollCallId(const Value: String);
    procedure SetTool_calls(const Value: String);
    procedure SetFId(const Value: integer);
    procedure SetCompletion_tokens(const Value: integer);
    procedure SetPrompt_tokens(const Value: integer);
    procedure SetTotal_tokens(const Value: integer);
  Protected
    FRole: String;
    FContent: String;
    FPrompt: String;
    FCompletion_tokens: integer;
    FTotal_tokens: integer;
    FPrompt_tokens: integer;
    FId: integer;
    FTollCallId: String;
    FFunctionName: String;
    FTool_calls: String;
    FMediaFiles: TAiMediaFiles;
  Public
    Constructor Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = '');
    Destructor Destroy; Override;

    Procedure AddMediaFile(aMediaFile: TAiMediaFile);
    Procedure LoadMediaFromFile(aFileName: String);
    Procedure LoadMediaFromStream(aFileName: String; Stream: TMemoryStream);
    Procedure LoadMediaFromBase64(aFileName: String; aBase64: String);

    Function StreamToBase64(Stream: TMemoryStream): String;
    Function ToJSon: TJSonArray; // Convierte el Objeto en un json para enviar al api

    Property Id: integer Read FId Write SetFId;
    Property Role: String read FRole write SetRole;
    Property Content: String read FContent write SetContent;
    Property Prompt: String read FPrompt write SetPrompt;
    Property Prompt_tokens: integer read FPrompt_tokens Write SetPrompt_tokens;
    Property Completion_tokens: integer read FCompletion_tokens Write SetCompletion_tokens;
    Property Total_tokens: integer read FTotal_tokens Write SetTotal_tokens;
    Property TollCallId: String read FTollCallId write SetTollCallId;
    Property FunctionName: String read FFunctionName write SetFunctionName;
    Property Tool_calls: String read FTool_calls write SetTool_calls;
    Property MediaFiles: TAiMediaFiles Read FMediaFiles;
  End;

  TAiChatMessages = Class(TList<TAiChatMessage>)
  Private
    FNativeInputFiles: TAiFileCategories;
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
  Protected
  Public
    Function ToJSon: TJSonArray;
    Procedure SaveToStream(Stream: TStream);
    Procedure SaveToFile(FileName: String);
    Procedure LoadFromStream(Stream: TStream);
    Procedure LoadFromFile(FileName: String);
    Property AsText: String Read GetAsText Write SetAsText;
    Property NativeInputFiles: TAiFileCategories read FNativeInputFiles write SetNativeInputFiles;
  End;

  TAiOpenChatResponseFormat = (tiaChatRfText, tiaChatRfJson, tiaChatRfJsonSchema);
  TAiOpenChatDataEvent = procedure(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: String) of object;
  TAiOpenChatBeforeSendEvent = procedure(const Sender: TObject; var aMsg: TAiChatMessage) of object;
  TAiOpenChatInitChat = procedure(const Sender: TObject; aRole: String; Var aText: String; Var aMemory: TJSonObject) of object;
  TAiOpenChatOnMediaFile = Procedure(Const Sender: TObject; Prompt: String; MediaFile: TAiMediaFile; Var Respuesta: String; Var aProcesado: Boolean) of object;

  TAiChatConfig = Class(TComponent)
  Private
    FApiKey: String;
    FModel: String;
    FDescription: String;
    FUrlApi: String;
    FSeed: integer;
    FTool_choice: string;
    FN: integer;
    FTop_p: Double;
    FInitialInstructions: TStrings;
    FResponse_format: TAiOpenChatResponseFormat;
    FStop: string;
    FTool_Active: Boolean;
    FTemperature: Double;
    FPresence_penalty: Double;
    FMax_tokens: integer;
    procedure SetApiKey(const Value: String);
    procedure SetDescription(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetUrlApi(const Value: String);
    procedure SetInitialInstructions(const Value: TStrings);
    procedure SetMax_tokens(const Value: integer);
    procedure SetN(const Value: integer);
    procedure SetPresence_penalty(const Value: Double);
    procedure SetResponse_format(const Value: TAiOpenChatResponseFormat);
    procedure SetSeed(const Value: integer);
    procedure SetStop(const Value: string);
    procedure SetTemperature(const Value: Double);
    procedure SetTool_Active(const Value: Boolean);
    procedure SetTool_choice(const Value: string);
    procedure SetTop_p(const Value: Double);
  Protected
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

  Published
    Property UrlApi: String read FUrlApi write SetUrlApi;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property Description: String read FDescription write SetDescription;

    Property Max_tokens: integer read FMax_tokens write SetMax_tokens;
    // 0 = null o el máximo
    Property N: integer read FN write SetN;
    // Defaults to 1 How many Chat completion choices to generate for each input message.Note that you will be charged based on the number of generated tokens across all of the choices.Keep n as 1 to minimize costs.
    Property Presence_penalty: Double read FPresence_penalty write SetPresence_penalty; // Defaults to 0 number between - 2.0 and 2.0
    Property Response_format: TAiOpenChatResponseFormat read FResponse_format write SetResponse_format;
    // object Optional an object specifying the format that the model must output.Compatible with gpt - 4 - 1106 - preview and gpt - 3.5 - turbo - 1106.
    Property Seed: integer read FSeed write SetSeed; // 0 no se envía
    Property Stop: string read FStop write SetStop;
    // Array de palabras separado por comas
    Property Temperature: Double read FTemperature write SetTemperature;
    // Defaults to 1  between 0 and 2.
    Property Top_p: Double read FTop_p write SetTop_p;
    // Defaults to 0 si es 0 no se envía,  entre 0 y 1
    Property Tool_choice: string read FTool_choice write SetTool_choice;
    Property Tool_Active: Boolean read FTool_Active write SetTool_Active;
    Property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
  End;

  TAiChat = class(TComponent)
  Private
    FOwner: TObject;
    FApiKey: String;
    FSeed: integer;
    FTool_choice: string;
    FN: integer;
    FTop_p: Double;
    FLogprobs: Boolean;
    FFrequency_penalty: Double;
    FStop: string;
    FLogit_bias: String;
    FTemperature: Double;
    FPresence_penalty: Double;
    FUser: String;
    FMax_tokens: integer;
    FAsynchronous: Boolean;
    FTop_logprobs: String;
    FModel: String;
    FInitialInstructions: TStrings;
    FCompletion_tokens: integer;
    FTotal_tokens: integer;
    FPrompt_tokens: integer;
    FTool_Active: Boolean;
    FUrl: String;
    FAIChatConfig: TAiChatConfig;
    FResponseTimeOut: integer;
    FOnInitChat: TAiOpenChatInitChat;
    FMemory: TStrings;

    FFunctions: TFunctionActionItems;
    FAIEngine: TAiChatConfig;
    FAiFunctions: TAiFunctions;
    FOnProcessMediaFile: TAiOpenChatOnMediaFile;
    FJsonSchema: TStrings;
    FStream_Usage: Boolean;
    FNativeInputFiles: TAiFileCategories;
    FNativeOutputFiles: TAiFileCategories;

    procedure SetApiKey(const Value: String);
    procedure SetFrequency_penalty(const Value: Double);
    procedure SetLogit_bias(const Value: String);
    procedure SetLogprobs(const Value: Boolean);
    procedure SetMax_tokens(const Value: integer);
    procedure SetN(const Value: integer);
    procedure SetPresence_penalty(const Value: Double);
    procedure SetResponse_format(const Value: TAiOpenChatResponseFormat);
    procedure SetSeed(const Value: integer);
    procedure SetStop(const Value: string);
    procedure SetTemperature(const Value: Double);
    procedure SetTool_choice(const Value: string);
    procedure SetTop_p(const Value: Double);
    procedure SetUser(const Value: String);
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetTop_logprobs(const Value: String);
    procedure SetOnReceiveDataEvent(const Value: TAiOpenChatDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
    procedure SetModel(const Value: String);
    procedure SetInitialInstructions(const Value: TStrings);
    procedure SetOnAddMessage(const Value: TAiOpenChatDataEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetTool_Active(const Value: Boolean);
    procedure SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
    procedure SetCompletion_tokens(const Value: integer);
    procedure SetPrompt_tokens(const Value: integer);
    procedure SetTotal_tokens(const Value: integer);
    procedure SetUrl(const Value: String);
    procedure SetAIChatConfig(const Value: TAiChatConfig);
    procedure SetLastError(const Value: String);
    procedure SetResponseTimeOut(const Value: integer);
    procedure SetOnInitChat(const Value: TAiOpenChatInitChat);
    procedure SetMemory(const Value: TStrings);
    procedure SetJsonSchema(const Value: TStrings);

    procedure SetAiFunctions(const Value: TAiFunctions);
    procedure SetOnProcessMediaFile(const Value: TAiOpenChatOnMediaFile);
    procedure SetStream_Usage(const Value: Boolean);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
    procedure SetNativeOutputFiles(const Value: TAiFileCategories);

  Protected
    FClient: TNetHTTPClient;
    FTmpRole: String;
    FTmpResponseText: String;
    FTmpResponseText1: String;
    FAbort: Boolean;
    FBusy: Boolean;
    FTools: TStrings;
    FLastContent: String;
    FLastPrompt: String;
    FLastError: String;
    FMessages: TAiChatMessages;
    FResponse_format: TAiOpenChatResponseFormat;
    FResponse: TStringStream;
    FOnReceiveDataEvent: TAiOpenChatDataEvent;
    FOnReceiveDataEnd: TAiOpenChatDataEvent;
    FOnAddMessage: TAiOpenChatDataEvent;
    FOnCallToolFunction: TOnCallToolFunction;
    FOnBeforeSendMessage: TAiOpenChatBeforeSendEvent;
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Virtual;
    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String; Overload; Virtual;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Overload; Virtual;
    Function InitChatCompletions: String; Virtual;
    Procedure ParseChat(jObj: TJSonObject); Virtual;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Virtual;
    Procedure DoCallFunction(ToolCall: TAiToolsFunction); Virtual;
    function GetTools: TStrings; virtual;
    Function PrepareSystemMsg: String; Virtual; // Crea el primer mensaje del chat para system, para configurar el asistente
    Procedure DoProcessMediaFile(aPrompt: String; aAiMediaFile: TAiMediaFile; Var Respuesta: String; Var Procesado: Boolean);
    Function AddMessageAndRun(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String; Overload;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Overload;
    Function AddMessageAndRunMsg(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): TAiChatMessage; Overload;

    Function AddMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje y lo adiciona a la conversación
    Function NewMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje pero no lo adiciona a la conversación
    Function Run(aMsg: TAiChatMessage = Nil): String; Virtual;
    Function GetLastMessage: TAiChatMessage;
    Function RemoveMesage(Msg: TAiChatMessage): Boolean; Overload;
    Function RemoveMesage(IdMsg: integer): Boolean; Overload;
    Procedure AddToMemory(Key, Value: String);
    Procedure RemoveFromMemory(Key: String);
    Procedure NewChat;
    Procedure Abort;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Overload; virtual;
    Function GetModels: TStringList; Overload; Virtual;
    Function GetMessages: TJSonArray; Virtual;

    Function PublicChatToSend: String;

    Property Messages: TAiChatMessages read FMessages;
    Property LastError: String read FLastError write SetLastError;

  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property Frequency_penalty: Double read FFrequency_penalty write SetFrequency_penalty; // -2 to 2
    Property Logit_bias: String read FLogit_bias write SetLogit_bias;
    // Vacío o entre -100 y 100
    Property Logprobs: Boolean read FLogprobs write SetLogprobs;
    Property Top_logprobs: String read FTop_logprobs write SetTop_logprobs;
    // vacio o between 0 and 5
    Property Max_tokens: integer read FMax_tokens write SetMax_tokens;
    // 0 = null o el máximo
    Property N: integer read FN write SetN;
    // Defaults to 1 How many Chat completion choices to generate for each input message.Note that you will be charged based on the number of generated tokens across all of the choices.Keep n as 1 to minimize costs.
    Property Presence_penalty: Double read FPresence_penalty write SetPresence_penalty; // Defaults to 0 number between - 2.0 and 2.0
    Property Response_format: TAiOpenChatResponseFormat read FResponse_format write SetResponse_format;
    // object Optional an object specifying the format that the model must output.Compatible with gpt - 4 - 1106 - preview and gpt - 3.5 - turbo - 1106.
    Property Seed: integer read FSeed write SetSeed; // 0 no se envía
    Property Stop: string read FStop write SetStop;
    // Array de palabras separado por comas
    Property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    Property Temperature: Double read FTemperature write SetTemperature;
    // Defaults to 1  between 0 and 2.
    Property Top_p: Double read FTop_p write SetTop_p;
    // Defaults to 0 si es 0 no se envía,  entre 0 y 1
    Property Tools: TStrings read GetTools;
    Property Tool_choice: string read FTool_choice write SetTool_choice;
    Property Tool_Active: Boolean read FTool_Active write SetTool_Active;
    Property User: String read FUser write SetUser;
    Property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
    Property Prompt_tokens: integer read FPrompt_tokens write SetPrompt_tokens;
    Property Completion_tokens: integer read FCompletion_tokens write SetCompletion_tokens;
    Property Total_tokens: integer read FTotal_tokens write SetTotal_tokens;
    Property LastContent: String Read FLastContent;
    Property LastPrompt: String Read FLastPrompt;
    Property Busy: Boolean Read FBusy;
    Property OnReceiveData: TAiOpenChatDataEvent read FOnReceiveDataEvent write SetOnReceiveDataEvent;
    Property OnReceiveDataEnd: TAiOpenChatDataEvent read FOnReceiveDataEnd write SetOnReceiveDataEnd;
    Property OnAddMessage: TAiOpenChatDataEvent read FOnAddMessage write SetOnAddMessage;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property OnBeforeSendMessage: TAiOpenChatBeforeSendEvent read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    Property OnInitChat: TAiOpenChatInitChat read FOnInitChat write SetOnInitChat;
    Property Url: String read FUrl write SetUrl;
    Property AIChatConfig: TAiChatConfig read FAIChatConfig write SetAIChatConfig;
    Property ResponseTimeOut: integer read FResponseTimeOut write SetResponseTimeOut;
    Property Memory: TStrings read FMemory Write SetMemory;
    Property Functions: TFunctionActionItems read FFunctions write FFunctions;
    Property AiFunctions: TAiFunctions read FAiFunctions write SetAiFunctions;
    Property OnProcessMediaFile: TAiOpenChatOnMediaFile read FOnProcessMediaFile write SetOnProcessMediaFile;
    Property JsonSchema: TStrings read FJsonSchema write SetJsonSchema;
    Property Stream_Usage: Boolean read FStream_Usage write SetStream_Usage;
    Property NativeInputFiles: TAiFileCategories read FNativeInputFiles write SetNativeInputFiles;
    Property NativeOutputFiles: TAiFileCategories read FNativeOutputFiles write SetNativeOutputFiles;
  end;

  // procedure Register;

implementation

{ procedure Register;
  begin
  RegisterComponents('MakerAI', [TAiChat, TAiChatConfig]);
  end;
}

{ TAiChat }

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure TAiChat.Abort;
begin
  FAbort := True;
end;

function TAiChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String;
Var
  Msg: TAiChatMessage;
  MensajeInicial: String;
begin
  Try
    // Este es el CallBack de los ToolsFunctions,

    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    {
      If (FMessages.Count = 0) or ((FMessages.Count mod 20) = 0) then
      Begin
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiOpenChatMessage.Create(MensajeInicial, 'system');
      Msg.FId := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
      End;
    }

    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.FId := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

function TAiChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
Var
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
  MensajeInicial: String;
  Respuesta: String;
  Procesado: Boolean;
begin

  Try
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then
    Begin
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiChatMessage.Create(MensajeInicial, 'system');
      Msg.FId := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole);
    Msg.FId := FMessages.Count + 1;
    FMessages.Add(Msg);

    If Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, Msg, Nil, aRole, aPrompt);
    End;

    For MF in aMediaFiles do
    Begin
      DoProcessMediaFile(aPrompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
      If Procesado then // Si el usuario convirtió el media file en un texto, para procesar el texto y no el archivo directamente
        Msg.Prompt := Msg.Prompt + sLineBreak + Respuesta;

      Msg.AddMediaFile(MF);
    End;

    FLastPrompt := Msg.Prompt; // aqui lleva el Prompt Inicial + la conversión de los MediaFiles a texto si el usuario lo permite

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

function TAiChat.AddMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  InternalAddMessage(aPrompt, aRole, []);
end;

function TAiChat.AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
begin
  InternalAddMessage(aPrompt, aRole, aMediaFiles);
  Result := Run;
end;

function TAiChat.AddMessageAndRunMsg(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): TAiChatMessage;
begin
  if FAsynchronous = False then
  Begin
    AddMessageAndRun(aPrompt, aRole, aMediaFiles);
    Result := Self.GetLastMessage; // Retorna el mensaje
  End
  Else
    Raise Exception.Create('Esta función no es compatible con el modo asincrónico')
end;

function TAiChat.AddMessageAndRun(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String;
begin
  InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName);
  Result := Run;
end;

procedure TAiChat.AddToMemory(Key, Value: String);
begin
  FMemory.AddPair(Key, Value);
end;

constructor TAiChat.Create(Sender: TComponent);
begin
  inherited;
  FOwner := Sender;
  FMessages := TAiChatMessages.Create;
  FTools := TStringList.Create;
  FMemory := TStringList.Create;
  FInitialInstructions := TStringList.Create;
  FFunctions := TFunctionActionItems.Create(Self, TFunctionActionItem);

  FResponse := TStringStream.Create('', TEncoding.UTF8);
  FClient := TNetHTTPClient.Create(Self);
  FClient.OnReceiveData := Self.OnInternalReceiveData;
  FClient.ResponseTimeOut := 60000;

  FModel := 'gpt4-o';
  FN := 1;
  FResponse_format := TAiOpenChatResponseFormat.tiaChatRfText;
  FTemperature := 1;
  FUser := 'user';
  FInitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  FMax_tokens := 300;
  FUrl := GlOpenAIUrl;
  FTop_p := 1;
  FResponseTimeOut := 60000;
  FStream_Usage := False; // Envia la estadistica de uso por token
  FTool_choice := 'auto';
end;

destructor TAiChat.Destroy;
begin
  FResponse.Free;
  FTools.Free;
  FClient.Free;
  FMemory.Free;
  FFunctions.Free;

  inherited;
end;

procedure TAiChat.DoCallFunction(ToolCall: TAiToolsFunction);
Var
  Funcion: TFunctionActionItem;
  Handle: Boolean;
begin
  If Assigned(FAiFunctions) then // Si está asignado el componente, busca la función en el componente
    Funcion := FAiFunctions.Functions.GetFunction(ToolCall.Name)
  Else // Si no está asignado el componente, lo busca directamente en las funciones locales
    Funcion := FFunctions.GetFunction(ToolCall.Name);

  If Assigned(Funcion) then
  Begin
    Funcion.OnAction(Self, Funcion, ToolCall.Name, ToolCall, Handle);
    If Handle = False then
    Begin
      If Assigned(FOnCallToolFunction) then
        FOnCallToolFunction(Self, ToolCall)
    End;
  End
  Else
  Begin
    If Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall)
  End;
end;

procedure TAiChat.DoProcessMediaFile(aPrompt: String; aAiMediaFile: TAiMediaFile; Var Respuesta: String; Var Procesado: Boolean);
begin
  If Assigned(FOnProcessMediaFile) then
  Begin
    Procesado := False;
    Respuesta := '';
    FOnProcessMediaFile(Self, aPrompt, aAiMediaFile, Respuesta, Procesado);
    aAiMediaFile.Procesado := Procesado;
    aAiMediaFile.Transcription := Respuesta;
  End;
end;

function TAiChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSonObject;
  ArgVal, JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  Nom, Valor: String;
  I: integer;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

    If Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        jObj := TJSonObject(JVal);
        If jObj.GetValue<String>('type') = 'function' then
        Begin
          jObj := TJSonObject(JVal);
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          Fun.Tipo := jObj.GetValue<String>('type');

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
              If Assigned(Arg) then
              Begin
                For I := 0 to Arg.Count - 1 do
                Begin
                  Nom := Arg.Pairs[I].JsonString.Value;
                  Valor := Arg.Pairs[I].JsonValue.Value;
                  Fun.Params.Values[Nom] := Valor;
                End;
              End;
            End;

          Except
            // Si no hay parámetros no marca error
          End;

          Result.Add(Fun.Id, Fun);
        End;
      End;
    End;
  End;
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
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;
  sModel: string;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'models';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);
    // Response.SaveToFile('c:\temp\models.json.txt');

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If jRes.TryGetValue<TJSonArray>('data', JArr) then
      Begin
        For JVal in JArr do
        Begin
          sModel := JVal.GetValue<String>('id');
          If sModel <> '' then
            Result.Add(sModel);
        End;
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

function TAiChat.GetTools: TStrings;
Var
  Funcs: TJSonArray;
begin
  Funcs := Functions.ToJSon;
  Try
    If Assigned(Funcs) and FTool_Active then // Si utiliza tools functions
    Begin
      If Assigned(FAiFunctions) then // Si está asignado el componente lo obtiene del componente
      Begin
        FTools.Text := FAiFunctions.GetTools;
        Result := FTools;
      End
      Else
      Begin // Si no está asignado el componente, lo obtiene de las funciones directamente
        FTools.Text := Funcs.Format;
        Result := FTools;
      End;
    End
    Else
    Begin
      FTools.Text := '';
      Result := FTools;
    End;
  Finally
    Funcs.Free;
  End;
end;

function TAiChat.InitChatCompletions: String;
Var
  AJSONObject, jObj, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
  LastMsg: TAiChatMessage;
  Res: String;
begin

  If FUser = '' then
    FUser := 'user';

  If FModel = '' then
    FModel := 'gpt-4o';

  // Las funciones no trabajan en modo ascincrono
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  // estas líneas no hacen falta, se dejan como gúia para proximas implementaciones
  // LastMsg := Messages.Last;
  { If Assigned(LastMsg) then
    Begin
    LAsincronico := LAsincronico and (Not(LastMsg.VisionUrls.Count > 0) or (LastMsg.VisionBase64.Count > 0));
    End;
  }

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(Tools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(Tools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(FTool_choice) <> '') then
      Begin
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(FTool_choice));
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;

    End;

    AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
    AJSONObject.AddPair('model', FModel);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(FTemperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(FMax_tokens));

    If FTop_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(FTop_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(FFrequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(FPresence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(FN));

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

    Res := UTF8ToString(AJSONObject.ToJSon);
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

procedure TAiChat.NewChat;
Var
  I: integer;
begin
  For I := FMessages.Count - 1 to 0 do
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

procedure TAiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  jObj, Delta: TJSonObject;
  sJson, Value, Role1: String;
  P: integer;
  Msg: TAiChatMessage;
begin

  If FClient.Asynchronous = False then
    Exit;

  AAbort := FAbort;

  If FAbort = True then
  Begin
    FBusy := False;
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
  End;

  Try
    FTmpResponseText := FTmpResponseText + FResponse.DataString;
    FTmpResponseText1 := FTmpResponseText1 + FResponse.DataString;

    FResponse.Clear;
    FResponse.Position := 0;

    If Copy(FTmpResponseText, 1, 5) = 'data:' then
      FTmpResponseText := Copy(FTmpResponseText, 6, Length(FTmpResponseText));

    Repeat
      P := Pos('data:', FTmpResponseText);
      If P > 0 then
      Begin
        sJson := Trim(Copy(FTmpResponseText, 1, P - 1));
        FTmpResponseText := Copy(FTmpResponseText, P + 6, Length(FTmpResponseText));
      End
      Else
      Begin
        If Trim(FTmpResponseText) = '[DONE]' then // Terminó el proceso
        Begin
          sJson := Trim(FTmpResponseText);
          FTmpResponseText := '';
        End
        Else
          sJson := '';
      End;

      If sJson = '[DONE]' then // Terminó el proceso
      Begin
        sJson := '';
        Msg := TAiChatMessage.Create(FLastContent, FTmpRole);
        Msg.FId := FMessages.Count + 1;
        FMessages.Add(Msg);
        FBusy := False;

        If Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, Msg, Nil, FTmpRole, FLastContent);
      End
      Else If sJson <> '' then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

        Try
          If Assigned(jObj) then
          Begin
            Delta := jObj.GetValue<TJSonArray>('choices')[0].GetValue<TJSonObject>('delta');
            Value := '';
            Delta.TryGetValue<String>('content', Value);
            Delta.TryGetValue<String>('role', Role1);

            If Role1 <> '' then
              FTmpRole := Role1;

            FLastContent := FLastContent + Value;

            If (Value <> '') and Assigned(FOnReceiveDataEvent) then
            Begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            End;
          End;
        Finally
          jObj.Free;
        End;
      End;

    Until sJson = '';

  Except

  End;
end;

procedure TAiChat.ParseChat(jObj: TJSonObject);
Var
  choices, JToolCalls: TJSonArray;
  JItem: TJSonObject;
  JVal: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;
  Role, Respuesta: String;
  Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: integer;
  Clave, sToolCalls, sRes: String;

begin

  // Id := JObj.GetValue('id').Value;
  // IdObject := JObj.GetValue('object').Value;
  // IdCreate := JObj.GetValue('created').GetValue<String>;
  Model := jObj.GetValue('model').Value;
  uso := jObj.GetValue('usage') as TJSonObject;
  aPrompt_tokens := uso.GetValue<integer>('prompt_tokens');
  aCompletion_tokens := uso.GetValue<integer>('completion_tokens');
  aTotal_tokens := uso.GetValue<integer>('total_tokens');

  jObj.TryGetValue<TJSonArray>('choices', choices);

  For JVal in choices do
  Begin
    JItem := TJSonObject(JVal);
    jMessage := JItem.GetValue<TJSonObject>('message');
    Role := jMessage.GetValue<String>('role');

    If jMessage.TryGetValue<String>('content', sRes) then
      Respuesta := Respuesta + sRes + sLineBreak;

    If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
      sToolCalls := JToolCalls.Format;
  End;

  Respuesta := Trim(Respuesta);
  Self.FLastContent := Respuesta;
  FPrompt_tokens := FPrompt_tokens + aPrompt_tokens;
  FCompletion_tokens := FCompletion_tokens + aCompletion_tokens;
  FTotal_tokens := FTotal_tokens + aTotal_tokens;

  Msg := TAiChatMessage.Create(Respuesta, Role);
  Msg.FPrompt := Respuesta;
  Msg.FTool_calls := sToolCalls;
  Msg.FPrompt_tokens := aPrompt_tokens;
  Msg.FCompletion_tokens := aCompletion_tokens;
  Msg.FTotal_tokens := aTotal_tokens;
  Msg.FId := FMessages.Count + 1;
  FMessages.Add(Msg);

  // If Assigned(FOnAddMessage) then
  // FOnAddMessage(Self, jObj, Role, Respuesta);

  LFunciones := ExtractToolCallFromJson(choices);

  Try
    If LFunciones.Count > 0 then
    Begin

      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tamaño del array para el número de tareas

      I := 0;
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];

        TaskList[I] := TTask.Create(
          procedure
          begin
            DoCallFunction(ToolCall);
          end);
        TaskList[I].Start;
        Inc(I);

      End;
      TTask.WaitForAll(TaskList);

      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        Msg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        Msg.FId := FMessages.Count + 1;
        FMessages.Add(Msg);
      End;

      Self.Run;

    End
    Else
    Begin
      FBusy := False;
      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, Msg, jObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

function TAiChat.PrepareSystemMsg: String;
Var
  S, Key, Val, MensajeInicial: String;
  I: integer;
  JMemory: TJSonObject;
begin
  // Si el formato de respuesta es Json, siempre debe llevar en la instrucción que el formato sea json
  If Self.Response_format = TAiOpenChatResponseFormat.tiaChatRfJson then
    S := 'Responde en formato json'
  Else
    S := '';

  MensajeInicial := FInitialInstructions.Text + sLineBreak + S;

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
        Raise Exception.Create('El formato de memoria debe ser Key=Value, no está bien configurado');
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

function TAiChat.RemoveMesage(IdMsg: integer): Boolean;
Var
  I: integer;
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

function TAiChat.Run(aMsg: TAiChatMessage = Nil): String;
Var
  ABody: String;
  sUrl, MensajeInicial: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
  Msg: TAiChatMessage;
  MF: TAiMediaFile;
  I: integer;
  Respuesta: String;
  Procesado: Boolean;
begin

  Msg := Nil;
  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'chat/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) or ((FMessages.Count mod 20) = 0) then
    Begin

      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiChatMessage.Create(MensajeInicial, 'system');
      Msg.FId := FMessages.Count + 1;

      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    If Assigned(aMsg) then
    Begin
      If Assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage(Self, aMsg);

      For I := 0 to aMsg.MediaFiles.Count - 1 do // MF in aMsg.MediaFiles do
      Begin
        MF := aMsg.MediaFiles[I];

        DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
        If Procesado then // Se asegura que el prompt sea complementado por las respuestas a los MediaFiles si el usuario lo aplica
          aMsg.Prompt := aMsg.Prompt + sLineBreak + Respuesta;

        aMsg.AddMediaFile(MF);
      End;

      aMsg.FId := FMessages.Count + 1;
      FMessages.Add(aMsg);
      FLastPrompt := aMsg.Prompt;

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, aMsg.Role, aMsg.Prompt);
    End;

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

    //St.SaveToFile('c:\temp\peticion.txt');
    //St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FResponse.Position := 0;
    // FResponse.SaveToFile('c:\temp\respuesta.txt');
    // FResponse.Position := 0;

    FLastContent := '';

    // If Self.Asynchronous = False then
    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(jObj);
          Result := FLastContent;

        Finally
          FreeAndNil(jObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free; // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

function TAiChat.RemoveMesage(Msg: TAiChatMessage): Boolean;
begin
  If FMessages.ItemValue(Msg) >= 0 then
    FMessages.Remove(Msg);
end;

procedure TAiChat.SetAIChatConfig(const Value: TAiChatConfig);
begin
  If Assigned(Value) and (FAIChatConfig <> Value) then
  Begin
    FAIChatConfig := Value;
    ApiKey := FAIChatConfig.ApiKey;
    Self.Url := FAIChatConfig.UrlApi;
    Self.Model := FAIChatConfig.Model;

    Self.Max_tokens := FAIChatConfig.Max_tokens;
    Self.N := FAIChatConfig.N;
    Self.Presence_penalty := FAIChatConfig.Presence_penalty;
    Self.Response_format := FAIChatConfig.Response_format;
    Self.Seed := FAIChatConfig.Seed;
    Self.Stop := FAIChatConfig.Stop;
    Self.Temperature := FAIChatConfig.Temperature;
    Self.Top_p := FAIChatConfig.Top_p;
    Self.Tool_choice := FAIChatConfig.Tool_choice;
    Self.Tool_Active := FAIChatConfig.Tool_Active;
    Self.InitialInstructions := FAIChatConfig.InitialInstructions;
  End;
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

procedure TAiChat.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChat.SetFrequency_penalty(const Value: Double);
begin
  FFrequency_penalty := Value;
end;

procedure TAiChat.SetInitialInstructions(const Value: TStrings);
begin
  FInitialInstructions.Text := Value.Text;
end;

procedure TAiChat.SetJsonSchema(const Value: TStrings);
begin
  FJsonSchema.Text := Value.Text;
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

procedure TAiChat.SetMax_tokens(const Value: integer);
begin
  FMax_tokens := Value;
end;

procedure TAiChat.SetMemory(const Value: TStrings);
begin
  FMemory.Text := Value.Text;
end;

procedure TAiChat.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiChat.SetN(const Value: integer);
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

procedure TAiChat.SetOnAddMessage(const Value: TAiOpenChatDataEvent);
begin
  FOnAddMessage := Value;
end;

procedure TAiChat.SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;
end;

procedure TAiChat.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
end;

procedure TAiChat.SetOnInitChat(const Value: TAiOpenChatInitChat);
begin
  FOnInitChat := Value;
end;

procedure TAiChat.SetOnProcessMediaFile(const Value: TAiOpenChatOnMediaFile);
begin
  FOnProcessMediaFile := Value;
end;

procedure TAiChat.SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveDataEnd := Value;
end;

procedure TAiChat.SetOnReceiveDataEvent(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveDataEvent := Value;
end;

procedure TAiChat.SetPresence_penalty(const Value: Double);
begin
  FPresence_penalty := Value;
end;

procedure TAiChat.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChat.SetResponseTimeOut(const Value: integer);
begin
  If Value < 1000 then
    FResponseTimeOut := 1000
  Else
    FResponseTimeOut := Value;

  FClient.ResponseTimeOut := FResponseTimeOut;
end;

procedure TAiChat.SetResponse_format(const Value: TAiOpenChatResponseFormat);
begin
  FResponse_format := Value;
end;

procedure TAiChat.SetSeed(const Value: integer);
begin
  FSeed := Value;
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

procedure TAiChat.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
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

{
  procedure TAiOpenChatMessage.AddBase64Image(aBase64: String);
  begin
  VisionBase64.Add(aBase64);
  end;
}

procedure TAiChatMessage.AddMediaFile(aMediaFile: TAiMediaFile);
begin
  FMediaFiles.Add(aMediaFile);
end;

{ TAOpeniChatMessage }

{ procedure TAiOpenChatMessage.AddStreamImage(Stream: TMemoryStream);
  Var
  Base64: String;
  begin
  Base64 := StreamToBase64(Stream);
  FVisionBase64.Add(Base64);
  end;

  procedure TAiOpenChatMessage.AddUrlImage(aUrl: String);
  begin
  FVisionUrls.Add(aUrl);
  end;
}

constructor TAiChatMessage.Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = '');
begin
  Inherited Create;
  // FVisionUrls := TStringList.Create;
  // FVisionBase64 := TStringList.Create;
  Self.FRole := aRole;
  Self.FPrompt := aPrompt;
  Self.FFunctionName := aFunctionName;
  Self.FTollCallId := aToolCallId;
  FMediaFiles := TAiMediaFiles.Create;
end;

destructor TAiChatMessage.Destroy;
begin
  // FVisionUrls.Free;
  // FVisionBase64.Free;
  FMediaFiles.Free;
  inherited;
end;

procedure TAiChatMessage.LoadMediaFromBase64(aFileName, aBase64: String);
Var
  Media: TAiMediaFile;
begin
  If Length(Trim(aBase64)) < 100 then
    Raise Exception.Create('El Base64 está vacío, no se cargará');

  If aFileName = '' then // Ver como se asigna un nombre a partir del contenido del stream
    aFileName := 'imagen.jpg';

  Media := TAiMediaFile.Create;
  Media.LoadFromBase64(aFileName, aBase64);

  AddMediaFile(Media);
end;

procedure TAiChatMessage.LoadMediaFromFile(aFileName: String);
Var
  Media: TAiMediaFile;
begin
  If Not FileExists(aFileName) then
    Raise Exception.Create('El archivo "' + aFileName + '" no se encuentra');

  Media := TAiMediaFile.Create;
  Media.LoadFromFile(aFileName);
  AddMediaFile(Media);
end;

procedure TAiChatMessage.LoadMediaFromStream(aFileName: String; Stream: TMemoryStream);
Var
  Media: TAiMediaFile;
begin
  If Stream.Size <= 100 then
    Raise Exception.Create('El stream está vacío');

  If aFileName = '' then // Ver como se asigna un nombre a partir del contenido del stream
    aFileName := 'imagen.jpg';

  Media := TAiMediaFile.Create;
  Media.LoadFromStream(aFileName, Stream);
  Self.AddMediaFile(Media);
end;

procedure TAiChatMessage.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChatMessage.SetContent(const Value: String);
begin
  FContent := Value;
end;

procedure TAiChatMessage.SetFId(const Value: integer);
begin
  FId := Value;
end;

procedure TAiChatMessage.SetFunctionName(const Value: String);
begin
  FFunctionName := Value;
end;

procedure TAiChatMessage.SetPrompt(const Value: String);
begin
  FPrompt := Value;
end;

procedure TAiChatMessage.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChatMessage.SetRole(const Value: String);
begin
  FRole := Value;
end;

procedure TAiChatMessage.SetTollCallId(const Value: String);
begin
  FTollCallId := Value;
end;

procedure TAiChatMessage.SetTool_calls(const Value: String);
begin
  FTool_calls := Value;
end;

procedure TAiChatMessage.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
end;

{ procedure TAiOpenChatMessage.SetVisionBase64(const Value: TStringList);
  begin
  FVisionBase64.Text := Value.Text;
  end;

  procedure TAiOpenChatMessage.SetVisionUrls(const Value: TStringList);
  begin
  FVisionUrls.Text := Value.Text;
  end;
}

function TAiChatMessage.StreamToBase64(Stream: TMemoryStream): String;
begin
  Stream.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
end;

function TAiChatMessage.ToJSon: TJSonArray;
Var
  Msg: TAiChatMessage;
  jObj, JMsg, jMsgImagen, jAudio: TJSonObject;
  JObjImg, jImgUrl: TJSonObject;
  JContent: TJSonArray;
  ImagePayload: TStringStream;
  Base64, Mime: String;
  MediaArr: TAiMediaFilesArray;
  S: String;
begin
  // Esta función solo toma el mensaje actual y una sola imágen, la primera que encuentra en la lista
  // Esto se hace especialmente para modelos que solo aceptan una imágene por petición y no un chat completo

  Result := TJSonArray.Create;

  Msg := Self;
  jObj := TJSonObject.Create;
  jObj.AddPair('role', Msg.FRole);

  If Msg.FTollCallId <> '' then
    jObj.AddPair('tool_call_id', Msg.FTollCallId);

  If Msg.FFunctionName <> '' then
    jObj.AddPair('name', Msg.FFunctionName);

  // de todos los archivos de medios selecciona las imágenes que es lo que podemos manejar por ahora
  // y las imágenes que no han sigo preprocesadas, por si el modelo no maneja imagenes, previamente
  // se deben haber procesado en en el momendo de adicionar el mensaje al chat
  MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

  If (Length(MediaArr) > 0) then
  Begin

    JContent := TJSonArray.Create;
    JMsg := TJSonObject.Create;
    JMsg.AddPair('type', 'text');
    JMsg.AddPair('text', Msg.FPrompt);
    JContent.Add(JMsg);

    If Msg.MediaFiles.Count > 0 then // Solo toma la primera imagen
    Begin
      Base64 := Msg.MediaFiles[0].Base64;
      Mime := Msg.MediaFiles[0].MimeType;

      ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:' + Mime + ';base64,' + Base64 + '"}}', TEncoding.UTF8);
      S := ImagePayload.DataString;
      try
        JContent.Add(TJSonObject.ParseJSONValue(ImagePayload.DataString) as TJSonObject);
      finally
        ImagePayload.Free;
      end;

    End;
    jObj.AddPair('content', JContent);
  End
  Else
  Begin

    jObj.AddPair('content', Msg.FPrompt);
  End;

  If Msg.FTool_calls <> '' then
    jObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));

  Result.Add(jObj);
end;

{ TAiOpenChatMessages }

function TAiChatMessages.GetAsText: String;
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    SaveToStream(St);
    Result := St.DataString;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.LoadFromFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If FileExists(FileName) then
    Begin
      St.LoadFromFile(FileName);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.LoadFromStream(Stream: TStream);
Var
  jObj, JItem: TJSonObject;
  JArr: TJSonArray;
  sJson, Model, S: String;
  St: TStringStream;
  Data: TAiChatMessages;
  Item: TAiChatMessage;
  I: integer;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    St.LoadFromStream(Stream);
    sJson := St.DataString;

    jObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

    If Assigned(jObj) and (jObj.TryGetValue<String>('model', Model)) then
    Begin
      If Model = 'AiOpenChat' then
      Begin
        JArr := TJSonArray(jObj.GetValue<TJSonArray>('data'));

        S := JArr.Format;
        If JArr.Count > 0 then
          Self.Clear;

        For I := 0 to JArr.Count - 1 do
        Begin
          JItem := TJSonObject(JArr[I]);

          Item := TJSon.JsonToObject<TAiChatMessage>(JItem);
          Self.Add(Item);
        End;
      End;
    End;
  Finally
    St.Free;
    FreeAndNil(jObj);
  End;
end;

procedure TAiChatMessages.SaveToFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    Self.SaveToStream(St);
    St.Position := 0;
    St.SaveToFile(FileName);
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.SaveToStream(Stream: TStream);
Var
  jObj, JItem: TJSonObject;
  JArr: TJSonArray;
  St: TStringStream;
  I: integer;
  Item: TAiChatMessage;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  jObj := TJSonObject.Create;
  JArr := TJSonArray.Create;

  Try
    jObj.AddPair('model', 'AiOpenChat');
    jObj.AddPair('type', 'Messages');
    jObj.AddPair('ver', '1.0');

    For I := 0 to Self.Count - 1 do
    Begin
      Item := Self.Items[I];
      JItem := TJSon.ObjectToJsonObject(Item);
      JArr.Add(JItem);
    End;

    jObj.AddPair('data', JArr);
    St.WriteString(jObj.Format);
    St.SaveToStream(Stream);
  Finally
    St.Free;
    jObj.Free;
  End;
end;

procedure TAiChatMessages.SetAsText(const Value: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If Trim(Value) <> '' then
    Begin
      St.WriteString(Value);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.SetNativeInputFiles(const Value: TAiFileCategories);
begin
  FNativeInputFiles := Value;
end;

function TAiChatMessages.ToJSon: TJSonArray;
Var
  I, J: integer;
  Msg: TAiChatMessage;
  jObj, JMsg, jMsgImagen: TJSonObject;
  JObjImg, jImgUrl, jAudio: TJSonObject;
  JContent: TJSonArray;
  ImagePayload: TStringStream;
  Base64, Mime: String;
  MediaArr: TAiMediaFilesArray;
  S: String;
  MediaFile: TAiMediaFile;
begin
  Result := TJSonArray.Create;

  For I := 0 to Count - 1 do
  Begin
    Msg := Self.Items[I];
    jObj := TJSonObject.Create;
    jObj.AddPair('role', Msg.FRole);

    If Msg.FTollCallId <> '' then
      jObj.AddPair('tool_call_id', Msg.FTollCallId);

    If Msg.FFunctionName <> '' then
      jObj.AddPair('name', Msg.FFunctionName);

    // de todos los archivos de medios selecciona las imágenes que es lo que podemos manejar por ahora
    // y las imágenes que no han sigo preprocesadas, por si el modelo no maneja imagenes, previamente
    // se deben haber procesado en en el momendo de adicionar el mensaje al chat
    MediaArr := Msg.MediaFiles.GetMediaList(FNativeInputFiles, False);

    If (Length(MediaArr) > 0) then
    Begin

      JContent := TJSonArray.Create;
      JMsg := TJSonObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FPrompt);
      JContent.Add(JMsg);

      For J := 0 to Length(MediaArr) - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
      Begin
        MediaFile := MediaArr[J];

        If MediaFile.FileCategory = TAiFileCategory.Tfc_Image then
        Begin

          Base64 := MediaFile.Base64;
          Mime := MediaFile.MimeType;

          // Esta es otra forma de hacer lo mismo con json directamente
          S := 'data:' + Mime + ';base64,' + Base64;

          jImgUrl := TJSonObject.Create;
          jImgUrl.AddPair('url', S);

          If Msg.MediaFiles[J].Detail <> '' then // Si define high or low.
            jImgUrl.AddPair('detail', MediaFile.Detail);

          JObjImg := TJSonObject.Create;
          JObjImg.AddPair('type', 'image_url');
          JObjImg.AddPair('image_url', jImgUrl);

          JContent.Add(JObjImg);
          jObj.AddPair('content', JContent);
        End
        Else If MediaFile.FileCategory = TAiFileCategory.Tfc_Audio then
        Begin
          If MediaFile.IdAudio <> '' then // Si es una respuesta del modelo va esto
          Begin
            jAudio := TJSonObject.Create;
            jAudio.AddPair('id', MediaFile.IdAudio);
            jObj.AddPair('audio', jAudio);
          End
          Else // Si es un audio del usuario va esto
          Begin
            jAudio := TJSonObject.Create;
            jAudio.AddPair('data', MediaFile.Base64);
            jAudio.AddPair('format', StringReplace(MediaFile.MimeType, 'audio/', '', [rfReplaceAll]));

            JContent := TJSonArray.Create;
            JMsg := TJSonObject.Create;
            JMsg.AddPair('type', 'input_audio');
            JMsg.AddPair('input_audio', jAudio);
            JContent.Add(JMsg);

            jObj.AddPair('content', JContent);
          End;
        End
        Else
        Begin
          jObj.AddPair('content', Msg.FPrompt);
        End;
      End;
    End
    Else
    Begin
      jObj.AddPair('content', Msg.FPrompt);
    End;

    If Msg.FTool_calls <> '' then
      jObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));

    Result.Add(jObj);
  End;
end;

// Esta es la versión original que si funciona
{function TAiChatMessages.ToJSon: TJSonArray;
Var
  I, J: integer;
  Msg: TAiChatMessage;
  jObj, JMsg, jMsgImagen: TJSonObject;
  JObjImg, jImgUrl, jAudio: TJSonObject;
  JContent: TJSonArray;
  ImagePayload: TStringStream;
  Base64, Mime: String;
  MediaArr: TAiMediaFilesArray;
  S: String;
  MediaFile: TAiMediaFile;
begin
  Result := TJSonArray.Create;

  For I := 0 to Count - 1 do
  Begin
    Msg := Self.Items[I];
    jObj := TJSonObject.Create;
    jObj.AddPair('role', Msg.FRole);

    If Msg.FTollCallId <> '' then
      jObj.AddPair('tool_call_id', Msg.FTollCallId);

    If Msg.FFunctionName <> '' then
      jObj.AddPair('name', Msg.FFunctionName);

    // de todos los archivos de medios selecciona las imágenes que es lo que podemos manejar por ahora
    // y las imágenes que no han sigo preprocesadas, por si el modelo no maneja imagenes, previamente
    // se deben haber procesado en en el momendo de adicionar el mensaje al chat
    MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

    If (Length(MediaArr) > 0) then
    Begin

      JContent := TJSonArray.Create;
      JMsg := TJSonObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FPrompt);
      JContent.Add(JMsg);

      For J := 0 to Length(MediaArr) - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
      Begin
        MediaFile := MediaArr[J];
        MediaFile.FileCategory;

        Base64 := MediaFile.Base64;
        Mime := MediaFile.MimeType;

        If MediaFile.FileCategory = TAiFileCategory.Tfc_Image then
        Begin

          // Esta es otra forma de hacer lo mismo con json directamente
          S := 'data:' + Mime + ';base64,' + Base64;

          jImgUrl := TJSonObject.Create;
          jImgUrl.AddPair('url', S);

          If Msg.MediaFiles[J].Detail <> '' then // Si define high or low.
            jImgUrl.AddPair('detail', MediaFile.Detail);

          JObjImg := TJSonObject.Create;
          JObjImg.AddPair('type', 'image_url');
          JObjImg.AddPair('image_url', jImgUrl);

          JContent.Add(JObjImg);
        End;

      End;

      jObj.AddPair('content', JContent);
    End
    Else
    Begin
      MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Audio], False);

      If (Length(MediaArr) > 0) then
      Begin
        For J := 0 to Length(MediaArr) - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
        Begin
          MediaFile := MediaArr[J];

          Base64 := MediaFile.Base64;
          Mime := MediaFile.MimeType;

          If MediaFile.FileCategory = TAiFileCategory.Tfc_Audio then
          Begin
            If MediaFile.IdAudio <> '' then // Si es una respuesta del modelo va esto
            Begin
              jAudio := TJSonObject.Create;
              jAudio.AddPair('id', MediaFile.IdAudio);
              jObj.AddPair('audio', jAudio);
            End
            Else // Si es un audio del usuario va esto
            Begin
              jAudio := TJSonObject.Create;
              jAudio.AddPair('data', MediaFile.Base64);
              jAudio.AddPair('format', StringReplace(MediaFile.MimeType, 'audio/', '', [rfReplaceAll]));

              JContent := TJSonArray.Create;
              JMsg := TJSonObject.Create;
              JMsg.AddPair('type', 'input_audio');
              JMsg.AddPair('input_audio', jAudio);
              JContent.Add(JMsg);

              jObj.AddPair('content', JContent);
            End;
          End;
        End;
      End
      Else
      Begin
        jObj.AddPair('content', Msg.FPrompt);
      End;
    End;

    If Msg.FTool_calls <> '' then
      jObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));

    Result.Add(jObj);
  End;
end;
}

{ TAIChatConfig }

constructor TAiChatConfig.Create(AOwner: TComponent);
begin
  inherited;
  FInitialInstructions := TStringList.Create;
  FInitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  Self.Max_tokens := 300;
  Self.N := 1;
  Self.Presence_penalty := 0;
  Self.Response_format := TAiOpenChatResponseFormat.tiaChatRfText;
  Self.Seed := 0;
  Self.Stop := '';
  Self.Temperature := 1;
  Self.Top_p := 1;
  Self.Tool_choice := '';
  Self.Tool_Active := False;
end;

destructor TAiChatConfig.Destroy;
begin
  FInitialInstructions.Free;
  inherited;
end;

procedure TAiChatConfig.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiChatConfig.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TAiChatConfig.SetInitialInstructions(const Value: TStrings);
begin
  FInitialInstructions.Text := Value.Text;
end;

procedure TAiChatConfig.SetMax_tokens(const Value: integer);
begin
  FMax_tokens := Value;
end;

procedure TAiChatConfig.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiChatConfig.SetN(const Value: integer);
begin
  FN := Value;
end;

procedure TAiChatConfig.SetPresence_penalty(const Value: Double);
begin
  FPresence_penalty := Value;
end;

procedure TAiChatConfig.SetResponse_format(const Value: TAiOpenChatResponseFormat);
begin
  FResponse_format := Value;
end;

procedure TAiChatConfig.SetSeed(const Value: integer);
begin
  FSeed := Value;
end;

procedure TAiChatConfig.SetStop(const Value: string);
begin
  FStop := Value;
end;

procedure TAiChatConfig.SetTemperature(const Value: Double);
begin
  FTemperature := Value;
end;

procedure TAiChatConfig.SetTool_Active(const Value: Boolean);
begin
  FTool_Active := Value;
end;

procedure TAiChatConfig.SetTool_choice(const Value: string);
begin
  FTool_choice := Value;
end;

procedure TAiChatConfig.SetTop_p(const Value: Double);
begin
  FTop_p := Value;
end;

procedure TAiChatConfig.SetUrlApi(const Value: String);
begin
  FUrlApi := Value;
end;

end.


