unit uMakerAi.Chat.OpenAi;

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
// 04/11/2024 - Se adiciona la propiedad TAiOpenChat.Modalities para el manejo de audio
// 04/11/2024 - Se adicionan la propiedades TAiOpenChat.voice y voice_format
// 04/11/2024 - se habilita la opción de recibir y generar audio utilizando TMediaFile
// 05/11/2024 - se adiciona la propiedad TAiOpenChat.Store

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Chat;

type

  //TAiModality = (Text, Audio);
  //TAiModilities = set of TAiModality;

  TAiOpenChat = class(TAiChat)
  Private
    //FModalities: TAiModilities;
    FVoice: String;
    Fvoice_format: String;
    FStore: Boolean;
    FParallel_ToolCalls: Boolean;
    FService_Tier: String;
    //procedure SetModalities(const Value: TAiModilities);
    procedure SetVoice(const Value: String);
    procedure Setvoice_format(const Value: String);
    procedure SetStore(const Value: Boolean);
    procedure SetParallel_ToolCalls(const Value: Boolean);
    procedure SetService_Tier(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String; Overload; Override;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Overload; Override;
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    Procedure DoCallFunction(ToolCall: TAiToolsFunction); Override;
    function GetTools: TStrings; Override;
    Function PrepareSystemMsg: String; Override; // Crea el primer mensaje del chat para system, para configurar el asistente
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function Run(aMsg: TAiChatMessage = Nil): String; Override;
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Overload; Override;
    Function GetModels: TStringList; Overload; Override;
    Function GetMessages: TJSonArray; Override;

  Published
    //Property Modalities: TAiModilities read FModalities write SetModalities;
    Property Voice: String read FVoice write SetVoice;
    Property voice_format: String read Fvoice_format write Setvoice_format;
    Property Store: Boolean read FStore write SetStore;
    Property Parallel_ToolCalls: Boolean read FParallel_ToolCalls write SetParallel_ToolCalls;
    Property Service_Tier: String read FService_Tier write SetService_Tier;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenChat, TAiChatConfig]);
end;

{ TAiChat }

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

function TAiOpenChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String; aFunctionName: String): String;
Var
  Msg: TAiChatMessage;
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
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

function TAiOpenChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
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
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiChatMessage.Create(aPrompt, aRole);
    Msg.Id := FMessages.Count + 1;
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

constructor TAiOpenChat.Create(Sender: TComponent);
begin
  inherited;
  Model := 'gpt4-o';
  N := 1;
  Response_format := TAiOpenChatResponseFormat.tiaChatRfText;
  Temperature := 1;
  User := 'user';
  InitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  Max_tokens := 300;
  Url := GlOpenAIUrl;
  Top_p := 1;
  ResponseTimeOut := 60000;
  FVoice := 'alloy';
  Fvoice_format := 'wav';
  FStore := False; // no almacene la información para modelos de distilación o evaluaciones
  FParallel_ToolCalls := True; // Por defecto realiza el llamado en paralelo, esto ahorra tiempo en las respuestas
  FService_Tier := 'auto'; // posibles valore auto y default  ver API documentación.
end;

destructor TAiOpenChat.Destroy;
begin
  inherited;
end;

procedure TAiOpenChat.DoCallFunction(ToolCall: TAiToolsFunction);
Var
  Funcion: TFunctionActionItem;
  Handle: Boolean;
begin
  If Assigned(AiFunctions) then // Si está asignado el componente, busca la función en el componente
    Funcion := AiFunctions.Functions.GetFunction(ToolCall.Name)
  Else // Si no está asignado el componente, lo busca directamente en las funciones locales
    Funcion := Functions.GetFunction(ToolCall.Name);

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

function TAiOpenChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg, jFunc, Arg: TJSonObject;
  JVal, JVal1: TJSonValue;
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

class function TAiOpenChat.GetModels(aApiKey, aUrl: String): TStringList;
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

function TAiOpenChat.GetModels: TStringList;
begin
  Result := GetModels(ApiKey, Url);
end;

function TAiOpenChat.GetTools: TStrings;
Var
  Funcs: TJSonArray;
begin
  Funcs := Functions.ToJSon;
  Try
    If Assigned(Funcs) and Tool_Active then // Si utiliza tools functions
    Begin
      If Assigned(AiFunctions) then // Si está asignado el componente lo obtiene del componente
      Begin
        FTools.Text := AiFunctions.GetTools;
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

function TAiOpenChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jAudio, jStrOptions: TJSonObject;
  JArr, jModalities: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'gpt-4o';

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

    If (LAsincronico = True) and (Stream_Usage = True) then
    Begin
      jStrOptions := TJSonObject.Create;
      jStrOptions.AddPair('include_usage', Stream_Usage);
      AJSONObject.AddPair('stream_options', jStrOptions);
    End;

    AJSONObject.AddPair('store', FStore);

    If Tool_Active and (Trim(Tools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(Tools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      AJSONObject.AddPair('parallel_tool_calls', FParallel_ToolCalls);

      If (Trim(Tool_choice) <> '') then
      Begin
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;

    End;

    AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
    AJSONObject.AddPair('model', Model);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));


    //Formato de salida puede generar texto y audio
    If NativeOutputFiles <> [] then
    Begin
      jModalities := TJSonArray.Create;
      If Tfc_Text in NativeOutputFiles then
        jModalities.Add('text');

      If Tfc_Audio in NativeOutputFiles then
        jModalities.Add('audio');

      AJSONObject.AddPair('modalities', jModalities);
    End;

    If Tfc_Audio in NativeOutputFiles  then
    Begin
      jAudio := TJSonObject.Create;
      jAudio.AddPair('voice', FVoice);
      jAudio.AddPair('format', Fvoice_format);
      AJSONObject.AddPair('audio', jAudio);
    End;

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

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If Logprobs = True then
    Begin
      If Logit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(Logit_bias));

      AJSONObject.AddPair('logprobs', TJSONBool.Create(Logprobs));

      If Top_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(Top_logprobs));
    End;

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    Result := UTF8ToString(AJSONObject.ToString);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

procedure AppendTextToFile(const FilePath, TextToAppend: string);
var
  LogFile: TextFile;
begin
  // Asignamos el archivo
  AssignFile(LogFile, FilePath);

  try
    // Abrimos el archivo en modo "append" para añadir al final
    if FileExists(FilePath) then
      Append(LogFile) // Abrimos en modo "append"
    else
      Rewrite(LogFile); // Creamos el archivo si no existe

    // Escribimos el texto al final del archivo
    WriteLn(LogFile, TextToAppend);
  finally
    // Cerramos el archivo
    CloseFile(LogFile);
  end;
end;

procedure TAiOpenChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
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

    // AppendTextToFile('C:\temp\logdata.txt', FResponse.DataString);

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
        Msg.Id := FMessages.Count + 1;
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

procedure TAiOpenChat.ParseChat(jObj: TJSonObject);
Var
  choices, JToolCalls: TJSonArray;
  JItem, jAudio: TJSonObject;
  JVal: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;
  Role, Respuesta, idAudio: String;
  AudioExpiresAt: integer;
  Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: integer;
  Clave, sToolCalls, sRes: String;
  AudioBase64: String;
  MediaFile: TAiMediaFile;

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

  Msg := TAiChatMessage.Create('', '');

  For JVal in choices do
  Begin
    JItem := TJSonObject(JVal);
    jMessage := JItem.GetValue<TJSonObject>('message');
    Role := jMessage.GetValue<String>('role');

    If jMessage.TryGetValue<String>('content', sRes) then
      Respuesta := Respuesta + sRes + sLineBreak;

    If jMessage.TryGetValue<TJSonObject>('audio', jAudio) then
    Begin
      jAudio.TryGetValue<String>('id', idAudio);

      jAudio.TryGetValue<String>('data', AudioBase64);

      If jAudio.TryGetValue<String>('transcript', sRes) then
        Respuesta := Respuesta + sRes + sLineBreak;

      jAudio.TryGetValue<integer>('expires_at', AudioExpiresAt);

      If (AudioBase64 <> '') then
      Begin
        MediaFile := TAiMediaFile.Create;
        MediaFile.LoadFromBase64('archivo.' + voice_format, AudioBase64);
        MediaFile.Transcription := sRes;
        MediaFile.idAudio := idAudio;
        Msg.AddMediaFile(MediaFile);
      End;
    End;

    If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
      sToolCalls := JToolCalls.Format;
  End;

  Respuesta := Trim(Respuesta);
  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  Msg.Prompt := Respuesta;
  Msg.Content := Respuesta;
  Msg.Role := Role;
  Msg.Tool_calls := sToolCalls;
  Msg.Prompt_tokens := aPrompt_tokens;
  Msg.Completion_tokens := aCompletion_tokens;
  Msg.Total_tokens := aTotal_tokens;
  Msg.Id := FMessages.Count + 1;

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
        Msg.Id := FMessages.Count + 1;
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

function TAiOpenChat.PrepareSystemMsg: String;
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

  MensajeInicial := InitialInstructions.Text + sLineBreak + S;

  JMemory := TJSonObject.Create;
  Try
    Try
      For I := 0 to Memory.Count - 1 do
      Begin
        Key := Memory.KeyNames[I];
        Val := Memory.Values[Key];
        JMemory.AddPair(Key, Val);
      End;
    Except
      ON E: Exception do
      Begin
        Raise Exception.Create('El formato de memoria debe ser Key=Value, no está bien configurado');
      End;
    End;

    If Assigned(OnInitChat) then // Da la oportunidad de inicializar el chat con parametros adicionales como la memoria
      OnInitChat(Self, 'system', MensajeInicial, JMemory);

    If Trim(MensajeInicial) <> '' then
      Result := MensajeInicial;

    If Length(Trim(JMemory.Format)) > 10 then
      Result := Result + sLineBreak + 'Para Recordar= ' + JMemory.Format;
  Finally
    JMemory.Free;
  End;
end;

function TAiOpenChat.Run(aMsg: TAiChatMessage = Nil): String;
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
  sUrl := Url + 'chat/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) or ((FMessages.Count mod 20) = 0) then
    Begin

      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiChatMessage.Create(MensajeInicial, 'system');
      Msg.Id := FMessages.Count + 1;

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

      aMsg.Id := FMessages.Count + 1;
      FMessages.Add(aMsg);
      FLastPrompt := aMsg.Prompt;

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, aMsg.Role, aMsg.Prompt);
    End;

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

     St.SaveToFile('c:\temp\peticion.txt');
     St.Position := 0;

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

{procedure TAiOpenChat.SetModalities(const Value: TAiModilities);
begin
  FModalities := Value;
end;
}

procedure TAiOpenChat.SetParallel_ToolCalls(const Value: Boolean);
begin
  FParallel_ToolCalls := Value;
end;

procedure TAiOpenChat.SetService_Tier(const Value: String);
begin
  FService_Tier := Value;
end;

procedure TAiOpenChat.SetStore(const Value: Boolean);
begin
  FStore := Value;
end;

procedure TAiOpenChat.SetVoice(const Value: String);
begin
  FVoice := Value;
end;

procedure TAiOpenChat.Setvoice_format(const Value: String);
begin
  Fvoice_format := Value;
end;

function TAiOpenChat.GetMessages: TJSonArray;
begin
  Result := FMessages.ToJSon;
end;

end.



