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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Chat.Ollama;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAi.Embeddings;

type

  TAiOllamaChat = Class(TAiChat)
  Private
    Fkeep_alive: String;
    procedure Setkeep_alive(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
  Public
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    //Function Run(aMsg: TAiChatMessage = Nil): String; Override;
    Function GetMessages: TJSonArray; Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
     property keep_alive : String read Fkeep_alive write Setkeep_alive;
  End;

  TAiOllamalEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float')
      : TAiEmbeddingData; Override;
    Procedure ParseEmbedding(JObj: TJSonObject); Override;
  end;

procedure Register;

implementation

Const
  GlAIUrl = 'http://localhost:11434/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOllamaChat, TAiOllamalEmbeddings]);
end;

class function TAiOllamaChat.GetDriverName: string;
Begin
  Result := 'Ollama';
End;

class procedure TAiOllamaChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@OLLAMA_API_KEY');
  Params.Add('Model=llama3');
  Params.Add('MaxTokens=4096');
  Params.Add('Temperature=0.7');
  Params.Add('TopP=1.0');
  Params.Add('TopK=5');
  Params.Add('BaseURL=http://localhost:11434/');
End;

procedure TAiOllamaChat.Setkeep_alive(const Value: String);
begin
  Fkeep_alive := Value;
end;

class function TAiOllamaChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiOllamaChat.Create(Sender);
End;

{ TAiOllamaChat }

constructor TAiOllamaChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@OLLAMA_API_KEY';
  Model := 'llama3:7b';
  Url := GlAIUrl;
  keep_alive := '1m';
end;

destructor TAiOllamaChat.Destroy;
begin

  inherited;
end;

{ procedure TAiOllamaChat.DoCallFunction(ToolCall: TAiToolsFunction);
  begin
  If Assigned(FOnCallToolFunction) then
  FOnCallToolFunction(Self, ToolCall)
  end;
}

{ //esta es la original, pero no funciona en la revisión del 2025 se modifica
  function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
  Var
  JObj, Msg, Arg: TJSonObject;
  JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  I: Integer;
  Nom, Valor: String;
  begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
  Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

  If Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
  Begin
  For JVal in JToolCalls do
  Begin
  JObj := TJSonObject(JVal);
  If JObj.GetValue<String>('type') = 'function' then
  Begin
  JObj := TJSonObject(JVal);
  Fun := TAiToolsFunction.Create;
  Fun.Id := JObj.GetValue<String>('id');
  Fun.Tipo := JObj.GetValue<String>('type');
  Fun.Name := JObj.GetValue<TJSonObject>('function').GetValue<String>('name');
  Fun.Arguments := JObj.GetValue<TJSonObject>('function').GetValue<String>('arguments');

  Arg := JObj.GetValue<TJSonObject>('function').GetValue<TJSonObject>('arguments');
  For I := 0 to Arg.Count - 1 do
  Begin
  Nom := Arg.Pairs[I].JsonString.Value;
  Valor := Arg.Pairs[I].JsonValue.Value;
  Fun.Params.Values[Nom] := Valor;
  End;

  Result.Add(Fun.Id, Fun);
  End;
  End;
  End;
  End;
  end;

}

// Se simplifica la recepción debido a que en la versión 2025 se eliminan parámetros
function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  JObj, Msg, Arg: TJSonObject;
  JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  FunId, I: Integer;
  Nom, Valor: String;
begin
  Result := TAiToolsFunctions.Create;

  FunId := 0;
  For JVal1 in jChoices do
  Begin
    Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

    If Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        JObj := TJSonObject(JVal);
        Fun := TAiToolsFunction.Create;
        Fun.Id := FunId.ToString;
        Fun.Tipo := 'function';
        Fun.Name := JObj.GetValue<TJSonObject>('function').GetValue<String>('name');
        Fun.Arguments := JObj.GetValue<TJSonObject>('function').GetValue<TJSonObject>('arguments').ToString;

        Arg := JObj.GetValue<TJSonObject>('function').GetValue<TJSonObject>('arguments');
        For I := 0 to Arg.Count - 1 do
        Begin
          Nom := Arg.Pairs[I].JsonString.Value;
          Valor := Arg.Pairs[I].JsonValue.Value;
          Fun.Params.Values[Nom] := Valor;
        End;

        Result.Add(Fun.Id, Fun);
        Inc(FunId);
      End;
    End;
  End;
end;

function TAiOllamaChat.GetMessages: TJSonArray;
Var
  I, J: Integer;
  Msg: TAiChatMessage;
  JObj: TJSonObject;
  jImages: TJSonArray;

  Base64: String;
  MediaArr: TAiMediaFilesArray;
  Mime: String;
begin
  Result := TJSonArray.Create;

  For I := 0 to Messages.Count - 1 do
  Begin
    Msg := Messages.Items[I];
    JObj := TJSonObject.Create;

    If Msg.TollCallId <> '' then
      JObj.AddPair('tool_call_id', Msg.TollCallId);

    If Msg.FunctionName <> '' then
      JObj.AddPair('name', Msg.FunctionName);

    JObj.AddPair('role', Msg.Role);
    JObj.AddPair('content', Msg.Prompt);

    MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

    If (Length(MediaArr) > 0) then
    Begin

      jImages := TJSonArray.Create;
      JObj.AddPair('images', jImages);

      For J := 0 to Msg.MediaFiles.Count - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
      Begin
        Base64 := Msg.MediaFiles[J].Base64;
        Mime := Msg.MediaFiles[J].MimeType;

        jImages.Add(Msg.MediaFiles[J].Base64);
      End;
    End;


    // Solo crea una lista de imagenes y siempre en base64
    { If (Msg.VisionUrls.Count > 0) or (Msg.VisionBase64.Count > 0) then
      Begin
      jImages := TJSonArray.Create;

      If Msg.VisionUrls.Count > 0 then
      Begin
      For J := 0 to Msg.VisionUrls.Count - 1 do
      Begin
      jImages.Add(Msg.VisionUrls[J]);
      End;
      End;

      If Msg.VisionBase64.Count > 0 then
      Begin
      For J := 0 to Msg.VisionBase64.Count - 1 do
      Begin
      jImages.Add(Msg.VisionBase64[J]);
      End;
      End;
      JObj.AddPair('images', jImages);
      End;
    }

    If Msg.Tool_calls <> '' then
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.Tool_calls)));

    Result.Add(JObj);
  End;
end;

class function TAiOllamaChat.GetModels(aApiKey, aUrl: String): TStringList;
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
    EndPointUrl := GlAIUrl;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'api/tags';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If jRes.TryGetValue<TJSonArray>('models', JArr) then
      Begin
        For JVal in JArr do
        Begin
          sModel := JVal.GetValue<String>('name');
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

function TAiOllamaChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'llama3.2:7b';

  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;
  FClient.ResponseTimeout := 1000 * 60 * 5;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    AJSONObject.AddPair('keep_alive', keep_alive);

    If Tool_Active and (Trim(Tools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(FTools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

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

    If LAsincronico or (FResponse_format = tiaChatRfJson) then
      AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'json_object'));
    // Else AJSONObject.AddPair('response_format', TJSonObject.Create.AddPair('type', 'text'));

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

    Result := UTF8ToString(AJSONObject.ToJSon);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiOllamaChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  JObj: TJSonObject;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'api/chat';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;
{$IFDEF APIDEBUG}
    St.SaveToFile('c:\temp\peticion.txt');
    St.Position := 0;
{$ENDIF}

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\respuesta.txt');
    FResponse.Position := 0;
{$ENDIF}

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(JObj, ResMsg);
          Result := FLastContent;

        Finally
          FreeAndNil(JObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free;
    // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  JObj, Delta: TJSonObject;
  sJson: String;
  Done: Boolean;
  Msg: TAiChatMessage;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta: String;

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
    sJson :=  UTF8Encode(FResponse.DataString);
    FResponse.Clear;
    FTmpResponseText := FTmpResponseText + sJson;

    If sJson <> '' then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

      If Assigned(JObj) then
      Begin
        If JObj.TryGetValue<Boolean>('done', Done) then
        Begin
          // Cuando termina de recibir el mensaje
          If Done = True then
          Begin
            Model := JObj.GetValue('model').Value;
            aPrompt_tokens := JObj.GetValue<Integer>('prompt_eval_count');
            aCompletion_tokens := JObj.GetValue<Integer>('eval_count');
            aTotal_tokens := aPrompt_tokens + aCompletion_tokens;

            // Actualiza los valores del componente
            Prompt_tokens := Prompt_tokens + aPrompt_tokens;
            Completion_tokens := Completion_tokens + aCompletion_tokens;
            Total_tokens := Total_tokens + aTotal_tokens;

            If JObj.TryGetValue<TJSonObject>('message', Delta) then
            Begin
              Respuesta := Delta.GetValue<String>('content');
              Role := Delta.GetValue<String>('role');
              FLastContent := FLastContent + Respuesta;

              Msg := TAiChatMessage.Create(FLastContent, Role);
              Msg.Prompt := FLastContent;
              // Msg.Tool_calls := sToolCalls;
              Msg.Prompt_tokens := aPrompt_tokens;
              Msg.Completion_tokens := aCompletion_tokens;
              Msg.Total_tokens := aTotal_tokens;
              Msg.Id := FMessages.Count + 1;
              FMessages.Add(Msg);
              FBusy := False;

              If Assigned(FOnReceiveDataEnd) then
                FOnReceiveDataEnd(Self, Msg, Nil, Role, FLastContent);

              // If Delta.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
              // sToolCalls := JToolCalls.Format;
            End;

          End
          Else // Todavía no ha terminado el mensaje
          Begin

            If JObj.TryGetValue<TJSonObject>('message', Delta) then
            Begin
              Respuesta := Delta.GetValue<String>('content');
              Role := Delta.GetValue<String>('role');
              FLastContent := FLastContent + Respuesta;

              Respuesta := StringReplace(Respuesta, #$A, sLineBreak, [rfReplaceAll]);
              If Assigned(FOnReceiveDataEvent) then
                FOnReceiveDataEvent(Self, Nil, JObj, Role, Respuesta);

            End;

          End;
        End;
      End;
    End;

  Except
    On E: Exception do
    begin
      LastError := 'El json "' + sJson + '" no es válido';
    end;

  End;

end;

procedure TAiOllamaChat.ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage);
Var
  choices, JToolCalls: TJSonArray;
  jMessage: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta: String;
  ToolMsg, AskMsg : TAiChatMessage;
  //Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave, sToolCalls: String;

begin
  AskMsg := GetLastMessage; // Obtiene el mensaje de la solicitud

  // Id := JObj.GetValue('id').Value;
  // IdObject := JObj.GetValue('object').Value;
  // IdCreate := JObj.GetValue('created').GetValue<String>;
  Model := JObj.GetValue('model').Value;
  aPrompt_tokens := JObj.GetValue<Integer>('prompt_eval_count');
  aCompletion_tokens := JObj.GetValue<Integer>('eval_count');
  aTotal_tokens := aPrompt_tokens + aCompletion_tokens;

  If JObj.TryGetValue<TJSonObject>('message', jMessage) then
  Begin
    Respuesta := jMessage.GetValue<String>('content').Trim + sLineBreak;
    Role := jMessage.GetValue<String>('role');

    If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
      sToolCalls := JToolCalls.Format;
  End;

  ///Msg := TAiChatMessage.Create(Respuesta, Role);

  DoProcessResponse(GetLastMessage, ResMsg, Respuesta);

  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  ResMsg.Prompt := Trim(ResMsg.Prompt+sLineBreak+Respuesta);
  ResMsg.Tool_calls := sToolCalls;
  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
  //ResMsg.Id := FMessages.Count + 1;
  //ResFMessages.Add(Msg);

  // If Assigned(FOnAddMessage) then
  // FOnAddMessage(Self, jObj, Role, Respuesta);

  choices := TJSonArray.Create;
  choices.Add(JObj);
  LFunciones := ExtractToolCallFromJson(choices);
  choices.Remove(0);
  choices.Free;

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
        ToolCall.ResMsg := ResMsg;
        ToolCall.AskMsg := AskMsg;

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
        ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        ToolMsg.Id := FMessages.Count + 1;
        FMessages.Add(ToolMsg);
      End;

      Self.Run(Nil, ResMsg);

    End
    Else
    Begin
      FBusy := False;
      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, JObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

{ TAiOlamalEmbeddings }

constructor TAiOllamalEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@OLLAMA_API_KEY';
  Url := GlAIUrl;
  FDimensions := 1024;
  FModel := 'snowflake-arctic-embed';

end;

{ TOllEmbeddings }

{ modelos disponibles en Ollama a mayo 2024 Library https://ollama.com/library
  Model := 'mxbai-embed-large'; //Vector[1024]
  Model := 'nomic-embed-text'; // Vector[768]
  Model := 'all-minilm';      //Vector[384]
  Model := 'snowflake-arctic-embed'; //Vector[1024]    //Esta es la mejor versión a mayo/2024

  Url para llamado http://IPOLLAMASERVER:11434/
}

function TAiOllamalEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String)
  : TAiEmbeddingData;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  If aModel = '' then
    aModel := FModel;

  If aDimensions <= 0 then
    aDimensions := FDimensions;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'api/embeddings';
  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('prompt', aInput);
    JObj.AddPair('model', aModel);
    JObj.AddPair('user', aUser);
    JObj.AddPair('dimensions', aDimensions);
    JObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    // Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
      Result := Self.Data;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

destructor TAiOllamalEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOllamalEmbeddings.ParseEmbedding(JObj: TJSonObject);
Var
  JArr: TJSonArray;
  J: Integer;
  Valor: Double;
begin
  // A diferencia de openAi el embedding es uno solo y no un arreglo

  JArr := JObj.GetValue<TJSonArray>('embedding');
  J := JArr.Count;
  SetLength(FData, J);

  // FillChar(FData, Length(FData) * SizeOf(Double), 0);

  For J := 0 to JArr.Count - 1 do
  Begin
    Valor := JArr.Items[J].GetValue<Double>;
    FData[J] := Valor;
  End;

  // FData := Emb;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);

end.
