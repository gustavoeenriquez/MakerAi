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
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAi.Embeddings, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings.Core, uMakerAi.Chat.Messages;

type

  TAiOllamaChat = Class(TAiChat)
  Private
    Fkeep_alive: String;
    FTmpToolCallsStr: string;
    procedure Setkeep_alive(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InitChatCompletions: String; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Procedure ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
  Public
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function GetMessages: TJSonArray; Override;

    // ----- FUNCIONES DE GESTI�N DE MODELOS  -----------
    procedure PullModel(const aModelName: string);
    procedure CreateModel(const aNewModelName, aModelfileContent: string);
    function ShowModelInfo(const aModelName: string): TJSonObject;
    procedure CopyModel(const aSourceModel, aDestinationModel: string);
    procedure DeleteModel(const aModelName: string);

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

  Published
    property keep_alive: String read Fkeep_alive write Setkeep_alive;
  End;

  TAiOllamaEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(JObj: TJSonObject); Override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  end;

procedure Register;

implementation

Const
  GlAIUrl = 'http://localhost:11434/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOllamaChat, TAiOllamaEmbeddings]);
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
  Params.Add('URL=http://localhost:11434/');
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
  Model := 'gpt-oss:20b';
  Url := GlAIUrl;
  keep_alive := '1m';
end;

destructor TAiOllamaChat.Destroy;
begin

  inherited;
end;

function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
var
  LResponseObj, LMessageObj, LToolCallObj, LFunctionObj, LArgumentsObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LToolCall: TAiToolsFunction;
  LItem: TJSONValue;
  LPair: TJSONPair;
begin
  Result := TAiToolsFunctions.Create;

  if (jChoices = nil) or (jChoices.Count = 0) then
    Exit;

  if not(jChoices.Items[0] is TJSonObject) then
    Exit;

  LResponseObj := jChoices.Items[0] as TJSonObject;

  if not LResponseObj.TryGetValue<TJSonObject>('message', LMessageObj) then
    Exit;

  if not LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then
    Exit;

  for LItem in LToolCallsArray do
  begin
    if not(LItem is TJSonObject) then
      Continue;

    LToolCallObj := LItem as TJSonObject;

    if LToolCallObj.TryGetValue<TJSonObject>('function', LFunctionObj) then
    begin
      LToolCall := TAiToolsFunction.Create;
      try
        // Ollama ahora s� incluye un 'id', pero lo generamos como fallback por si acaso.
        LToolCall.Id := LToolCallObj.GetValue<string>('id', 'call_' + TGuid.NewGuid.ToString);
        LToolCall.Name := LFunctionObj.GetValue<string>('name', '');
        LToolCall.Tipo := 'function';

        if LFunctionObj.TryGetValue<TJSonObject>('arguments', LArgumentsObj) then
        begin
          LToolCall.Arguments := LArgumentsObj.Format;
          for LPair in LArgumentsObj do
          begin
            LToolCall.Params.Values[LPair.JsonString.Value] := LPair.JsonValue.Value;
          end;
        end
        else
        begin
          LToolCall.Arguments := '{}';
        end;

        Result.Add(LToolCall.Id, LToolCall);
      except
        LToolCall.Free;
        raise;
      end;
    end;
  end;
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

    If Msg.ToolCallId <> '' then
      JObj.AddPair('tool_call_id', Msg.ToolCallId);

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

    If Msg.Tool_calls <> '' then

{$IF CompilerVersion < 35}
      JObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(Msg.Tool_calls));
{$ELSE}
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.Tool_calls)));
{$ENDIF}
    Result.Add(JObj);
  End;
end;

class function TAiOllamaChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSONValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlAIUrl;

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
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
  AJSONObject, jOptions: TJSonObject;
  JArr, JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LModel: String;
begin
  // 1. Configuraci�n b�sica y Modelo
  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  If LModel = '' then
    LModel := 'gpt-oss:20b'; // Fallback seguro

  // Configuramos el cliente HTTP seg�n la propiedad del componente
  FClient.Asynchronous := Self.Asynchronous;

  // Aumentamos timeout por defecto ya que los modelos locales pueden tardar en cargar
  if FClient.ResponseTimeout < 60000 then
    FClient.ResponseTimeout := 1000 * 60 * 5; // 5 minutos por defecto

  AJSONObject := TJSonObject.Create;
  jOptions := TJSonObject.Create; // Objeto para par�metros avanzados
  Lista := TStringList.Create;

  Try
    // --- PAR�METROS RA�Z ---
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('messages', GetMessages); // Usa GetMessages (revisaremos este despu�s)

    // Respetamos la configuraci�n as�ncrona (True/False)
    AJSONObject.AddPair('stream', TJSONBool.Create(Self.Asynchronous));

    if Fkeep_alive <> '' then
      AJSONObject.AddPair('keep_alive', Fkeep_alive);

    // 1. Structured Outputs (JSON Schema)
    if FResponse_format = tiaChatRfJsonSchema then
    begin
      if JsonSchema.Text <> '' then
      begin
        try
          // Ollama espera el esquema DIRECTAMENTE en el par�metro "format".
          // No requiere wrappers como "json_schema" o "schema".
          Var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);
          var
          JSchema := TJSonObject.ParseJSONValue(sShema);

          if Assigned(JSchema) then
          begin
            if JSchema is TJSonObject then
              AJSONObject.AddPair('format', JSchema as TJSonObject)
            else
              JSchema.Free; // Si no es un objeto v�lido, limpiar
          end;
        except
          // Manejo silencioso de errores de parseo, se enviar� sin formato o ignorado
        end;
      end;
    end
    // 2. JSON Mode (Simple)
    else if (FResponse_format = tiaChatRfJson) then
    begin
      AJSONObject.AddPair('format', 'json');
    end;

    // --- MANEJO DE TOOLS ---
    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin
{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Assigned(JArr) then
        AJSONObject.AddPair('tools', JArr);
    End;

    // --- PAR�METROS "OPTIONS" (Diferencia clave con OpenAI) ---
    // Ollama requiere encapsular estos par�metros dentro de 'options'

    if Temperature > 0 then
      jOptions.AddPair('temperature', TJSONNumber.Create(Temperature));

    // OJO: Ollama usa 'num_predict' en lugar de 'max_tokens'
    if Max_tokens > 0 then
      jOptions.AddPair('num_predict', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      jOptions.AddPair('top_p', TJSONNumber.Create(Top_p));

    If Frequency_penalty <> 0 then
      jOptions.AddPair('frequency_penalty', TJSONNumber.Create(Frequency_penalty));

    If Presence_penalty <> 0 then
      jOptions.AddPair('presence_penalty', TJSONNumber.Create(Presence_penalty));

    If Seed > 0 then
      jOptions.AddPair('seed', TJSONNumber.Create(Seed));

    // Manejo de Stop Sequences
    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      jOptions.AddPair('stop', JStop);
    End;

    // Agregamos el objeto options al JSON principal
    if jOptions.Count > 0 then
      AJSONObject.AddPair('options', jOptions)
    else
      jOptions.Free; // Si no se a�adi� al padre, hay que liberarlo

    // Generaci�n del String final
    Result := AJSONObject.ToJSON;

  Finally
    AJSONObject.Free;
    // jOptions se libera autom�ticamente si fue a�adido a AJSONObject
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

  DoStateChange(acsConnecting, 'Sending request...');

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'api/chat';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;

    LogDebug('-Peticion-');
    LogDebug(ABody);

    St.WriteString(ABody);
    St.Position := 0;
    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        Var
        S := Res.ContentAsString;

        LogDebug('-Resultado-');
        LogDebug(S);

        JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(JObj, ResMsg);
          Result := FLastContent;

        Finally
          // FreeAndNil(JObj);  //se comenta porque marca error porque ya ha sido eliminado previamente en alguna parte
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
    // Esto no funciona en multiarea, as� que se libera cuando no lo es.
  End;
end;

function IsCompleteJson(const S: string): Boolean;
var
  I, Balance: Integer;
  InString, Escape: Boolean;
begin
  Balance := 0;
  InString := False;
  Escape := False;

  for I := 1 to Length(S) do
  begin
    if Escape then
    begin
      Escape := False;
      Continue;
    end;

    case S[I] of
      '\':
        if InString then
          Escape := True;

      '"':
        InString := not InString;

      '{':
        if not InString then
          Inc(Balance);

      '}':
        if not InString then
          Dec(Balance);
    end;
  end;

  Result := (Balance = 0) and (not InString);
end;

procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LJsonObject, LMessageObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LChunkStr, LJsonLine, LRole, LContentPart, LThinkingPart: string;
  LDone: Boolean;
  LFinalMsg: TAiChatMessage;
  LStreamFinished: Boolean;

  procedure ProcessFinalJsonObject(AJson: TJSonObject);
  var
    LMsgObj: TJSonObject;
  begin
    LStreamFinished := True;
    FBusy := False;

    // Si capturamos tool_calls en fragmentos anteriores pero el JSON final no los tiene,
    // los re-inyectamos para que ParseChat los procese.
    if not FTmpToolCallsStr.IsEmpty then
    begin
      if not AJson.TryGetValue<TJSonObject>('message', LMsgObj) then
      begin
        LMsgObj := TJSonObject.Create;
        AJson.AddPair('message', LMsgObj);
      end;

      if LMsgObj.GetValue('tool_calls') = nil then
      begin
{$IF CompilerVersion < 35}
        LMsgObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(FTmpToolCallsStr));
{$ELSE}
        LMsgObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(FTmpToolCallsStr)));
{$ENDIF}
      end;
    end;

    LFinalMsg := TAiChatMessage.Create('', 'assistant');
    try
      ParseChat(AJson, LFinalMsg);
    except
      on E: Exception do
      begin
        LFinalMsg.Free;
        raise;
      end;
    end;

    // Limpieza de buffers para la siguiente petici�n
    FLastContent := '';
    FTmpResponseText := '';
    FTmpToolCallsStr := '';
  end;

begin
  if not Self.Asynchronous then
    Exit;

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallsStr := '';
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  try
    // 1. Acumular el nuevo chunk de datos
    LChunkStr := FResponse.DataString;
    FResponse.Clear;
    FTmpResponseText := FTmpResponseText + LChunkStr;
    LStreamFinished := False;

    // --- Bucle principal para procesar l�neas (Ollama env�a un JSON por l�nea) ---
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      LJsonLine := Copy(FTmpResponseText, 1, Pos(#10, FTmpResponseText) - 1);
      Delete(FTmpResponseText, 1, Pos(#10, FTmpResponseText));

      if LJsonLine.Trim.IsEmpty then
        Continue;

      LJsonObject := TJSonObject.ParseJSONValue(LJsonLine.Trim) as TJSonObject;
      if not Assigned(LJsonObject) then
        Continue;

      try
        LDone := LJsonObject.GetValue<Boolean>('done', False);

        if not LDone then // --- FRAGMENTO INTERMEDIO ---
        begin
          if LJsonObject.TryGetValue<TJSonObject>('message', LMessageObj) then
          begin
            LRole := LMessageObj.GetValue<string>('role', 'assistant');

            // 1. CAPTURAR TOOL CALLS (Fundamental para Ollama)
            if LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then
            begin
              // Guardamos el string de las herramientas. Ollama suele enviarlas
              // completas en un solo chunk, pero lo sobreescribimos por seguridad.
              FTmpToolCallsStr := LToolCallsArray.ToJSON;
              DoStateChange(acsToolCalling, 'Tool call detected in stream...');
            end;

            // 2. PROCESAR CONTENIDO
            if LMessageObj.TryGetValue<string>('content', LContentPart) and (LContentPart <> '') then
            begin
              LContentPart := StringReplace(LContentPart, #$A, sLineBreak, [rfReplaceAll]);
              FLastContent := FLastContent + LContentPart;
              if Assigned(FOnReceiveDataEvent) then
                FOnReceiveDataEvent(Self, nil, LJsonObject, LRole, LContentPart);
            end;

            // 3. PROCESAR RAZONAMIENTO (Thinking)
            if LMessageObj.TryGetValue<string>('thinking', LThinkingPart) and (LThinkingPart <> '') then
            begin
              LThinkingPart := StringReplace(LThinkingPart, #$A, sLineBreak, [rfReplaceAll]);
              if Assigned(OnReceiveThinking) then
                OnReceiveThinking(Self, nil, LJsonObject, LRole, LThinkingPart);
            end;
          end;
        end
        else // --- FRAGMENTO FINAL ---
        begin
          ProcessFinalJsonObject(LJsonObject);
          Break;
        end;
      finally
        LJsonObject.Free;
      end;
    end;

    // --- MANEJO DE FRAGMENTO FINAL (sin salto de l�nea) ---
    if (not LStreamFinished) and (FTmpResponseText.Trim <> '') then
    begin
      LJsonObject := TJSonObject.ParseJSONValue(FTmpResponseText.Trim) as TJSonObject;
      if Assigned(LJsonObject) then
      try
        if LJsonObject.GetValue<Boolean>('done', False) then
          ProcessFinalJsonObject(LJsonObject);
      finally
        LJsonObject.Free;
      end;
    end;

  except
    on E: Exception do
    begin
      FBusy := False;
      DoError('Error en OnInternalReceiveData (Ollama Stream): ' + E.Message, E);
    end;
  end;
end;


procedure TAiOllamaChat.ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage);
var
  LMessageObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LAskMsg: TAiChatMessage;
  LRole, LContent, LModel, LReasoning: string;
  LPromptTokens, LEvalTokens: Integer;

  // Tools
  LChoicesSimulado: TJSonArray;
  LFunciones: TAiToolsFunctions;
  LToolCall: TAiToolsFunction;
  LToolMsg, LHistoryToolMsg: TAiChatMessage;

  // Paralelismo
  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave: String;

  // Archivos
  Code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  CodeFile: TCodeFile;
  MF: TAiMediaFile;
  St: TStringStream;
begin
  if not Assigned(JObj) then
    Exit;

  // 1. EXTRAER METADATOS Y ESTAD�STICAS
  LModel := JObj.GetValue<string>('model', '');
  // Ollama usa nombres espec�ficos para los tokens
  LPromptTokens := JObj.GetValue<Integer>('prompt_eval_count', 0);
  LEvalTokens := JObj.GetValue<Integer>('eval_count', 0);

  // Actualizar contadores globales del componente
  Self.Prompt_tokens := Self.Prompt_tokens + LPromptTokens;
  Self.Completion_tokens := Self.Completion_tokens + LEvalTokens;
  Self.Total_tokens := Self.Total_tokens + LPromptTokens + LEvalTokens;

  // 2. VALIDAR LA EXISTENCIA DE "MESSAGE"
  if not JObj.TryGetValue<TJSonObject>('message', LMessageObj) then
  begin
    // Si no hay mensaje pero el JSON indica que termin�, disparamos el evento de fin
    if JObj.GetValue<Boolean>('done', False) then
    begin
      DoStateChange(acsFinished, 'Done');
      if Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, JObj, 'assistant', FLastContent);
    end;
    Exit;
  end;

  // 3. EXTRAER CONTENIDO DEL MENSAJE
  LRole := LMessageObj.GetValue<string>('role', 'assistant');
  LReasoning := LMessageObj.GetValue<string>('thinking', '');
  LContent := LMessageObj.GetValue<string>('content', '');

  // Sincronizar el contenido acumulado durante el streaming (FLastContent) con el ResMsg
  if (FLastContent <> '') then
    ResMsg.Content := FLastContent
  else
  begin
    ResMsg.Content := LContent;
    FLastContent := LContent;
  end;

  // Configurar propiedades del mensaje de respuesta
  ResMsg.Prompt := ResMsg.Content;
  ResMsg.Role := LRole;
  ResMsg.Model := LModel;
  ResMsg.ReasoningContent := LReasoning;
  ResMsg.Prompt_tokens := LPromptTokens;
  ResMsg.Completion_tokens := LEvalTokens;
  ResMsg.Total_tokens := LPromptTokens + LEvalTokens;

  LAskMsg := GetLastMessage;

  // 4. L�GICA DE LLAMADO A FUNCIONES (TOOLS)
  // Verificamos si Ollama nos ha devuelto tool_calls
  if LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) and (LToolCallsArray.Count > 0) then
  begin
    // --- CASO A: El modelo solicita ejecutar herramientas ---

    // A.1 Guardamos el mensaje del asistente (la petici�n de tool) en el historial
    LHistoryToolMsg := TAiChatMessage.Create(ResMsg.Content, LRole);
    LHistoryToolMsg.Tool_calls := LToolCallsArray.ToJSON;
    LHistoryToolMsg.Id := FMessages.Count + 1;
    FMessages.Add(LHistoryToolMsg);

    DoStateChange(acsToolExecuting, 'Executing local tools...');

    // A.2 Extraer las definiciones de las funciones
    LChoicesSimulado := TJSonArray.Create;
    LFunciones := nil;
    try
      // Simulamos una estructura compatible con ExtractToolCallFromJson
      LChoicesSimulado.Add(JObj.Clone as TJSonObject);
      LFunciones := ExtractToolCallFromJson(LChoicesSimulado);

      if (LFunciones <> nil) and (LFunciones.Count > 0) then
      begin
        // A.3 Ejecuci�n en paralelo de las funciones encontradas
        NumTasks := LFunciones.Count;
        SetLength(TaskList, NumTasks);
        I := 0;

        for Clave in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Clave];
          LToolCall.ResMsg := ResMsg;
          LToolCall.AskMsg := LAskMsg;

          TaskList[I] := TTask.Create(
            procedure
            var
              CapturaTool: TAiToolsFunction;
            begin
              CapturaTool := LToolCall;
              try
                DoCallFunction(CapturaTool);
              except
                on E: Exception do
                  TThread.Queue(nil,
                    procedure
                    begin
                      DoError('Function Execution Error: ' + CapturaTool.Name, E);
                    end);
              end;
            end);
          TaskList[I].Start;
          Inc(I);
        end;

        // Esperar a que todas las funciones terminen (bloqueo controlado)
        TTask.WaitForAll(TaskList);

        // A.4 A�adir los resultados de las funciones (role: tool) al historial
        for LToolCall in LFunciones.Values do
        begin
          LToolMsg := TAiChatMessage.Create(LToolCall.Response, 'tool', LToolCall.Id, LToolCall.Name);
          LToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(LToolMsg);
        end;

        // A.5 Re-ejecutar el Run para que el modelo analice los resultados de las herramientas
        // Limpiamos el ResMsg para recibir la respuesta final
        ResMsg.Content := '';
        ResMsg.Tool_calls := '';
        FLastContent := '';
        Self.Run(nil, ResMsg);
      end;
    finally
      LChoicesSimulado.Free;
      if Assigned(LFunciones) then
        LFunciones.Free;
    end;
  end
  else
  begin
    // --- CASO B: Respuesta de texto normal o final de cadena ---

    // B.1 Extracci�n autom�tica de bloques de c�digo si se solicita
    if (tfc_ExtracttextFile in NativeOutputFiles) and (ResMsg.Content <> '') then
    begin
      Code := TMarkdownCodeExtractor.Create;
      try
        CodeFiles := Code.ExtractCodeFiles(ResMsg.Content);
        for CodeFile in CodeFiles do
        begin
          St := TStringStream.Create(CodeFile.Code, TEncoding.UTF8);
          try
            St.Position := 0;
            MF := TAiMediaFile.Create;
            // Cargamos el c�digo extra�do como un archivo adjunto al mensaje
            MF.LoadFromStream('file.' + CodeFile.FileType, St as TMemoryStream);
            ResMsg.MediaFiles.Add(MF);
          finally
            St.Free;
          end;
        end;
      finally
        Code.Free;
      end;
    end;

    // B.2 Notificar el procesamiento de la respuesta (Hooks externos)
    DoProcessResponse(LAskMsg, ResMsg, FLastContent);

    // B.3 Gesti�n del historial (Evitar Doble Add en As�ncrono)
    // En s�ncrono, TAiChat.Run a�ade el mensaje al finalizar.
    // En as�ncrono, como el Run ya sali�, debemos a�adirlo aqu�.
    if Self.Asynchronous and (FMessages.IndexOf(ResMsg) = -1) then
    begin
      ResMsg.Id := FMessages.Count + 1;
      FMessages.Add(ResMsg);
    end;

    // B.4 Finalizaci�n y notificaci�n a la UI
    DoStateChange(acsFinished, 'Done');

    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, JObj, LRole, FLastContent);

    FBusy := False;
  end;
end;

// ----- FUNCIONES DE GESTI�N DE MODELOS  -----------

procedure TAiOllamaChat.CopyModel(const aSourceModel, aDestinationModel: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  LUrl := TPath.Combine(Self.Url, 'api/copy');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('source', aSourceModel);
    LJsonObject.AddPair('destination', aDestinationModel);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al copiar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.CreateModel(const aNewModelName, aModelfileContent: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
  LStatus: string;
  LCompleted, LTotal: Int64;
begin
  LUrl := TPath.Combine(Self.Url, 'api/create');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aNewModelName);
    LJsonObject.AddPair('modelfile', aModelfileContent);
    LJsonObject.AddPair('stream', TJSONBool.Create(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al crear el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValue<string>('status');
            LCompleted := 0;
            LTotal := 0;
            LStatusObj.TryGetValue<Int64>('completed', LCompleted);
            LStatusObj.TryGetValue<Int64>('total', LTotal);
            OnProgressEvent(Self, LStatus, LCompleted, LTotal);
          finally
            LStatusObj.Free;
          end;
      end;
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;
end;

procedure TAiOllamaChat.DeleteModel(const aModelName: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  LUrl := TPath.Combine(Self.Url, 'api/delete');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    // TNetHTTPClient.Delete necesita un TStream como body
    LResponse := FClient.Delete(LUrl, LBodyStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al eliminar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.PullModel(const aModelName: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
  LStatus: string;
  LCompleted, LTotal: Int64;
begin
  LUrl := TPath.Combine(Self.Url, 'api/pull');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LJsonObject.AddPair('stream', TJSONBool.Create(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    // Hacemos la llamada s�ncrona, pero Ollama devuelve el stream completo de una vez
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al descargar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      // Si el evento de progreso est� asignado, lo disparamos
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValue<string>('status');
            LCompleted := 0;
            LTotal := 0;
            // TryGetValue es m�s seguro si los campos no siempre est�n presentes
            LStatusObj.TryGetValue<Int64>('completed', LCompleted);
            LStatusObj.TryGetValue<Int64>('total', LTotal);
            OnProgressEvent(Self, LStatus, LCompleted, LTotal);
          finally
            LStatusObj.Free;
          end;
      end;
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;
end;

function TAiOllamaChat.ShowModelInfo(const aModelName: string): TJSonObject;
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  //Result := nil;
  LUrl := TPath.Combine(Self.Url, 'api/show');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream);

    if LResponse.StatusCode = 200 then
    begin
      // El llamador es responsable de liberar el TJSONObject devuelto
      Result := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
    end
    else
    begin
      raise Exception.CreateFmt('Error al obtener informaci�n del modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

{ TAiOlamalEmbeddings }

constructor TAiOllamaEmbeddings.Create(aOwner: TComponent);
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
  Model := 'snowflake-arctic-embed'; //Vector[1024]    //Esta es la mejor versi�n a mayo/2024

  Url para llamado http://IPOLLAMASERVER:11434/
}

function TAiOllamaEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
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

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
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

    St.WriteString(JObj.ToJSON);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
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

destructor TAiOllamaEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOllamaEmbeddings.ParseEmbedding(JObj: TJSonObject);
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

{ TAiOllamaEmbeddings - Factory class methods }

class function TAiOllamaEmbeddings.GetDriverName: string;
begin
  Result := 'Ollama';
end;

class function TAiOllamaEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiOllamaEmbeddings.Create(aOwner);
end;

class procedure TAiOllamaEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@OLLAMA_API_KEY';
  Params.Values['Url'] := GlAIUrl;
  Params.Values['Model'] := 'snowflake-arctic-embed';
  Params.Values['Dimensions'] := '1024';
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);
TAiEmbeddingFactory.Instance.RegisterDriver(TAiOllamaEmbeddings);

end.
