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


// Modelos con vision actualmente en Groq (Abr 2026):
//   meta-llama/llama-4-scout-17b-16e-instruct  (131K ctx, 8K output, vision + tools)
//   openai/gpt-oss-120b                        (131K ctx, 65K output, vision + reasoning)
// Limites de vision en Groq:
//   - Imagen maxima por URL: 20MB | por base64: 4MB
//   - Maximo 5 imagenes por request (llama-4-scout)

unit uMakerAi.Chat.Groq;

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
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core, uMakerAi.Embeddings.Core, uMakerAi.Chat.Messages;

Type
  // Este modelo de reasoning por ahora solo se ha detectado en Groq, as� que se implementa solo aqu�

  TAiReasoningFormat = (rfAuto, rfParsed, rfRaw, rfHidden);
  TAiReasoningEffort = (reAuto, reNone, reDefault);

  TAiGroqChat = Class(TAiChat)
  Private
    FReasoningFormat: TAiReasoningFormat;
    FReasoningEffort: TAiReasoningEffort;
    function  BuildChunkRequestCode(const AOriginalCode: string; AOffset, AEnd: Integer): string;
    function  CallCodeInterpreterForChunk(const ACode: string): string;
    function  FetchRemainingChunks(const AOriginalCode, APartialB64: string; AOffset: Integer): string;
  Protected
    Function InitChatCompletions: String; Override;
    Function InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
    procedure ProcessExecutedTools(const AExecutedToolsJSON: string; ResMsg: TAiChatMessage); override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

  TAiGroqEmbeddings = Class(TAiEmbeddings)
  Public
    // groq actualmente no maneja modelos de embeddings
    Function CreateEmbedding(Input, User: String; Dimensions: Integer = 1536; Model: String = 'Llama3-8b-8192'; EncodingFormat: String = 'float'): TAiEmbeddingData; Override;
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.groq.com/openai/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGroqChat]);
end;

{ TAiOllamaChat }

class function TAiGroqChat.GetDriverName: string;
Begin
  Result := 'Groq';
End;

class procedure TAiGroqChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GROQ_API_KEY');
  Params.Add('Model=llama-3.1-8b-instant');
  Params.Add('Max_Tokens=4096');
  Params.Add('URL=https://api.groq.com/openai/v1/');
End;

class function TAiGroqChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGroqChat.Create(Sender);
End;

constructor TAiGroqChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GROQ_API_KEY';
  Model := 'llama-3.1-8b-instant';
  Url := GlAIUrl;
  FReasoningFormat := rfAuto;
  FReasoningEffort := reAuto;
end;

destructor TAiGroqChat.Destroy;
begin

  inherited;
end;

function TAiGroqChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'llama-3.1-8b-instant';

  // Las funciones no trabajan en modo ascincrono
  // LAsincronico := Self.Asynchronous and (not Self.Tool_Active);
  LAsincronico := Self.Asynchronous;

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    if (ModelConfig.Format = 'Raw') and (Tool_Active or (Response_format = tiaChatRfJson) or (Response_format = tiaChatRfJsonSchema)) then
    begin
      Raise Exception.Create('Groq Error: ReasoningFormat no puede ser "raw" cuando se usan Tools o JSON mode. Use "parsed" o "hidden".');
    end;

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin
{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools est�n mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin
{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(Tool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tool_choice', jToolChoice);
      End;
    End;

    // Groq compound (Tool_Active=False + cap_CodeInterpreter): el modelo maneja herramientas
    // internamente, pero sin tool_choice explícito Groq rechaza las llamadas internas con
    // "Tool choice is none, but model called a tool". Necesita tool_choice:"auto" sin tools array.
    if (cap_CodeInterpreter in ModelConfig.ModelCaps) and (not Tool_Active) then
    begin
      if not Assigned(AJSONObject.GetValue('tool_choice')) then
        AJSONObject.AddPair('tool_choice', 'auto');
    end;

    // Groq explicit code_interpreter tool — solo para modelos que requieren declararlo
    // (gpt-oss-20b, etc.). Los modelos compound lo tienen nativo y rechazan este campo.
    // Criterio: inyectar solo si Tool_Active=True (compound usa Tool_Active=False).
    if (cap_CodeInterpreter in ModelConfig.ModelCaps) and Tool_Active then
    begin
      var JExistingTools := AJSONObject.GetValue('tools') as TJSonArray;
      if Assigned(JExistingTools) then
      begin
        var JCodeTool := TJSonObject.Create;
        JCodeTool.AddPair('type', 'code_interpreter');
        JExistingTools.Add(JCodeTool);
      end
      else
      begin
        var JToolsArr := TJSonArray.Create;
        var JCodeTool := TJSonObject.Create;
        JCodeTool.AddPair('type', 'code_interpreter');
        JToolsArr.Add(JCodeTool);
        AJSONObject.AddPair('tools', JToolsArr);
      end;
      // Groq requires tool_choice="auto" when code_interpreter is present.
      // The function-tools block above uses a TJSONObject cast that silently fails
      // for the string value "auto", so tool_choice may not have been added yet —
      // check before adding to avoid duplicate keys.
      if not Assigned(AJSONObject.GetValue('tool_choice')) then
        AJSONObject.AddPair('tool_choice', 'auto');
    end;

    AJSONObject.AddPair('messages', GetMessages);

    AJSONObject.AddPair('model', LModel);

    // Reasoning: guardado estrictamente por familia de modelos para evitar param leak al cambiar modelo
    // - openai/gpt-oss-*: include_reasoning:true + reasoning_effort (low/medium/high)
    // - qwen/*:           reasoning_format (parsed/raw/hidden) + reasoning_effort (default/none)
    // - otros modelos:    ninguno de estos parametros (causarian error 422)
    if LModel.StartsWith('openai/gpt-oss') then
    begin
      if ModelConfig.ThinkingLevel <> tlDefault then
      begin
        AJSONObject.AddPair('include_reasoning', TJSONBool.Create(True));
        case ModelConfig.ThinkingLevel of
          tlLow:    AJSONObject.AddPair('reasoning_effort', 'low');
          tlMedium: AJSONObject.AddPair('reasoning_effort', 'medium');
          tlHigh:   AJSONObject.AddPair('reasoning_effort', 'high');
        end;
      end;
    end
    else if LModel.StartsWith('qwen/') then
    begin
      if ModelConfig.ThinkingLevel <> tlDefault then
      begin
        // Thinking activo: reasoning_format parsed + reasoning_effort=default
        var LFormat: String := ModelConfig.Format;
        if LFormat = '' then
          LFormat := 'parsed'; // default: reasoning en campo separado message.reasoning
        AJSONObject.AddPair('reasoning_format', LFormat);
        AJSONObject.AddPair('reasoning_effort', 'default');
      end
      else if ModelConfig.Format <> '' then
      begin
        // Format explicitamente seteado sin ThinkingLevel (ej: 'hidden' para non-thinking)
        AJSONObject.AddPair('reasoning_format', ModelConfig.Format);
      end;
    end;
    // Otros modelos (llama, mistral, kimi, etc.): sin params de reasoning

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));

    // Groq docs: reasoning models usan max_completion_tokens (incluye reasoning tokens en el budget)
    // Only include when > 0; omitting it lets Groq use the full remaining context automatically.
    if Max_tokens > 0 then
    begin
      if LModel.StartsWith('openai/gpt-oss') or LModel.StartsWith('qwen/') then
        AJSONObject.AddPair('max_completion_tokens', TJSONNumber.Create(Max_tokens))
      else
        AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));
    end;

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    // 1. JSON Schema (Structured Outputs)
    if (FResponse_format = tiaChatRfJsonSchema) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'json_schema');

      if JsonSchema.Text <> '' then
      begin
        Var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);

        var
        JInnerSchema := TJSonObject.ParseJSONValue(sShema) as TJSonObject;
        if Assigned(JInnerSchema) then
        begin
          // Wrapper para Groq (Estilo OpenAI Classic)
          var
          JSchemaWrapper := TJSonObject.Create;

          // 'name' es OBLIGATORIO en esta estructura
          JSchemaWrapper.AddPair('name', 'structured_response');

          // El esquema va dentro de 'schema'
          JSchemaWrapper.AddPair('schema', JInnerSchema);

          // NOTA: No enviamos "strict": true por defecto para maximizar compatibilidad
          // con modelos Groq que no soportan constrained decoding completo a�n.

          JResponseFormat.AddPair('json_schema', JSchemaWrapper);
        end;
      end;

      AJSONObject.AddPair('response_format', JResponseFormat);
    end

    // 2. JSON Mode (Simple)
    else if (FResponse_format = tiaChatRfJson) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'json_object');
      AJSONObject.AddPair('response_format', JResponseFormat);
    end

    // 3. Text Mode (Solo si se especifica expl�citamente, o dejar por defecto)
    else if (FResponse_format = tiaChatRfText) then
    begin
      var
      JResponseFormat := TJSonObject.Create;
      JResponseFormat.AddPair('type', 'text');
      AJSONObject.AddPair('response_format', JResponseFormat);
    end;

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    // NOTA: Groq no soporta logprobs, logit_bias ni top_logprobs en chat completions (error 400)

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSon));
    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

{ TAiGroqEmbeddings }

function TAiGroqEmbeddings.CreateEmbedding(Input, User: String; Dimensions: Integer; Model, EncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // OJO OJO OJO OJO
  Raise Exception.Create('Actualmente Groq no maneja modelos de embeddings');

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 34}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  Try
    jObj.AddPair('input', Input);
    jObj.AddPair('model', Model);
    jObj.AddPair('user', User);
    jObj.AddPair('encoding_format', EncodingFormat);

    // St.WriteString(UTF8Encode(jObj.Format));
    St.WriteString(jObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

    if not Assigned(Res) then
      raise Exception.CreateFmt('Connection failed: no response from %s', [sUrl]);

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(jObj);
      Result := Self.FData;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    jObj.Free;
  End;
end;

function TAiGroqChat.InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList;
  I: Integer;
  LModel: String;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripci?n.');

  sUrl := Url + 'audio/transcriptions';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 34}
  Client.SynchronizeEvents := False;
{$ENDIF}
  LResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  Granularities := TStringList.Create;
  LTempStream := TMemoryStream.Create;
  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

{$IF CompilerVersion >= 36}
    Body.AddStream('file', LTempStream, False, aMediaFile.FileName, aMediaFile.MimeType);
{$ELSE}
    Body.AddStream('file', LTempStream, aMediaFile.FileName, aMediaFile.MimeType);
{$ENDIF}
    Body.AddField('model', LModel);

    if not AskMsg.Prompt.IsEmpty then
      Body.AddField('prompt', AskMsg.Prompt);

    if not TranscriptionParams.ResponseFormat.IsEmpty then
      Body.AddField('response_format', TranscriptionParams.ResponseFormat)
    else
      Body.AddField('response_format', 'json');

    if not TranscriptionParams.Language.IsEmpty then
      Body.AddField('language', TranscriptionParams.Language);

    if Self.Temperature > 0 then
      Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

    if not TranscriptionParams.TimestampGranularities.IsEmpty then
    begin
      Granularities.CommaText := TranscriptionParams.TimestampGranularities;
      for I := 0 to Granularities.Count - 1 do
        Body.AddField('timestamp_granularities[]', Trim(Granularities[I]));
    end;

    Res := Client.Post(sUrl, Body, LResponseStream, Headers);

    if Res.StatusCode = 200 then
    begin
      LResponseObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;
      if not Assigned(LResponseObj) then
        LResponseObj := TJSonObject.Create(TJSonPair.Create('text', Res.ContentAsString));
      try
        ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
      finally
        LResponseObj.Free;
      end;
      Result := ResMsg.Prompt;
    end
    else
      raise Exception.CreateFmt('Error en la transcripci?n: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  finally
    Body.Free;
    Client.Free;
    LResponseStream.Free;
    LTempStream.Free;
    Granularities.Free;
  end;
end;

{ TAiGroqChat — chunking para code interpreter }

// Modifica el código Python original para imprimir solo el slice [AOffset:AEnd] del b64.
// Busca el bloque FILE_B64_BEGIN / print(<expr>) / FILE_B64_END y lo reemplaza.
function TAiGroqChat.BuildChunkRequestCode(const AOriginalCode: string; AOffset, AEnd: Integer): string;
var
  Lines: TStringList;
  I, IBeginLine, IPrintLine, IEndLine: Integer;
  B64Expr, LLine, LTrim: string;
  SB: TStringList;
begin
  IBeginLine := -1; IPrintLine := -1; IEndLine := -1;
  B64Expr    := '';
  Lines := TStringList.Create;
  SB    := TStringList.Create;
  try
    Lines.Text := AOriginalCode;
    for I := 0 to Lines.Count - 1 do
    begin
      LTrim := Trim(Lines[I]);
      if (IBeginLine < 0) and (Pos('FILE_B64_BEGIN', LTrim) > 0) then
        IBeginLine := I
      else if (IBeginLine >= 0) and (IPrintLine < 0) and LTrim.StartsWith('print(') then
      begin
        IPrintLine := I;
        // extraer expresión: print(EXPR) → EXPR
        LLine := LTrim;
        if LLine.EndsWith(')') then
          B64Expr := Copy(LLine, 7, Length(LLine) - 7);  // quitar 'print(' y ')'
      end
      else if (IBeginLine >= 0) and (Pos('FILE_B64_END', LTrim) > 0) then
      begin
        IEndLine := I;
        Break;
      end;
    end;

    if (IBeginLine < 0) or (B64Expr = '') or (IEndLine < 0) then
    begin
      // Patrón no encontrado: agregar chunk al final del código original
      Result := AOriginalCode + #10 +
        '_ci_b64 = globals().get(''_ci_b64'', '''')' + #10 +
        'print(''CHUNK_START'')' + #10 +
        Format('print(_ci_b64[%d:%d])', [AOffset, AEnd]) + #10 +
        'print(''CHUNK_END'')';
      Exit;
    end;

    // Líneas anteriores al bloque FILE_B64
    for I := 0 to IBeginLine - 1 do
      SB.Add(Lines[I]);

    // Si la expresión usa .read(), insertar seek(0) con la variable de buffer detectada
    if Pos('.read()', B64Expr) > 0 then
    begin
      // Detectar variable del buffer: base64.b64encode(buf.read()) → buf
      var LBufVar := '';
      var IRead := Pos('.read()', B64Expr);
      if IRead > 1 then
      begin
        var ITmp := IRead - 1;
        while (ITmp > 0) and CharInSet(B64Expr[ITmp], ['a'..'z','A'..'Z','0'..'9','_']) do
          Dec(ITmp);
        LBufVar := Copy(B64Expr, ITmp + 1, IRead - ITmp - 1);
      end;
      if LBufVar <> '' then
        SB.Add(LBufVar + '.seek(0)');
    end;

    // Bloque de chunk
    SB.Add('_ci_b64 = ' + B64Expr);
    SB.Add('print(''CHUNK_START'')');
    SB.Add(Format('print(_ci_b64[%d:%d])', [AOffset, AEnd]));
    SB.Add('print(''CHUNK_END'')');

    // Líneas posteriores al bloque FILE_B64_END
    for I := IEndLine + 1 to Lines.Count - 1 do
      SB.Add(Lines[I]);

    Result := SB.Text;
  finally
    Lines.Free;
    SB.Free;
  end;
end;

// Hace una llamada HTTP directa a Groq con code_interpreter y devuelve el tool output concatenado.
function TAiGroqChat.CallCodeInterpreterForChunk(const ACode: string): string;
const
  RETRY_MAX = 5;
var
  LClient: TNetHTTPClient;
  LReqStream, LRespStream: TStringStream;
  LHeaders: TNetHeaders;
  LRes: IHTTPResponse;
  LPayload, LMsg, LTool: TJSonObject;
  LMsgs, LTools: TJSonArray;
  LOutVal: TJSonValue;
  LExecTools: TJSonArray;
  LRetry: Integer;
  LWait: Integer;
  sBody: string;
begin
  Result := '';
  LClient := TNetHTTPClient.Create(nil);
  try
{$IF CompilerVersion >= 34}
    LClient.SynchronizeEvents := False;
{$ENDIF}
    LClient.ResponseTimeout := ResponseTimeOut;
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey),
                 TNetHeader.Create('Content-Type',   'application/json')];

    // Construir payload mínimo
    LMsg := TJSonObject.Create;
    LMsg.AddPair('role', 'user');
    LMsg.AddPair('content', 'Run this Python code:' + #10 + ACode);
    LMsgs := TJSonArray.Create;
    LMsgs.Add(LMsg);

    LTool := TJSonObject.Create;
    LTool.AddPair('type', 'code_interpreter');
    LTools := TJSonArray.Create;
    LTools.Add(LTool);

    LPayload := TJSonObject.Create;
    try
      LPayload.AddPair('model',                TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model));
      LPayload.AddPair('messages',             LMsgs);
      LPayload.AddPair('tools',                LTools);
      LPayload.AddPair('tool_choice',          'auto');
      LPayload.AddPair('max_completion_tokens', TJSONNumber.Create(65536));
      LPayload.AddPair('stream',               TJSONBool.Create(False));
      sBody := LPayload.ToJSON;
    finally
      LPayload.Free;
    end;

    LRetry := 0;
    repeat
      LReqStream  := TStringStream.Create(sBody, TEncoding.UTF8);
      LRespStream := TStringStream.Create('', TEncoding.UTF8);
      try
        LRes := LClient.Post(Url + 'chat/completions', LReqStream, LRespStream, LHeaders);
        if LRes.StatusCode = 429 then
        begin
          // Rate limit — esperar y reintentar
          LWait := 35000;
          var sErr := LRes.ContentAsString;
          var IWaitPos := Pos('try again in ', sErr);
          if IWaitPos > 0 then
          begin
            var sAfter := Copy(sErr, IWaitPos + 13, 10);
            var IDot := Pos('.', sAfter);
            var ISpace := Pos(' ', sAfter);
            var IEnd := ISpace;
            if (IDot > 0) and (IDot < ISpace) then IEnd := IDot;
            var sNum := Copy(sAfter, 1, IEnd - 1);
            var N: Double;
            if TryStrToFloat(sNum, N) then
              LWait := Trunc(N * 1000) + 3000;
          end;
          Sleep(LWait);
          Inc(LRetry);
          Continue;
        end;
        if LRes.StatusCode = 200 then
        begin
          var JResp := TJSonObject.ParseJSONValue(LRespStream.DataString) as TJSonObject;
          if Assigned(JResp) then
          try
            // Extraer executed_tools de choices[0].message
            var JChoices: TJSonArray;
            if JResp.TryGetValue<TJSonArray>('choices', JChoices) and (JChoices.Count > 0) then
            begin
              var JMsgNode: TJSonObject;
              if TJSonObject(JChoices.Items[0]).TryGetValue<TJSonObject>('message', JMsgNode) then
              begin
                if JMsgNode.TryGetValue<TJSonArray>('executed_tools', LExecTools) then
                begin
                  for LOutVal in LExecTools do
                  begin
                    var sOut: string := '';
                    if (LOutVal as TJSonObject).TryGetValue<string>('output', sOut) then
                      Result := Result + sOut;
                  end;
                end;
              end;
            end;
            // compound-beta: executed_tools en raíz
            if (Result = '') and JResp.TryGetValue<TJSonArray>('executed_tools', LExecTools) then
              for LOutVal in LExecTools do
              begin
                var sOut: string := '';
                if (LOutVal as TJSonObject).TryGetValue<string>('output', sOut) then
                  Result := Result + sOut;
              end;
          finally
            JResp.Free;
          end;
        end;
        Break; // salir del retry loop
      finally
        LReqStream.Free;
        LRespStream.Free;
      end;
    until LRetry >= RETRY_MAX;
  finally
    LClient.Free;
  end;
end;

// Extrae base64 de un tool output (con o sin marcadores CHUNK_START/CHUNK_END).
function ExtractB64FromOutput(const AOutput: string; AExpectedLen: Integer): string;
const
  B64_CHARS = ['A'..'Z','a'..'z','0'..'9','+','/','='];
var
  iB, iE: Integer;
  sRaw, sClean: string;
begin
  Result := '';
  iB := Pos('CHUNK_START', AOutput);
  iE := Pos('CHUNK_END',   AOutput);
  if (iB > 0) and (iE > iB) then
  begin
    var iStart := iB + Length('CHUNK_START');
    while (iStart <= Length(AOutput)) and CharInSet(AOutput[iStart], [#10, #13]) do
      Inc(iStart);
    sRaw := Copy(AOutput, iStart, iE - iStart);
  end
  else
  begin
    // Sin marcadores: tomar todo si es >90% base64
    sRaw := AOutput;
  end;
  sClean := '';
  for var ch in sRaw do
    if CharInSet(ch, B64_CHARS) then
      sClean := sClean + ch;
  if AExpectedLen > 0 then
    Result := Copy(sClean, 1, AExpectedLen)
  else
    Result := sClean;
end;

// Pide los chunks restantes y devuelve el b64 completo.
function TAiGroqChat.FetchRemainingChunks(const AOriginalCode, APartialB64: string; AOffset: Integer): string;
const
  CHUNK_SIZE = 40000;
var
  LFullB64: string;
  LOffset: Integer;
  LChunkCode, LOutput, LChunk: string;
begin
  LFullB64 := APartialB64;
  LOffset  := AOffset;
  repeat
    LChunkCode := BuildChunkRequestCode(AOriginalCode, LOffset, LOffset + CHUNK_SIZE);
    LOutput    := CallCodeInterpreterForChunk(LChunkCode);
    if LOutput = '' then Break;
    LChunk := ExtractB64FromOutput(LOutput, CHUNK_SIZE);
    if LChunk = '' then Break;
    LFullB64 := LFullB64 + LChunk;
    if Length(LChunk) < CHUNK_SIZE then Break; // último chunk
    Inc(LOffset, Length(LChunk));
  until False;
  Result := LFullB64;
end;

// Override: detecta truncamiento de FILE_B64_BEGIN y completa el b64 con chunks adicionales.
procedure TAiGroqChat.ProcessExecutedTools(const AExecutedToolsJSON: string; ResMsg: TAiChatMessage);
var
  JArr: TJSonArray;
  JItem: TJSonObject;
  JVal: TJSonValue;
  sOutput, sCode, sFileName, sAfterBegin: string;
  sPartialB64, sFullB64: string;
  iB, iE, iNL, iDataStart, iOffset: Integer;
begin
  if AExecutedToolsJSON = '' then
  begin
    inherited ProcessExecutedTools(AExecutedToolsJSON, ResMsg);
    Exit;
  end;

  JArr := TJSonObject.ParseJSONValue(AExecutedToolsJSON) as TJSonArray; // ParseJSONValue es método de clase de TJSONObject (TJSonValue no lo expone en D10.4)
  if not Assigned(JArr) then
  begin
    inherited ProcessExecutedTools(AExecutedToolsJSON, ResMsg);
    Exit;
  end;

  try
    for JVal in JArr do
    begin
      JItem := JVal as TJSonObject;
      if not Assigned(JItem) then Continue;
      var sType: string := '';
      JItem.TryGetValue<string>('type', sType);
      if (sType <> 'function') and (sType <> 'python') then Continue;

      JItem.TryGetValue<string>('output',    sOutput);
      JItem.TryGetValue<string>('arguments', sCode);

      if (sCode = '') or (sOutput = '') then Continue;

      iB := Pos('FILE_B64_BEGIN:', sOutput);
      iE := Pos('FILE_B64_END',    sOutput);

      // Solo actuar si hay FILE_B64_BEGIN pero NO FILE_B64_END (output truncado)
      if (iB <= 0) or (iE > 0) then Continue;

      // Extraer nombre de archivo
      sAfterBegin := Copy(sOutput, iB + Length('FILE_B64_BEGIN:'), MaxInt);
      iNL := Pos(#10, sAfterBegin);
      if iNL = 0 then iNL := Pos(#13, sAfterBegin);
      sFileName := 'ci_output.bin';
      if iNL > 0 then
        sFileName := Trim(Copy(sAfterBegin, 1, iNL - 1));

      // Extraer base64 parcial ya recibido
      iDataStart := iB + Length('FILE_B64_BEGIN:') + iNL;
      var sPartialRaw := Copy(sOutput, iDataStart, MaxInt);
      sPartialB64 := '';
      for var ch in sPartialRaw do
        if CharInSet(ch, ['A'..'Z','a'..'z','0'..'9','+','/','=']) then
          sPartialB64 := sPartialB64 + ch;

      iOffset := Length(sPartialB64);
      if iOffset = 0 then Continue;

      // Obtener chunks restantes
      sFullB64 := FetchRemainingChunks(sCode, sPartialB64, iOffset);

      if sFullB64 <> '' then
      begin
        // Reemplazar output con el b64 completo bien delimitado
        JItem.RemovePair('output');
        JItem.AddPair('output',
          'FILE_B64_BEGIN:' + sFileName + #10 + sFullB64 + #10 + 'FILE_B64_END');
      end;
    end;

    inherited ProcessExecutedTools(JArr.ToJSON, ResMsg);
  finally
    JArr.Free;
  end;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiGroqChat);

end.
