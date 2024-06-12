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


// OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO
// No funciona el llamado en Stream
// Falta el envío de imágenes

// OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO OJO

unit uAiAnthropic;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uOpenAI, uAiOpenChat, uAiToolFunctions;

type

  TAiClaudeChat = Class(TAiOpenChat)
  Private
    Function GetToolJSon: TJSonArray;
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): String; Override;
    Function InternalAddMessage(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Override;

    Function InitChatCompletions: String; Override;
    Procedure ParseChat(JObj: TJSonObject); Override;
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    function ExtractToolCallJson(jChoices: TJSonArray): TJSonArray; // construye el llamado a las funciones en el mensaje

    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Function GetMessages: TJSonArray; Override;

  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function Run(aMsg: TAiOpenChatMessage = Nil): String; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.anthropic.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiClaudeChat]);
end;

{ TAiClaudChat }

constructor TAiClaudeChat.Create(Sender: TComponent);
begin
  inherited;
  FClient.OnReceiveData := Self.OnInternalReceiveData;
  FClient.ResponseTimeOut := 60000;

  Model := 'claude-3-haiku-20240307';
  N := 1;
  Response_format := TAiOpenChatResponseFormat.tiaChatRfText;
  Temperature := 1;
  User := 'user';
  InitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  Max_tokens := 300;
  Url := GlAIUrl;
  Top_p := 1;
  ResponseTimeOut := 60000;

end;

destructor TAiClaudeChat.Destroy;
begin

  inherited;
end;

function TAiClaudeChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
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
    If JVal1.GetValue<String>('type') = 'tool_use' then
    Begin
      Fun := TAiToolsFunction.Create;
      Fun.Id := JVal1.GetValue<String>('id');
      Fun.Tipo := 'function';
      Fun.Name := JVal1.GetValue<String>('name');
      If JVal1.TryGetValue<TJSonObject>('input', Arg) then
      Begin
        Fun.Arguments := Arg.Format;
        For I := 0 to Arg.Count - 1 do
        Begin
          Nom := Arg.Pairs[I].JsonString.Value;
          Valor := Arg.Pairs[I].JsonValue.Value;
          Fun.Params.Values[Nom] := Valor;
        End;
      End;
      // aquí debe ir los parámetros
      Result.Add(Fun.Id, Fun);
    End;
  End;
end;

function TAiClaudeChat.ExtractToolCallJson(jChoices: TJSonArray): TJSonArray;
Var
  JObj, Msg, Arg: TJSonObject;
  JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
  I: Integer;
  Nom, Valor: String;
begin

  Result := TJSonArray.Create;

  For JVal1 in jChoices do
  Begin
    If JVal1.GetValue<String>('type') = 'tool_use' then
    Begin
      JObj := TJSonObject.Create;
      JObj.AddPair('type', 'tool_use');
      JObj.AddPair('id', JVal1.GetValue<String>('id'));
      JObj.AddPair('name', JVal1.GetValue<String>('name'));
      If JVal1.TryGetValue<TJSonObject>('input', Arg) then
        JObj.AddPair('input', Arg);
      Result.Add(JObj);
    End;
  End;
end;

function TAiClaudeChat.GetMessages: TJSonArray;
Var
  I, J: Integer;
  Msg: TAiOpenChatMessage;
  JObj, JObj1, jMsgImagen, jSource: TJSonObject;
  jImages, jContentArr: TJSonArray;
  ImagePayload: TStringStream;
  Base64: String;
  MediaArr: TAiMediaFilesArray;
  Mime: String;
begin
  Result := TJSonArray.Create;

  For I := 0 to Messages.Count - 1 do
  Begin
    Msg := Messages.Items[I];
    // If Msg.Prompt <> '' then
    Begin
      JObj := TJSonObject.Create;

      JObj.AddPair('role', Msg.Role);

      MediaArr := Msg.MediaFiles.GetMediaList(Tfc_Image, False);

      // Si es una respuesta a un llamado
      If (Msg.TollCallId <> '') or (Msg.Tool_calls <> '') or (Length(MediaArr) > 0) then
      Begin
        jContentArr := TJSonArray.Create;

        If Msg.TollCallId <> '' then
        Begin
          JObj1 := TJSonObject.Create;
          jContentArr.Add(JObj1);
          JObj1.AddPair('type', 'tool_result');
          JObj1.AddPair('tool_use_id', Msg.TollCallId);
          JObj1.AddPair('content', Msg.Prompt);
          JObj.AddPair('content', jContentArr);
        End;

        If Msg.Tool_calls <> '' then // Si es el llamado a la función
        Begin
          JObj.Free; // Libera el que se creó arriba a nivel general
          JObj := TJSonObject(TJSonObject.ParseJSONValue(Msg.Tool_calls));
        End;

        If (Length(MediaArr) > 0) then
        Begin
          For J := 0 to Msg.MediaFiles.Count - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
          Begin
            Base64 := Msg.MediaFiles[J].Base64;
            Mime := Msg.MediaFiles[J].MimeType;

            JObj1 := TJSonObject.Create;
            jContentArr.Add(JObj1);
            JObj.AddPair('content', jContentArr);
            JObj1.AddPair('type', 'image');

            { TODO : Este código no funciona, dice que el formato del base64 está errado, Faltar revisar bien la documentación }
            // OJO OJO OJO OJO OJO Este código no funciona, dice que el formato del base64 está errado
            jSource := TJSonObject.Create;
            JObj1.AddPair('source', jSource);
            {
              jSource.AddPair('type', 'base64');
              jSource.AddPair('media_type', Mime); // OJO OJO OJO aquí debe llevar el formato de la imagen
              jSource.AddPair('data', Base64);
            }

            // Este es el código que funciona en el componente de AiOpen y aquí no funciona

            ImagePayload := TStringStream.Create('{"type": "base64","media_type": "' + Mime + '", "data":"' + Base64 + '"}', TEncoding.UTF8);
            try
              JObj1.AddPair('source', TJSonObject.ParseJSONValue(ImagePayload.DataString) as TJSonObject);
            finally
              ImagePayload.Free;
            end;

          End;

          If Msg.Prompt <> '' then
          Begin
            JObj1 := TJSonObject.Create;
            jContentArr.Add(JObj1);
            JObj1.AddPair('type', 'text');
            JObj1.AddPair('text', Msg.Prompt);
          End;
        End;
      End
      Else
      Begin
        JObj.AddPair('content', Msg.Prompt);
      End;
      { TODO : Se comenta esta sección hasta que se corrija el error presentado con las imágenes }
      {
        // Solo crea una lista de imagenes y siempre en base64
        If (Msg.VisionUrls.Count > 0) or (Msg.VisionBase64.Count > 0) then
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

      Result.Add(JObj);
    End;
  End;
end;

class function TAiClaudeChat.GetModels(aApiKey, aUrl: String): TStringList;
begin
  // No existe en la documentación del api los modelos
  Result := TStringList.Create;
  Result.Add('claude-3-opus-20240229');
  Result.Add('claude-3-sonnet-20240229');
  Result.Add('claude-3-haiku-20240307');
end;

function TAiClaudeChat.GetToolJSon: TJSonArray;
Var
  I, J: Integer;
  Item: TFunctionActionItem;
  Param: TFunctionParamsItem;
  JObj, jParam, jProperties, jInputSchema, jDet: TJSonObject;
  jRequired: TJSonArray;
begin
  Result := TJSonArray.Create;

  // Primer ciclo para las funciones
  For I := 0 to Functions.Count - 1 do
  Begin
    Item := TFunctionActionItem(Functions[I]);
    JObj := TJSonObject.Create;

    If (Item.Enabled) and (Item.ToolType = tt_function) then
    Begin
      JObj.AddPair('name', Item.FunctionName);
      JObj.AddPair('description', Item.Description.Text.Trim);

      jInputSchema := TJSonObject.Create;
      jInputSchema.AddPair('type', 'object');
      jProperties := TJSonObject.Create;
      jInputSchema.AddPair('properties', jProperties);
      JObj.AddPair('input_schema', jInputSchema);
      jRequired := TJSonArray.Create;


      // --------- adicionar parámetros --------------

      For J := 0 to Item.Parameters.Count - 1 do
      Begin
        Param := Item.Parameters[J];

        jProperties.AddPair(Param.Name, Param.ToJSon);

        If Param.Required then
          jRequired.Add(Param.Name);
      End;

      If jRequired.Count > 0 then
        JObj.AddPair('required', jRequired)
      else
        jRequired.Free;
      // ---------------------------------------------
    End;

    If Assigned(JObj) then
      Result.Add(JObj);
  End;
end;

function TAiClaudeChat.InitChatCompletions: String;
Var
  AJSONObject, JObj, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  LastMsg: TAiOpenChatMessage;
begin

  If User = '' then
    User := 'user';

  If Model = '' then
    Model := 'claude-3-haiku-20240307';

  // claude-3-opus-20240229
  // claude-3-sonnet-20240229
  // claude-3-haiku-20240307

  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active then
    Begin
      JArr := GetToolJSon;

      If Assigned(JArr) then
      Begin
        AJSONObject.AddPair('tools', JArr);

        If (Trim(Tool_choice) <> '') then
        Begin
          jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(Tool_choice));
          If Assigned(jToolChoice) then
            AJSONObject.AddPair('tools_choice', jToolChoice);
        End;
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages); // FMessages.ToJSon);
    AJSONObject.AddPair('model', Model);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

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

Function TAiClaudeChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): String;
Var
  Msg: TAiOpenChatMessage;
  MensajeInicial: String;
begin

  Try
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    { TODO : Antrhopic trabaja con un Mensaje del Sistema Inicial diferente a los otros, falta corregir esto }
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then //Solo acepta esto al iniciar el chat
    Begin
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiOpenChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiOpenChatMessage.Create('De acuerdo, seguiré las instrucciones', 'assistant');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiOpenChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
End;

Function TAiClaudeChat.InternalAddMessage(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String;
Var
  Msg: TAiOpenChatMessage;
  MensajeInicial: String;
  MF: TAiMediaFile;
  Procesado: Boolean;
  Respuesta: String;
begin

  Try
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then //Solo acepta esto al iniciar el chat
    Begin

      // Si el formato de respuesta es Json, siempre debe llevar en la instrucción que el formato sea json
      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiOpenChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiOpenChatMessage.Create('De acuerdo, seguiré las instrucciones', 'assistant');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiOpenChatMessage.Create(aPrompt, aRole);
    Msg.Id := FMessages.Count + 1;
    FMessages.Add(Msg);

    If Assigned(FOnAddMessage) then
      FOnAddMessage(Self, Msg, Nil, aRole, aPrompt);

    For MF in aMediaFiles do
    Begin
      DoProcessMediaFile(aPrompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
      If Procesado then
        Msg.Prompt := Msg.Prompt + sLineBreak + Respuesta;

      Msg.AddMediaFile(MF);
    End;

    FLastPrompt := Msg.Prompt; // Aquí va el prompt inicial + la conversión de todos los mediafiles si el usuario lo hizo en el evento

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);
  Finally
  End;
End;

procedure TAiClaudeChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  JObj, Delta: TJSonObject;
  sJson, Value, Role1: String;
  P: Integer;
  Msg: TAiOpenChatMessage;
begin
  // OJO, no está llamando esta función, al parecer no puede trabajar en modo stream
  // Esta función se asigna directamente el componente FClient en el oncreate
  // y se debe llamar automáticamente debido al protocolo

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
        Msg := TAiOpenChatMessage.Create(FLastContent, FTmpRole);
        Msg.Id := FMessages.Count + 1;
        FMessages.Add(Msg);
        FBusy := False;

        If Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, Msg, Nil, FTmpRole, FLastContent);
      End
      Else If sJson <> '' then
      Begin
        JObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

        Try
          If Assigned(JObj) then
          Begin
            Delta := JObj.GetValue<TJSonArray>('choices')[0].GetValue<TJSonObject>('delta');
            Value := '';
            Delta.TryGetValue<String>('content', Value);
            Delta.TryGetValue<String>('role', Role1);

            If Role1 <> '' then
              FTmpRole := Role1;

            FLastContent := FLastContent + Value;

            If (Value <> '') and Assigned(FOnReceiveDataEvent) then
            Begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, JObj, FTmpRole, Value);
            End;
          End;
        Finally
          JObj.Free;
        End;
      End;

    Until sJson = '';

  Except

  End;
end;

procedure TAiClaudeChat.ParseChat(JObj: TJSonObject);
Var
  choices: TJSonArray;
  JItem, JToolCalls: TJSonObject;
  JVal: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta: String;
  Msg: TAiOpenChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Id, Clave, sToolCalls, sRes: String;

begin

  Id := JObj.GetValue('id').Value;
  // IdObject := JObj.GetValue('object').Value;
  // IdCreate := JObj.GetValue('created').GetValue<String>;
  Model := JObj.GetValue('model').Value;
  Role := JObj.GetValue('role').Value;
  If JObj.TryGetValue<TJSonObject>('usage', uso) then
  Begin
    aPrompt_tokens := uso.GetValue<Integer>('input_tokens');
    aCompletion_tokens := uso.GetValue<Integer>('output_tokens');
    aTotal_tokens := aPrompt_tokens + aCompletion_tokens;
  End;

  JObj.TryGetValue<TJSonArray>('content', choices);

  For JVal in choices do
  Begin
    JItem := TJSonObject(JVal);
    If JItem.GetValue<String>('type') = 'text' then
      Respuesta := Respuesta + JItem.GetValue<String>('text') + sLineBreak;
  End;

  // Solo toma el último elemento de la lista para construir este item
  If JItem.GetValue<String>('type') = 'tool_use' then
  Begin
    JToolCalls := TJSonObject.Create;
    JToolCalls.AddPair('role', Role);

    JToolCalls.AddPair('content', ExtractToolCallJson(choices));

    sToolCalls := JToolCalls.Format;
  End;

  LFunciones := ExtractToolCallFromJson(choices);

  Respuesta := Trim(Respuesta);
  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  Msg := TAiOpenChatMessage.Create(Respuesta, Role);
  Msg.Prompt := Respuesta;
  Msg.Tool_calls := sToolCalls;
  Msg.Prompt_tokens := aPrompt_tokens;
  Msg.Completion_tokens := aCompletion_tokens;
  Msg.Total_tokens := aTotal_tokens;
  Msg.Id := FMessages.Count + 1;
  FMessages.Add(Msg);

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
        Msg := TAiOpenChatMessage.Create(ToolCall.Response, 'user', ToolCall.Id, ToolCall.Name);
        Msg.Id := FMessages.Count + 1;
        FMessages.Add(Msg);
      End;

      Self.Run;

    End
    Else
    Begin
      FBusy := False;
      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, Msg, JObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

function TAiClaudeChat.Run(aMsg: TAiOpenChatMessage): String;
Var
  ABody: String;
  sUrl, MensajeInicial: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  JObj: TJSonObject;
  Msg: TAiOpenChatMessage;
  I : Integer;
  MF : TAiMediaFile;
  Respuesta : String;
  Procesado : Boolean;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'messages';

  Try
    FHeaders := [TNetHeader.Create('x-api-key', ApiKey)];
    FHeaders := FHeaders + [TNetHeader.Create('anthropic-version', '2023-06-01')];
    FHeaders := FHeaders + [TNetHeader.Create('content-type', 'application/json')];

    If Tool_Active then
      FHeaders := FHeaders + [TNetHeader.Create('anthropic-beta', 'tools-2024-05-16')];

    FClient.ContentType := 'application/json';

    { TODO : En antrhopic el mensaje system es diferente, falta adaptarlo bien, revisar la documentación }
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) then // or ((FMessages.Count mod 20) = 0) then  //solo acepta esto al comienzo como primer mensaje
    Begin

      MensajeInicial := Self.PrepareSystemMsg;

      Msg := TAiOpenChatMessage.Create(MensajeInicial, 'user');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      Msg := TAiOpenChatMessage.Create('De acuerdo, seguiré las instrucciones', 'assistant');
      Msg.Id := FMessages.Count + 1;
      FMessages.Add(Msg);

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', MensajeInicial);
    End;

    If Assigned(aMsg) then
    Begin

      If Assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage(Self, Msg);

      aMsg.Id := FMessages.Count + 1;
      FMessages.Add(aMsg);

      For I := 0 to aMsg.MediaFiles.Count - 1 do // MF in aMsg.MediaFiles do
      Begin
        MF := aMsg.MediaFiles[I];

        DoProcessMediaFile(aMsg.Prompt, MF, Respuesta, Procesado); // Envía el archivo por si lo quiere procesar otra AI especializada, Ej.
        If Procesado then // Se asegura que el prompt sea complementado por las respuestas a los MediaFiles si el usuario lo aplica
          aMsg.Prompt := aMsg.Prompt + sLineBreak + Respuesta;

        aMsg.AddMediaFile(MF);
      End;

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

    FResponse.SaveToFile('c:\temp\respuesta.txt');
    FResponse.Position := 0;

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(JObj);
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

end.
