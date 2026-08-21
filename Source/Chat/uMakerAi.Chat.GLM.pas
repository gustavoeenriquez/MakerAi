unit uMakerAi.Chat.GLM;

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
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// Driver GLM (Zhipu AI / Z.ai) - API OpenAI-compatible
// Docs: https://docs.z.ai/api-reference/llm/chat-completion
// Endpoint internacional: https://api.z.ai/api/paas/v4/
// (China continental: https://open.bigmodel.cn/api/paas/v4/ - cambiar la propiedad URL)
//
// Particularidades del API (ago 2026):
// - thinking: {"type":"enabled"|"disabled"} en el request; el razonamiento vuelve
//   en message.reasoning_content (parse y streaming ya los captura la clase base).
// - reasoning_effort (low/medium/high) solo lo acepta la familia glm-5.x.
// - Rangos de sampling mas estrechos que OpenAI: temperature [0,1], top_p [0.01,1],
//   max_tokens hasta 131072 (el driver aplica clamp).
// - Vision formato OpenAI (image_url); tambien acepta video_url y file_url.

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
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core, uMakerAI.chat.Messages;

Type

  TAiGLMChat = Class(TAiChat)
  Private
    Function ModelSupportsThinking(Const AModel: String): Boolean;
  Protected
    Function InitChatCompletions: String; Override;
  Public
    Function GetMessages: TJSonArray; Override;
    Constructor Create(Sender: TComponent); Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.z.ai/api/paas/v4/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGLMChat]);
end;

{ TAiGLMChat }

class function TAiGLMChat.GetDriverName: string;
Begin
  Result := 'GLM';
End;

class procedure TAiGLMChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@GLM_API_KEY');
  Params.Add('Model=glm-4.7');
  Params.Add('Max_Tokens=8192');
  Params.Add('URL=' + GlAIUrl);
End;

class function TAiGLMChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiGLMChat.Create(Sender);
End;

constructor TAiGLMChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@GLM_API_KEY';
  Model := 'glm-4.7';
  Url := GlAIUrl;
end;

function TAiGLMChat.ModelSupportsThinking(Const AModel: String): Boolean;
begin
  // Familia hibrida con thinking controlable: glm-4.5+, glm-4.6(v), glm-4.7 y
  // toda la familia glm-5.x (incluye glm-5v-*). Modelos previos (glm-4-32b...)
  // no aceptan el campo thinking.
  Result := StartsText('glm-5', AModel) or StartsText('glm-4.5', AModel) or
    StartsText('glm-4.6', AModel) or StartsText('glm-4.7', AModel);
end;

function TAiGLMChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jThinking: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
  LTemperature, LTopP: Double;
  LMaxTokens: Integer;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'glm-4.7';

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

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
        Raise Exception.Create('La propiedad Tools estan mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin

{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(Tool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSONObject.ParseJSONValue(Tool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tool_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages);

    AJSONObject.AddPair('model', LModel);

    // Thinking hibrido: el API lo trae ACTIVADO por defecto en glm-4.7/5.x,
    // asi que el driver lo controla explicitamente (mismo patron DeepSeek V4):
    // cap_Reasoning -> enabled; sin el cap -> disabled (modo rapido/economico).
    // EXCEPCION: glm-5.3 usa forced thinking y NO acepta disabled.
    // El razonamiento llega en reasoning_content (capturado por la clase base).
    if ModelSupportsThinking(LModel) then
    begin
      jThinking := TJSonObject.Create;
      if (cap_Reasoning in ModelConfig.ModelCaps) or StartsText('glm-5.3', LModel) then
      begin
        jThinking.AddPair('type', 'enabled');
        // reasoning_effort documentado solo en glm-5.2/5.3; valores low/high/max
        // (default del API: max). Mismo mapeo que DeepSeek V4.
        if StartsText('glm-5.2', LModel) or StartsText('glm-5.3', LModel) then
        begin
          case ModelConfig.ThinkingLevel of
            tlLow:    AJSONObject.AddPair('reasoning_effort', 'low');
            tlMedium: AJSONObject.AddPair('reasoning_effort', 'high');
            tlHigh:   AJSONObject.AddPair('reasoning_effort', 'max');
            // tlDefault: no se envia, el API usa max
          end;
        end;
      end
      else
        jThinking.AddPair('type', 'disabled');
      AJSONObject.AddPair('thinking', jThinking);
    end;

    // Z.ai acota temperature a [0,1] (con thinking recomiendan 1.0)
    LTemperature := Trunc(Temperature * 100) / 100;
    if LTemperature > 1 then
      LTemperature := 1;
    if LTemperature < 0 then
      LTemperature := 0;
    AJSONObject.AddPair('temperature', TJSONNumber.Create(LTemperature));

    // max_tokens acepta hasta 131072
    LMaxTokens := Max_tokens;
    if LMaxTokens > 131072 then
      LMaxTokens := 131072;
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(LMaxTokens));

    // top_p acota a [0.01, 1]; 0 = no enviar (usa el default del modelo)
    If Top_p <> 0 then
    Begin
      LTopP := Top_p;
      if LTopP > 1 then
        LTopP := 1;
      if LTopP < 0.01 then
        LTopP := 0.01;
      AJSONObject.AddPair('top_p', TJSONNumber.Create(LTopP));
    End;

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    // Nota: no se envian frequency/presence_penalty, n, logprobs ni seed;
    // el API de Z.ai no los documenta y podrian ser rechazados.

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiGLMChat.GetMessages: TJSonArray;
var
  I: Integer;
  JObj: TJSonObject;
  Msg: TAiChatMessage;
begin
  Result := inherited GetMessages;
  // Z.ai EXIGE devolver reasoning_content en el historial multi-turno para
  // mantener la coherencia del razonamiento (docs: "remember to return the
  // historical reasoning_content"). TAiChatMessages.ToJSon no lo serializa
  // por defecto; mismo patron que DeepSeek V4.
  if Self.Messages.Count <> Result.Count then Exit;
  for I := 0 to Self.Messages.Count - 1 do
  begin
    Msg := Self.Messages.Items[I];
    if (Msg.ReasoningContent <> '') and (Result.Items[I] is TJSonObject) then
    begin
      JObj := TJSonObject(Result.Items[I]);
      if JObj.GetValue('reasoning_content') = nil then
        JObj.AddPair('reasoning_content', Msg.ReasoningContent);
    end;
  end;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiGLMChat);

end.
