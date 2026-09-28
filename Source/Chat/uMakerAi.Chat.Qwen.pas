unit uMakerAi.Chat.Qwen;

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

// Driver Qwen (Alibaba Cloud Model Studio / DashScope) - API OpenAI-compatible
// Docs: https://www.alibabacloud.com/help/en/model-studio/compatibility-of-openai-with-dashscope
// Endpoint internacional (Singapur): https://dashscope-intl.aliyuncs.com/compatible-mode/v1/
// Otras regiones: cambiar URL (EE.UU. https://dashscope-us.aliyuncs.com/compatible-mode/v1/,
// China https://dashscope.aliyuncs.com/compatible-mode/v1/). La API key queda atada a la
// region donde se creo: con otra region el API responde 401.
//
// Particularidades verificadas en runtime (sep 2026):
// - El razonamiento viene ACTIVADO por defecto en los hibridos (qwen3.x, qwen-plus/flash/
//   turbo, qwen3-max, VL, omni). El driver lo controla SIEMPRE con enable_thinking:
//   cap_Reasoning -> true; sin el cap -> false (modo rapido, sin pagar tokens de razonar).
//   enable_thinking=false lo aceptan todos los modelos probados (los que no razonan lo
//   ignoran). El razonamiento vuelve en reasoning_content (lo captura la clase base,
//   con y sin streaming).
// - thinking_budget (tokens de razonamiento) se deriva de ThinkingLevel.
// - Modelos que solo razonan (qwq-*, *-thinking-*): no se les envia enable_thinking.
//   qwq-plus SOLO responde en streaming: sin stream devuelve vacio y sin error.
// - Modelos abiertos (qwen3-32b, qwen3.6-27b, ...) aceptan enable_thinking=true solo en
//   streaming (400 sin stream): en sincrono el driver envia false.
// - tools + stream funcionan (la restriccion de la documentacion no aplica a estos modelos).
// - Vision formato OpenAI (image_url) en qwen3.8-flash/max, qwen3.7-plus, qwen3-vl-*,
//   qwen3.8-omni-flash. Imagenes muy pequenas (2x2) se rechazan con 400.

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.RegularExpressions,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAI.chat.Messages;

Type

  TAiQwenChat = Class(TAiChat)
  Private
    Function IsThinkingOnly(Const AModel: String): Boolean;
    Function IsOpenWeight(Const AModel: String): Boolean;
  Protected
    Function InitChatCompletions: String; Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://dashscope-intl.aliyuncs.com/compatible-mode/v1/';
  GlDefaultModel = 'qwen3.8-flash';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenChat]);
end;

{ TAiQwenChat }

class function TAiQwenChat.GetDriverName: string;
Begin
  Result := 'Qwen';
End;

class procedure TAiQwenChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@DASHSCOPE_API_KEY');
  Params.Add('Model=' + GlDefaultModel);
  Params.Add('Max_Tokens=8192');
  Params.Add('URL=' + GlAIUrl);
End;

class function TAiQwenChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiQwenChat.Create(Sender);
End;

constructor TAiQwenChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@DASHSCOPE_API_KEY';
  Model := GlDefaultModel;
  Url := GlAIUrl;
end;

function TAiQwenChat.IsThinkingOnly(Const AModel: String): Boolean;
begin
  // Siempre razonan: no aceptan apagarlo, asi que no se les manda enable_thinking
  Result := StartsText('qwq', AModel) or ContainsText(AModel, '-thinking');
end;

function TAiQwenChat.IsOpenWeight(Const AModel: String): Boolean;
begin
  // Pesos abiertos: el tamano va en el nombre (qwen3-32b, qwen3.6-27b, qwen3-235b-a22b...)
  Result := TRegEx.IsMatch(AModel, '-\d+(\.\d+)?b(-|$)', [roIgnoreCase]);
end;

function TAiQwenChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jStreamOpts: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico, LThinking: Boolean;
  Res, LModel: String;
  LTemperature: Double;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := GlDefaultModel;

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));
    if LAsincronico then
    begin
      // Sin esto el stream no trae usage y los contadores de tokens quedan en 0
      jStreamOpts := TJSonObject.Create;
      jStreamOpts.AddPair('include_usage', TJSONBool.Create(True));
      AJSONObject.AddPair('stream_options', jStreamOpts);
    end;

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

    // Razonamiento: el API lo trae ACTIVADO por defecto en los hibridos, asi que
    // se controla siempre (mismo criterio que GLM / DeepSeek V4): cap_Reasoning
    // -> true; sin el cap -> false (rapido y sin tokens de razonamiento).
    if not IsThinkingOnly(LModel) then
    begin
      LThinking := cap_Reasoning in ModelConfig.ModelCaps;
      // Los modelos abiertos solo razonan en streaming (400 en sincrono)
      if LThinking and IsOpenWeight(LModel) and not LAsincronico then
        LThinking := False;
      AJSONObject.AddPair('enable_thinking', TJSONBool.Create(LThinking));
      if LThinking then
        case ModelConfig.ThinkingLevel of
          tlLow:    AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(1024));
          tlMedium: AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(4096));
          tlHigh:   AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(16384));
          // tlDefault: sin presupuesto, el largo maximo del modelo
        end;
    end;

    // DashScope acepta temperature en [0, 2)
    LTemperature := Trunc(Temperature * 100) / 100;
    if LTemperature >= 2 then
      LTemperature := 1.99;
    if LTemperature < 0 then
      LTemperature := 0;
    AJSONObject.AddPair('temperature', TJSONNumber.Create(LTemperature));

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

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiQwenChat);

end.
