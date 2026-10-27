unit uMakerAi.Chat.MiniMax;

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

// Driver MiniMax - API OpenAI-compatible
// Docs: https://platform.minimax.io/docs/api-reference/text-openai-api
// Endpoint internacional: https://api.minimax.io/v1/
//
// Particularidades del API (oct 2026, segun la doc; SIN prueba runtime):
// - thinking: {"type":"adaptive"|"disabled"}, activo por defecto. En
//   MiniMax-M3.1-Flash 'disabled' da 400 (thinking siempre activo); en M3 lo
//   apaga; en M2.x se acepta pero se ignora.
// - reasoning_split=true saca el razonamiento a message.reasoning_content (lo
//   captura la clase base en parse y streaming). Con false llega dentro de
//   content entre etiquetas <think>; M3.1-Flash no admite false. El driver
//   siempre manda true.
// - reasoning_effort (low/medium/high/xhigh/max, default max) SOLO en
//   M3.1-Flash; 'none' da 400.
// - temperature y top_p se ignoran (M3/M3.1 usan siempre 1 y 0.95); n solo 1.
//   No se envian.
// - max_completion_tokens es el limite recomendado (el thinking cuenta dentro).
// - El razonamiento se reenvia en el historial (multi-turno y tool calls).
// - Vision (image_url) y video (video_url) en M3/M3.1; sin audio de entrada.

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

  TAiMiniMaxChat = Class(TAiChat)
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
  GlAIUrl = 'https://api.minimax.io/v1/';
  GlDefaultModel = 'MiniMax-M3';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMiniMaxChat]);
end;

{ TAiMiniMaxChat }

class function TAiMiniMaxChat.GetDriverName: string;
Begin
  Result := 'MiniMax';
End;

class procedure TAiMiniMaxChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@MINIMAX_API_KEY');
  Params.Add('Model=' + GlDefaultModel);
  Params.Add('Max_Tokens=16000');
  Params.Add('URL=' + GlAIUrl);
End;

class function TAiMiniMaxChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiMiniMaxChat.Create(Sender);
End;

constructor TAiMiniMaxChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@MINIMAX_API_KEY';
  Model := GlDefaultModel;
  Url := GlAIUrl;
end;

function TAiMiniMaxChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jThinking, jStreamOpts: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico, LIsFlash: Boolean;
  Res, LModel: String;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := GlDefaultModel;

  // MiniMax-M3.1-Flash: thinking siempre activo y unico con reasoning_effort
  LIsFlash := StartsText('MiniMax-M3.1-Flash', LModel);

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));
    // Sin esto el stream no trae el consumo de tokens
    if LAsincronico then
    begin
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

    // El razonamiento siempre en reasoning_content (lo captura la base), nunca
    // como <think> dentro de content
    AJSONObject.AddPair('reasoning_split', TJSONBool.Create(True));

    // Thinking: el API lo trae ACTIVO por defecto. cap_Reasoning -> adaptive;
    // sin el cap -> disabled (modo rapido), salvo M3.1-Flash, que no admite
    // apagarlo (400): ahi no se envia y queda el default.
    if (cap_Reasoning in ModelConfig.ModelCaps) or (not LIsFlash) then
    begin
      jThinking := TJSonObject.Create;
      if cap_Reasoning in ModelConfig.ModelCaps then
        jThinking.AddPair('type', 'adaptive')
      else
        jThinking.AddPair('type', 'disabled');
      AJSONObject.AddPair('thinking', jThinking);
    end;

    // reasoning_effort solo en M3.1-Flash (default del API: max, el mas caro).
    // 'none' da 400: tlNone/tlMinimal se piden como low.
    if LIsFlash then
    begin
      case ModelConfig.ThinkingLevel of
        tlNone, tlMinimal, tlLow: AJSONObject.AddPair('reasoning_effort', 'low');
        tlMedium:                 AJSONObject.AddPair('reasoning_effort', 'medium');
        tlHigh:                   AJSONObject.AddPair('reasoning_effort', 'high');
        tlXHigh:                  AJSONObject.AddPair('reasoning_effort', 'xhigh');
        tlMax:                    AJSONObject.AddPair('reasoning_effort', 'max');
        // tlDefault: no se envia, el API usa max
      end;
    end;

    // max_completion_tokens: limite recomendado por MiniMax (incluye thinking)
    if Max_tokens > 0 then
      AJSONObject.AddPair('max_completion_tokens', TJSONNumber.Create(Max_tokens));

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    // Nota: no se envian temperature, top_p, penalties, n, logprobs ni seed;
    // el API de MiniMax los ignora (y top_p fuera de (0,1] da error).

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiMiniMaxChat.GetMessages: TJSonArray;
var
  I: Integer;
  JObj: TJSonObject;
  Msg: TAiChatMessage;
begin
  Result := inherited GetMessages;
  // MiniMax pide conservar el razonamiento en el historial (multi-turno y tool
  // calls). TAiChatMessages.ToJSon no lo serializa por defecto; mismo patron
  // que GLM y DeepSeek V4.
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

TAiChatFactory.Instance.RegisterDriver(TAiMiniMaxChat);

end.
