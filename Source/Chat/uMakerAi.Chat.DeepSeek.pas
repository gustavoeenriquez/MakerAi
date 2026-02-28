// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Driver para DeepSeek (https://api.deepseek.com/v1/)
//
// Compatible con OpenAI /chat/completions. Diferencia principal:
//   - deepseek-reasoner devuelve "reasoning_content" en delta y en message
//   - El campo se acumula en FLastReasoning (base class) y se incluye en
//     el historial via TAiChatMessages.ToJson (reasoning_content != '')
//   - Todo esto ya lo maneja la clase base TAiChat — este driver solo
//     configura URL, ApiKey, Model por defecto.
//
// Modelos:
//   deepseek-chat     — modelo conversacional OpenAI-compatible
//   deepseek-reasoner — modelo con razonamiento (reasoning_content)
//   deepseek-coder    — especializado en código

unit uMakerAi.Chat.DeepSeek;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  UMakerAi.ParamsRegistry;

type

  // ---------------------------------------------------------------------------
  //  TAiDeepSeekChat — driver DeepSeek (OpenAI-compatible + reasoning_content)
  // ---------------------------------------------------------------------------
  TAiDeepSeekChat = class(TAiChat)
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
  published
  end;

implementation

const
  GlDeepSeekUrl = 'https://api.deepseek.com/v1/';

{ TAiDeepSeekChat }

class function TAiDeepSeekChat.GetDriverName: string;
begin
  Result := 'DeepSeek';
end;

class procedure TAiDeepSeekChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@DEEPSEEK_API_KEY');
  Params.Add('Model=deepseek-chat');
  Params.Add('Max_Tokens=4096');
  Params.Add('URL=' + GlDeepSeekUrl);
end;

class function TAiDeepSeekChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiDeepSeekChat.Create(Sender);
end;

constructor TAiDeepSeekChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  ApiKey := '@DEEPSEEK_API_KEY';
  Model  := 'deepseek-chat';
  Url    := GlDeepSeekUrl;
end;

destructor TAiDeepSeekChat.Destroy;
begin
  inherited Destroy;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiDeepSeekChat);

end.
