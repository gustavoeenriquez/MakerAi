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
// Kimi (Moonshot AI) expone un endpoint OpenAI-compatible en
// https://api.moonshot.ai/v1/
//
// Sin sobrescrituras de metodos — hereda todo de TAiChat.
// Solo diferencia: URL y ApiKey defaults.
//
// API key: variable de entorno KIMI_API_KEY

unit uMakerAi.Chat.Kimi;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMakerAi.Chat,
  uMakerAi.Core,
  UMakerAi.ParamsRegistry;

const
  GlKimiUrl = 'https://api.moonshot.ai/v1/';

type

  TAiKimiChat = class(TAiChat)
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;
    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
  published
  end;

implementation

{ TAiKimiChat }

class function TAiKimiChat.GetDriverName: string;
begin
  Result := 'Kimi';
end;

class procedure TAiKimiChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@KIMI_API_KEY');
  Params.Add('Model=kimi-k2');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=' + GlKimiUrl);
end;

class function TAiKimiChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiKimiChat.Create(Sender);
end;

constructor TAiKimiChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@KIMI_API_KEY';
  Model  := 'kimi-k2';
  Url    := GlKimiUrl;
end;

destructor TAiKimiChat.Destroy;
begin
  inherited;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiKimiChat);

end.
