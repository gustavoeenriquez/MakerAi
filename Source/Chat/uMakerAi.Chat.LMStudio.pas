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
// LM Studio expone un endpoint OpenAI-compatible en localhost:1234/v1/
// No requiere ninguna sobrescritura de metodos — hereda todo de TAiChat.
// Solo diferencia: URL por defecto y ApiKey='1234' (local, sin autenticacion).

unit uMakerAi.Chat.LMStudio;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMakerAi.Chat,
  uMakerAi.Core,
  UMakerAi.ParamsRegistry;

const
  GlLMStudioUrl = 'http://127.0.0.1:1234/v1/';

type

  TAiLMStudioChat = class(TAiChat)
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
  end;

implementation

{ TAiLMStudioChat }

class function TAiLMStudioChat.GetDriverName: string;
begin
  Result := 'LMStudio';
end;

class procedure TAiLMStudioChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=1234');          // LM Studio local, sin autenticacion
  Params.Add('Model=lmstudio-local'); // el modelo cargado en LM Studio
  Params.Add('MaxTokens=4096');
  Params.Add('URL=' + GlLMStudioUrl);
end;

class function TAiLMStudioChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiLMStudioChat.Create(Sender);
end;

constructor TAiLMStudioChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '1234';             // local, no se requiere autenticacion
  Model  := 'lmstudio-local';  // nombre del modelo activo en LM Studio
  Url    := GlLMStudioUrl;
end;

destructor TAiLMStudioChat.Destroy;
begin
  inherited;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiLMStudioChat);

end.
