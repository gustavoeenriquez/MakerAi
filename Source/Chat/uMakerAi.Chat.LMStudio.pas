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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Chat.LMStudio;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}

  uMakerAi.ParamsRegistry,
  uMakerAi.Chat,
  uMakerAi.Core,
  uMakerAi.Embeddings;

type
  TAiLMStudioChat = class(TAiChat)
  private
  protected
  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;

    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
  published
  end;


  TAiLMStudioEmbeddings = Class(TAiEmbeddings)
  Public
    constructor Create(aOwner: TComponent); override;
  End;


procedure Register;

implementation

const
  GlLMStudioUrl = 'http://127.0.0.1:1234/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiLMStudioChat, TAiLMStudioEmbeddings]);
end;

{ TAiLMStudioChat }

class function TAiLMStudioChat.GetDriverName: string;
begin
  Result := 'LMStudio';
end;

class procedure TAiLMStudioChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=1234'); // LMStudio normalmente no requiere API key
  Params.Add('Model=lmstudio-local');
  Params.Add('MaxTokens=4096');
  Params.Add('URL='+GlLMStudioUrl);
end;

class function TAiLMStudioChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiLMStudioChat.Create(Sender);
end;

constructor TAiLMStudioChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '1234'; // local, no se requiere autenticación
  Model := 'lmstudio-local';
  Url := GlLMStudioUrl;
end;

destructor TAiLMStudioChat.Destroy;
begin
  inherited;
end;


{ TAiLMStudioEmbeddings }

constructor TAiLMStudioEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '1234';
  Url := GlLMStudioUrl;
  FDimensions := 1024;
  FModel := 'snowflake-arctic-embed-m-v1.5';
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiLMStudioChat);

end.

