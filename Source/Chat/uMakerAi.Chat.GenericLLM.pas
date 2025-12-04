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


unit uMakerAi.Chat.GenericLLM;

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
  TAiGenericChat = class(TAiChat)
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



procedure Register;

implementation

const
  GlGenericUrl = 'http://127.0.0.1:1234/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGenericChat]);
end;

{ TAiGenericChat }

class function TAiGenericChat.GetDriverName: string;
begin
  Result := 'GenericLLM';
end;

class procedure TAiGenericChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=1234'); // LMStudio normalmente no requiere API key
  Params.Add('Model=generic-local');
  Params.Add('MaxTokens=4096');
  Params.Add('URL='+GlGenericUrl);
end;

class function TAiGenericChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiGenericChat.Create(Sender);
end;

constructor TAiGenericChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '1234'; // local, no se requiere autenticación
  Model := 'generic-local';
  Url := GlGenericUrl;
end;

destructor TAiGenericChat.Destroy;
begin
  inherited;
end;



initialization
  TAiChatFactory.Instance.RegisterDriver(TAiGenericChat);

end.

