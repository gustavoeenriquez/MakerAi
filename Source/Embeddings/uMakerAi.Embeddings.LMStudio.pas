unit uMakerAi.Embeddings.LMStudio;

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

interface

uses
  System.SysUtils, System.Classes,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  TAiLMStudioEmbeddings = Class(TAiEmbeddings)
  Public
    constructor Create(aOwner: TComponent); override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  End;


procedure Register;


implementation


const
  GlLMStudioUrl = 'http://127.0.0.1:1234/v1/';



procedure Register;
Begin
  RegisterComponents('MakerAI', [TAiLMStudioEmbeddings]);
End;


{ TAiLMStudioEmbeddings }

constructor TAiLMStudioEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
end;

{ TAiLMStudioEmbeddings - Factory class methods }

class function TAiLMStudioEmbeddings.GetDriverName: string;
begin
  Result := 'LMStudio';
end;

class function TAiLMStudioEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiLMStudioEmbeddings.Create(aOwner);
end;

class procedure TAiLMStudioEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '1234';
  Params.Values['Url'] := GlLMStudioUrl;
  Params.Values['Model'] := 'snowflake-arctic-embed-m-v1.5';
  Params.Values['Dimensions'] := '1024';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiLMStudioEmbeddings);

end.
