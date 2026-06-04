unit uMakerAi.Embeddings.Mistral;

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
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.URLClient, System.Net.HttpClient,
  System.Net.Mime, System.Net.HttpClientComponent,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  TAiMistralEmbeddings = Class(TAiEmbeddings)
  Public
    // Mistral embeddings
    Constructor Create(aOwner: TComponent); Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: integer = 1536; aModel: String = 'mistral-embed'; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  End;


procedure Register;


implementation

Const
  GlMistralUrl = 'https://api.mistral.ai/v1/';


procedure Register;
Begin
  RegisterComponents('MakerAI', [TAiMistralEmbeddings]);
End;



{ TAiMistralEmbeddings }

constructor TAiMistralEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
end;

function TAiMistralEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSONObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  jInput: TJSonArray;
begin

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSONObject.Create;

  If aModel = '' then
    aModel := FModel;

  Try
    jObj.AddPair('model', aModel);

    jInput := TJSonArray.Create;
    jInput.Add(aInput);

    jObj.AddPair('input', jInput); // Este se adiciona por compatibilidad con ollama
    // JObj.AddPair('prompt', aInput);
    // JObj.AddPair('user', aUser);
    // JObj.AddPair('dimensions', aDimensions);
    jObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(jObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSONObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(jObj);
      Result := Self.FData;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    jObj.Free;
  End;
end;

{ TAiMistralEmbeddings - Factory class methods }

class function TAiMistralEmbeddings.GetDriverName: string;
begin
  Result := 'Mistral';
end;

class function TAiMistralEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiMistralEmbeddings.Create(aOwner);
end;

class procedure TAiMistralEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@MISTRAL_API_KEY';
  Params.Values['Url'] := GlMistralUrl;
  Params.Values['Model'] := 'mistral-embed';
  Params.Values['Dimensions'] := '-1';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiMistralEmbeddings);

end.
