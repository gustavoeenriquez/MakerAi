unit uMakerAi.Embeddings.Gemini;

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
  System.SysUtils, System.Classes, System.JSON,
  System.Net.URLClient, System.Net.HttpClient,
  System.Net.Mime, System.Net.HttpClientComponent,
  System.Generics.Collections, System.NetConsts,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  TAiGeminiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSONObject); Override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  end;

procedure Register;


implementation

Const
  GlGeminiUrl = 'https://generativelanguage.googleapis.com/v1beta/';


procedure Register;
Begin
  RegisterComponents('MakerAI', [TAiGeminiEmbeddings]);
End;


{ TAiGeminiEmbeddings }

constructor TAiGeminiEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
end;

function TAiGeminiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: THTTPClient;
  jRequestRoot, jContent, jPart: TJSONObject;
  jParts: TJSonArray;
  jResponseRoot: TJSONObject;
  Res: IHTTPResponse;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  jRequestRoot := TJSONObject.Create;
  try
    if aModel.IsEmpty then
      aModel := FModel;
    sUrl := FUrl + 'models/' + aModel + ':embedContent?key=' + ApiKey;
    jPart := TJSONObject.Create;
    jPart.AddPair('text', TJSONString.Create(aInput));
    jParts := TJSonArray.Create;
    jParts.AddElement(jPart);
    jContent := TJSONObject.Create;
    jContent.AddPair('parts', jParts);
    jRequestRoot.AddPair('model', TJSONString.Create('models/' + aModel));
    jRequestRoot.AddPair('content', jContent);
    if aDimensions > 0 then
      jRequestRoot.AddPair('outputDimensionality', TJSONNumber.Create(aDimensions));

    St := TStringStream.Create(jRequestRoot.ToString, TEncoding.UTF8);
    try
      Client.ContentType := 'application/json';
      Res := Client.Post(sUrl, St);
    finally
      St.Free;
    end;

    if Res.StatusCode = 200 then
    begin
      jResponseRoot := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        ParseEmbedding(jResponseRoot);
        Result := Self.FData;
      finally
        jResponseRoot.Free;
      end;
    end
    else
      Raise Exception.CreateFmt('Error Gemini Embeddings: %d - %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    jRequestRoot.Free;
    Client.Free;
  end;
end;

procedure TAiGeminiEmbeddings.ParseEmbedding(jObj: TJSONObject);
var
  LEmbeddingObj: TJSONObject;
  LValuesArray: TJSonArray;
  Emb: TAiEmbeddingData;
  I: Integer;
begin
  if not jObj.TryGetValue<TJSONObject>('embedding', LEmbeddingObj) then
    Exit;
  if not LEmbeddingObj.TryGetValue<TJSonArray>('values', LValuesArray) then
    Exit;
  SetLength(Emb, LValuesArray.Count);
  for I := 0 to LValuesArray.Count - 1 do
    Emb[I] := LValuesArray.Items[I].GetValue<Double>;
  FData := Emb;
end;

destructor TAiGeminiEmbeddings.Destroy;
begin
  inherited;
end;

{ TAiGeminiEmbeddings - Factory class methods }

class function TAiGeminiEmbeddings.GetDriverName: string;
begin
  Result := 'Gemini';
end;

class function TAiGeminiEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiGeminiEmbeddings.Create(aOwner);
end;

class procedure TAiGeminiEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@GEMINI_API_KEY';
  Params.Values['Url'] := GlGeminiUrl;
  Params.Values['Model'] := 'gemini-embedding-001';
  Params.Values['Dimensions'] := '768';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiGeminiEmbeddings);

end.
