unit uMakerAi.Embeddings.OpenAi;

// MIT License
//
// Copyright (c) 2013 Gustavo Enriquez - CimaMaker
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

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Generics.Collections,
  System.Net.URLClient, System.Net.HttpClient,
  System.Net.Mime, System.Net.HttpClientComponent,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  TAiOpenAiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSonObject); Override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  end;


procedure Register;


implementation

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';


procedure Register;
Begin
  RegisterComponents('MakerAI', [TAiOpenAiEmbeddings]);
End;


{ TAiOpenAiEmbeddings }

constructor TAiOpenAiEmbeddings.Create(AOwner: TComponent);
begin
  inherited;
end;

function TAiOpenAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  if aDimensions <= 0 then
    aDimensions := FDimensions;

  Try
    jObj.AddPair('input', aInput); // Este se adiciona por compatibilidad con ollama
    jObj.AddPair('prompt', aInput);
    jObj.AddPair('model', aModel);
    jObj.AddPair('user', aUser);
    jObj.AddPair('dimensions', aDimensions);
    jObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(jObj.ToJson);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
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

destructor TAiOpenAiEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOpenAiEmbeddings.ParseEmbedding(jObj: TJSonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSONValue;
  j: Integer;
  Usage: TJSonObject;

begin
  jObj.TryGetValue<String>('model', FModel);

  If jObj.TryGetValue<TJSonObject>('usage', Usage) then
  Begin
    Usage.TryGetValue<Integer>('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue<Integer>('total_tokens', Ftotal_tokens);
  End;

  jData := jObj.GetValue<TJSonArray>('data');

  SetLength(FData, jData.Count);

  // var i := 0;
  For JVal in jData do
  Begin
    // El embedding de OpenAi Retorna un array, pero solo se toma el primero de la fila
    JArr := TJSonObject(JVal).GetValue<TJSonArray>('embedding');
    j := JArr.Count;
    SetLength(Emb, j);
    // FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For j := 0 to JArr.Count - 1 do
      Emb[j] := JArr.Items[j].GetValue<Double>;

    FData := Emb;
    // Inc(i);
    Break; // Si el embedding de OpenAI retorna varios solo tomamos el primero, usualmente solo hay uno
  End;
end;

{ TAiOpenAiEmbeddings - Factory class methods }

class function TAiOpenAiEmbeddings.GetDriverName: string;
begin
  Result := 'OpenAi';
end;

class function TAiOpenAiEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiOpenAiEmbeddings.Create(aOwner);
end;

class procedure TAiOpenAiEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@OPENAI_API_KEY';
  Params.Values['Url'] := GlOpenAIUrl;
  Params.Values['Model'] := 'text-embedding-3-small';
  Params.Values['Dimensions'] := '1536';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiOpenAiEmbeddings);

end.
