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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Embeddings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.NetConsts,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Core;

type

  TAiEmbeddingData = TArray<Double>;

  TAiEmbeddings = Class(TComponent)
  Private
    procedure SetApiKey(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetData(const Value: TAiEmbeddingData);
    procedure SetUrl(const Value: String);
    procedure SetDimensions(const Value: Integer);
    function GetApiKey: String;
  Protected
    FApiKey: String;
    FModel: String;
    Ftotal_tokens: Integer;
    Fprompt_tokens: Integer;
    FData: TAiEmbeddingData;
    FUrl: String;
    FDimensions: Integer;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float')
      : TAiEmbeddingData; Virtual;
    Procedure ParseEmbedding(JObj: TJSonObject); Virtual;
    Function ToJsonArray: TJSonArray; Overload;
    class Function ToJsonArray(Val: TAiEmbeddingData): TJSonArray; Overload;

    class function Magnitude(const V: TAiEmbeddingData): Double;
    class function DotProduct(const A, B: TAiEmbeddingData): Double;
    class function CosineSimilarity(const A, B: TAiEmbeddingData): Double;

    Property Data: TAiEmbeddingData read FData write SetData;

  Published
    Property ApiKey: String read GetApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property prompt_tokens: Integer read Fprompt_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
    Property Url: String read FUrl write SetUrl;
    Property Dimensions: Integer read FDimensions write SetDimensions;
  End;

implementation

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

  { TEmbeddings }

class function TAiEmbeddings.CosineSimilarity(const A, B: TAiEmbeddingData): Double;
var
  MagA, MagB: Double;
begin
  MagA := Magnitude(A);
  MagB := Magnitude(B);
  if (MagA = 0) or (MagB = 0) then
    Result := 0 // Para evitar división por cero
  else
    Result := DotProduct(A, B) / (MagA * MagB);
end;

constructor TAiEmbeddings.Create(aOwner: TComponent);
begin
  Inherited;
  Url := GlOpenAIUrl;
  FDimensions := 1536;
  FModel := 'text-embedding-3-small';
end;

destructor TAiEmbeddings.Destroy;
begin

  inherited;
end;

class function TAiEmbeddings.DotProduct(const A, B: TAiEmbeddingData): Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := Low(A) to High(A) do
    Result := Result + A[i] * B[i];
end;

function TAiEmbeddings.GetApiKey: String;
begin
  // Si está en modo de diseño, simplemente retorna el valor tal cual
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
  begin
    Result := FApiKey;
    Exit;
  end;

  // En modo de ejecución
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
  Begin
    // Retorna el valor de la variable de entorno, quitando el '@'
    Try
      Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
    Except
      Result := FApiKey;
    End;
  End
  else
    Result := FApiKey;
end;

class function TAiEmbeddings.Magnitude(const V: TAiEmbeddingData): Double;
var
  Sum: Double;
  i: Integer;
begin
  Sum := 0.0;
  for i := Low(V) to High(V) do
    Sum := Sum + V[i] * V[i];
  Result := Sqrt(Sum);
end;

procedure TAiEmbeddings.ParseEmbedding(JObj: TJSonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSonValue;
  J: Integer;
  Usage: TJSonObject;

begin
  JObj.TryGetValue<String>('model', FModel);

  If JObj.TryGetValue<TJSonObject>('usage', Usage) then
  Begin
    Usage.TryGetValue<Integer>('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue<Integer>('total_tokens', Ftotal_tokens);
  End;

  jData := JObj.GetValue<TJSonArray>('data');

  SetLength(FData, jData.Count);

  // var i := 0;
  For JVal in jData do
  Begin
    // El embedding de OpenAi Retorna un array, pero solo se toma el primero de la fila
    JArr := TJSonObject(JVal).GetValue<TJSonArray>('embedding');
    J := JArr.Count;
    SetLength(Emb, J);
    // FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For J := 0 to JArr.Count - 1 do
      Emb[J] := JArr.Items[J].GetValue<Double>;

    FData := Emb;
    // Inc(i);
    Break; // Si el embedding de OpenAI retorna varios solo tomamos el primero, usualmente solo hay uno
  End;
end;

procedure TAiEmbeddings.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiEmbeddings.SetData(const Value: TAiEmbeddingData);
begin
  FData := Value;
end;

procedure TAiEmbeddings.SetDimensions(const Value: Integer);
begin
  FDimensions := Value;
end;

procedure TAiEmbeddings.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiEmbeddings.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

class function TAiEmbeddings.ToJsonArray(Val: TAiEmbeddingData): TJSonArray;
Var
  i: Integer;
begin
  Result := TJSonArray.Create;

  For i := 0 to Length(Val) - 1 do
    Result.Add(Val[i]);
end;

function TAiEmbeddings.ToJsonArray: TJSonArray;
Var
  J: Integer;
  JEmb: TJSonArray;
begin
  Try
    JEmb := TJSonArray.Create;
    For J := 0 to Length(FData) - 1 do
      JEmb.Add(FData[J]);

    Result := JEmb;
  Finally

  End;
end;

function TAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := TNetHTTPClient.Create(Nil);
  Client.SynchronizeEvents := False;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  JObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  if aDimensions <= 0 then
    aDimensions := FDimensions;

  Try
    JObj.AddPair('input', aInput); // Este se adiciona por compatibilidad con ollama
    JObj.AddPair('prompt', aInput);
    JObj.AddPair('model', aModel);
    JObj.AddPair('user', aUser);
    JObj.AddPair('dimensions', aDimensions);
    JObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
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
    JObj.Free;
  End;
end;

end.
