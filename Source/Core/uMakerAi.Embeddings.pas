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
// Nombre: Gustavo Enr�quez
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
  REST.JSON, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}

  uMakerAi.Embeddings.core;

type
  // Por compatibilidad, mantenemos el nombre, pero ahora hereda de la clase base.
  TAiEmbeddings = class(TAiEmbeddingsCore)
  private
    procedure SetApiKey(const Value: String);
    function GetApiKey: String;
    procedure SetUrl(const Value: String);
  protected
    FApiKey: String;
    FUrl: String;
    // Este m�todo es ahora 'override' para proporcionar la implementaci�n espec�fica.
    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; override;
  public
    constructor Create(aOwner: TComponent); override;
    // Este m�todo es espec�fico de la implementaci�n de OpenAI
    procedure ParseEmbedding(JObj: TJsonObject); Virtual;
  published
    // Propiedades espec�ficas de esta implementaci�n
    property ApiKey: String read GetApiKey write SetApiKey;
    property Url: String read FUrl write SetUrl;
  end;

implementation

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

  { TAiEmbeddings }

constructor TAiEmbeddings.Create(aOwner: TComponent);
begin
  inherited; // Llama al constructor de Core.TAiEmbeddings
  Url := GlOpenAIUrl;
  FModel := 'text-embedding-3-small';
end;

procedure TAiEmbeddings.ParseEmbedding(JObj: TJsonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSonValue;
  J: Integer;
  Usage: TJsonObject;

begin
  JObj.TryGetValue<String>('model', FModel);

  If JObj.TryGetValue<TJsonObject>('usage', Usage) then
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
    JArr := TJsonObject(JVal).GetValue<TJSonArray>('embedding');
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

function TAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  JObj := TJsonObject.Create;

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
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
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
End;

function TAiEmbeddings.GetApiKey: String;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
  begin
    Result := FApiKey;
    Exit;
  end;
  if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

procedure TAiEmbeddings.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiEmbeddings.SetUrl(const Value: String);
begin
  if Value <> '' then
    FUrl := Value
  else
    FUrl := GlOpenAIUrl;
end;

end.
