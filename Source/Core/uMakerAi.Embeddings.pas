// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Embeddings
// Driver OpenAI para generación de embeddings. Hereda de TAiEmbeddingsCore.
unit uMakerAi.Embeddings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Embeddings.Core;

type
  TAiEmbeddings = class(TAiEmbeddingsCore)
  private
    procedure SetApiKey(const Value: string);
    function  GetApiKey: string;
    procedure SetUrl(const Value: string);
  protected
    FApiKey: string;
    FUrl   : string;
  public
    constructor Create(aOwner: TComponent); override;
    procedure ParseEmbedding(JObj: TJSONObject); virtual;
    function CreateEmbedding(aInput, aUser: string; aDimensions: Integer = -1;
        aModel: string = ''; aEncodingFormat: string = 'float'): TAiEmbeddingData; override;
    // Factory pattern
    class function GetDriverName: string; virtual;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; virtual;
    class procedure RegisterDefaultParams(Params: TStrings); virtual;
  published
    property ApiKey: string read GetApiKey write SetApiKey;
    property Url   : string read FUrl      write SetUrl;
  end;

implementation

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

// ---------------------------------------------------------------------------
// TAiEmbeddings
// ---------------------------------------------------------------------------

constructor TAiEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  FUrl   := GlOpenAIUrl;
  FModel := 'text-embedding-3-small';
end;

procedure TAiEmbeddings.ParseEmbedding(JObj: TJSONObject);
var
  JTmp      : TJSONData;
  JUsage    : TJSONObject;
  JArrData  : TJSONArray;
  JItem     : TJSONObject;
  JArrVector: TJSONArray;
  Emb       : TAiEmbeddingData;
  I         : Integer;
begin
  if not Assigned(JObj) then Exit;

  // Modelo
  JTmp := JObj.Find('model');
  if Assigned(JTmp) then FModel := JTmp.AsString;

  // Tokens de uso (opcional)
  JTmp := JObj.Find('usage');
  if Assigned(JTmp) and (JTmp is TJSONObject) then
  begin
    JUsage := TJSONObject(JTmp);
    JTmp := JUsage.Find('prompt_tokens');
    if Assigned(JTmp) then Fprompt_tokens := JTmp.AsInteger;
    JTmp := JUsage.Find('total_tokens');
    if Assigned(JTmp) then Ftotal_tokens := JTmp.AsInteger;
  end;

  // Array "data"
  JTmp := JObj.Find('data');
  if (not Assigned(JTmp)) or not (JTmp is TJSONArray) then
    raise Exception.Create(
        'La respuesta de la API no contiene el array de datos esperado ("data").');
  JArrData := TJSONArray(JTmp);
  if JArrData.Count = 0 then
    raise Exception.Create('El array de datos ("data") está vacío.');

  // Solo procesamos el primer embedding
  if not (JArrData.Items[0] is TJSONObject) then
    raise Exception.Create(
        'Formato de ítem de datos inválido en la respuesta JSON.');
  JItem := TJSONObject(JArrData.Items[0]);

  JTmp := JItem.Find('embedding');
  if (not Assigned(JTmp)) or not (JTmp is TJSONArray) then
    raise Exception.Create(
        'No se encontró el campo "embedding" en los datos de respuesta.');
  JArrVector := TJSONArray(JTmp);
  if JArrVector.Count = 0 then
    raise Exception.Create('El vector de embedding está vacío.');

  SetLength(Emb, JArrVector.Count);
  for I := 0 to JArrVector.Count - 1 do
  begin
    try
      Emb[I] := JArrVector.Items[I].AsFloat;
    except
      Emb[I] := 0.0;
    end;
  end;
  FData := Emb;
end;

function TAiEmbeddings.CreateEmbedding(aInput, aUser: string;
    aDimensions: Integer; aModel, aEncodingFormat: string): TAiEmbeddingData;
var
  Client    : TFPHTTPClient;
  ReqBody   : TJSONObject;
  ReqStream : TStringStream;
  RespStream: TStringStream;
  RespJSON  : TJSONObject;
  sUrl      : string;
  sApiKey   : string;
begin
  // Delegar a evento personalizado si está asignado
  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  if aModel = ''      then aModel      := FModel;
  if aDimensions <= 0 then aDimensions := FDimensions;
  sApiKey := GetApiKey;

  // URL base sin barra final + /embeddings
  sUrl := FUrl;
  while (Length(sUrl) > 0) and (sUrl[Length(sUrl)] = '/') do
    SetLength(sUrl, Length(sUrl) - 1);
  sUrl := sUrl + '/embeddings';

  ReqBody := TJSONObject.Create;
  try
    ReqBody.Add('input',           aInput);
    ReqBody.Add('prompt',          aInput);   // compatibilidad Ollama
    ReqBody.Add('model',           aModel);
    ReqBody.Add('user',            aUser);
    ReqBody.Add('dimensions',      TJSONIntegerNumber.Create(aDimensions));
    ReqBody.Add('encoding_format', aEncodingFormat);

    ReqStream  := TStringStream.Create(ReqBody.AsJSON);
    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + sApiKey);
      Client.AddHeader('Content-Type',  'application/json');
      Client.RequestBody := ReqStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      RespJSON := TJSONObject(GetJSON(RespStream.DataString));
      try
        ParseEmbedding(RespJSON);
        Result := FData;
      finally
        RespJSON.Free;
      end;
    finally
      Client.Free;
      ReqStream.Free;
      RespStream.Free;
    end;
  finally
    ReqBody.Free;
  end;
end;

function TAiEmbeddings.GetApiKey: string;
begin
  if (Length(FApiKey) > 1) and (FApiKey[1] = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiEmbeddings.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAiEmbeddings.SetUrl(const Value: string);
begin
  if Value <> '' then FUrl := Value
  else FUrl := GlOpenAIUrl;
end;

class function TAiEmbeddings.GetDriverName: string;
begin
  Result := '';
end;

class function TAiEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiEmbeddings.Create(aOwner);
end;

class procedure TAiEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey']     := '@OPENAI_API_KEY';
  Params.Values['Url']        := GlOpenAIUrl;
  Params.Values['Model']      := 'text-embedding-3-small';
  Params.Values['Dimensions'] := '1536';
end;

end.
