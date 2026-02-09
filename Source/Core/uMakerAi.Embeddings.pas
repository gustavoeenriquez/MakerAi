// MIT License
//
// Copyright (c) <year> <copyright holders>
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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMakerAi.Embeddings;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  // FPC: Unidades estándar de FPC sin prefijo System
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math,
  {$ELSE}
  // Delphi: Unidades con namespace System, incluye Net/HTTP/JSON/REST nativos
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.NetConsts,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,

  {$ENDIF}
  uMakerAi.Embeddings.Core,
  uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper, uRttiHelper;

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
    // Este método es ahora 'override' para proporcionar la implementación específica.
  public
    constructor Create(aOwner: TComponent); override;
    // Este método es específico de la implementación de OpenAI
    procedure ParseEmbedding(JObj: TJsonObject); Virtual;
    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; override;
  published
    // Propiedades específicas de esta implementación
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
var
  JArrData, JArrVector: TJSONArray;
  Emb: TAiEmbeddingData;
  JVal, LItem: TJSONValue;
  Usage: TJSONObject;
  i: Integer;
begin
  // Validación inicial
  if not Assigned(JObj) then
    Exit;

  // 1. Obtener el modelo
  JObj.TryGetValue('model', FModel);

  // 2. Uso de tokens (opcional)
  if JObj.TryGetValue('usage', Usage) then
  begin
    Usage.TryGetValue('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue('total_tokens', Ftotal_tokens);
  end;

  // 3. Obtener el array 'data' con validación
  if not JObj.TryGetValue('data', JArrData) then
    raise Exception.Create('La respuesta de la API no contiene el array de datos esperado ("data").');

  if JArrData.Count = 0 then
    raise Exception.Create('El array de datos ("data") está vacío.');

  // Preparar array para múltiples embeddings (aunque solo usemos el primero)
  SetLength(FData, JArrData.Count);

  // 4. Procesar el primer embedding (compatibilidad con versión original)
  for JVal in JArrData do
  begin
    // Validar que sea un objeto
    if not (JVal is TJSONObject) then
      raise Exception.Create('Formato de ítem de datos inválido en la respuesta JSON.');

    // 5. Obtener el vector de embedding
    if not TJSONObject(JVal).TryGetValue('embedding', JArrVector) then
      raise Exception.Create('No se encontró el campo "embedding" en los datos de respuesta.');

    if JArrVector.Count = 0 then
      raise Exception.Create('El vector de embedding está vacío.');

    // 6. Dimensionar y llenar el vector con validación de tipo
    SetLength(Emb, JArrVector.Count);
    for i := 0 to JArrVector.Count - 1 do
    begin
      LItem := JArrVector.Items[i];
      if LItem is TJSONNumber then
        Emb[i] := TJSONNumber(LItem).AsDouble
      else
        Emb[i] := 0.0;
    end;

    // 7. Asignar el embedding procesado
    FData := Emb;

    // Solo procesamos el primer elemento (comportamiento original)
    Break;
  end;
end;

function TAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  RequestBody, ResponseJSON: TJSONObject;
  Res: IHTTPResponse;
  ResponseStream: TStringStream;
  RequestStream: TStringStream;
  sUrl: String;
begin
  // Delegación a evento si está asignado
  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  Client := TNetHTTPClient.Create(nil);
  Client.ConfigureForAsync;
  RequestStream := TStringStream.Create('', TEncoding.UTF8);
  ResponseStream := TStringStream.Create('', TEncoding.UTF8);
  RequestBody := TJSONObject.Create;

  try
    // Construir URL limpiando barras finales
    sUrl := FUrl.TrimRight(['/']) + '/embeddings';

    // Configurar valores por defecto
    if aModel = '' then
      aModel := FModel;
    if aDimensions <= 0 then
      aDimensions := FDimensions;

    // Construcción del JSON de petición
    RequestBody.AddPair('input', aInput);     // OpenAI
    RequestBody.AddPair('prompt', aInput);    // Compatibilidad con Ollama
    RequestBody.AddPair('model', aModel);
    RequestBody.AddPair('user', aUser);
    RequestBody.AddPair('dimensions', CreateJSONNumber(aDimensions));
    RequestBody.AddPair('encoding_format', aEncodingFormat);

    // Escribir el JSON al stream
    RequestStream.WriteString(RequestBody.Format);
    RequestStream.Position := 0;

    // Configurar headers
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];

    Client.ContentType := 'application/json';

    // Realizar la petición
    Res := Client.Post(sUrl, RequestStream, ResponseStream, Headers);
    ResponseStream.Position := 0;

{$IFDEF APIDEBUG}
    ResponseStream.SaveToFile('c:\temp\response.txt');
{$ENDIF}

    if Res.StatusCode = 200 then
    begin
      // Parsear la respuesta JSON
      ResponseJSON := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        if not Assigned(ResponseJSON) then
          raise Exception.Create('La respuesta de la API no es un JSON válido.');

        ParseEmbedding(ResponseJSON);
        Result := Self.FData;
      finally
        ResponseJSON.Free;
      end;
    end
    else
    begin
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  finally
    Client.Free;
    RequestStream.Free;
    ResponseStream.Free;
    RequestBody.Free;
  end;
end;

function TAiEmbeddings.GetApiKey: String;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
  begin
    Result := FApiKey;
    Exit;
  end;
  if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := CompatGetEnvVar(Copy(FApiKey, 2, Length(FApiKey)))
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
