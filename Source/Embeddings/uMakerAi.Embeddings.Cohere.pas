unit uMakerAi.Embeddings.Cohere;

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
  System.Generics.Collections,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core;

type
  TAiCohereInputType = (citSearchDocument, citSearchQuery, citClassification, citClustering);

  TAiCohereEmbeddings = class(TAiEmbeddings)
  private
    FInputType: TAiCohereInputType;
    function GetInputTypeAsString: string;
  protected
    // Este método es para procesar la respuesta específica de Cohere.
    procedure ParseCohereEmbedding(jObj: TJSonObject);
  public
    constructor Create(aOwner: TComponent); override;
    // Sobrescribimos el método principal para la implementación de Cohere.
    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  published
    property InputType: TAiCohereInputType read FInputType write FInputType;
  end;

procedure Register;

implementation


procedure Register;
Begin
  RegisterComponents('MakerAI', [TAiCohereEmbeddings]);
End;



{ TAiCohereEmbeddings }

constructor TAiCohereEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  FInputType := citSearchQuery;
end;

function TAiCohereEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  JTexts, JEmbeddingTypes: TJSONArray;
  Res: IHTTPResponse;
  ResponseStream: TStringStream;
  BodyStream: TStringStream;
  sUrl: String;
  LModel: string;
  LResponseJson: TJSonObject;
begin
  // Si el evento está asignado, se delega la lógica (comportamiento de la clase base)
  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  Client := TNetHTTPClient.Create(Nil);
  BodyStream := TStringStream.Create('', TEncoding.UTF8);
  ResponseStream := TStringStream.Create('', TEncoding.UTF8);
  jObj := TJSonObject.Create;
  try
    sUrl := FUrl + 'embed';

    if aModel <> '' then
      LModel := aModel
    else
      LModel := FModel;

    // 1. Construir el cuerpo de la petición JSON
    jObj.AddPair('model', LModel);
    jObj.AddPair('input_type', GetInputTypeAsString);

    // La API espera un array de textos. Creamos uno con el único input.
    JTexts := TJSONArray.Create;
    JTexts.Add(aInput);
    jObj.AddPair('texts', JTexts);

    // Solicitamos solo embeddings de tipo 'float'
    JEmbeddingTypes := TJSONArray.Create;
    JEmbeddingTypes.Add('float');
    jObj.AddPair('embedding_types', JEmbeddingTypes);

    // Opcional: Cohere no usa 'dimensions' como OpenAI, sino 'output_dimension'.
    // Lo añadimos si es un valor válido para Cohere.
    if (aDimensions = 256) or (aDimensions = 512) or (aDimensions = 1024) or (aDimensions = 1536) then
    begin
      jObj.AddPair('output_dimension', aDimensions);
    end
    else if (aDimensions > 0) then
    begin
      // Opcional: Lanzar un warning o un error si el usuario especifica una dimensión
      // que no es válida para este modelo, para evitar confusiones.
      // Por ahora, simplemente lo ignoramos.
    end;

    BodyStream.WriteString(jObj.ToString);
    BodyStream.Position := 0;

    // 2. Ejecutar la llamada a la API
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, BodyStream, ResponseStream, Headers);
    ResponseStream.Position := 0;

    // 3. Procesar la respuesta
    if Res.StatusCode = 200 then
    begin
      // 1. Parsear el string de respuesta y castearlo a un TJSONObject.
      LResponseJson := TJSonObject.ParseJSONValue(ResponseStream.DataString) as TJSonObject;
      try
        // 2. Pasar el objeto JSON parseado directamente al método de parseo.
        ParseCohereEmbedding(LResponseJson);
        Result := Self.FData;
      finally
        // 3. Liberar la memoria del objeto JSON que creamos.
        LResponseJson.Free;
      end;
    end
    Else
    begin
      raise Exception.CreateFmt('Error en Cohere Embed API: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  finally
    Client.Free;
    BodyStream.Free;
    ResponseStream.Free;
    jObj.Free;
  end;
end;

function TAiCohereEmbeddings.GetInputTypeAsString: string;
begin
  case FInputType of
    citSearchDocument:
      Result := 'search_document';
    citSearchQuery:
      Result := 'search_query';
    citClassification:
      Result := 'classification';
    citClustering:
      Result := 'clustering';
  else
    Result := 'search_query'; // Default seguro
  end;
end;

procedure TAiCohereEmbeddings.ParseCohereEmbedding(jObj: TJSonObject);
var
  JEmbeddingsObj: TJSonObject;
  JFloatEmbeddingsArray: TJSONArray;
  JFirstEmbedding: TJSONArray;
  J: Integer;
  JMeta, JBilledUnits: TJSonObject;
begin
  // Limpiar datos anteriores
  SetLength(FData, 0);
  Fprompt_tokens := 0;
  Ftotal_tokens := 0;

  // Extraer el uso de tokens
  if jObj.TryGetValue<TJSonObject>('meta', JMeta) then
    if JMeta.TryGetValue<TJSonObject>('billed_units', JBilledUnits) then
      JBilledUnits.TryGetValue<Integer>('input_tokens', Fprompt_tokens);

  Ftotal_tokens := Fprompt_tokens; // En embeddings, total = prompt

  // Navegar la estructura de respuesta de Cohere
  if jObj.TryGetValue<TJSonObject>('embeddings', JEmbeddingsObj) then
  begin
    // Buscamos el array de embeddings de tipo 'float'
    if JEmbeddingsObj.TryGetValue<TJSONArray>('float', JFloatEmbeddingsArray) then
    begin
      // Como solo pedimos un texto, solo nos interesa el primer vector
      if JFloatEmbeddingsArray.Count > 0 then
      begin
        JFirstEmbedding := JFloatEmbeddingsArray.Items[0] as TJSONArray;
        if Assigned(JFirstEmbedding) then
        begin
          // Convertir el TJSONArray a nuestro TAiEmbeddingData (TArray<Double>)
          SetLength(FData, JFirstEmbedding.Count);
          for J := 0 to JFirstEmbedding.Count - 1 do
            FData[J] := JFirstEmbedding.Items[J].GetValue<Double>;
        end;
      end;
    end;
  end;
end;

{ TAiCohereEmbeddings - Factory class methods }

class function TAiCohereEmbeddings.GetDriverName: string;
begin
  Result := 'Cohere';
end;

class function TAiCohereEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiCohereEmbeddings.Create(aOwner);
end;

class procedure TAiCohereEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@COHERE_API_KEY';
  Params.Values['Url'] := 'https://api.cohere.com/v2/';
  Params.Values['Model'] := 'embed-english-v3.0';
  Params.Values['Dimensions'] := '1024';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiCohereEmbeddings);

end.
