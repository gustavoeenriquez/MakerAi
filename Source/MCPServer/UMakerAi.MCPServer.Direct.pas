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


unit UMakerAi.MCPServer.Direct;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  UMakerAi.MCPServer.Core;

type
  TAiMCPDirectConnection = class(TAiMCPServer)
  private
    FRequestIDCounter: Integer;

    function ExecuteDirectRequest(const AMethod: string; AParams: TJSONObject): TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start; Override;
    procedure Stop; Override;

    // --- API de Alto Nivel ---
    function ListTools: TJSONObject;
    function ListResources: TJSONObject;
    function ReadResource(const AURI: string): TJSONObject;
    function CallTool(const AToolName: string; AArguments: TJSONObject): TJSONObject; overload;
    function CallTool(const AToolName: string; AArguments: TStrings): TJSONObject; overload;
  end;

procedure Register;

implementation

uses System.Generics.Collections;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMCPDirectConnection]);
end;

{ TAiMCPDirectConnection }

constructor TAiMCPDirectConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequestIDCounter := 0;
end;

destructor TAiMCPDirectConnection.Destroy;
begin
  inherited Destroy;
end;

procedure TAiMCPDirectConnection.Start;
begin
  inherited Start;
end;

procedure TAiMCPDirectConnection.Stop;
begin
  inherited Stop;
end;

function TAiMCPDirectConnection.ExecuteDirectRequest(const AMethod: string; AParams: TJSONObject): TJSONObject;
var
  RequestObj, ResponseJSON: TJSONObject;
  RequestStr, ResponseStr: string;
  JsonValue: TJSONValue;
  ResultValue: TJSONValue;
  ErrorObj: TJSONObject;
begin
  Result := nil;
  if not IsActive then
    raise Exception.Create('Direct Connection is not active. Call Start first.');

  // El dueño de AParams es este método, se libera aquí.
  // Si AParams es nil, creamos uno vacío.
  if not Assigned(AParams) then
    AParams := TJSONObject.Create
  else
    AParams.Owned := False; // Aseguramos que no se libere con su padre

  RequestObj := TJSONObject.Create;
  try
    Inc(FRequestIDCounter);
    RequestObj.AddPair('jsonrpc', '2.0');
    RequestObj.AddPair('id', FRequestIDCounter);
    RequestObj.AddPair('method', AMethod);
    RequestObj.AddPair('params', AParams); // AParams es ahora propiedad de RequestObj

    RequestStr := RequestObj.ToJSON;
  finally
    RequestObj.Free; // Libera RequestObj y AParams
  end;

  // Ejecutar la petición
  ResponseStr := FLogicServer.ExecuteRequest(RequestStr, 'direct_connection');

  // Parsear la respuesta
  if ResponseStr = '' then
    Exit; // Notificación, sin resultado

  JsonValue := TJSONObject.ParseJSONValue(ResponseStr);
  if not(JsonValue is TJSONObject) then
  begin
    JsonValue.Free;
    raise Exception.Create('Invalid JSON-RPC response from server logic.');
  end;

  ResponseJSON := TJSONObject(JsonValue);
  try
    // Comprobar si hay un error en la respuesta JSON-RPC
    if ResponseJSON.TryGetValue<TJSONObject>('error', ErrorObj) then
    begin
      raise Exception.CreateFmt('JSON-RPC Error: %s (Code: %d)', [ErrorObj.GetValue<string>('message', 'Unknown'),
        ErrorObj.GetValue<Integer>('code', 0)]);
    end;

    // Extraer el resultado
    if ResponseJSON.TryGetValue('result', ResultValue) then
    begin
      // Clonamos el resultado para que el llamador sea el dueño
      Result := ResultValue.Clone as TJSONObject;
    end;
  finally
    ResponseJSON.Free;
  end;
end;

function TAiMCPDirectConnection.ListTools: TJSONObject;
begin
  Result := ExecuteDirectRequest('tools/list', nil);
end;

function TAiMCPDirectConnection.ListResources: TJSONObject;
begin
  Result := ExecuteDirectRequest('resources/list', nil);
end;

function TAiMCPDirectConnection.ReadResource(const AURI: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('uri', AURI);
  Result := ExecuteDirectRequest('resources/read', Params);
end;

function TAiMCPDirectConnection.CallTool(const AToolName: string; AArguments: TJSONObject): TJSONObject;
var
  Params: TJSONObject;
begin
  // AArguments es pasado al método ExecuteDirectRequest, que tomará posesión de él.
  // El llamador no debe liberar AArguments.
  if not Assigned(AArguments) then
    AArguments := TJSONObject.Create;

  Params := TJSONObject.Create;
  Params.AddPair('name', AToolName);
  Params.AddPair('arguments', AArguments);

  Result := ExecuteDirectRequest('tools/call', Params);
end;

function TAiMCPDirectConnection.CallTool(const AToolName: string; AArguments: TStrings): TJSONObject;
var
  ArgsObject: TJSONObject;
  i: Integer;
begin
  ArgsObject := TJSONObject.Create;
  if Assigned(AArguments) then
  begin
    for i := 0 to AArguments.Count - 1 do
    begin
      ArgsObject.AddPair(AArguments.Names[i], AArguments.ValueFromIndex[i]);
    end;
  end;
  // Llamamos a la versión principal. A partir de aquí, no debemos liberar ArgsObject.
  Result := CallTool(AToolName, ArgsObject);
end;

end.
