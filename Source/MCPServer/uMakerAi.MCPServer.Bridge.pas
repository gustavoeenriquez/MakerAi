// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Bridge: adapta TAiFunctions/TFunctionActionItem → IAiMCPTool.
// Permite exponer las funciones locales de TAiFunctions como herramientas MCP.
//
// Adaptaciones respecto a la version Delphi:
//   - TJSONObject.TryGetValue<TJSONObject> → Find + is TJSONObject
//   - LToolCall.Response.IsEmpty          → LToolCall.Response = ''
//   - LToolCall.Response.Trim.StartsWith  → Copy(Trim(...),1,10)
//   - var LParsed (inline)                → declaracion en var block
//   - LResMsg.MediaFiles                  → compatibilidad con TAiChatMessages FPC

unit uMakerAi.MCPServer.Bridge;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPServer.Core,
  uMakerAi.Chat.Messages;

type
  // -------------------------------------------------------------------------
  // TTAiFunctionToolProxy
  //
  // Implementa IAiMCPTool para exponer cualquier funcion de TAiFunctions
  // (local o remota de otro MCP) a traves de nuestro servidor MCP.
  // -------------------------------------------------------------------------
  TTAiFunctionToolProxy = class(TInterfacedObject, IAiMCPTool)
  private
    FFunctionItem : TFunctionActionItem;
    FAiFunctions  : TAiFunctions;

    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject;

  public
    constructor Create(AAiFunctions: TAiFunctions;
        AItem: TFunctionActionItem);
    destructor Destroy; override;

    function Execute(const Arguments: TJSONObject;
        const AuthContext: TAiAuthContext): TJSONObject;

    property Name        : string      read GetName;
    property Description : string      read GetDescription;
    property InputSchema : TJSONObject read GetInputSchema;
  end;

implementation

// ---------------------------------------------------------------------------
// TTAiFunctionToolProxy
// ---------------------------------------------------------------------------

constructor TTAiFunctionToolProxy.Create(AAiFunctions: TAiFunctions;
    AItem: TFunctionActionItem);
begin
  inherited Create;
  FAiFunctions  := AAiFunctions;
  FFunctionItem := AItem;
end;

destructor TTAiFunctionToolProxy.Destroy;
begin
  // No liberamos FFunctionItem ni FAiFunctions — no somos sus duenos
  inherited;
end;

function TTAiFunctionToolProxy.GetName: string;
begin
  Result := FFunctionItem.FunctionName;
end;

function TTAiFunctionToolProxy.GetDescription: string;
begin
  Result := Trim(FFunctionItem.Description.Text);
end;

function TTAiFunctionToolProxy.GetInputSchema: TJSONObject;
var
  LFullOpenAiJson : TJSONObject;
  LFuncNode       : TJSONData;
  LParamsNode     : TJSONData;
begin
  Result := nil;

  // Obtenemos la definicion completa que genera el componente local.
  // Formato: {"type":"function","function":{"name":"...","parameters":{...}}}
  LFullOpenAiJson := FFunctionItem.ToJSon(False);

  if not Assigned(LFullOpenAiJson) then
  begin
    Result := TJSONObject.Create;
    Result.Add('type', TJSONString.Create('object'));
    Result.Add('properties', TJSONObject.Create);
    Exit;
  end;

  try
    // Navegar hasta "function" → "parameters"
    LFuncNode := LFullOpenAiJson.Find('function');
    if Assigned(LFuncNode) and (LFuncNode is TJSONObject) then
    begin
      LParamsNode := TJSONObject(LFuncNode).Find('parameters');
      if Assigned(LParamsNode) and (LParamsNode is TJSONObject) then
        Result := TJSONObject(LParamsNode.Clone);
    end;

    // Si no tiene parametros, devolvemos un schema vacio valido
    if not Assigned(Result) then
    begin
      Result := TJSONObject.Create;
      Result.Add('type', TJSONString.Create('object'));
      Result.Add('properties', TJSONObject.Create);
    end;

  finally
    LFullOpenAiJson.Free;
  end;
end;

function TTAiFunctionToolProxy.Execute(const Arguments: TJSONObject;
    const AuthContext: TAiAuthContext): TJSONObject;
var
  LToolCall  : TAiToolsFunction;
  LBuilder   : TAiMCPResponseBuilder;
  LResMsg    : TAIChatMessage;
  LParsed    : TJSONData;
  I          : Integer;
  TrimmedResp: string;
begin
  Result   := nil;
  LBuilder := TAiMCPResponseBuilder.New;
  LToolCall := TAiToolsFunction.Create;
  LResMsg  := TAIChatMessage.Create('', '');
  try
    // 1. Preparar la llamada
    LToolCall.name := FFunctionItem.FunctionName;
    if Assigned(Arguments) then
      LToolCall.Arguments := Arguments.AsJSON;

    // El mensaje temporal captura posibles mediafiles generados
    LToolCall.ResMsg := LResMsg;

    // 2. Ejecutar el motor de TAiFunctions
    if FAiFunctions.DoCallFunction(LToolCall) then
    begin
      // 3. Procesar respuesta de texto
      TrimmedResp := Trim(LToolCall.Response);
      if LToolCall.Response <> '' then
      begin
        // Si la respuesta ya es un JSON MCP con 'content', lo usamos directamente
        if Copy(TrimmedResp, 1, 10) = '{"content"' then
        begin
          LParsed := GetJSON(LToolCall.Response);
          if Assigned(LParsed) and (LParsed is TJSONObject) then
          begin
            Result := TJSONObject(LParsed);
            // Result ya listo — no usar builder para el texto
          end
          else
          begin
            if Assigned(LParsed) then LParsed.Free;
            LBuilder.AddText(LToolCall.Response);
          end;
        end
        else
          LBuilder.AddText(LToolCall.Response);
      end;

      // 4. Procesar MediaFiles (imagenes, audio, etc.)
      if Assigned(LResMsg.MediaFiles) and (LResMsg.MediaFiles.Count > 0) then
      begin
        for I := 0 to LResMsg.MediaFiles.Count - 1 do
        begin
          LBuilder.AddFileFromStream(
              LResMsg.MediaFiles[I].Content,
              LResMsg.MediaFiles[I].Filename,
              LResMsg.MediaFiles[I].MimeType);
        end;
      end;

      // Construir resultado si aun no lo tenemos
      if not Assigned(Result) then
        Result := LBuilder.Build;
    end
    else
    begin
      // La funcion devolvio False — error
      Result := TAiMCPResponseBuilder.New
          .SetError('La ejecucion de la herramienta no devolvio resultado exitoso.')
          .Build;
    end;

  finally
    LToolCall.Free;
    LResMsg.Free;
    LBuilder.Free;
  end;
end;

end.
