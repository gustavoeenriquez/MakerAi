program MCPClientTest;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI - Demo 067: Cliente de prueba para FileSystemMCP (Demo 037)
// =============================================================================
// Conecta via StdIO al servidor MCP compilado en Demo 037,
// lista las herramientas disponibles y las ejecuta:
//   - ReadDirectory: lista contenido de un directorio
//   - CreateDirectory: crea un directorio nuevo
//
// Asegurate de que el servidor este compilado:
//   C:\mcp\servers\FileSystemMCP.exe
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  uMakerAi.MCPClient.Core,
  uMakerAi.Core;

const
  MCP_SERVER_EXE = 'C:\mcp\servers\FileSystemMCP.exe';
  TEST_LIST_DIR  = 'C:\';
  TEST_NEW_DIR   = 'C:\mcp\test_demo067';

// -----------------------------------------------------------------------------
// Helpers de presentacion
// -----------------------------------------------------------------------------

procedure PrintSep(const Title: string = '');
begin
  if Title = '' then
    Writeln(StringOfChar('-', 60))
  else
  begin
    Writeln;
    Writeln(StringOfChar('=', 60));
    Writeln('  ', Title);
    Writeln(StringOfChar('=', 60));
  end;
end;

procedure PrintJSON(const Label_: string; AJson: TJSONObject);
begin
  if Assigned(AJson) then
    Writeln(Label_, ': ', AJson.Format(2))
  else
    Writeln(Label_, ': <nil>');
end;

// -----------------------------------------------------------------------------
// Extrae el texto de la respuesta MCP (array "content" -> type:text -> text)
// -----------------------------------------------------------------------------
function ExtractTextFromMCPResult(AResult: TJSONObject): string;
var
  ContentArr: TJSONArray;
  Item: TJSONValue;
  ItemObj: TJSONObject;
  ItemType, ItemText: string;
begin
  Result := '';
  if not Assigned(AResult) then Exit;

  if AResult.TryGetValue<TJSONArray>('content', ContentArr) then
  begin
    for Item in ContentArr do
    begin
      if Item is TJSONObject then
      begin
        ItemObj := TJSONObject(Item);
        ItemObj.TryGetValue<string>('type', ItemType);
        if ItemType = 'text' then
        begin
          ItemObj.TryGetValue<string>('text', ItemText);
          if Result <> '' then Result := Result + #10;
          Result := Result + ItemText;
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Prueba principal
// -----------------------------------------------------------------------------
procedure RunTest;
var
  Client   : TMCPClientStdIo;
  MediaList: TObjectList<TAiMediaFile>;
  ToolsJson: TJSONObject;
  ToolsArr : TJSONArray;
  Tool     : TJSONValue;
  ToolName, ToolDesc: string;
  Args     : TJSONObject;
  Result_  : TJSONObject;
  Output   : string;
begin
  PrintSep('Demo 067 - MCP Client Test (FileSystemMCP)');
  Writeln('Servidor: ', MCP_SERVER_EXE);
  Writeln;

  Client    := TMCPClientStdIo.Create(nil);
  MediaList := TObjectList<TAiMediaFile>.Create(True);
  try
    // ── 1. Configurar la conexion StdIO ──────────────────────────────────────
    Client.Params.Values['Command'] := MCP_SERVER_EXE;
    Client.Params.Values['Timeout'] := '10000';

    // ── 2. Inicializar (handshake MCP) ───────────────────────────────────────
    PrintSep('PASO 1: Initialize');
    if not Client.Initialize then
    begin
      Writeln('ERROR: No se pudo inicializar el servidor MCP.');
      Writeln('  Verifica que exista: ', MCP_SERVER_EXE);
      Exit;
    end;
    Writeln('Inicializacion OK.');

    // ── 3. Listar herramientas ───────────────────────────────────────────────
    PrintSep('PASO 2: tools/list');
    ToolsJson := Client.ListTools;
    try
      if not Assigned(ToolsJson) then
      begin
        Writeln('ERROR: ListTools devolvio nil.');
        Exit;
      end;

      if ToolsJson.TryGetValue<TJSONArray>('tools', ToolsArr) then
      begin
        Writeln(Format('Herramientas encontradas: %d', [ToolsArr.Count]));
        PrintSep;
        for Tool in ToolsArr do
        begin
          if Tool is TJSONObject then
          begin
            TJSONObject(Tool).TryGetValue<string>('name',        ToolName);
            TJSONObject(Tool).TryGetValue<string>('description', ToolDesc);
            Writeln(Format('  [%s]  %s', [ToolName, ToolDesc]));
          end;
        end;
      end
      else
        Writeln('Respuesta: ', ToolsJson.Format(2));
    finally
      ToolsJson.Free;
    end;

    // ── 4. Llamar ReadDirectory ──────────────────────────────────────────────
    PrintSep(Format('PASO 3: ReadDirectory("%s")', [TEST_LIST_DIR]));
    Args := TJSONObject.Create;
    try
      Args.AddPair('DirectoryPath', TEST_LIST_DIR);
      Result_ := Client.CallTool('ReadDirectory', Args, MediaList);
    finally
      Args.Free;
    end;
    try
      Output := ExtractTextFromMCPResult(Result_);
      if Output <> '' then
        Writeln(Output)
      else if Assigned(Result_) then
        Writeln(Result_.Format(2))
      else
        Writeln('ERROR: resultado nil');
    finally
      Result_.Free;
    end;

    // ── 5. Llamar CreateDirectory ────────────────────────────────────────────
    PrintSep(Format('PASO 4: CreateDirectory("%s")', [TEST_NEW_DIR]));
    Args := TJSONObject.Create;
    try
      Args.AddPair('DirectoryPath', TEST_NEW_DIR);
      Result_ := Client.CallTool('CreateDirectory', Args, MediaList);
    finally
      Args.Free;
    end;
    try
      Output := ExtractTextFromMCPResult(Result_);
      if Output <> '' then
        Writeln(Output)
      else if Assigned(Result_) then
        Writeln(Result_.Format(2))
      else
        Writeln('ERROR: resultado nil');
    finally
      Result_.Free;
    end;

    // ── 6. Verificar que el directorio fue creado ────────────────────────────
    PrintSep('PASO 5: Verificacion');
    if System.SysUtils.DirectoryExists(TEST_NEW_DIR) then
      Writeln('OK - Directorio creado exitosamente: ', TEST_NEW_DIR)
    else
      Writeln('AVISO - El directorio no existe (puede ser restriccion de permisos)');

    // ── 7. Desconectar ───────────────────────────────────────────────────────
    Client.Disconnect;
    Writeln;
    Writeln('Servidor desconectado.');

  finally
    MediaList.Free;
    Client.Free;
  end;
end;

// =============================================================================
begin
  try
    RunTest;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
