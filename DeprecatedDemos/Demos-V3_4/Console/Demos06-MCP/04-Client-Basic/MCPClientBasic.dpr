program MCPClientBasic;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 06-MCP / 04-Client-Basic
// =============================================================================
// Cliente MCP basico que se conecta via StdIO al servidor compilado en
// 01-Server-StdIO y ejecuta sus herramientas directamente.
//
// Conceptos que cubre:
//   - TMCPClientStdIo: cliente MCP para conexion via stdin/stdout
//   - Client.Params.Values['Command']: ruta al ejecutable del servidor
//   - Client.Initialize: handshake MCP (protocolo de inicio)
//   - Client.ListTools: listar herramientas del servidor
//   - Client.CallTool(name, args, mediaList): llamar una herramienta
//   - Parsear la respuesta MCP (array "content" -> type:text -> text)
//
// Prerequisito: compilar 01-Server-StdIO primero.
//   El ejecutable debe estar en: Win64\Release\MCPServerStdIO.exe
//   relativo al directorio padre del demo.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  uMakerAi.MCPClient.Core,
  uMakerAi.Core;

const
  // Ruta al servidor compilado (01-Server-StdIO), relativa al ejecutable
  SERVER_EXE_REL = '..\01-Server-StdIO\Win64\Release\MCPServerStdIO.exe';

// =============================================================================
//  Helpers
// =============================================================================
procedure Sep(const Title: String = '');
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

// Extrae el texto de la respuesta MCP (content array -> type:text -> text)
function ExtraerTexto(AResult: TJSONObject): String;
var
  ContentArr: TJSONArray;
  Item      : TJSONValue;
  ItemObj   : TJSONObject;
  ItemType, ItemText: String;
begin
  Result := '';
  if not Assigned(AResult) then Exit;
  if AResult.TryGetValue<TJSONArray>('content', ContentArr) then
    for Item in ContentArr do
      if Item is TJSONObject then
      begin
        ItemObj := TJSONObject(Item);
        ItemObj.TryGetValue<String>('type', ItemType);
        if ItemType = 'text' then
        begin
          ItemObj.TryGetValue<String>('text', ItemText);
          if Result <> '' then Result := Result + #10;
          Result := Result + ItemText;
        end;
      end;
end;

// =============================================================================
//  Prueba principal
// =============================================================================
procedure RunTest;
var
  Client    : TMCPClientStdIo;
  MediaList : TObjectList<TAiMediaFile>;
  ToolsJson : TJSONObject;
  ToolsArr  : TJSONArray;
  Tool      : TJSONValue;
  ToolName, ToolDesc: String;
  Args      : TJSONObject;
  Resp      : TJSONObject;
  ServerPath: String;
begin
  Sep('Demo 04 - MCPClientBasic');

  // Resolver la ruta del servidor relativa al ejecutable actual
  ServerPath := ExpandFileName(ExtractFilePath(ParamStr(0)) + SERVER_EXE_REL);
  Writeln('Servidor: ', ServerPath);
  Writeln;

  if not TFile.Exists(ServerPath) then
  begin
    Writeln('AVISO: Servidor no encontrado en: ', ServerPath);
    Writeln('Compila primero 01-Server-StdIO (Win64 Release).');
    Writeln;
    Writeln('Continuando demo con descripcion de pasos...');
    Writeln;
    Sep('PASO 1: Inicializar conexion StdIO');
    Writeln('  Client.Params.Values[''Command''] := ServerPath');
    Writeln('  Client.Initialize  // handshake MCP');
    Sep('PASO 2: Listar herramientas');
    Writeln('  ToolsJson := Client.ListTools');
    Sep('PASO 3: Llamar herramienta get_datetime');
    Writeln('  Args := TJSONObject.Create;');
    Writeln('  Args.AddPair(''format'', ''readable'');');
    Writeln('  Resp := Client.CallTool(''get_datetime'', Args, MediaList)');
    Sep('PASO 4: Llamar herramienta greet');
    Writeln('  Args.AddPair(''name'', ''MakerAI'');');
    Writeln('  Args.AddPair(''language'', ''es'');');
    Writeln('  Resp := Client.CallTool(''greet'', Args, MediaList)');
    Exit;
  end;

  Client    := TMCPClientStdIo.Create(nil);
  MediaList := TObjectList<TAiMediaFile>.Create(True);
  try
    // Paso 1: Configurar y conectar
    Client.Params.Values['Command'] := ServerPath;
    Client.Params.Values['Timeout'] := '10000';

    Sep('PASO 1: Initialize (handshake MCP)');
    if not Client.Initialize then
    begin
      Writeln('ERROR: No se pudo inicializar el servidor MCP.');
      Exit;
    end;
    Writeln('Inicializacion OK.');

    // Paso 2: Listar herramientas disponibles
    Sep('PASO 2: ListTools');
    ToolsJson := Client.ListTools;
    try
      if Assigned(ToolsJson) then
      begin
        if ToolsJson.TryGetValue<TJSONArray>('tools', ToolsArr) then
        begin
          Writeln(Format('Herramientas encontradas: %d', [ToolsArr.Count]));
          for Tool in ToolsArr do
            if Tool is TJSONObject then
            begin
              TJSONObject(Tool).TryGetValue<String>('name',        ToolName);
              TJSONObject(Tool).TryGetValue<String>('description', ToolDesc);
              Writeln(Format('  [%s] %s', [ToolName, ToolDesc]));
            end;
        end;
      end;
    finally
      ToolsJson.Free;
    end;

    // Paso 3: Llamar get_datetime
    Sep('PASO 3: CallTool — get_datetime');
    Args := TJSONObject.Create;
    try
      Args.AddPair('format', 'readable');
      Resp := Client.CallTool('get_datetime', Args, MediaList);
      try
        if Assigned(Resp) then
          Writeln('Resultado: ', ExtraerTexto(Resp))
        else
          Writeln('ERROR: CallTool retorno nil');
      finally
        Resp.Free;
      end;
    finally
      Args.Free;
    end;

    // Paso 4: Llamar greet
    Sep('PASO 4: CallTool — greet');
    Args := TJSONObject.Create;
    try
      Args.AddPair('name',     'MakerAI Developer');
      Args.AddPair('language', 'es');
      Resp := Client.CallTool('greet', Args, MediaList);
      try
        if Assigned(Resp) then
          Writeln('Resultado: ', ExtraerTexto(Resp))
        else
          Writeln('ERROR: CallTool retorno nil');
      finally
        Resp.Free;
      end;
    finally
      Args.Free;
    end;

    Sep('Demo completado OK');

  finally
    MediaList.Free;
    Client.Free;
  end;
end;

begin
  try
    RunTest;
  except
    on E: Exception do
      Writeln('FATAL: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
