// MIT License
// MakerAI - Demo 065: PPM Console Test
// Prueba aislada del flujo PPM: search → download → extract → initialize
// Ejecutar desde consola: PPMConsoleTest.exe

program PPMConsoleTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Zip,
  System.Net.HttpClient, System.Net.URLClient,
  System.JSON, System.Math, System.Generics.Collections,
  uMakerAi.Core,
  uMakerAi.Tools.Functions,
  uMakerAi.MCPClient.Core,
  uMakerAi.Chat.Initializations;

// ---------------------------------------------------------------------------
// Helpers de output
// ---------------------------------------------------------------------------
procedure Log(const S: string); begin Writeln(S); end;
procedure Sep; begin Writeln(StringOfChar('-', 60)); end;
procedure OK(const S: string); begin Writeln('[OK]  ' + S); end;
procedure ERR(const S: string); begin Writeln('[ERR] ' + S); end;
procedure INFO(const S: string); begin Writeln('[..] ' + S); end;

// ---------------------------------------------------------------------------
// Test 1: Verificar conectividad con el registry
// ---------------------------------------------------------------------------
procedure TestHealth;
var
  LClient: THTTPClient;
  LResp: IHTTPResponse;
begin
  Sep;
  Log('TEST 1 — Health check del registry');
  LClient := THTTPClient.Create;
  try
    try
      LResp := LClient.Get('https://ppm.pascalai.org/v1/health');
      if LResp.StatusCode = 200 then
        OK('Registry accesible. Respuesta: ' + LResp.ContentAsString(TEncoding.UTF8))
      else
        ERR('Status: ' + IntToStr(LResp.StatusCode));
    except
      on E: Exception do
        ERR('Excepcion: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LClient.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Test 2: Buscar herramientas MCP
// ---------------------------------------------------------------------------
procedure TestSearch(const AQuery: string);
var
  LFunctions: TAiFunctions;
  LResult: TJSONObject;
  LTools: TJSONArray;
  I: Integer;
begin
  Sep;
  Log('TEST 2 — SearchPPMMCP("' + AQuery + '")');
  LFunctions := TAiFunctions.Create(nil);
  try
    // OnLog requiere 'of object' — se omite en console test
    try
      LResult := LFunctions.SearchPPMMCP(AQuery);
      if not Assigned(LResult) then
      begin
        ERR('SearchPPMMCP devolvio nil');
        Exit;
      end;
      try
        Log('JSON recibido (primeros 300 chars): ' +
            Copy(LResult.ToJSON, 1, 300));

        if LResult.TryGetValue<TJSONArray>('tools', LTools) then
        begin
          OK(Format('Encontradas %d herramientas', [LTools.Count]));
          for I := 0 to Min(LTools.Count - 1, 4) do
          begin
            var LTool := LTools.Items[I] as TJSONObject;
            var LName, LDesc: string;
            LTool.TryGetValue<string>('name', LName);
            LTool.TryGetValue<string>('description', LDesc);
            Log(Format('  [%d] %s — %s', [I, LName, Copy(LDesc, 1, 60)]));
          end;
        end
        else
          ERR('No se encontro campo "tools" en la respuesta');
      finally
        LResult.Free;
      end;
    except
      on E: Exception do
        ERR('Excepcion en SearchPPMMCP: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LFunctions.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Test 3: Resolver versión de un paquete
// ---------------------------------------------------------------------------
procedure TestResolveVersion(const AName: string);
var
  LClient: THTTPClient;
  LResp: IHTTPResponse;
  LJson, LPackage, LVer: TJSONObject;
  LVersions: TJSONArray;
  LBody, LVersion: string;
begin
  Sep;
  Log('TEST 3 — Resolver versión de "' + AName + '"');
  LClient := THTTPClient.Create;
  try
    try
      LResp := LClient.Get('https://ppm.pascalai.org/v1/packages/' + AName);
      Log('Status: ' + IntToStr(LResp.StatusCode));
      LBody := LResp.ContentAsString(TEncoding.UTF8);
      Log('Body (primeros 400): ' + Copy(LBody, 1, 400));

      if LResp.StatusCode = 200 then
      begin
        LJson := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
        if Assigned(LJson) then
        try
          if LJson.TryGetValue<TJSONObject>('package', LPackage) and
             LPackage.TryGetValue<TJSONArray>('versions', LVersions) and
             (LVersions.Count > 0) then
          begin
            LVer := LVersions.Items[0] as TJSONObject;
            LVer.TryGetValue<string>('version', LVersion);
            OK('Version resuelta: ' + LVersion);
          end
          else
            ERR('Estructura JSON inesperada');
        finally
          LJson.Free;
        end
        else
          ERR('No se pudo parsear JSON');
      end;
    except
      on E: Exception do
        ERR('Excepcion: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LClient.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Test 4: Descargar .paipkg y listar su contenido
// ---------------------------------------------------------------------------
procedure TestDownloadAndInspect(const AName: string);
var
  LClient: THTTPClient;
  LStream: TFileStream;
  LResp: IHTTPResponse;
  LUrl, LTempFile: string;
  LZip: TZipFile;
  I: Integer;
begin
  Sep;
  Log('TEST 4 — Download + inspeccion de "' + AName + '"');

  // Primero resolver versión manualmente para el test
  LUrl := Format('https://ppm.pascalai.org/v1/packages/%s/latest/download', [AName]);
  // Intentar con "latest" como alias
  LTempFile := TPath.Combine(TPath.GetTempPath, AName + '-test.paipkg');

  INFO('Descargando desde: ' + LUrl);
  LClient := THTTPClient.Create;
  try
    try
      LStream := TFileStream.Create(LTempFile, fmCreate);
      try
        LResp := LClient.Get(LUrl, LStream);
        Log('Status descarga: ' + IntToStr(LResp.StatusCode) +
            ' | Bytes: ' + IntToStr(LStream.Size));
      finally
        LStream.Free;
      end;

      if (LResp.StatusCode = 200) and TFile.Exists(LTempFile) and
         (TFile.GetSize(LTempFile) > 0) then
      begin
        OK('Archivo descargado: ' + LTempFile);
        // Inspeccionar el ZIP
        LZip := TZipFile.Create;
        try
          try
            LZip.Open(LTempFile, zmRead);
            Log('Entradas en el ZIP:');
            for I := 0 to LZip.FileCount - 1 do
              Log('  ' + LZip.FileName[I]);
            LZip.Close;
          except
            on E: Exception do
              ERR('Error al abrir ZIP: ' + E.ClassName + ' — ' + E.Message);
          end;
        finally
          LZip.Free;
        end;
      end
      else
        ERR('Descarga fallida o archivo vacio');
    except
      on E: Exception do
        ERR('Excepcion en descarga: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LClient.Free;
    if TFile.Exists(LTempFile) then
      TFile.Delete(LTempFile);
  end;
end;

// ---------------------------------------------------------------------------
// Test 5: InstallMCPFromPPM completo
// ---------------------------------------------------------------------------
procedure TestInstall(const AName: string);
var
  LFunctions: TAiFunctions;
  LItem: TMCPClientItem;
begin
  Sep;
  Log('TEST 5 — InstallMCPFromPPM("' + AName + '")');
  LFunctions := TAiFunctions.Create(nil);
  try
    // OnLog requiere 'of object' — se omite en console test
    try
      LItem := LFunctions.InstallMCPFromPPM(AName);
      if Assigned(LItem) then
      begin
        OK('Instalado. Exe: ' + LItem.Params.Values['Command']);
        INFO('Inicializando cliente MCP...');
        try
          if LItem.MCPClient.Initialize then
            OK('Cliente inicializado. Tools disponibles via MCP.')
          else
            ERR('No se pudo inicializar el cliente MCP');
        except
          on E: Exception do
            ERR('Excepcion en Initialize: ' + E.ClassName + ' — ' + E.Message);
        end;
      end
      else
        ERR('InstallMCPFromPPM devolvio nil');
    except
      on E: Exception do
        ERR('Excepcion en InstallMCPFromPPM: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LFunctions.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Test 6: CallTool directo (reproduce el "Invalid pointer operation")
// ---------------------------------------------------------------------------
procedure TestCallTool(const AName, AToolName: string);
var
  LFunctions: TAiFunctions;
  LItem: TMCPClientItem;
  LArgs: TJSONObject;
  LMedia: TObjectList<TAiMediaFile>;
  LResult: TJSONObject;
begin
  Sep;
  Log('TEST 6 — CallTool directo: ' + AName + ' -> ' + AToolName);
  LFunctions := TAiFunctions.Create(nil);
  try
    try
      // Instalar (o recuperar si ya existe)
      LItem := LFunctions.InstallMCPFromPPM(AName);
      if not Assigned(LItem) then
      begin
        ERR('No se pudo obtener el cliente para: ' + AName);
        Exit;
      end;
      OK('Cliente listo. Exe: ' + LItem.Params.Values['Command']);

      // Inicializar (obtiene lista de tools y marca Available=True)
      INFO('Llamando Initialize...');
      if not LItem.MCPClient.Initialize then
      begin
        ERR('Initialize devolvio False — el cliente no esta disponible');
        Exit;
      end;
      OK('Initialize OK. Available=' + BoolToStr(LItem.MCPClient.Available, True));

      // Llamar la tool directamente
      // mcp-postgres: tool = 'postgres', args = {connection:{...}, operation:'tables'}
      INFO('Llamando CallTool("' + AToolName + '")...');
      LMedia := TObjectList<TAiMediaFile>.Create(True);
      LArgs  := TJSONObject.Create;
      var LConn := TJSONObject.Create;
      LConn.AddPair('host',     'localhost');
      LConn.AddPair('port',     TJSONNumber.Create(5432));
      LConn.AddPair('database', 'postgres');
      LConn.AddPair('user',     'postgres');
      LConn.AddPair('password', 'masterkey');
      LArgs.AddPair('connection', LConn);
      LArgs.AddPair('operation',  'tables');
      try
        LResult := LItem.MCPClient.CallTool(AToolName, LArgs, LMedia);
        // NOTA: LArgs pasa ownership a CallTool — no llamar LArgs.Free
        if Assigned(LResult) then
        begin
          OK('CallTool exitoso. Respuesta (primeros 200 chars): ' +
             Copy(LResult.ToJSON, 1, 200));
          LResult.Free;
        end
        else
          ERR('CallTool devolvio nil (timeout o error del servidor)');
      except
        on E: Exception do
          ERR('EXCEPCION en CallTool: ' + E.ClassName + ' — ' + E.Message);
      end;
      LMedia.Free;

    except
      on E: Exception do
        ERR('EXCEPCION en TestCallTool: ' + E.ClassName + ' — ' + E.Message);
    end;
  finally
    LFunctions.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Writeln('=== PPM Console Test ===');
    Writeln('Registry: https://ppm.pascalai.org');
    Writeln;

    TestHealth;
    TestSearch('postgres');
    TestResolveVersion('mcp-postgres');
    TestDownloadAndInspect('mcp-postgres');
    TestInstall('mcp-postgres');
    TestCallTool('mcp-postgres', 'postgres');

    Sep;
    Writeln('=== FIN ===');
  except
    on E: Exception do
    begin
      ERR('EXCEPCION FATAL: ' + E.ClassName + ' — ' + E.Message);
    end;
  end;

  Writeln;
  Write('Presiona Enter para salir...');
  Readln;
end.
