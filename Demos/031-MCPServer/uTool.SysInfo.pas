// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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


unit uTool.SysInfo;

interface

uses
  uMakerAi.MCPServer.Core,
  System.SysUtils,
  System.Classes,
  System.JSON; // <-- Añadir System.JSON

type
  TSysInfoParams = class
  private
    FInfoType: string;
    FIncludeDetails: Boolean;
    FFormatAsJson: Boolean; // <-- Nuevo parámetro
  public
    [AiMCPSchemaDescription('Tipo de información: "basic", "memory", "disk", "network" o "all"')]
    property InfoType: string read FInfoType write FInfoType;

    [AiMCPOptional]
    [AiMCPSchemaDescription('Incluir detalles adicionales (default: false)')]
    property IncludeDetails: Boolean read FIncludeDetails write FIncludeDetails;

    [AiMCPOptional]
    // <-- Nuevo parámetro para controlar el formato de salida
    [AiMCPSchemaDescription
      ('Devolver la información como un objeto JSON estructurado (default: true). Si es false, devuelve texto formateado.')]
    property FormatAsJson: Boolean read FFormatAsJson write FFormatAsJson;
  end;

  TSysInfoTool = class(TAiMCPToolBase<TSysInfoParams>)
  private
    // Funciones que devuelven JSON
    function GetBasicInfoAsJson: TJSONObject;
    function GetMemoryInfoAsJson: TJSONObject;
    function GetDiskInfoAsJson: TJSONArray;
    function GetNetworkInfoAsJson: TJSONObject;
    function GetAllInfoAsJson(IncludeDetails: Boolean): TJSONObject;

    // Funciones antiguas que devuelven texto (para retrocompatibilidad)
    function GetBasicInfoAsText: string;
    function GetMemoryInfoAsText: string;
    function GetDiskInfoAsText: string;
    function GetNetworkInfoAsText: string;
    function GetAllInfoAsText(IncludeDetails: Boolean): string;

  protected
    // CAMBIO: La firma ahora devuelve TJSONObject
    function ExecuteWithParams(const AParams: TSysInfoParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; Override;
  end;

procedure RegisterTools(ALogicServer: TAiMCPServer);

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd, Posix.SysStatvfs
{$ENDIF}
  ;   // System.Classes ya viene del uses de la interface

// =============================================================================
//  Helpers de sistema, aislados por plataforma.
//
//  El codigo original llamaba a la API de Windows directamente desde el cuerpo
//  de cada funcion (GetComputerName, GlobalMemoryStatusEx, GetDiskFreeSpaceEx),
//  asi que este demo no compilaba fuera de Windows: el servidor MCP completo se
//  quedaba sin Linux por una herramienta de ejemplo. Aqui se concentra lo unico
//  que cambia; las seis funciones de arriba pasan a ser portables.
// =============================================================================

type
  TDiskEntry = record
    Path: string;
    TotalGB: Double;
    FreeGB: Double;
  end;

function SysHostName: string;
{$IFDEF MSWINDOWS}
var
  LBuf: array [0 .. MAX_COMPUTERNAME_LENGTH] of Char;
  LSize: DWORD;
begin
  Result := '';
  LSize := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(LBuf, LSize) then
    Result := string(LBuf);
end;
{$ELSE}
var
  LBuf: array [0 .. 255] of AnsiChar;
begin
  Result := '';
  FillChar(LBuf, SizeOf(LBuf), 0);
  if gethostname(@LBuf[0], Length(LBuf) - 1) = 0 then
    Result := string(AnsiString(PAnsiChar(@LBuf[0])));
end;
{$ENDIF}

function SysMemoryInfo(out ATotalGB, AAvailGB: Double; out ALoadPct: Integer): Boolean;
{$IFDEF MSWINDOWS}
var
  LMem: TMemoryStatusEx;
begin
  ATotalGB := 0; AAvailGB := 0; ALoadPct := 0;
  FillChar(LMem, SizeOf(LMem), 0);
  LMem.dwLength := SizeOf(TMemoryStatusEx);
  Result := GlobalMemoryStatusEx(LMem);
  if Result then
  begin
    ATotalGB := LMem.ullTotalPhys / (1024 * 1024 * 1024);
    AAvailGB := LMem.ullAvailPhys / (1024 * 1024 * 1024);
    ALoadPct := LMem.dwMemoryLoad;
  end;
end;
{$ELSE}
// /proc/meminfo, en kB. Se usa MemAvailable y no MemFree: MemFree ignora la
// cache reclamable y en Linux siempre parece alarmantemente baja.
var
  LLines: TStringList;
  I, P: Integer;
  LKey, LVal: string;
  LTotalKb, LAvailKb: Int64;
begin
  ATotalGB := 0; AAvailGB := 0; ALoadPct := 0;
  Result := False;
  if not FileExists('/proc/meminfo') then
    Exit;
  LTotalKb := 0; LAvailKb := 0;
  LLines := TStringList.Create;
  try
    // OJO: TStringList.LoadFromFile NO sirve con /proc. Esos ficheros son
    // virtuales y reportan Size = 0, y LoadFromStream se va con ERangeError
    // ("Range check error") en vez de devolver una lista vacia. Hay que leerlos
    // con Read() sobre un stream, que si entrega el contenido.
    var LFS := TFileStream.Create('/proc/meminfo', fmOpenRead or fmShareDenyNone);
    try
      var LBuf: TBytes;
      SetLength(LBuf, 65536);
      var LRead := LFS.Read(LBuf[0], Length(LBuf));
      if LRead > 0 then
        LLines.Text := TEncoding.UTF8.GetString(LBuf, 0, LRead);
    finally
      LFS.Free;
    end;
    for I := 0 to LLines.Count - 1 do
    begin
      P := Pos(':', LLines[I]);
      if P <= 0 then
        Continue;
      LKey := Trim(Copy(LLines[I], 1, P - 1));
      // Copy con MaxInt desborda al sumar indice+longitud y, con range
      // checking activo, lanza 'Range check error'. Se acota de verdad.
      LVal := Trim(Copy(LLines[I], P + 1, Length(LLines[I]) - P));
      LVal := Trim(StringReplace(LVal, 'kB', '', [rfIgnoreCase]));
      if SameText(LKey, 'MemTotal') then
        LTotalKb := StrToInt64Def(LVal, 0)
      else if SameText(LKey, 'MemAvailable') then
        LAvailKb := StrToInt64Def(LVal, 0);
    end;
  finally
    LLines.Free;
  end;
  if LTotalKb > 0 then
  begin
    ATotalGB := LTotalKb / (1024 * 1024);
    AAvailGB := LAvailKb / (1024 * 1024);
    ALoadPct := Round((LTotalKb - LAvailKb) * 100 / LTotalKb);
    Result := True;
  end;
end;
{$ENDIF}

function SysDisks: TArray<TDiskEntry>;
{$IFDEF MSWINDOWS}
var
  LDrive: Char;
  LRoot: string;
  LFreeAvail, LTotal, LTotalFree: Int64;
begin
  SetLength(Result, 0);
  for LDrive := 'A' to 'Z' do
  begin
    LRoot := LDrive + ':';
    if GetDriveType(PChar(LRoot)) = DRIVE_FIXED then
      if GetDiskFreeSpaceEx(PChar(LRoot), LFreeAvail, LTotal, @LTotalFree) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].Path := LRoot;
        Result[High(Result)].TotalGB := LTotal / (1024 * 1024 * 1024);
        Result[High(Result)].FreeGB := LTotalFree / (1024 * 1024 * 1024);
      end;
  end;
end;
{$ELSE}
// En POSIX no hay letras de unidad: se informa del sistema de ficheros raiz,
// que es lo que interesa en un servidor.
var
  LSt: _statvfs;
begin
  SetLength(Result, 0);
  if statvfs('/', LSt) = 0 then
  begin
    SetLength(Result, 1);
    Result[0].Path := '/';
    Result[0].TotalGB := (Double(LSt.f_blocks) * LSt.f_frsize) / (1024 * 1024 * 1024);
    Result[0].FreeGB := (Double(LSt.f_bavail) * LSt.f_frsize) / (1024 * 1024 * 1024);
  end;
end;
{$ENDIF}

{ TSysInfoTool }

procedure RegisterTools(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo para registrar herramientas.');

  ALogicServer.RegisterTool('system_info',
    function: IAiMCPTool
    begin
      Result := TSysInfoTool.Create;
    end);
end;

constructor TSysInfoTool.Create;
begin
  inherited Create;
  FName := 'system_info';
  FDescription :=
    'Obtiene información del sistema (CPU, memoria, disco, etc.) donde se está ejecutando el servidor. Puede devolver la información como texto formateado o como un objeto JSON estructurado.';
  // Por defecto, FormatAsJson será True
  TSysInfoParams.Create.FFormatAsJson := True;
end;

// CAMBIO: La función principal ahora devuelve TJSONObject y decide el formato
function TSysInfoTool.ExecuteWithParams(const AParams: TSysInfoParams; const AuthContext: TAiAuthContext): TJSONObject;
var
  LInfoType: string;
  ResultText: string;
  ResultJson: TJSONObject;
begin
  LInfoType := Trim(LowerCase(AParams.InfoType));
  ResultText := '';
  try
    // Decidir si se devuelve JSON o texto
    if AParams.FormatAsJson then
    begin
      // --- Lógica para devolver JSON ---
      if (LInfoType = 'basic') then
        ResultJson := GetBasicInfoAsJson
      else if (LInfoType = 'memory') then
        ResultJson := GetMemoryInfoAsJson
      else if (LInfoType = 'disk') then
      begin
        ResultJson := TJSONObject.Create;
        ResultJson.AddPair('disks', GetDiskInfoAsJson);
      end
      else if (LInfoType = 'network') then
        ResultJson := GetNetworkInfoAsJson
      else if (LInfoType = 'all') or (LInfoType = '') then
        ResultJson := GetAllInfoAsJson(AParams.IncludeDetails)
      else
        raise Exception.Create('Tipo de información no válido. Use: basic, memory, disk, network, all');

      // Añadir info del usuario al JSON
      if Assigned(ResultJson) then
        ResultJson.AddPair('executedBy', AuthContext.UserID);

      // Usamos el builder para añadir el JSON como texto. La IA lo parseará.
      Result := TAiMCPResponseBuilder.New.AddText(ResultJson.ToJSON).Build;
      ResultJson.Free; // Liberamos el JSON que creamos
    end
    else
    begin
      // --- Lógica para devolver Texto (como antes) ---
      if (LInfoType = 'basic') then
        ResultText := GetBasicInfoAsText
      else if (LInfoType = 'memory') then
        ResultText := GetMemoryInfoAsText
      else if (LInfoType = 'disk') then
        ResultText := GetDiskInfoAsText
      else if (LInfoType = 'network') then
        ResultText := GetNetworkInfoAsText
      else if (LInfoType = 'all') or (LInfoType = '') then
        ResultText := GetAllInfoAsText(AParams.IncludeDetails)
      else
        ResultText := '❌ Tipo de información no válido. Use: basic, memory, disk, network, all';

      // Añadir info del usuario al texto
      if AuthContext.IsAuthenticated then
        ResultText := ResultText + sLineBreak + sLineBreak + '✅ Ejecutado por el usuario: ' + AuthContext.UserID
      else
        ResultText := ResultText + sLineBreak + sLineBreak + '⚠️ Ejecutado por un usuario anónimo.';

      Result := TAiMCPResponseBuilder.New.AddText(ResultText).Build;
    end;
  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New.AddText('❌ Error obteniendo información del sistema: ' + E.Message).Build;
  end;
end;

// =============================================================================
// Implementación de funciones que devuelven JSON
// =============================================================================

function TSysInfoTool.GetBasicInfoAsJson: TJSONObject;
var
  LName: string;
begin
  Result := TJSONObject.Create;
  LName := SysHostName;
  if LName <> '' then
    Result.AddPair('computerName', LName);

  var
  LOsVer := TJSONObject.Create;
  LOsVer.AddPair('major', TOSVersion.Major);
  LOsVer.AddPair('minor', TOSVersion.Minor);
  LOsVer.AddPair('build', TOSVersion.Build);
  // Antes se llamaba 'windowsVersion'. Corriendo en Linux ese nombre era una
  // mentira, asi que pasa a 'osVersion' y se acompana del nombre real del SO.
  Result.AddPair('osVersion', LOsVer);
  Result.AddPair('osName', TOSVersion.ToString);

{$IF Defined(WIN64) or Defined(LINUX64) or Defined(CPUX64)}
  Result.AddPair('architecture', '64-bit');
{$ELSE}
  Result.AddPair('architecture', '32-bit');
{$ENDIF}
  Result.AddPair('currentDateTime', FormatDateTime('c', Now)); // ISO 8601 format
end;

function TSysInfoTool.GetMemoryInfoAsJson: TJSONObject;
var
  LTotal, LAvail: Double;
  LLoad: Integer;
begin
  Result := TJSONObject.Create;
  if SysMemoryInfo(LTotal, LAvail, LLoad) then
  begin
    Result.AddPair('totalPhysical_gb', LTotal);
    Result.AddPair('availablePhysical_gb', LAvail);
    Result.AddPair('memoryLoad_percent', LLoad);
  end;
end;

function TSysInfoTool.GetDiskInfoAsJson: TJSONArray;
var
  LDisks: TArray<TDiskEntry>;
  I: Integer;
  LDisk: TJSONObject;
begin
  Result := TJSONArray.Create;
  LDisks := SysDisks;
  for I := 0 to High(LDisks) do
  begin
    LDisk := TJSONObject.Create;
    LDisk.AddPair('drive', LDisks[I].Path);
    LDisk.AddPair('total_gb', LDisks[I].TotalGB);
    LDisk.AddPair('free_gb', LDisks[I].FreeGB);
    Result.AddElement(LDisk);
  end;
end;

function TSysInfoTool.GetNetworkInfoAsJson: TJSONObject;
var
  LName: string;
begin
  Result := TJSONObject.Create;
  LName := SysHostName;
  if LName <> '' then
    Result.AddPair('networkName', LName);
  Result.AddPair('details', 'For more network details, specific APIs are required.');
end;

function TSysInfoTool.GetAllInfoAsJson(IncludeDetails: Boolean): TJSONObject;
begin
  Result := GetBasicInfoAsJson;
  Result.AddPair('memory', GetMemoryInfoAsJson);
  Result.AddPair('disks', GetDiskInfoAsJson);
  Result.AddPair('network', GetNetworkInfoAsJson);

  if IncludeDetails then
  begin
    var
    LDetails := TJSONObject.Create;
    LDetails.AddPair('mcpServerPath', ExtractFilePath(ParamStr(0)));
    LDetails.AddPair('delphiVersion', '12.2'); // O obtenerlo dinámicamente si es posible
    Result.AddPair('extraDetails', LDetails);
  end;
end;


// =============================================================================
// Implementación de funciones de Texto (sin cambios, solo renombradas)
// =============================================================================

function TSysInfoTool.GetBasicInfoAsText: string;
var
  LName: string;
begin
  // Código original de GetBasicInfo
  Result := '🖥️ INFORMACIÓN BÁSICA DEL SISTEMA' + sLineBreak + sLineBreak;
  LName := SysHostName;
  if LName <> '' then
    Result := Result + 'Nombre del equipo: ' + LName + sLineBreak;
  // TOSVersion.Name en Linux devuelve la cadena entera de uname (kernel, fecha
  // de compilacion...), que en una linea de resumen queda ilegible: se usa una
  // etiqueta corta y el detalle va aparte.
{$IFDEF MSWINDOWS}
  Result := Result + Format('Windows: %d.%d (Build %d)',
    [TOSVersion.Major, TOSVersion.Minor, TOSVersion.Build]) + sLineBreak;
{$ELSE}
  Result := Result + Format('Linux: kernel %d.%d', [TOSVersion.Major, TOSVersion.Minor]) + sLineBreak;
{$ENDIF}
  Result := Result + 'Detalle del SO: ' + TOSVersion.Name + sLineBreak;
{$IF Defined(WIN64) or Defined(LINUX64) or Defined(CPUX64)}
  Result := Result + 'Arquitectura: 64-bit' + sLineBreak;
{$ELSE}
  Result := Result + 'Arquitectura: 32-bit' + sLineBreak;
{$ENDIF}
  Result := Result + 'Fecha/Hora: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
end;

function TSysInfoTool.GetMemoryInfoAsText: string;
begin
  // Código original de GetMemoryInfo
  var
    LTotal, LAvail: Double;
  var
    LLoad: Integer;
  Result := '💾 INFORMACIÓN DE MEMORIA' + sLineBreak + sLineBreak;
  if SysMemoryInfo(LTotal, LAvail, LLoad) then
  begin
    Result := Result + Format('Memoria física total: %.2f GB', [LTotal]) + sLineBreak;
    Result := Result + Format('Memoria física disponible: %.2f GB', [LAvail]) + sLineBreak;
    Result := Result + Format('Uso de memoria: %d%%', [LLoad]);
  end;
end;

function TSysInfoTool.GetDiskInfoAsText: string;
var
  LDisks: TArray<TDiskEntry>;
  I: Integer;
begin
  Result := '💿 INFORMACIÓN DE DISCOS' + sLineBreak;
  LDisks := SysDisks;
  for I := 0 to High(LDisks) do
    Result := Result + sLineBreak + Format('Unidad %s: %.2f GB libres de %.2f GB totales',
      [LDisks[I].Path, LDisks[I].FreeGB, LDisks[I].TotalGB]);
end;

function TSysInfoTool.GetNetworkInfoAsText: string;
  var
    LName: string;
begin
  Result := '🌐 INFORMACIÓN DE RED' + sLineBreak + sLineBreak;
  LName := SysHostName;
  if LName <> '' then
    Result := Result + 'Nombre de red: ' + LName + sLineBreak;
  Result := Result + 'Nota: Para más detalles de red, se requieren APIs específicas.';
end;

function TSysInfoTool.GetAllInfoAsText(IncludeDetails: Boolean): string;
begin
  // Código original de GetAllInfo
  Result := GetBasicInfoAsText + sLineBreak + sLineBreak + GetMemoryInfoAsText + sLineBreak + sLineBreak + GetDiskInfoAsText + sLineBreak +
    sLineBreak + GetNetworkInfoAsText;
  if IncludeDetails then
    Result := Result + sLineBreak + sLineBreak + '📊 DETALLES ADICIONALES' + sLineBreak + 'Servidor MCP ejecutándose desde: ' +
      ExtractFilePath(ParamStr(0)) + sLineBreak + 'Compilado con: Delphi 12.2';
end;

end.
