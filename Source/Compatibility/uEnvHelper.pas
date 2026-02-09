unit uEnvHelper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
    Classes, SysUtils,
    {$IFDEF MSWINDOWS}
    Windows;
    {$ELSE}
    Unix, BaseUnix;
    {$ENDIF}
  {$ELSE}
    System.Classes, System.SysUtils,
    {$IFDEF MSWINDOWS}
    Winapi.Windows;
    {$ENDIF}
    {$IFDEF POSIX}
    Posix.Stdlib;
    {$ENDIF}
  {$ENDIF}

type
  { TEnvHelper }
  TEnvHelper = class
  public
    { Carga las variables desde un archivo .env ubicado en Config/.env
      Busca automáticamente subiendo hasta 7 niveles en el árbol de directorios. }
    class procedure LoadEnv(const AFileName: string = '.env');
    { Retorna una variable de entorno del proceso actual. }
    class function GetEnv(const AName: string): string;
  end;

  // Alias para compatibilidad hacia atras si es necesario, 
  // aunque se recomienda usar TEnvHelper.
  TAiEnvLoader = TEnvHelper deprecated 'Use TEnvHelper instead';

implementation

{$IFDEF FPC}
  {$IFDEF UNIX}
  function setenv(name: PAnsiChar; value: PAnsiChar; overwrite: Integer): Integer; cdecl; external 'c' name 'setenv';

  // COMPAT: 'cvar' keyword not valid in {$mode delphi}. Using libc function instead.
  function get_environ: PPChar; cdecl; external 'c' name '__environ';

  procedure SyncPascalEnv;
  begin
    System.envp := get_environ;
  end;
  {$ENDIF}
{$ENDIF}

{ TEnvHelper }

class procedure TEnvHelper.LoadEnv(const AFileName: string);
var
  LFilePath: string;
  LContent: TStringList;
  I: Integer;
  LLine: string;
  LName, LValue: string;
  LPos: Integer;
  LCurrentDir: string;
begin
  // 1. Intentar encontrar el archivo .env en Config/ buscando hacia arriba en el árbol de directorios
  LCurrentDir := ExtractFilePath(ParamStr(0));
  if LCurrentDir = '' then LCurrentDir := GetCurrentDir;
  
  // Asegurar ruta absoluta y con delimitador final
  LCurrentDir := ExpandFileName(LCurrentDir);
  LCurrentDir := IncludeTrailingPathDelimiter(LCurrentDir);
  
  // Buscar en Config/.env subiendo hasta 7 niveles
  I := 0;
  LFilePath := LCurrentDir + 'Config' + PathDelim + AFileName;
  
  while (not FileExists(LFilePath)) and (I < 7) do
  begin
    // Subir un nivel: ExtractFileDir elimina el último directorio
    LCurrentDir := ExtractFileDir(ExcludeTrailingPathDelimiter(LCurrentDir));
    
    // Si llegamos a la raíz (o cadena vacía), detener
    if (LCurrentDir = '') or (LCurrentDir = PathDelim) then Break;
    
    LCurrentDir := IncludeTrailingPathDelimiter(LCurrentDir);
    LFilePath := LCurrentDir + 'Config' + PathDelim + AFileName;
    Inc(I);
  end;
  
  if not FileExists(LFilePath) then
    Exit;

  LContent := TStringList.Create;
  try
    LContent.LoadFromFile(LFilePath);
    for I := 0 to LContent.Count - 1 do
    begin
      LLine := Trim(LContent[I]);
      
      // Ignorar comentarios y líneas vacías
      if (LLine = '') or (LLine[1] = '#') then
        Continue;

      // Buscar el signo igual
      LPos := Pos('=', LLine);
      if LPos > 0 then
      begin
        LName := Trim(Copy(LLine, 1, LPos - 1));
        LValue := Trim(Copy(LLine, LPos + 1, Length(LLine)));

        // Quitar comillas si existen
        if (Length(LValue) >= 2) and (LValue[1] = '"') and (LValue[Length(LValue)] = '"') then
          LValue := Copy(LValue, 2, Length(LValue) - 2)
        else if (Length(LValue) >= 2) and (LValue[1] = '''') and (LValue[Length(LValue)] = '''') then
          LValue := Copy(LValue, 2, Length(LValue) - 2);

        // Establecer la variable de entorno
        if (LName <> '') then
        begin
          {$IFDEF MSWINDOWS}
          SetEnvironmentVariable(PChar(LName), PChar(LValue));
          {$ELSE}
            {$IFDEF FPC}
            if setenv(PAnsiChar(AnsiString(LName)), PAnsiChar(AnsiString(LValue)), 1) <> 0 then
                 ; // Ignorar error
            {$ELSE}
              // Delphi Linux / MacOS POSIX
              // Posix.Stdlib.setenv(name, value, overwrite)
              // Requiere marshalling
            {$ENDIF}
          {$ENDIF}
        end;
      end;
    end;

    // Sincronizar el entorno en Linux para que SysUtils.GetEnvironmentVariable vea los cambios
    {$IFDEF FPC}
      {$IFDEF UNIX}
      SyncPascalEnv;
      {$ENDIF}
    {$ENDIF}
  finally
    LContent.Free;
  end;
end;

class function TEnvHelper.GetEnv(const AName: string): string;
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentVariable(AName);
  {$ELSE}
  Result := System.SysUtils.GetEnvironmentVariable(AName);
  {$ENDIF}
end;

end.
