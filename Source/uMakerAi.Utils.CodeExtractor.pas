unit uMakerAi.Utils.CodeExtractor;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.RegularExpressions;

type
  // Registro para almacenar información del archivo extraído
  TCodeFile = record
    FileType: string;
    Code: string;
    LineNumber: Integer; // Línea donde se encontró el bloque
  end;

  // Lista de archivos de código
  TCodeFileList = TList<TCodeFile>;

  // Clase principal para extraer archivos de código
  TMarkdownCodeExtractor = class
  private
    FCodeFiles: TCodeFileList;
    function GetLanguageFromExtension(const AExtension: string): string;
    function NormalizeLanguage(const ALanguage: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Método principal para extraer archivos de código del texto markdown
    function ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;

    // Método para limpiar la lista de archivos
    procedure Clear;

    // Propiedad para acceder a los archivos extraídos
    property CodeFiles: TCodeFileList read FCodeFiles;
  end;

implementation

{ TMarkdownCodeExtractor }

constructor TMarkdownCodeExtractor.Create;
begin
  inherited Create;
  FCodeFiles := TCodeFileList.Create;
end;

destructor TMarkdownCodeExtractor.Destroy;
begin
  FCodeFiles.Free;
  inherited Destroy;
end;

procedure TMarkdownCodeExtractor.Clear;
begin
  FCodeFiles.Clear;
end;

function TMarkdownCodeExtractor.GetLanguageFromExtension(const AExtension: string): string;
begin
  // Mapeo de extensiones a lenguajes
  if SameText(AExtension, '.pas') or SameText(AExtension, '.dpr') or SameText(AExtension, '.dpk') then
    Result := 'delphi'
  else if SameText(AExtension, '.py') then
    Result := 'python'
  else if SameText(AExtension, '.js') then
    Result := 'javascript'
  else if SameText(AExtension, '.cs') then
    Result := 'csharp'
  else if SameText(AExtension, '.cpp') or SameText(AExtension, '.cc') or SameText(AExtension, '.cxx') then
    Result := 'cpp'
  else if SameText(AExtension, '.c') then
    Result := 'c'
  else if SameText(AExtension, '.h') then
    Result := 'c'
  else if SameText(AExtension, '.java') then
    Result := 'java'
  else if SameText(AExtension, '.php') then
    Result := 'php'
  else if SameText(AExtension, '.rb') then
    Result := 'ruby'
  else if SameText(AExtension, '.go') then
    Result := 'go'
  else if SameText(AExtension, '.rs') then
    Result := 'rust'
  else if SameText(AExtension, '.sql') then
    Result := 'sql'
  else if SameText(AExtension, '.html') or SameText(AExtension, '.htm') then
    Result := 'html'
  else if SameText(AExtension, '.css') then
    Result := 'css'
  else if SameText(AExtension, '.xml') then
    Result := 'xml'
  else if SameText(AExtension, '.json') then
    Result := 'json'
  else
    Result := 'text'; // Tipo por defecto
end;

function TMarkdownCodeExtractor.NormalizeLanguage(const ALanguage: string): string;
var
  LowerLang: string;
begin
  LowerLang := LowerCase(Trim(ALanguage));

  // Normalizar nombres de lenguajes comunes
  if (LowerLang = 'pascal') or (LowerLang = 'delphi') or (LowerLang = 'objectpascal') then
    Result := 'delphi'
  else if (LowerLang = 'js') or (LowerLang = 'javascript') then
    Result := 'javascript'
  else if (LowerLang = 'c++') or (LowerLang = 'cpp') or (LowerLang = 'cxx') then
    Result := 'cpp'
  else if (LowerLang = 'c#') or (LowerLang = 'csharp') then
    Result := 'csharp'
  else if (LowerLang = 'py') or (LowerLang = 'python') then
    Result := 'python'
  else if LowerLang = '' then
    Result := 'text'
  else
    Result := LowerLang;
end;

function TMarkdownCodeExtractor.ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;
var
  Lines: TStringList;
  i: Integer;
  CurrentLine: string;
  InCodeBlock: Boolean;
  CodeContent: TStringBuilder;
  CurrentLanguage: string;
  CodeFile: TCodeFile;
  StartLineNumber: Integer;
  RegexPattern: string;
  Match: TMatch;
begin
  Clear;
  Result := FCodeFiles;

  if Trim(AMarkdownText) = '' then
    Exit;

  Lines := TStringList.Create;
  CodeContent := TStringBuilder.Create;
  try
    Lines.Text := AMarkdownText;
    InCodeBlock := False;
    CurrentLanguage := '';
    StartLineNumber := 0;

    // Patrón para detectar bloques de código con ```
    RegexPattern := '^\s*```\s*(\w+)?\s*$';

    for i := 0 to Lines.Count - 1 do
    begin
      CurrentLine := Lines[i];
      Match := TRegEx.Match(CurrentLine, RegexPattern);

      if Match.Success then
      begin
        if not InCodeBlock then
        begin
          // Inicio de bloque de código
          InCodeBlock := True;
          StartLineNumber := i + 1; // +1 porque las líneas se cuentan desde 1
          CodeContent.Clear;

          // Extraer el lenguaje si está especificado
          if Match.Groups.Count > 1 then
            CurrentLanguage := NormalizeLanguage(Match.Groups[1].Value)
          else
            CurrentLanguage := 'text';
        end
        else
        begin
          // Fin de bloque de código
          InCodeBlock := False;

          // Crear el registro del archivo de código
          CodeFile.FileType := CurrentLanguage;
          CodeFile.Code := CodeContent.ToString;
          CodeFile.LineNumber := StartLineNumber;

          // Agregar solo si hay contenido
          if Trim(CodeFile.Code) <> '' then
            FCodeFiles.Add(CodeFile);

          // Resetear variables
          CurrentLanguage := '';
          CodeContent.Clear;
        end;
      end
      else if InCodeBlock then
      begin
        // Línea dentro del bloque de código
        if CodeContent.Length > 0 then
          CodeContent.AppendLine;
        CodeContent.Append(CurrentLine);
      end;
    end;

    // Si quedó un bloque abierto al final del texto
    if InCodeBlock and (CodeContent.Length > 0) then
    begin
      CodeFile.FileType := CurrentLanguage;
      CodeFile.Code := CodeContent.ToString;
      CodeFile.LineNumber := StartLineNumber;
      FCodeFiles.Add(CodeFile);
    end;

  finally
    Lines.Free;
    CodeContent.Free;
  end;
end;

end.

// Ejemplo de uso:
{
program TestMarkdownExtractor;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MarkdownCodeExtractor;

procedure TestExtractor;
var
  Extractor: TMarkdownCodeExtractor;
  MarkdownText: string;
  CodeFiles: TCodeFileList;
  i: Integer;
begin
  MarkdownText := 'Aquí tienes un ejemplo de código en Delphi para calcular la serie de Fibonacci.' + sLineBreak +
                  '```delphi' + sLineBreak +
                  'program FibonacciSeries;' + sLineBreak +
                  '{$APPTYPE CONSOLE}' + sLineBreak +
                  'uses' + sLineBreak +
                  '  SysUtils;' + sLineBreak +
                  'function Fibonacci(n: Integer): Integer;' + sLineBreak +
                  'begin' + sLineBreak +
                  '  if n <= 0 then' + sLineBreak +
                  '    Result := 0' + sLineBreak +
                  '  else if n = 1 then' + sLineBreak +
                  '    Result := 1' + sLineBreak +
                  '  else' + sLineBreak +
                  '    Result := Fibonacci(n - 1) + Fibonacci(n - 2);' + sLineBreak +
                  'end;' + sLineBreak +
                  '```' + sLineBreak +
                  'Y aquí hay un ejemplo en Python:' + sLineBreak +
                  '```python' + sLineBreak +
                  'def fibonacci(n):' + sLineBreak +
                  '    if n <= 0:' + sLineBreak +
                  '        return 0' + sLineBreak +
                  '    elif n == 1:' + sLineBreak +
                  '        return 1' + sLineBreak +
                  '    else:' + sLineBreak +
                  '        return fibonacci(n-1) + fibonacci(n-2)' + sLineBreak +
                  '```';

  Extractor := TMarkdownCodeExtractor.Create;
  try
    CodeFiles := Extractor.ExtractCodeFiles(MarkdownText);

    WriteLn('Archivos de código encontrados: ', CodeFiles.Count);
    WriteLn('');

    for i := 0 to CodeFiles.Count - 1 do
    begin
      WriteLn('Archivo ', i + 1, ':');
      WriteLn('Tipo: ', CodeFiles[i].FileType);
      WriteLn('Línea: ', CodeFiles[i].LineNumber);
      WriteLn('Código:');
      WriteLn(CodeFiles[i].Code);
      WriteLn('----------------------------------------');
    end;

  finally
    Extractor.Free;
  end;

  ReadLn;
end;

begin
  TestExtractor;
end.
}
