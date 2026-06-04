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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMakerAi.Utils.CodeExtractor;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  // Registro para almacenar informaci?n del archivo extra?do
  TCodeFile = record
    FileName : String;
    FileType: string;
    Code: string;
    LineNumber: Integer; // L?nea donde se encontr? el bloque
  end;

  // Lista de archivos de c?digo
  TCodeFileList = TList<TCodeFile>;

  // Clase principal para extraer archivos de c?digo
  TMarkdownCodeExtractor = class
  private
    FCodeFiles: TCodeFileList;
    function NormalizeLanguage(const ALanguage: string): string;
    function TryParseFenceOpen(const ALine: string; out ALanguage, AFileName: string): Boolean;
    function IsFenceClose(const ALine: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // M?todo principal para extraer archivos de c?digo del texto markdown
    function ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;

    // M?todo para limpiar la lista de archivos
    procedure Clear;

    // Propiedad para acceder a los archivos extra?dos
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

function TMarkdownCodeExtractor.IsFenceClose(const ALine: string): Boolean;
var
  Trimmed: string;
begin
  Trimmed := Trim(ALine);
  Result := (Trimmed = '```');
end;

function TMarkdownCodeExtractor.TryParseFenceOpen(const ALine: string;
  out ALanguage, AFileName: string): Boolean;
var
  Trimmed, Rest, Token: string;
  ColonPos, SpacePos, QuoteStart, QuoteEnd: Integer;

  function IsWordChars(const S: string): Boolean;
  var
    j: Integer;
  begin
    Result := S <> '';
    for j := 1 to Length(S) do
      if not CharInSet(S[j], ['a'..'z', 'A'..'Z', '0'..'9', '_', '+', '#']) then
        Exit(False);
  end;

begin
  Result := False;
  ALanguage := '';
  AFileName := '';
  Trimmed := Trim(ALine);

  if not Trimmed.StartsWith('```') then
    Exit;

  Rest := Copy(Trimmed, 4, MaxInt);

  // Caso 1: Solo ``` (sin lenguaje ni archivo)
  if Trim(Rest) = '' then
  begin
    Result := True;
    Exit;
  end;

  // Caso 2: ```language:filename.ext (separador dos puntos)
  ColonPos := Pos(':', Rest);
  if ColonPos > 1 then
  begin
    Token := Trim(Copy(Rest, 1, ColonPos - 1));
    if IsWordChars(Token) then
    begin
      ALanguage := Token;
      AFileName := Trim(Copy(Rest, ColonPos + 1, MaxInt));
      Result := True;
      Exit;
    end;
  end;

  // Caso 3: ```language title="filename.ext" (atributo title)
  SpacePos := Pos(' ', Rest);
  if SpacePos > 1 then
  begin
    Token := Copy(Rest, 1, SpacePos - 1);
    if IsWordChars(Token) then
    begin
      ALanguage := Token;
      // Buscar title="..."
      Rest := Trim(Copy(Rest, SpacePos + 1, MaxInt));
      if Rest.StartsWith('title="') then
      begin
        QuoteStart := 8; // despu?s de title="
        QuoteEnd := Pos('"', Copy(Rest, QuoteStart, MaxInt));
        if QuoteEnd > 0 then
          AFileName := Copy(Rest, QuoteStart, QuoteEnd - 1);
      end;
      Result := True;
      Exit;
    end;
  end;

  // Caso 4: ```language (solo lenguaje, sin extras)
  Token := Trim(Rest);
  if IsWordChars(Token) then
  begin
    ALanguage := Token;
    Result := True;
    Exit;
  end;

  // No matchea ning?n patr?n conocido
end;

function TMarkdownCodeExtractor.ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;
var
  Lines: TStringList;
  i: Integer;
  CurrentLine: string;
  InCodeBlock: Boolean;
  CodeContent: TStringBuilder;
  CurrentLanguage: string;
  CurrentFileName: string;
  CodeFile: TCodeFile;
  StartLineNumber: Integer;
  ParsedLang, ParsedFileName: string;
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
    CurrentFileName := '';
    StartLineNumber := 0;

    for i := 0 to Lines.Count - 1 do
    begin
      CurrentLine := Lines[i];

      if not InCodeBlock then
      begin
        if TryParseFenceOpen(CurrentLine, ParsedLang, ParsedFileName) then
        begin
          InCodeBlock := True;
          StartLineNumber := i + 1; // +1 porque las l?neas se cuentan desde 1
          CodeContent.Clear;
          CurrentLanguage := NormalizeLanguage(ParsedLang);
          CurrentFileName := ParsedFileName;
        end;
      end
      else
      begin
        if IsFenceClose(CurrentLine) then
        begin
          InCodeBlock := False;

          // Crear el registro del archivo de c?digo
          CodeFile.FileName := CurrentFileName;
          CodeFile.FileType := CurrentLanguage;
          CodeFile.Code := CodeContent.ToString;
          CodeFile.LineNumber := StartLineNumber;

          // Agregar solo si hay contenido
          if Trim(CodeFile.Code) <> '' then
            FCodeFiles.Add(CodeFile);

          // Resetear variables
          CurrentLanguage := '';
          CurrentFileName := '';
          CodeContent.Clear;
        end
        else
        begin
          // L?nea dentro del bloque de c?digo
          if CodeContent.Length > 0 then
            CodeContent.AppendLine;
          CodeContent.Append(CurrentLine);
        end;
      end;
    end;

    // Si qued? un bloque abierto al final del texto
    if InCodeBlock and (CodeContent.Length > 0) then
    begin
      CodeFile.FileName := CurrentFileName;
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
