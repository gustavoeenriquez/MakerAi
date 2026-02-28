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
// Adaptaciones respecto a la version Delphi:
//   - System.SysUtils/Classes/Generics.Collections → nombres sin prefijo System.
//   - TList<TCodeFile>          → specialize TList<TCodeFile>
//   - Trimmed.StartsWith('`')   → Copy(Trimmed, 1, N) = '...'
//     (los string helpers de Delphi no existen en FPC objfpc mode)
//   - TStringBuilder             → igual, disponible en Classes desde FPC 3.2
//   - CharInSet                  → igual, disponible en SysUtils FPC
//   - Nested function IsWordChars → igual, soportada en FPC
//   - Exit(value)                → igual, soportado en FPC 2.6+

unit uMakerAi.Utils.CodeExtractor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  // Registro para almacenar información del bloque de código extraído
  TCodeFile = record
    FileName  : string;
    FileType  : string;
    Code      : string;
    LineNumber: Integer; // Línea donde se encontró el bloque
  end;

  // Lista de archivos de código
  TCodeFileList = specialize TList<TCodeFile>;

  // Clase principal para extraer bloques de código de texto Markdown
  TMarkdownCodeExtractor = class
  private
    FCodeFiles: TCodeFileList;
    function NormalizeLanguage(const ALanguage: string): string;
    function TryParseFenceOpen(const ALine: string;
                               out ALanguage, AFileName: string): Boolean;
    function IsFenceClose(const ALine: string): Boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    // Extrae todos los bloques de código del texto Markdown
    function ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;

    // Limpia la lista de archivos
    procedure Clear;

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
begin
  Result := Trim(ALine) = '```';
end;

function TMarkdownCodeExtractor.TryParseFenceOpen(const ALine: string;
                                                  out ALanguage, AFileName: string): Boolean;
var
  Trimmed, Rest, Token: string;
  ColonPos, SpacePos, QuoteStart, QuoteEnd: Integer;

  // Verifica que todos los caracteres de S sean válidos para un nombre de lenguaje
  function IsWordChars(const S: string): Boolean;
  var
    j: Integer;
  begin
    Result := S <> '';
    for j := 1 to Length(S) do
      if not CharInSet(S[j], ['a'..'z', 'A'..'Z', '0'..'9', '_', '+', '#']) then
      begin
        Result := False;
        Exit;
      end;
  end;

begin
  Result    := False;
  ALanguage := '';
  AFileName := '';
  Trimmed   := Trim(ALine);

  // FPC: sustituye Trimmed.StartsWith('```') por comparación directa de prefijo
  if Copy(Trimmed, 1, 3) <> '```' then
    Exit;

  Rest := Copy(Trimmed, 4, MaxInt);

  // Caso 1: Solo ``` sin lenguaje ni nombre de archivo
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
      Result    := True;
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
      Rest      := Trim(Copy(Rest, SpacePos + 1, MaxInt));

      // FPC: sustituye Rest.StartsWith('title="') por comparación de prefijo
      if Copy(Rest, 1, 7) = 'title="' then
      begin
        QuoteStart := 8; // primer carácter después de title="
        QuoteEnd   := Pos('"', Copy(Rest, QuoteStart, MaxInt));
        if QuoteEnd > 0 then
          AFileName := Copy(Rest, QuoteStart, QuoteEnd - 1);
      end;
      Result := True;
      Exit;
    end;
  end;

  // Caso 4: ```language  (solo lenguaje, sin extras)
  Token := Trim(Rest);
  if IsWordChars(Token) then
  begin
    ALanguage := Token;
    Result    := True;
  end;
end;

function TMarkdownCodeExtractor.ExtractCodeFiles(const AMarkdownText: string): TCodeFileList;
var
  Lines           : TStringList;
  i               : Integer;
  CurrentLine     : string;
  InCodeBlock     : Boolean;
  CodeContent     : TStringBuilder;
  CurrentLanguage : string;
  CurrentFileName : string;
  CodeFile        : TCodeFile;
  StartLineNumber : Integer;
  ParsedLang      : string;
  ParsedFileName  : string;
begin
  Clear;
  Result := FCodeFiles;

  if Trim(AMarkdownText) = '' then
    Exit;

  Lines       := TStringList.Create;
  CodeContent := TStringBuilder.Create;
  try
    Lines.Text      := AMarkdownText;
    InCodeBlock     := False;
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
          InCodeBlock     := True;
          StartLineNumber := i + 1; // líneas se cuentan desde 1
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

          // Construir el registro del bloque extraído
          CodeFile.FileName   := CurrentFileName;
          CodeFile.FileType   := CurrentLanguage;
          CodeFile.Code       := CodeContent.ToString;
          CodeFile.LineNumber := StartLineNumber;

          // Solo añadir si tiene contenido real
          if Trim(CodeFile.Code) <> '' then
            FCodeFiles.Add(CodeFile);

          // Resetear para el próximo bloque
          CurrentLanguage := '';
          CurrentFileName := '';
          CodeContent.Clear;
        end
        else
        begin
          // Línea dentro del bloque de código
          if CodeContent.Length > 0 then
            CodeContent.AppendLine;
          CodeContent.Append(CurrentLine);
        end;
      end;
    end;

    // Bloque sin cerrar al final del texto: guardarlo igual
    if InCodeBlock and (CodeContent.Length > 0) then
    begin
      CodeFile.FileName   := CurrentFileName;
      CodeFile.FileType   := CurrentLanguage;
      CodeFile.Code       := CodeContent.ToString;
      CodeFile.LineNumber := StartLineNumber;
      FCodeFiles.Add(CodeFile);
    end;

  finally
    Lines.Free;
    CodeContent.Free;
  end;
end;

end.
