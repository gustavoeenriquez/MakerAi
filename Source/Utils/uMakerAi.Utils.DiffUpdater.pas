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

unit uMakerAi.Utils.DiffUpdater;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TDiffOperation = (doContext, doAdd, doDelete);

  TDiffLine = record
    Operation: TDiffOperation;
    Content: string;
  end;

  TDiffHunk = class
  private
    FOriginalStart: Integer;
    FOriginalCount: Integer;
    FNewStart: Integer;
    FNewCount: Integer;
    FLines: TList<TDiffLine>;
  public
    constructor Create;
    destructor Destroy; override;
    property OriginalStart: Integer read FOriginalStart write FOriginalStart;
    property OriginalCount: Integer read FOriginalCount write FOriginalCount;
    property NewStart: Integer read FNewStart write FNewStart;
    property NewCount: Integer read FNewCount write FNewCount;
    property Lines: TList<TDiffLine> read FLines;
  end;

  TDiffParser = class
  private
    function ParseHunkHeader(const Line: string; out Hunk: TDiffHunk): Boolean;
  public
    function Parse(const DiffText: string): TList<TDiffHunk>;
  end;

  TDiffApplier = class
  private
    function MatchHunkAt(const FileLines: TStringList; Hunk: TDiffHunk; StartIndex: Integer): Boolean;
    function FindHunkPosition(const FileLines: TStringList; Hunk: TDiffHunk; out ActualStart: Integer): Boolean;
  public
    function Apply(const OriginalContent: string; const DiffText: string; out NewContent: string; out ErrorMsg: string): Boolean;
  end;

implementation

uses
  System.StrUtils, System.Math;

{ TDiffHunk }

constructor TDiffHunk.Create;
begin
  inherited;
  FLines := TList<TDiffLine>.Create;
end;

destructor TDiffHunk.Destroy;
begin
  FLines.Free;
  inherited;
end;

{ TDiffParser }

function TDiffParser.ParseHunkHeader(const Line: string; out Hunk: TDiffHunk): Boolean;
var
  Parts: TArray<string>;
  OrigPart, NewPart: string;
  CommaPos: Integer;
begin
  Result := False;
  Hunk := nil;

  // Formato esperado: @@ -start,count +start,count @@
  // Pero GPT-5 a veces envía solo "@@" o "@@ context @@"
  if not Line.StartsWith('@@') then Exit;

  try
    Hunk := TDiffHunk.Create;

    // VALORES POR DEFECTO (Si el parsing falla, asumimos inicio de archivo o búsqueda difusa)
    Hunk.OriginalStart := 1;
    Hunk.OriginalCount := 0;
    Hunk.NewStart := 1;
    Hunk.NewCount := 0;

    // Intentamos parsear estrictamente
    Parts := Line.Split(['@@'], TStringSplitOptions.ExcludeEmpty);

    if Length(Parts) >= 1 then
    begin
      Parts := Trim(Parts[0]).Split([' ']);

      if Length(Parts) >= 2 then
      begin
        // 1. Original (-start,count)
        OrigPart := Parts[0];
        if OrigPart.StartsWith('-') then
        begin
          OrigPart := Copy(OrigPart, 2, Length(OrigPart));
          CommaPos := Pos(',', OrigPart);
          if CommaPos > 0 then
          begin
            Hunk.OriginalStart := StrToIntDef(Copy(OrigPart, 1, CommaPos - 1), 0);
            Hunk.OriginalCount := StrToIntDef(Copy(OrigPart, CommaPos + 1, Length(OrigPart)), 1);
          end
          else
          begin
            Hunk.OriginalStart := StrToIntDef(OrigPart, 0);
            Hunk.OriginalCount := 1;
          end;
        end;

        // 2. Nuevo (+start,count)
        NewPart := Parts[1];
        if NewPart.StartsWith('+') then
        begin
          NewPart := Copy(NewPart, 2, Length(NewPart));
          CommaPos := Pos(',', NewPart);
          if CommaPos > 0 then
          begin
            Hunk.NewStart := StrToIntDef(Copy(NewPart, 1, CommaPos - 1), 0);
            Hunk.NewCount := StrToIntDef(Copy(NewPart, CommaPos + 1, Length(NewPart)), 1);
          end
          else
          begin
            Hunk.NewStart := StrToIntDef(NewPart, 0);
            Hunk.NewCount := 1;
          end;
        end;
      end;
    end;

    // IMPORTANTE: Incluso si el parsing de números falló (ej. la línea era solo "@@"),
    // devolvemos True porque hemos creado un Hunk válido con valores por defecto (0,0).
    // El TDiffApplier usará el contenido (contexto) para encontrar dónde aplicarlo.
    Result := True;

  except
    if Assigned(Hunk) then FreeAndNil(Hunk);
    Result := False;
  end;
end;

function TDiffParser.Parse(const DiffText: string): TList<TDiffHunk>;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  CurrentHunk: TDiffHunk;
  DiffLine: TDiffLine;
  HasHeaders: Boolean;
begin
  Result := TList<TDiffHunk>.Create;
  Lines := TStringList.Create;
  try
    Lines.Text := DiffText; // Normaliza CRLF/LF
    CurrentHunk := nil;
    HasHeaders := False;

    // 1. Detección rápida de cabeceras
    for I := 0 to Lines.Count - 1 do
      if Lines[I].StartsWith('@@') then
      begin
        HasHeaders := True;
        Break;
      end;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];

      // Ignorar cabeceras de metadatos git
      if Line.StartsWith('+++') or Line.StartsWith('---') or Line.StartsWith('\') then
        Continue;

      if Line.StartsWith('@@') then
      begin
        if ParseHunkHeader(Line, CurrentHunk) then
          Result.Add(CurrentHunk)
        else if Assigned(CurrentHunk) then
          FreeAndNil(CurrentHunk);
      end
      else
      begin
        // LÓGICA DE RECUPERACIÓN (FALLBACK):
        // Si encontramos líneas de contenido (+/-) pero NO tenemos un Hunk activo
        // (y especialmente si no se detectaron cabeceras en todo el archivo),
        // creamos un "Hunk Virtual" que asume inicio en línea 0.
        if (CurrentHunk = nil) and not HasHeaders and
           (Line.StartsWith('+') or Line.StartsWith('-') or Line.StartsWith(' ')) then
        begin
          CurrentHunk := TDiffHunk.Create;
          CurrentHunk.OriginalStart := 0; // Asumimos creación o inicio de archivo
          CurrentHunk.OriginalCount := 0;
          CurrentHunk.NewStart := 1;
          CurrentHunk.NewCount := 0;
          Result.Add(CurrentHunk);
        end;

        if Assigned(CurrentHunk) then
        begin
          if Line.StartsWith('+') then
          begin
            DiffLine.Operation := doAdd;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.Lines.Add(DiffLine);
          end
          else if Line.StartsWith('-') then
          begin
            DiffLine.Operation := doDelete;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.Lines.Add(DiffLine);
          end
          else if Line.StartsWith(' ') then
          begin
            DiffLine.Operation := doContext;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.Lines.Add(DiffLine);
          end
          else if Line = '' then
          begin
             // Línea vacía suele interpretarse como contexto vacío
             DiffLine.Operation := doContext;
             DiffLine.Content := '';
             CurrentHunk.Lines.Add(DiffLine);
          end;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ TDiffApplier }

function TDiffApplier.MatchHunkAt(const FileLines: TStringList; Hunk: TDiffHunk; StartIndex: Integer): Boolean;
var
  I, CurrentFileIdx: Integer;
  DiffLine: TDiffLine;
  FileContent, DiffContent: string;
begin
  Result := True;
  CurrentFileIdx := StartIndex;

  for I := 0 to Hunk.Lines.Count - 1 do
  begin
    DiffLine := Hunk.Lines[I];

    if DiffLine.Operation in [doContext, doDelete] then
    begin
      if CurrentFileIdx >= FileLines.Count then Exit(False);

      // MEJORA: Usar TrimRight para ignorar diferencias invisibles de espacios al final
      FileContent := TrimRight(FileLines[CurrentFileIdx]);
      DiffContent := TrimRight(DiffLine.Content);

      if FileContent <> DiffContent then Exit(False);

      Inc(CurrentFileIdx);
    end;
  end;
end;

function TDiffApplier.FindHunkPosition(const FileLines: TStringList; Hunk: TDiffHunk; out ActualStart: Integer): Boolean;
var
  SearchRadius, ExpectedStart, Offset: Integer;
begin
  ExpectedStart := Hunk.OriginalStart - 1;
  if ExpectedStart < 0 then ExpectedStart := 0;

  // 1. Intento exacto
  if MatchHunkAt(FileLines, Hunk, ExpectedStart) then
  begin
    ActualStart := ExpectedStart;
    Exit(True);
  end;

  // 2. Búsqueda difusa (+/- 20 líneas)
  SearchRadius := 20;
  for Offset := 1 to SearchRadius do
  begin
    // Abajo
    if MatchHunkAt(FileLines, Hunk, ExpectedStart + Offset) then
    begin
      ActualStart := ExpectedStart + Offset;
      Exit(True);
    end;
    // Arriba
    if (ExpectedStart - Offset >= 0) and MatchHunkAt(FileLines, Hunk, ExpectedStart - Offset) then
    begin
      ActualStart := ExpectedStart - Offset;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TDiffApplier.Apply(const OriginalContent: string; const DiffText: string; out NewContent: string; out ErrorMsg: string): Boolean;
var
  Parser: TDiffParser;
  Hunks: TList<TDiffHunk>;
  FileLines, NewLines: TStringList;
  I, J, ActualStart, CurrentPos: Integer;
  Hunk: TDiffHunk;
  DiffLine: TDiffLine;
begin
  Result := False;
  NewContent := '';
  ErrorMsg := '';

  Parser := TDiffParser.Create;
  FileLines := TStringList.Create;
  NewLines := TStringList.Create;
  try
    Hunks := Parser.Parse(DiffText);
    try
      if Hunks.Count = 0 then
      begin
        ErrorMsg := 'No se encontraron bloques de cambios (hunks) válidos.';
        Exit;
      end;

      FileLines.Text := OriginalContent;
      CurrentPos := 0;

      for I := 0 to Hunks.Count - 1 do
      begin
        Hunk := Hunks[I];

        if not FindHunkPosition(FileLines, Hunk, ActualStart) then
        begin
          ErrorMsg := Format('Fallo al aplicar Hunk #%d: No se encontró el contexto (Original Start: %d).', [I + 1, Hunk.OriginalStart]);
          Exit;
        end;

        if ActualStart < CurrentPos then
        begin
          ErrorMsg := Format('Fallo al aplicar Hunk #%d: Solapamiento detectado.', [I + 1]);
          Exit;
        end;

        // Copiar original previo
        while CurrentPos < ActualStart do
        begin
          NewLines.Add(FileLines[CurrentPos]);
          Inc(CurrentPos);
        end;

        // Aplicar Hunk
        for J := 0 to Hunk.Lines.Count - 1 do
        begin
          DiffLine := Hunk.Lines[J];
          case DiffLine.Operation of
            doContext:
            begin
              NewLines.Add(DiffLine.Content);
              Inc(CurrentPos);
            end;
            doDelete: Inc(CurrentPos);
            doAdd: NewLines.Add(DiffLine.Content);
          end;
        end;
      end;

      // Copiar resto del archivo
      while CurrentPos < FileLines.Count do
      begin
        NewLines.Add(FileLines[CurrentPos]);
        Inc(CurrentPos);
      end;

      NewContent := NewLines.Text;
      Result := True;

    finally
      for Hunk in Hunks do Hunk.Free;
      Hunks.Free;
    end;
  finally
    Parser.Free;
    FileLines.Free;
    NewLines.Free;
  end;
end;

end.
