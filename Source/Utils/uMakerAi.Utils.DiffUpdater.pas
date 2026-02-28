// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Utils.DiffUpdater
// Aplica diffs unificados (unified diff) a contenido de texto.
unit uMakerAi.Utils.DiffUpdater;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs;

type
  TDiffOperation = (doContext, doAdd, doDelete);

  TDiffLine = record
    Operation: TDiffOperation;
    Content: string;
  end;

  // FPC: TList<TDiffLine> (record) no soportado como genérico.
  // Usamos array dinámico con Count explícito.
  TDiffLines = array of TDiffLine;

  TDiffHunk = class
  private
    FOriginalStart: Integer;
    FOriginalCount: Integer;
    FNewStart: Integer;
    FNewCount: Integer;
    FLines: TDiffLines;
    FLineCount: Integer;
    function GetLine(I: Integer): TDiffLine;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddLine(const ADL: TDiffLine);
    property OriginalStart: Integer read FOriginalStart write FOriginalStart;
    property OriginalCount: Integer read FOriginalCount write FOriginalCount;
    property NewStart: Integer read FNewStart write FNewStart;
    property NewCount: Integer read FNewCount write FNewCount;
    property LineCount: Integer read FLineCount;
    property Lines[I: Integer]: TDiffLine read GetLine;
  end;

  TDiffParser = class
  private
    function ParseHunkHeader(const Line: string; out Hunk: TDiffHunk): Boolean;
  public
    // Retorna TObjectList (OwnsObjects=True) con los hunks encontrados
    function Parse(const DiffText: string): TObjectList;
  end;

  TDiffApplier = class
  private
    function MatchHunkAt(const FileLines: TStringList; Hunk: TDiffHunk; StartIndex: Integer): Boolean;
    function FindHunkPosition(const FileLines: TStringList; Hunk: TDiffHunk; out ActualStart: Integer): Boolean;
  public
    function Apply(const OriginalContent: string; const DiffText: string;
        out NewContent: string; out ErrorMsg: string): Boolean;
  end;

implementation

{ TDiffHunk }

constructor TDiffHunk.Create;
begin
  inherited;
  FLineCount := 0;
  SetLength(FLines, 8);
end;

destructor TDiffHunk.Destroy;
begin
  SetLength(FLines, 0);
  inherited;
end;

function TDiffHunk.GetLine(I: Integer): TDiffLine;
begin
  Result := FLines[I];
end;

procedure TDiffHunk.AddLine(const ADL: TDiffLine);
begin
  if FLineCount >= Length(FLines) then
    SetLength(FLines, Length(FLines) * 2);
  FLines[FLineCount] := ADL;
  Inc(FLineCount);
end;

{ TDiffParser }

function TDiffParser.ParseHunkHeader(const Line: string; out Hunk: TDiffHunk): Boolean;
var
  P1, P2: Integer;
  Inner, OrigPart, NewPart: string;
  CommaPos, SpacePos: Integer;
begin
  Result := False;
  Hunk := nil;

  if Copy(Line, 1, 2) <> '@@' then Exit;

  try
    Hunk := TDiffHunk.Create;
    // Valores por defecto
    Hunk.OriginalStart := 1;
    Hunk.OriginalCount := 0;
    Hunk.NewStart := 1;
    Hunk.NewCount := 0;

    // Extraer contenido entre el primer '@@' y el segundo '@@'
    P1 := 3; // saltar primer '@@'
    P2 := Pos('@@', Line, P1);
    if P2 > 0 then
      Inner := Trim(Copy(Line, P1, P2 - P1))
    else
      Inner := Trim(Copy(Line, P1, Length(Line)));

    // Separar parte original (-) y nueva (+)
    SpacePos := Pos(' ', Inner);
    if SpacePos > 0 then
    begin
      OrigPart := Trim(Copy(Inner, 1, SpacePos - 1));
      NewPart := Trim(Copy(Inner, SpacePos + 1, Length(Inner)));
      // Recortar posible texto de contexto después de la parte nueva
      SpacePos := Pos(' ', NewPart);
      if SpacePos > 0 then
        NewPart := Copy(NewPart, 1, SpacePos - 1);
    end
    else
    begin
      OrigPart := Inner;
      NewPart := '';
    end;

    // Parsear parte original: -start,count
    if (Length(OrigPart) > 0) and (OrigPart[1] = '-') then
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

    // Parsear parte nueva: +start,count
    if (Length(NewPart) > 0) and (NewPart[1] = '+') then
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

    Result := True;

  except
    if Assigned(Hunk) then FreeAndNil(Hunk);
    Result := False;
  end;
end;

function TDiffParser.Parse(const DiffText: string): TObjectList;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
  CurrentHunk: TDiffHunk;
  DiffLine: TDiffLine;
  HasHeaders: Boolean;
begin
  Result := TObjectList.Create;
  Result.OwnsObjects := True;
  Lines := TStringList.Create;
  try
    Lines.Text := DiffText;
    CurrentHunk := nil;
    HasHeaders := False;

    // Detectar si hay cabeceras @@
    for I := 0 to Lines.Count - 1 do
      if Copy(Lines[I], 1, 2) = '@@' then
      begin
        HasHeaders := True;
        Break;
      end;

    for I := 0 to Lines.Count - 1 do
    begin
      Line := Lines[I];

      // Ignorar cabeceras de metadatos git (fuera de hunks)
      if (CurrentHunk = nil) and
         ((Copy(Line, 1, 3) = '+++') or (Copy(Line, 1, 3) = '---') or
          (Copy(Line, 1, 1) = '\')) then
        Continue;

      if Copy(Line, 1, 2) = '@@' then
      begin
        if ParseHunkHeader(Line, CurrentHunk) then
          Result.Add(CurrentHunk)
        else if Assigned(CurrentHunk) then
          FreeAndNil(CurrentHunk);
      end
      else
      begin
        // Recuperación: crear hunk virtual si no hay cabeceras
        if (CurrentHunk = nil) and not HasHeaders and
           ((Copy(Line, 1, 1) = '+') or (Copy(Line, 1, 1) = '-') or
            (Copy(Line, 1, 1) = ' ')) then
        begin
          CurrentHunk := TDiffHunk.Create;
          CurrentHunk.OriginalStart := 0;
          CurrentHunk.OriginalCount := 0;
          CurrentHunk.NewStart := 1;
          CurrentHunk.NewCount := 0;
          Result.Add(CurrentHunk);
        end;

        if Assigned(CurrentHunk) then
        begin
          if Copy(Line, 1, 1) = '+' then
          begin
            DiffLine.Operation := doAdd;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.AddLine(DiffLine);
          end
          else if Copy(Line, 1, 1) = '-' then
          begin
            DiffLine.Operation := doDelete;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.AddLine(DiffLine);
          end
          else if Copy(Line, 1, 1) = ' ' then
          begin
            DiffLine.Operation := doContext;
            DiffLine.Content := Copy(Line, 2, Length(Line));
            CurrentHunk.AddLine(DiffLine);
          end
          else if Line = '' then
          begin
            DiffLine.Operation := doContext;
            DiffLine.Content := '';
            CurrentHunk.AddLine(DiffLine);
          end;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

{ TDiffApplier }

function TDiffApplier.MatchHunkAt(const FileLines: TStringList;
    Hunk: TDiffHunk; StartIndex: Integer): Boolean;
var
  I, CurrentFileIdx: Integer;
  DiffLine: TDiffLine;
  FileContent, DiffContent: string;
begin
  Result := True;
  CurrentFileIdx := StartIndex;

  for I := 0 to Hunk.LineCount - 1 do
  begin
    DiffLine := Hunk.Lines[I];

    if DiffLine.Operation in [doContext, doDelete] then
    begin
      if CurrentFileIdx >= FileLines.Count then
      begin
        Result := False;
        Exit;
      end;

      FileContent := TrimRight(FileLines[CurrentFileIdx]);
      DiffContent := TrimRight(DiffLine.Content);

      if FileContent <> DiffContent then
      begin
        Result := False;
        Exit;
      end;

      Inc(CurrentFileIdx);
    end;
  end;
end;

function TDiffApplier.FindHunkPosition(const FileLines: TStringList;
    Hunk: TDiffHunk; out ActualStart: Integer): Boolean;
var
  SearchRadius, ExpectedStart, Offset: Integer;
begin
  ExpectedStart := Hunk.OriginalStart - 1;
  if ExpectedStart < 0 then ExpectedStart := 0;

  // 1. Intento exacto
  if MatchHunkAt(FileLines, Hunk, ExpectedStart) then
  begin
    ActualStart := ExpectedStart;
    Result := True;
    Exit;
  end;

  // 2. Búsqueda difusa (+/- 20 líneas)
  SearchRadius := 20;
  for Offset := 1 to SearchRadius do
  begin
    if MatchHunkAt(FileLines, Hunk, ExpectedStart + Offset) then
    begin
      ActualStart := ExpectedStart + Offset;
      Result := True;
      Exit;
    end;
    if (ExpectedStart - Offset >= 0) and
       MatchHunkAt(FileLines, Hunk, ExpectedStart - Offset) then
    begin
      ActualStart := ExpectedStart - Offset;
      Result := True;
      Exit;
    end;
  end;

  ActualStart := 0;
  Result := False;
end;

function TDiffApplier.Apply(const OriginalContent: string;
    const DiffText: string; out NewContent: string;
    out ErrorMsg: string): Boolean;
var
  Parser: TDiffParser;
  Hunks: TObjectList;
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
        Hunk := TDiffHunk(Hunks[I]);

        if not FindHunkPosition(FileLines, Hunk, ActualStart) then
        begin
          ErrorMsg := Format('Fallo al aplicar Hunk #%d: No se encontró el contexto (Original Start: %d).',
              [I + 1, Hunk.OriginalStart]);
          Exit;
        end;

        if ActualStart < CurrentPos then
        begin
          ErrorMsg := Format('Fallo al aplicar Hunk #%d: Solapamiento detectado.', [I + 1]);
          Exit;
        end;

        // Copiar líneas originales previas al hunk
        while CurrentPos < ActualStart do
        begin
          NewLines.Add(FileLines[CurrentPos]);
          Inc(CurrentPos);
        end;

        // Aplicar hunk
        for J := 0 to Hunk.LineCount - 1 do
        begin
          DiffLine := Hunk.Lines[J];
          case DiffLine.Operation of
            doContext:
              begin
                NewLines.Add(FileLines[CurrentPos]);
                Inc(CurrentPos);
              end;
            doDelete:
              Inc(CurrentPos);
            doAdd:
              NewLines.Add(DiffLine.Content);
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
      Hunks.Free;
    end;
  finally
    Parser.Free;
    FileLines.Free;
    NewLines.Free;
  end;
end;

end.
