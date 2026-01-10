unit uMakerAi.RAG.Graph.Lexer;

interface

uses
  System.SysUtils, System.Character;

type
  { Tipos de tokens soportados por el lenguaje GQL Lite de MakerAI }
  TTokenKind = (tkEOF,
    // Estructura
    tkLParen, tkRParen, // ( )
    tkLBracket, tkRBracket, // [ ]
    tkLBrace, tkRBrace, // { }
    tkColon, tkComma, tkDot, // : , .
    tkDash, // -
    tkArrowLeft, // <-
    tkArrowRight, // ->

    // Literales
    tkIdentifier, // variables, labels, keys
    tkString, // 'texto' o "texto"
    tkNumber, // 123 o 123.45
    tkBoolean, // true o false
    tkNull, // null

    tkCount, tkSum, tkAvg, tkContains, tkDepth, // profundidad de la búsqueda

    // Palabras Reservadas
    tkMatch, tkWhere, tkReturn, tkAnd, tkOr, tkShow, // SHOW
    tkLabels, // LABELS
    tkEdges, // EDGES (o RELATIONSHIPS si prefieres)

    tkShortest, tkPath, tkTo, // Shortest Path
    tkGet, tkCentrality, // Centrality
    tkDegrees, tkTop, // Degrees

    // Operadores Comparativos
    tkEqual, // =
    tkNotEqual, // <>
    tkGreater, // >
    tkGreaterEqual, // >=
    tkLess, // <
    tkLessEqual // <=
    );

  TToken = record
    Kind: TTokenKind;
    Text: string;
    Position: Integer;
  end;

  { Analizador Léxico (Lexer) }
  TGraphLexer = class
  private
    FText: string;
    FPos: Integer;
    function Peek: Char;
    function Next: Char;
    procedure SkipWhitespace;
    function IsEOF: Boolean;
    function ReadIdentifier: TToken;
    function ReadString: TToken;
    function ReadNumber: TToken;
  public
    constructor Create(const AText: string);
    function NextToken: TToken;
  end;

implementation

constructor TGraphLexer.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPos := 1;
end;

function TGraphLexer.Peek: Char;
begin
  if FPos > Length(FText) then
    Result := #0
  else
    Result := FText[FPos];
end;

function TGraphLexer.Next: Char;
begin
  Result := Peek;
  Inc(FPos);
end;

procedure TGraphLexer.SkipWhitespace;
begin
  while (not IsEOF) and Peek.IsWhiteSpace do
    Next;
end;

function TGraphLexer.IsEOF: Boolean;
begin
  Result := FPos > Length(FText);
end;

function TGraphLexer.ReadIdentifier: TToken;
var
  Start: Integer;
  UpperText: string;
begin
  Start := FPos;
  while (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_')) do
    Next;

  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;

  UpperText := Result.Text.ToUpper;

  if UpperText = 'MATCH' then
    Result.Kind := tkMatch
  else if UpperText = 'WHERE' then
    Result.Kind := tkWhere
  else if UpperText = 'RETURN' then
    Result.Kind := tkReturn
  else if UpperText = 'AND' then
    Result.Kind := tkAnd
  else if UpperText = 'OR' then
    Result.Kind := tkOr
  else if (UpperText = 'TRUE') or (UpperText = 'FALSE') then
    Result.Kind := tkBoolean
  else if UpperText = 'NULL' then
    Result.Kind := tkNull

  else if UpperText = 'SHOW' then
    Result.Kind := tkShow
  else if UpperText = 'LABELS' then
    Result.Kind := tkLabels
  else if UpperText = 'EDGES' then
    Result.Kind := tkEdges

  else if UpperText = 'CONTAINS' then
    Result.Kind := tkContains
  else if UpperText = 'SUM' then
    Result.Kind := tkSum
  else if UpperText = 'AVG' then
    Result.Kind := tkAvg
  else if UpperText = 'COUNT' then
    Result.Kind := tkCount

  else if UpperText = 'DEPTH' then
    Result.Kind := tkDepth

  else if UpperText = 'SHORTEST' then
    Result.Kind := tkShortest
  else if UpperText = 'PATH' then
    Result.Kind := tkPath
  else if UpperText = 'TO' then
    Result.Kind := tkTo
  else if UpperText = 'GET' then
    Result.Kind := tkGet
  else if UpperText = 'CENTRALITY' then
    Result.Kind := tkCentrality
  else if UpperText = 'DEGREES' then
    Result.Kind := tkDegrees
  else if UpperText = 'TOP' then
    Result.Kind := tkTop

  else
    Result.Kind := tkIdentifier;
end;

function TGraphLexer.ReadString: TToken;
var
  Quote: Char;
  Start: Integer;
begin
  Quote := Next; // comilla inicial
  Start := FPos;

  while (not IsEOF) and (Peek <> Quote) do
    Next;

  Result.Kind := tkString;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start - 1;

  if not IsEOF then
    Next; // comilla final
end;

function TGraphLexer.ReadNumber: TToken;
var
  Start: Integer;
  HasSeparator: Boolean;
begin
  Start := FPos;
  HasSeparator := False;

  while (not IsEOF) and (Peek.IsDigit or (Peek = '.')) do
  begin
    if Peek = '.' then
    begin
      if HasSeparator then
        Break;
      HasSeparator := True;
    end;
    Next;
  end;

  Result.Kind := tkNumber;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TGraphLexer.NextToken: TToken;
begin
  SkipWhitespace;
  Result.Position := FPos;
  Result.Text := '';

  if IsEOF then
  begin
    Result.Kind := tkEOF;
    Exit;
  end;

  case Peek of
    '(':
      begin
        Result.Text := '(';
        Next;
        Result.Kind := tkLParen;
      end;
    ')':
      begin
        Result.Text := ')';
        Next;
        Result.Kind := tkRParen;
      end;
    '[':
      begin
        Result.Text := '[';
        Next;
        Result.Kind := tkLBracket;
      end;
    ']':
      begin
        Result.Text := ']';
        Next;
        Result.Kind := tkRBracket;
      end;
    '{':
      begin
        Result.Text := '{';
        Next;
        Result.Kind := tkLBrace;
      end;
    '}':
      begin
        Result.Text := '}';
        Next;
        Result.Kind := tkRBrace;
      end;
    ':':
      begin
        Result.Text := ':';
        Next;
        Result.Kind := tkColon;
      end;
    ',':
      begin
        Result.Text := ',';
        Next;
        Result.Kind := tkComma;
      end;
    '.':
      begin
        Result.Text := '.';
        Next;
        Result.Kind := tkDot;
      end;
    '=':
      begin
        Result.Text := '=';
        Next;
        Result.Kind := tkEqual;
      end;

    '<':
      begin
        Next;
        if Peek = '-' then
        begin
          Next;
          Result.Kind := tkArrowLeft;
          Result.Text := '<-';
        end
        else if Peek = '>' then
        begin
          Next;
          Result.Kind := tkNotEqual;
          Result.Text := '<>';
        end
        else if Peek = '=' then
        begin
          Next;
          Result.Kind := tkLessEqual;
          Result.Text := '<=';
        end
        else
        begin
          Result.Kind := tkLess;
          Result.Text := '<';
        end;
      end;

    '>':
      begin
        Next;
        if Peek = '=' then
        begin
          Next;
          Result.Kind := tkGreaterEqual;
          Result.Text := '>=';
        end
        else
        begin
          Result.Kind := tkGreater;
          Result.Text := '>';
        end;
      end;

    '-':
      begin
        Next;
        if Peek = '>' then
        begin
          Next;
          Result.Kind := tkArrowRight;
          Result.Text := '->';
        end
        else
        begin
          Result.Kind := tkDash;
          Result.Text := '-';
        end;
      end;

    '''', '"':
      Exit(ReadString);

  else
    if Peek.IsLetter or (Peek = '_') then
      Exit(ReadIdentifier)
    else if Peek.IsDigit then
      Exit(ReadNumber)
    else
      raise Exception.CreateFmt('Error Léxico: Carácter inesperado "%s" en posición %d', [Peek, FPos]);
  end;
end;

end.
