// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.RAG.Graph.GQL
// Parser GQL Lite para consultas de grafo (lexer + parser Cypher-like).
unit uMakerAi.RAG.Graph.GQL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Variants, TypInfo,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.MetaData;

type
  { Tipos de tokens soportados por el lenguaje GQL Lite de MakerAI }
  TTokenKind = (tkEOF,

    // Estructura y Delimitadores
    tkLParen, tkRParen, tkLBracket, tkRBracket, tkLBrace, tkRBrace,
    tkColon, tkComma, tkDot, tkDash, tkArrowLeft, tkArrowRight,

    // Literales
    tkIdentifier, tkString, tkNumber, tkBoolean, tkNull,

    // Palabras Reservadas: Estructura de Consulta
    tkMatch, tkWhere, tkReturn, tkShow, tkLabels, tkEdges,

    // Operadores Lógicos
    tkAnd, tkOr, tkNot,

    // Operadores de Comparación y Texto
    tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual,
    tkLike, tkILike, tkContains, tkIn, tkIs, tkAsterisk,

    // Funciones de Agregación y Modificadores
    tkCount, tkSum, tkAvg, tkDepth,

    // Palabras Reservadas: Algoritmos de Grafo
    tkShortest, tkPath, tkTo, tkGet, tkCentrality, tkDegrees, tkTop);

  TGraphCommandType = (cmdNone, cmdShowLabels, cmdShowEdges, cmdShortestPath,
      cmdCentrality, cmdDegrees);

  TToken = record
    Kind    : TTokenKind;
    Text    : string;
    Position: Integer;
  end;

  { TGraphLexer }
  TGraphLexer = class
  private
    FText: string;
    FPos : Integer;
    function  Peek: Char;
    function  Next: Char;
    procedure SkipWhitespace;
    function  IsEOF: Boolean;
    function  ReadIdentifier: TToken;
    function  ReadString: TToken;
    function  ReadNumber: TToken;
  public
    constructor Create(const AText: string);
    function NextToken: TToken;
  end;

  { TGraphParser }
  TGraphParser = class
  private
    FLexer                : TGraphLexer;
    FCurrent              : TToken;
    FNodeVarKeys          : TStringList;  // sorted, Objects = TMatchNodePattern (non-owning)
    FCommandType          : TGraphCommandType;
    FCommandSourcePattern : TMatchNodePattern;
    FCommandTargetPattern : TMatchNodePattern;
    FCommandLimit         : Integer;

    procedure    Advance;
    function     GetTokenTextAndNext: string;
    procedure    Expect(AKind: TTokenKind);

    function  ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
    function  ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
    procedure ParseProperties(AProps: TAiEmbeddingMetaData);

    function  ParseExpression: TGraphExpression;
    function  ParseAndExpression: TGraphExpression;
    function  ParseComparison: TGraphExpression;
    function  ParsePrimary: TGraphExpression;
    function  ParseValueList: Variant;

    procedure ParseMatchClause(AQuery: TGraphMatchQuery);
    procedure ParseWhereClause(AQuery: TGraphMatchQuery);
    procedure ParseReturnClause(AQuery: TGraphMatchQuery);
    procedure ClearCommandPatterns;

    function  FindNodeVar(const AVarName: string;
        out APattern: TMatchNodePattern): Boolean;
    procedure AddNodeVar(const AVarName: string; APattern: TMatchNodePattern);
  public
    constructor Create(const AText: string);
    destructor  Destroy; override;
    function    Parse: TGraphMatchQuery;

    property CommandType          : TGraphCommandType  read FCommandType;
    property CommandSourcePattern : TMatchNodePattern  read FCommandSourcePattern;
    property CommandTargetPattern : TMatchNodePattern  read FCommandTargetPattern;
    property CommandLimit         : Integer            read FCommandLimit;
  end;

implementation

// ---------------------------------------------------------------------------
// Helpers locales
// ---------------------------------------------------------------------------

function InvFS: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator  := '.';
  Result.ThousandSeparator := ',';
end;

function IsWhiteChar(C: Char): Boolean; inline;
begin
  Result := C in [#9, #10, #13, ' '];
end;

function IsDigitChar(C: Char): Boolean; inline;
begin
  Result := C in ['0'..'9'];
end;

function IsLetterChar(C: Char): Boolean; inline;
begin
  Result := C in ['A'..'Z', 'a'..'z'];
end;

function IsLetterOrDigitChar(C: Char): Boolean; inline;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9'];
end;

// ---------------------------------------------------------------------------
// TGraphLexer
// ---------------------------------------------------------------------------

constructor TGraphLexer.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPos  := 1;
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

function TGraphLexer.IsEOF: Boolean;
begin
  Result := FPos > Length(FText);
end;

procedure TGraphLexer.SkipWhitespace;
begin
  while (not IsEOF) and IsWhiteChar(Peek) do
    Next;
end;

function TGraphLexer.ReadString: TToken;
var
  Quote: Char;
  Start: Integer;
begin
  Quote := Next; // consumir comilla de apertura
  Start := FPos;
  while (not IsEOF) and (Peek <> Quote) do
    Next;
  Result.Kind     := tkString;
  Result.Text     := Copy(FText, Start, FPos - Start);
  Result.Position := Start - 1;
  if not IsEOF then
    Next; // consumir comilla de cierre
end;

function TGraphLexer.ReadNumber: TToken;
var
  Start : Integer;
  HasDot: Boolean;
begin
  Start  := FPos;
  HasDot := False;
  while (not IsEOF) and (IsDigitChar(Peek) or (Peek = '.')) do
  begin
    if Peek = '.' then
    begin
      if HasDot then Break;
      HasDot := True;
    end;
    Next;
  end;
  Result.Kind     := tkNumber;
  Result.Text     := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TGraphLexer.ReadIdentifier: TToken;
var
  Start     : Integer;
  UpperText : string;
begin
  Start := FPos;
  while (not IsEOF) and (IsLetterOrDigitChar(Peek) or (Peek = '_')) do
    Next;

  Result.Text     := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
  UpperText       := UpperCase(Result.Text);

  if      UpperText = 'MATCH'    then Result.Kind := tkMatch
  else if UpperText = 'WHERE'    then Result.Kind := tkWhere
  else if UpperText = 'RETURN'   then Result.Kind := tkReturn
  else if UpperText = 'AND'      then Result.Kind := tkAnd
  else if UpperText = 'OR'       then Result.Kind := tkOr
  else if UpperText = 'NOT'      then Result.Kind := tkNot
  else if (UpperText = 'TRUE') or (UpperText = 'FALSE') then
    Result.Kind := tkBoolean
  else if UpperText = 'NULL'     then Result.Kind := tkNull
  else if UpperText = 'SHOW'     then Result.Kind := tkShow
  else if UpperText = 'LABELS'   then Result.Kind := tkLabels
  else if UpperText = 'EDGES'    then Result.Kind := tkEdges
  else if UpperText = 'CONTAINS' then Result.Kind := tkContains
  else if UpperText = 'LIKE'     then Result.Kind := tkLike
  else if UpperText = 'ILIKE'    then Result.Kind := tkILike
  else if UpperText = 'IN'       then Result.Kind := tkIn
  else if UpperText = 'IS'       then Result.Kind := tkIs
  else if UpperText = 'SUM'      then Result.Kind := tkSum
  else if UpperText = 'AVG'      then Result.Kind := tkAvg
  else if UpperText = 'COUNT'    then Result.Kind := tkCount
  else if UpperText = 'DEPTH'    then Result.Kind := tkDepth
  else if UpperText = 'SHORTEST' then Result.Kind := tkShortest
  else if UpperText = 'PATH'     then Result.Kind := tkPath
  else if UpperText = 'TO'       then Result.Kind := tkTo
  else if UpperText = 'GET'      then Result.Kind := tkGet
  else if UpperText = 'CENTRALITY' then Result.Kind := tkCentrality
  else if UpperText = 'DEGREES'  then Result.Kind := tkDegrees
  else if UpperText = 'TOP'      then Result.Kind := tkTop
  else
    Result.Kind := tkIdentifier;
end;

function TGraphLexer.NextToken: TToken;
begin
  SkipWhitespace;
  Result.Position := FPos;
  Result.Text     := '';

  if IsEOF then
  begin
    Result.Kind := tkEOF;
    Exit;
  end;

  case Peek of
    '(':
      begin
        Result.Text := '('; Next; Result.Kind := tkLParen;
      end;
    ')':
      begin
        Result.Text := ')'; Next; Result.Kind := tkRParen;
      end;
    '[':
      begin
        Result.Text := '['; Next; Result.Kind := tkLBracket;
      end;
    ']':
      begin
        Result.Text := ']'; Next; Result.Kind := tkRBracket;
      end;
    '{':
      begin
        Result.Text := '{'; Next; Result.Kind := tkLBrace;
      end;
    '}':
      begin
        Result.Text := '}'; Next; Result.Kind := tkRBrace;
      end;
    ':':
      begin
        Result.Text := ':'; Next; Result.Kind := tkColon;
      end;
    ',':
      begin
        Result.Text := ','; Next; Result.Kind := tkComma;
      end;
    '.':
      begin
        Result.Text := '.'; Next; Result.Kind := tkDot;
      end;
    '=':
      begin
        Result.Text := '='; Next; Result.Kind := tkEqual;
      end;
    '*':
      begin
        Result.Text := '*'; Next; Result.Kind := tkAsterisk;
      end;
    '<':
      begin
        Next;
        if Peek = '-' then
        begin
          Next; Result.Kind := tkArrowLeft; Result.Text := '<-';
        end
        else if Peek = '>' then
        begin
          Next; Result.Kind := tkNotEqual; Result.Text := '<>';
        end
        else if Peek = '=' then
        begin
          Next; Result.Kind := tkLessEqual; Result.Text := '<=';
        end
        else
        begin
          Result.Kind := tkLess; Result.Text := '<';
        end;
      end;
    '>':
      begin
        Next;
        if Peek = '=' then
        begin
          Next; Result.Kind := tkGreaterEqual; Result.Text := '>=';
        end
        else
        begin
          Result.Kind := tkGreater; Result.Text := '>';
        end;
      end;
    '-':
      begin
        Next;
        if Peek = '>' then
        begin
          Next; Result.Kind := tkArrowRight; Result.Text := '->';
        end
        else
        begin
          Result.Kind := tkDash; Result.Text := '-';
        end;
      end;
    '''', '"':
      Result := ReadString;
  else
    if IsLetterChar(Peek) or (Peek = '_') then
      Result := ReadIdentifier
    else if IsDigitChar(Peek) then
      Result := ReadNumber
    else
      raise Exception.CreateFmt(
          'Error Léxico: Carácter inesperado "%s" en posición %d', [Peek, FPos]);
  end;
end;

// ---------------------------------------------------------------------------
// TGraphParser
// ---------------------------------------------------------------------------

constructor TGraphParser.Create(const AText: string);
begin
  inherited Create;
  FLexer                := TGraphLexer.Create(AText);
  FNodeVarKeys          := TStringList.Create;
  FNodeVarKeys.Sorted   := True;
  FNodeVarKeys.Duplicates := dupIgnore;
  FCommandType          := cmdNone;
  FCommandSourcePattern := nil;
  FCommandTargetPattern := nil;
  FCommandLimit         := 0;
  Advance;
end;

destructor TGraphParser.Destroy;
begin
  ClearCommandPatterns;
  FNodeVarKeys.Free;
  FLexer.Free;
  inherited;
end;

procedure TGraphParser.ClearCommandPatterns;
begin
  FreeAndNil(FCommandSourcePattern);
  FreeAndNil(FCommandTargetPattern);
end;

function TGraphParser.FindNodeVar(const AVarName: string;
    out APattern: TMatchNodePattern): Boolean;
var
  Idx: Integer;
begin
  Idx := FNodeVarKeys.IndexOf(AVarName);
  if Idx >= 0 then
  begin
    APattern := TMatchNodePattern(FNodeVarKeys.Objects[Idx]);
    Result   := True;
  end
  else
  begin
    APattern := nil;
    Result   := False;
  end;
end;

procedure TGraphParser.AddNodeVar(const AVarName: string;
    APattern: TMatchNodePattern);
begin
  FNodeVarKeys.AddObject(AVarName, APattern);
end;

procedure TGraphParser.Advance;
begin
  FCurrent := FLexer.NextToken;
end;

function TGraphParser.GetTokenTextAndNext: string;
begin
  Result := FCurrent.Text;
  Advance;
end;

procedure TGraphParser.Expect(AKind: TTokenKind);
begin
  if FCurrent.Kind <> AKind then
    raise Exception.CreateFmt(
        'Error Sintáctico: Se esperaba %s y se encontró "%s" en posición %d',
        [GetEnumName(TypeInfo(TTokenKind), Ord(AKind)),
         FCurrent.Text, FCurrent.Position]);
  Advance;
end;

function TGraphParser.ParseValueList: Variant;
var
  TmpVals : array of Variant;
  Count, I: Integer;
begin
  Expect(tkLBracket);
  Count := 0;
  SetLength(TmpVals, 0);

  if FCurrent.Kind <> tkRBracket then
  begin
    while True do
    begin
      SetLength(TmpVals, Count + 1);
      case FCurrent.Kind of
        tkString :
          TmpVals[Count] := FCurrent.Text;
        tkNumber :
          TmpVals[Count] := StrToFloat(FCurrent.Text, InvFS);
        tkBoolean:
          TmpVals[Count] := SameText(FCurrent.Text, 'TRUE');
      else
        raise Exception.CreateFmt(
            'Error: Valor de lista inválido. Se encontró "%s".', [FCurrent.Text]);
      end;
      Inc(Count);
      Advance;
      if FCurrent.Kind = tkComma then
        Advance
      else
        Break;
    end;
  end;
  Expect(tkRBracket);

  if Count = 0 then
    Result := Null
  else
  begin
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Count - 1 do
      Result[I] := TmpVals[I];
  end;
end;

function TGraphParser.ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
var
  VarName, LblName: string;
  Found           : TMatchNodePattern;
begin
  Expect(tkLParen);
  VarName := '';
  LblName := '';

  if FCurrent.Kind = tkIdentifier then
    VarName := GetTokenTextAndNext;

  if FCurrent.Kind = tkColon then
  begin
    Advance;
    LblName := FCurrent.Text;
    Expect(tkIdentifier);
  end;

  if (VarName <> '') and FindNodeVar(VarName, Found) then
  begin
    Result := Found;
    if (Result.NodeLabel = '') and (LblName <> '') then
      Result.NodeLabel := LblName;
  end
  else
  begin
    Result           := TMatchNodePattern.Create;
    Result.Variable  := VarName;
    Result.NodeLabel := LblName;

    if VarName <> '' then
      AddNodeVar(VarName, Result);

    AQuery.AddNodePattern(Result);
  end;

  if FCurrent.Kind = tkLBrace then
    ParseProperties(Result.Properties);

  Expect(tkRParen);
end;

function TGraphParser.ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
var
  Left, Right: Boolean;
begin
  Left := (FCurrent.Kind = tkArrowLeft);
  if Left then
    Advance
  else
    Expect(tkDash);

  Expect(tkLBracket);
  Result := TMatchEdgePattern.Create;

  if FCurrent.Kind = tkIdentifier then
    Result.Variable := GetTokenTextAndNext;

  if FCurrent.Kind = tkColon then
  begin
    Advance;
    Result.EdgeLabel := FCurrent.Text;
    Expect(tkIdentifier);
  end;

  if FCurrent.Kind = tkLBrace then
    ParseProperties(Result.Properties);

  Expect(tkRBracket);

  Right := (FCurrent.Kind = tkArrowRight);
  if Right then
    Advance
  else
    Expect(tkDash);

  if Left and Right then
    ADirection := gdBoth
  else if Left then
    ADirection := gdIncoming
  else if Right then
    ADirection := gdOutgoing
  else
    ADirection := gdBoth;
end;

procedure TGraphParser.ParseProperties(AProps: TAiEmbeddingMetaData);
var
  Key: string;
begin
  Expect(tkLBrace);
  while FCurrent.Kind <> tkRBrace do
  begin
    Key := FCurrent.Text;
    Expect(tkIdentifier);
    Expect(tkColon);

    case FCurrent.Kind of
      tkString :
        AProps[Key] := GetTokenTextAndNext;
      tkNumber :
        AProps[Key] := StrToFloat(GetTokenTextAndNext, InvFS);
      tkBoolean:
        AProps[Key] := SameText(GetTokenTextAndNext, 'true');
      tkNull   :
        begin
          AProps[Key] := Null;
          Advance;
        end;
    else
      raise Exception.CreateFmt(
          'Error: Valor de propiedad no soportado. Se encontró "%s".',
          [FCurrent.Text]);
    end;

    if FCurrent.Kind = tkComma then
      Advance;
  end;
  Expect(tkRBrace);
end;

procedure TGraphParser.ParseWhereClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkWhere);
  AQuery.WhereClause := ParseExpression;
end;

function TGraphParser.ParseExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseAndExpression;
  while FCurrent.Kind = tkOr do
  begin
    Advance;
    L := TBinaryExpr.Create(L, opOr, ParseAndExpression);
  end;
  Result := L;
end;

procedure TGraphParser.ParseMatchClause(AQuery: TGraphMatchQuery);
var
  SrcNode, DstNode: TMatchNodePattern;
  Edge            : TMatchEdgePattern;
  Dir             : TGraphDirection;
begin
  SrcNode := ParseNode(AQuery);
  while FCurrent.Kind in [tkDash, tkArrowLeft] do
  begin
    Edge           := ParseEdge(Dir);
    Edge.Direction := Dir;
    DstNode        := ParseNode(AQuery);
    AQuery.AddMatchClause(TMatchClause.Create(SrcNode.Variable, Edge,
        DstNode.Variable));
    SrcNode := DstNode;
  end;
end;

function TGraphParser.ParseAndExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseComparison;
  while FCurrent.Kind = tkAnd do
  begin
    Advance;
    L := TBinaryExpr.Create(L, opAnd, ParseComparison);
  end;
  Result := L;
end;

function TGraphParser.ParseComparison: TGraphExpression;
var
  L     : TGraphExpression;
  Op    : TBinaryOp;
  InList: Variant;
begin
  L := ParsePrimary;

  if FCurrent.Kind = tkIs then
  begin
    Advance;
    if FCurrent.Kind = tkNot then
    begin
      Advance;
      Expect(tkNull);
      Result := TBinaryExpr.Create(L, opIsNotNull, nil);
    end
    else if FCurrent.Kind = tkNull then
    begin
      Advance;
      Result := TBinaryExpr.Create(L, opIsNull, nil);
    end
    else
      raise Exception.Create(
          'Error: Se esperaba NULL o NOT NULL después de IS.');
    Exit;
  end;

  if FCurrent.Kind = tkNot then
  begin
    Advance;
    if FCurrent.Kind = tkIn then
    begin
      Advance;
      InList := ParseValueList;
      Result := TBinaryExpr.Create(L, opNotIn, TLiteralExpr.Create(InList));
      Exit;
    end
    else
      raise Exception.Create(
          'Error Sintáctico: Uso inválido de NOT. Se esperaba NOT IN.');
  end;

  if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual,
                       tkLess, tkLessEqual, tkContains, tkLike, tkILike, tkIn] then
  begin
    case FCurrent.Kind of
      tkEqual       : Op := opEqual;
      tkNotEqual    : Op := opNotEqual;
      tkGreater     : Op := opGreater;
      tkGreaterEqual: Op := opGreaterEqual;
      tkLess        : Op := opLess;
      tkLessEqual   : Op := opLessEqual;
      tkContains    : Op := opContains;
      tkLike        : Op := opLike;
      tkILike       : Op := opILike;
      tkIn          : Op := opIn;
    else
      Op := opEqual;
    end;
    Advance;

    if Op = opIn then
    begin
      InList := ParseValueList;
      Result := TBinaryExpr.Create(L, Op, TLiteralExpr.Create(InList));
    end
    else
      Result := TBinaryExpr.Create(L, Op, ParsePrimary);
  end
  else
    Result := L;
end;

function TGraphParser.ParsePrimary: TGraphExpression;
var
  V, P: string;
begin
  case FCurrent.Kind of
    tkLParen:
      begin
        Advance;
        Result := ParseExpression;
        Expect(tkRParen);
      end;
    tkIdentifier:
      begin
        V := GetTokenTextAndNext;
        Expect(tkDot);
        P := FCurrent.Text;
        Expect(tkIdentifier);
        Result := TPropertyExpr.Create(V, P);
      end;
    tkString:
      Result := TLiteralExpr.Create(GetTokenTextAndNext);
    tkBoolean:
      Result := TLiteralExpr.Create(SameText(GetTokenTextAndNext, 'true'));
    tkNumber:
      Result := TLiteralExpr.Create(StrToFloat(GetTokenTextAndNext, InvFS));
    tkNull:
      begin
        Advance;
        Result := TLiteralExpr.Create(Null);
      end;
  else
    raise Exception.CreateFmt(
        'Error: Se esperaba una expresión válida en el WHERE. Se encontró "%s".',
        [FCurrent.Text]);
  end;
end;

procedure TGraphParser.ParseReturnClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkReturn);
  if FCurrent.Kind = tkEOF then
    raise Exception.Create('Error: Se esperaba una expresión después de RETURN.');

  while True do
  begin
    if FCurrent.Kind in [tkCount, tkSum, tkAvg] then
    begin
      Advance;
      Expect(tkLParen);
      if FCurrent.Kind = tkIdentifier then
      begin
        Advance;
        if FCurrent.Kind = tkDot then
        begin
          Advance;
          Expect(tkIdentifier);
        end;
      end
      else if FCurrent.Kind = tkAsterisk then
        Advance
      else
        raise Exception.Create(
            'Error: Se esperaba una variable o propiedad dentro de la función.');
      Expect(tkRParen);
    end
    else if FCurrent.Kind = tkIdentifier then
    begin
      Advance;
      if FCurrent.Kind = tkDot then
      begin
        Advance;
        Expect(tkIdentifier);
      end;
    end
    else if FCurrent.Kind in [tkString, tkNumber, tkBoolean, tkNull] then
      Advance
    else
      raise Exception.CreateFmt(
          'Error Sintáctico: Expresión de retorno no válida "%s".',
          [FCurrent.Text]);

    if (FCurrent.Kind = tkIdentifier) and
        SameText(FCurrent.Text, 'AS') then
    begin
      Advance;
      Expect(tkIdentifier);
    end;

    if FCurrent.Kind = tkComma then
      Advance
    else
      Break;
  end;
end;

function TGraphParser.Parse: TGraphMatchQuery;
var
  TempQuery       : TGraphMatchQuery;
  Src, Dst        : TMatchNodePattern;
begin
  Result       := nil;
  FCommandType := cmdNone;
  ClearCommandPatterns;
  FNodeVarKeys.Clear;

  // Comandos de Introspección (SHOW ...)
  if FCurrent.Kind = tkShow then
  begin
    Advance;
    if FCurrent.Kind = tkLabels then
      FCommandType := cmdShowLabels
    else if FCurrent.Kind = tkEdges then
      FCommandType := cmdShowEdges
    else
      raise Exception.Create(
          'Error Sintáctico: Se esperaba LABELS o EDGES después de SHOW.');
    Advance;
    Exit;
  end;

  // Comandos de Algoritmos de Grafo
  if FCurrent.Kind = tkShortest then
  begin
    Advance;
    Expect(tkPath);
    FCommandType := cmdShortestPath;
    TempQuery := TGraphMatchQuery.Create;
    try
      Src := ParseNode(TempQuery);
      TempQuery.NodePatterns.FreeObjects := False;
      TempQuery.NodePatterns.Remove(Src);
      TempQuery.NodePatterns.FreeObjects := True;
      FCommandSourcePattern := Src;
      Expect(tkTo);
      Dst := ParseNode(TempQuery);
      TempQuery.NodePatterns.FreeObjects := False;
      TempQuery.NodePatterns.Remove(Dst);
      TempQuery.NodePatterns.FreeObjects := True;
      FCommandTargetPattern := Dst;
    finally
      TempQuery.Free;
    end;
    Exit;
  end;

  if FCurrent.Kind = tkGet then
  begin
    Advance;
    if FCurrent.Kind = tkCentrality then
    begin
      Advance;
      FCommandType := cmdCentrality;
      TempQuery := TGraphMatchQuery.Create;
      try
        Src := ParseNode(TempQuery);
        TempQuery.NodePatterns.FreeObjects := False;
        TempQuery.NodePatterns.Remove(Src);
        TempQuery.NodePatterns.FreeObjects := True;
        FCommandSourcePattern := Src;
      finally
        TempQuery.Free;
      end;
    end
    else if FCurrent.Kind = tkDegrees then
    begin
      Advance;
      FCommandType  := cmdDegrees;
      Expect(tkTop);
      FCommandLimit := StrToIntDef(GetTokenTextAndNext, 10);
    end
    else
      raise Exception.Create(
          'Error Sintáctico: Se esperaba CENTRALITY o DEGREES después de GET.');
    Exit;
  end;

  // Consultas MATCH estándar
  Result := TGraphMatchQuery.Create;
  try
    if FCurrent.Kind = tkMatch then
      Advance;

    ParseMatchClause(Result);
    while FCurrent.Kind = tkComma do
    begin
      Advance;
      ParseMatchClause(Result);
    end;

    if FCurrent.Kind = tkDepth then
    begin
      Advance;
      if FCurrent.Kind = tkNumber then
        Result.Depth := Trunc(StrToFloat(GetTokenTextAndNext, InvFS))
      else
        raise Exception.Create('Error: Se esperaba un número después de DEPTH.');
    end;

    if FCurrent.Kind = tkWhere then
      ParseWhereClause(Result);

    if FCurrent.Kind = tkReturn then
      ParseReturnClause(Result);

    if FCurrent.Kind <> tkEOF then
      raise Exception.CreateFmt(
          'Error: Sintaxis incorrecta. Se esperaba EOF, se encontró "%s" en posición %d',
          [FCurrent.Text, FCurrent.Position]);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.
