unit uMakerAi.RAG.Graph.GQL;

interface

uses

  System.SysUtils, System.Character, System.Generics.Collections, System.Variants, System.TypInfo,
  uMakerAi.RAG.Graph.Core, // Necesario para TBinaryOp, TGraphExpression, etc.
  uMakerAi.RAG.MetaData; // Necesario para TFilterOperator

type
  { Tipos de tokens soportados por el lenguaje GQL Lite de MakerAI }
  TTokenKind = (tkEOF,

    // Estructura y Delimitadores
    tkLParen, // (
    tkRParen, // )
    tkLBracket, // [
    tkRBracket, // ]
    tkLBrace, // {
    tkRBrace, // }
    tkColon, // :
    tkComma, // ,
    tkDot, // .
    tkDash, // -
    tkArrowLeft, // <-
    tkArrowRight, // ->

    // Literales
    tkIdentifier, // Variables, etiquetas, claves (también `con espacios`)
    tkString, // 'Texto' o "Texto"
    tkNumber, // 123, 123.45, 1e3
    tkBoolean, // TRUE, FALSE
    tkNull, // NULL

    // Palabras Reservadas: Estructura de Consulta
    tkMatch, tkWhere, tkReturn, tkShow, tkLabels, tkEdges,

    // Operadores Lógicos
    tkAnd, tkOr, tkNot,

    // Operadores de Comparación y Texto (Alineados con TAiFilterCriteria)
    tkEqual, // =
    tkNotEqual, // <>
    tkGreater, // >
    tkGreaterEqual, // >=
    tkLess, // <
    tkLessEqual, // <=
    tkLike, // LIKE
    tkILike, // ILIKE
    tkContains, // CONTAINS
    tkIn, // IN
    tkIs, // IS
    tkAsterisk,

    // Funciones de Agregación y Modificadores
    tkCount, tkSum, tkAvg, tkDepth,

    // Palabras Reservadas: Algoritmos de Grafo
    tkShortest, tkPath, tkTo, tkGet, tkCentrality, tkDegrees, tkTop,

    // Parámetros y fin de sentencia
    tkParameter, // $nombre (Text = nombre sin el $)
    tkSemicolon, // ;

    // Aritmética (el - es tkDash y el * es tkAsterisk)
    tkPlus, // +
    tkSlash, // /
    tkPercent, // %
    tkPipe); // | (alternativas de etiqueta: [:A|B])

  TGraphCommandType = (cmdNone, cmdShowLabels, cmdShowEdges, cmdShortestPath, cmdCentrality, cmdDegrees,
    cmdShowSchema, cmdShowProperties, cmdCommunities, cmdNeighbors, cmdCentralityTop, cmdAllShortestPaths);

  TToken = record
    Kind: TTokenKind;
    Text: string;
    Position: Integer;
  end;

  { Error léxico o sintáctico de MakerGQL. Position: posición (desde 1) en el texto. }
  EGQLParserError = class(Exception)
  private
    FPosition: Integer;
  public
    constructor CreateAt(const AMsg: string; APosition: Integer);
    property Position: Integer read FPosition;
  end;

  { Analizador Léxico (Lexer) }
  TGraphLexer = class
  private
    FText: string;
    FPos: Integer;

    // Métodos internos de navegación
    function Peek: Char;
    function PeekAt(AOffset: Integer): Char;
    function Next: Char;
    procedure SkipWhitespace; // También salta comentarios // y /* */
    function IsEOF: Boolean;

    // Métodos de lectura de tipos específicos
    function ReadIdentifier: TToken;
    function ReadQuotedIdentifier: TToken;
    function ReadParameter: TToken;
    function ReadString: TToken;
    function ReadNumber: TToken;
  public
    constructor Create(const AText: string);
    function NextToken: TToken;
  end;

  // ================================================
  // ============ GRAPH PARSER ======================
  // ================================================

  TGraphParser = class
  private
    FLexer: TGraphLexer;
    FCurrent: TToken;
    FParams: TDictionary<string, Variant>; // Parámetros $nombre (no es dueño)
    FAnonCount: Integer;
    FPrevEnd: Integer; // Posición donde terminó el último token consumido
    FAllowAggregates: Boolean; // Dentro de RETURN/WITH/ORDER BY
    FScope: TList<string>; // Variables con nombre visibles (para RETURN *)
    FCurOptional: Boolean; // Dentro de un OPTIONAL MATCH
    FCurNewPatterns: TList<TMatchNodePattern>; // Donde anotar los patrones nuevos del OPTIONAL MATCH
    FPatternDepth: Integer; // > 0 dentro de un patrón del WHERE
    FCommandType: TGraphCommandType;
    FCommandSourcePattern: TMatchNodePattern;
    FCommandTargetPattern: TMatchNodePattern;
    FCommandLimit: Integer;
    FCommandLabel: string; // SHOW PROPERTIES OF <etiqueta>
    FExplain: Boolean;

    procedure Next;
    function GetTokenTextAndNext: string;
    procedure Expect(AKind: TTokenKind);
    function IsWord(const AWord: string): Boolean;
    procedure ExpectWord(const AWord: string);
    procedure ExpectEnd;
    function TakePropertyKey: string;
    function ParamValue(const AName: string; APosition: Integer): Variant;
    function ParseLiteralValue(out AValue: Variant): Boolean;
    function ParseIntegerValue(const AErrorMsg: string): Integer;

    // Métodos internos de parsing
    function ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
    function ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
    procedure ParseProperties(AProps: TDictionary<string, Variant>);

    // Métodos de parsing de expresiones (precedencia: OR < XOR < AND < NOT < comparación)
    function ParseExpression: TGraphExpression;
    function ParseXorExpression: TGraphExpression;
    function ParseAndExpression: TGraphExpression;
    function ParseNotExpression: TGraphExpression;
    function ParseComparison: TGraphExpression;
    function ParseAdditive: TGraphExpression; // + -
    function ParseMultiplicative: TGraphExpression; // * / %
    function ParseUnary: TGraphExpression; // -x
    function ParsePrimary: TGraphExpression;
    function ParseFunctionCall(const AName: string; APosition: Integer): TGraphExpression;
    function ParseValueList: Variant;
    function ParseInList: TGraphExpression;

    procedure ParseMatchClause(AQuery: TGraphMatchQuery; AClauses: TObjectList<TMatchClause>);
    function ParseLabelAlternatives: string;
    function LooksLikePattern: Boolean;
    function ParsePatternExpr(ABraced: Boolean): TGraphExpression;
    procedure ParseWhereClause(AQuery: TGraphMatchQuery);
    function ParseProjection(AIsReturn: Boolean): TGQLProjection;
    function IsWriteKeyword: Boolean;
    procedure ParseSetItems(AItems: TObjectList<TGQLSetItem>; ARemove: Boolean);
    function ParseWriteClause: TGQLWriteClause;
    function ParseAggregate(const AName: string; APosition: Integer): TGraphExpression;
    procedure ClearCommandPatterns;

  public
    // AParams: valores de los $parámetros. El parser no lo libera.
    constructor Create(const AText: string; AParams: TDictionary<string, Variant> = nil);
    destructor Destroy; override;
    function Parse: TGraphMatchQuery;

    property CommandType: TGraphCommandType read FCommandType;
    property CommandSourcePattern: TMatchNodePattern read FCommandSourcePattern;
    property CommandTargetPattern: TMatchNodePattern read FCommandTargetPattern;
    property CommandLimit: Integer read FCommandLimit;
    property CommandLabel: string read FCommandLabel;
    // EXPLAIN <consulta>: describir el plan sin ejecutarla
    property Explain: Boolean read FExplain;
  end;

implementation

uses
  uMakerAi.Localization;

{ EGQLParserError }

constructor EGQLParserError.CreateAt(const AMsg: string; APosition: Integer);
begin
  inherited Create(AMsg);
  FPosition := APosition;
end;

{ TGraphLexer }

constructor TGraphLexer.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPos := 1;
end;

function TGraphLexer.Peek: Char;
begin
  Result := PeekAt(0);
end;

function TGraphLexer.PeekAt(AOffset: Integer): Char;
begin
  if FPos + AOffset > Length(FText) then
    Result := #0
  else
    Result := FText[FPos + AOffset];
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
var
  StartPos: Integer;
begin
  while not IsEOF do
  begin
    if Peek.IsWhiteSpace then
      Next
    else if (Peek = '/') and (PeekAt(1) = '/') then
    begin
      // Comentario de línea: hasta el fin de línea
      while (not IsEOF) and not CharInSet(Peek, [#10, #13]) do
        Next;
    end
    else if (Peek = '/') and (PeekAt(1) = '*') then
    begin
      // Comentario de bloque
      StartPos := FPos;
      Inc(FPos, 2);
      while (not IsEOF) and not((Peek = '*') and (PeekAt(1) = '/')) do
        Next;
      if IsEOF then
        raise EGQLParserError.CreateAt(AiFmt('rag.gql.unterminated_comment', [StartPos]), StartPos);
      Inc(FPos, 2);
    end
    else
      Break;
  end;
end;

function TGraphLexer.ReadString: TToken;
var
  Quote, C: Char;
  StartPos: Integer;
  SB: TStringBuilder;
begin
  StartPos := FPos;
  Quote := Next; // Consumir comilla de apertura (' o ")
  SB := TStringBuilder.Create;
  try
    while True do
    begin
      if IsEOF then
        raise EGQLParserError.CreateAt(AiFmt('rag.gql.unterminated_string', [StartPos]), StartPos);
      C := Next;
      if C = Quote then
      begin
        // Comilla doblada = comilla literal: 'O''Brien'
        if Peek = Quote then
          SB.Append(Next)
        else
          Break; // Comilla de cierre
      end
      else if (C = '\') and not IsEOF then
      begin
        // Escapes: \' \" \\ \n \t \r. Cualquier otra barra queda literal (C:\datos)
        case Peek of
          '''', '"', '\':
            SB.Append(Next);
          'n':
            begin
              SB.Append(#10);
              Next;
            end;
          't':
            begin
              SB.Append(#9);
              Next;
            end;
          'r':
            begin
              SB.Append(#13);
              Next;
            end;
        else
          SB.Append(C);
        end;
      end
      else
        SB.Append(C);
    end;

    Result.Kind := tkString;
    Result.Text := SB.ToString; // Texto SIN las comillas
    Result.Position := StartPos;
  finally
    SB.Free;
  end;
end;

function TGraphLexer.ReadNumber: TToken;
var
  Start: Integer;
begin
  Start := FPos;

  while (not IsEOF) and Peek.IsDigit do
    Next;

  // Parte decimal: solo si al punto le sigue un dígito
  if (Peek = '.') and PeekAt(1).IsDigit then
  begin
    Next;
    while (not IsEOF) and Peek.IsDigit do
      Next;
  end;

  // Exponente: 1e5, 2.5E-3
  if CharInSet(Peek, ['e', 'E']) and (PeekAt(1).IsDigit or (CharInSet(PeekAt(1), ['+', '-']) and PeekAt(2).IsDigit)) then
  begin
    Next;
    if CharInSet(Peek, ['+', '-']) then
      Next;
    while (not IsEOF) and Peek.IsDigit do
      Next;
  end;

  Result.Kind := tkNumber;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TGraphLexer.ReadQuotedIdentifier: TToken;
var
  StartPos: Integer;
  C: Char;
  SB: TStringBuilder;
begin
  // `fecha de compra`: identificador con espacios o palabra reservada. `` = comilla literal
  StartPos := FPos;
  Next; // Consumir `
  SB := TStringBuilder.Create;
  try
    while True do
    begin
      if IsEOF then
        raise EGQLParserError.CreateAt(AiFmt('rag.gql.unterminated_string', [StartPos]), StartPos);
      C := Next;
      if C = '`' then
      begin
        if Peek = '`' then
          SB.Append(Next)
        else
          Break;
      end
      else
        SB.Append(C);
    end;
    Result.Kind := tkIdentifier;
    Result.Text := SB.ToString;
    Result.Position := StartPos;
  finally
    SB.Free;
  end;
end;

function TGraphLexer.ReadParameter: TToken;
var
  StartPos: Integer;
begin
  StartPos := FPos;
  Next; // Consumir $
  if not(Peek.IsLetter or (Peek = '_')) then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.lex_error', ['$', StartPos]), StartPos);

  while (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_')) do
    Next;

  Result.Kind := tkParameter;
  Result.Text := Copy(FText, StartPos + 1, FPos - StartPos - 1);
  Result.Position := StartPos;
end;

function TGraphLexer.ReadIdentifier: TToken;
var
  Start: Integer;
  UpperText: string;
begin
  Start := FPos;

  // Leemos letras, dígitos o guiones bajos
  while (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_')) do
    Next;

  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;

  // Normalizamos a mayúsculas para chequear palabras reservadas
  UpperText := Result.Text.ToUpper;

  // --- MAPEO DE PALABRAS RESERVADAS ---
  // BETWEEN, XOR, STARTS WITH y ENDS WITH NO son reservadas: el parser las
  // reconoce por contexto, así que siguen sirviendo como nombres.

  // Estructura Básica
  if UpperText = 'MATCH' then
    Result.Kind := tkMatch
  else if UpperText = 'WHERE' then
    Result.Kind := tkWhere
  else if UpperText = 'RETURN' then
    Result.Kind := tkReturn

    // Operadores Lógicos
  else if UpperText = 'AND' then
    Result.Kind := tkAnd
  else if UpperText = 'OR' then
    Result.Kind := tkOr
  else if UpperText = 'NOT' then
    Result.Kind := tkNot

    // Valores Booleanos / Nulos
  else if (UpperText = 'TRUE') or (UpperText = 'FALSE') then
    Result.Kind := tkBoolean
  else if UpperText = 'NULL' then
    Result.Kind := tkNull

    // Introspección
  else if UpperText = 'SHOW' then
    Result.Kind := tkShow
  else if UpperText = 'LABELS' then
    Result.Kind := tkLabels
  else if UpperText = 'EDGES' then
    Result.Kind := tkEdges

    // Operadores de Comparación Textual y Listas
  else if UpperText = 'CONTAINS' then
    Result.Kind := tkContains
  else if UpperText = 'LIKE' then
    Result.Kind := tkLike
  else if UpperText = 'ILIKE' then
    Result.Kind := tkILike
  else if UpperText = 'IN' then
    Result.Kind := tkIn
  else if UpperText = 'IS' then
    Result.Kind := tkIs

    // Agregación y Modificadores
  else if UpperText = 'SUM' then
    Result.Kind := tkSum
  else if UpperText = 'AVG' then
    Result.Kind := tkAvg
  else if UpperText = 'COUNT' then
    Result.Kind := tkCount
  else if UpperText = 'DEPTH' then
    Result.Kind := tkDepth

    // Algoritmos de Grafo
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

    // Si no es reservada, es un identificador
  else
    Result.Kind := tkIdentifier;
end;

function TGraphLexer.NextToken: TToken;

  procedure Single(AKind: TTokenKind);
  begin
    Result.Text := Next;
    Result.Kind := AKind;
  end;

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
    // Delimitadores Simples
    '(':
      Single(tkLParen);
    ')':
      Single(tkRParen);
    '[':
      Single(tkLBracket);
    ']':
      Single(tkRBracket);
    '{':
      Single(tkLBrace);
    '}':
      Single(tkRBrace);
    ':':
      Single(tkColon);
    ',':
      Single(tkComma);
    '.':
      Single(tkDot);
    '=':
      Single(tkEqual);
    '*':
      Single(tkAsterisk);
    ';':
      Single(tkSemicolon);
    '+':
      Single(tkPlus);
    '/': // // y /* ya los saltó SkipWhitespace como comentarios
      Single(tkSlash);
    '%':
      Single(tkPercent);
    '|':
      Single(tkPipe);

    // Operadores Compuestos que empiezan con '<'
    '<':
      begin
        Next;
        if Peek = '-' then
        begin
          Next;
          Result.Kind := tkArrowLeft; // <-
          Result.Text := '<-';
        end
        else if Peek = '>' then
        begin
          Next;
          Result.Kind := tkNotEqual; // <>
          Result.Text := '<>';
        end
        else if Peek = '=' then
        begin
          Next;
          Result.Kind := tkLessEqual; // <=
          Result.Text := '<=';
        end
        else
        begin
          Result.Kind := tkLess; // <
          Result.Text := '<';
        end;
      end;

    // Operadores Compuestos que empiezan con '>'
    '>':
      begin
        Next;
        if Peek = '=' then
        begin
          Next;
          Result.Kind := tkGreaterEqual; // >=
          Result.Text := '>=';
        end
        else
        begin
          Result.Kind := tkGreater; // >
          Result.Text := '>';
        end;
      end;

    // Operadores Compuestos que empiezan con '-'
    '-':
      begin
        Next;
        if Peek = '>' then
        begin
          Next;
          Result.Kind := tkArrowRight; // ->
          Result.Text := '->';
        end
        else
        begin
          Result.Kind := tkDash; // - (arista o signo negativo, según el contexto)
          Result.Text := '-';
        end;
      end;

    // Cadenas de texto
    '''', '"':
      Result := ReadString;

    // Identificador entre comillas invertidas
    '`':
      Result := ReadQuotedIdentifier;

    // Parámetro
    '$':
      Result := ReadParameter;

  else
    // Identificadores y Números
    if Peek.IsLetter or (Peek = '_') then
      Result := ReadIdentifier
    else if Peek.IsDigit then
      Result := ReadNumber
    else
      // Error: Carácter desconocido
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.lex_error', [Peek, FPos]), FPos);
  end;
end;


// ================================================
// ============ GRAPH PARSER ======================
// ================================================

{ TGraphParser }

constructor TGraphParser.Create(const AText: string; AParams: TDictionary<string, Variant>);
begin
  inherited Create;
  FLexer := TGraphLexer.Create(AText);
  FParams := AParams;
  FCommandType := cmdNone;
  FCommandSourcePattern := nil;
  FCommandTargetPattern := nil;
  FCommandLimit := 0;
  FAnonCount := 0;
  FScope := TList<string>.Create;
  Next;
end;

destructor TGraphParser.Destroy;
begin
  ClearCommandPatterns;
  FScope.Free;
  FLexer.Free;
  inherited;
end;

procedure TGraphParser.ClearCommandPatterns;
begin
  if Assigned(FCommandSourcePattern) then
    FreeAndNil(FCommandSourcePattern);
  if Assigned(FCommandTargetPattern) then
    FreeAndNil(FCommandTargetPattern);
end;

procedure TGraphParser.Next;
begin
  FPrevEnd := FLexer.FPos;
  FCurrent := FLexer.NextToken;
end;

function TGraphParser.GetTokenTextAndNext: string;
begin
  Result := FCurrent.Text;
  Next;
end;

procedure TGraphParser.Expect(AKind: TTokenKind);
begin
  if FCurrent.Kind <> AKind then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.expected', [GetEnumName(TypeInfo(TTokenKind), Ord(AKind)), FCurrent.Text,
      FCurrent.Position]), FCurrent.Position);
  Next;
end;

function TGraphParser.IsWord(const AWord: string): Boolean;
begin
  // Palabras de contexto (BETWEEN, XOR, STARTS, ENDS, WITH): llegan como identificador
  Result := (FCurrent.Kind = tkIdentifier) and SameText(FCurrent.Text, AWord);
end;

procedure TGraphParser.ExpectWord(const AWord: string);
begin
  if not IsWord(AWord) then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.expected', [AWord, FCurrent.Text, FCurrent.Position]), FCurrent.Position);
  Next;
end;

procedure TGraphParser.ExpectEnd;
begin
  // Se acepta un ; final
  if FCurrent.Kind = tkSemicolon then
    Next;
  if FCurrent.Kind <> tkEOF then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.expected_eof', [FCurrent.Text, FCurrent.Position]), FCurrent.Position);
end;

function TGraphParser.TakePropertyKey: string;
begin
  // Después del punto vale cualquier palabra, incluso reservada: n.count, r.path
  if (FCurrent.Kind = tkIdentifier) or (not(FCurrent.Kind in [tkString, tkParameter]) and (FCurrent.Text <> '') and
    (FCurrent.Text[1].IsLetter or (FCurrent.Text[1] = '_'))) then
    Result := GetTokenTextAndNext
  else
  begin
    Expect(tkIdentifier); // Lanza el error con la posición
    Result := '';
  end;
end;

function TGraphParser.ParamValue(const AName: string; APosition: Integer): Variant;
begin
  if (FParams = nil) or not FParams.TryGetValue(AName, Result) then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.param_missing', [AName, APosition]), APosition);
end;

function TGraphParser.ParseLiteralValue(out AValue: Variant): Boolean;
begin
  // Valor literal: 'texto', 12.5, -3, TRUE, NULL o $parametro. False si el token no es un valor.
  Result := True;
  case FCurrent.Kind of
    tkString:
      AValue := GetTokenTextAndNext;
    tkNumber:
      AValue := StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant);
    tkBoolean:
      AValue := SameText(GetTokenTextAndNext, 'TRUE');
    tkNull:
      begin
        AValue := Null;
        Next;
      end;
    tkParameter:
      begin
        AValue := ParamValue(FCurrent.Text, FCurrent.Position);
        Next;
      end;
    tkDash: // Número negativo: -5
      begin
        Next;
        if FCurrent.Kind <> tkNumber then
          raise EGQLParserError.CreateAt(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]), FCurrent.Position);
        AValue := -StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant);
      end;
  else
    Result := False;
  end;
end;

function TGraphParser.ParseIntegerValue(const AErrorMsg: string): Integer;
begin
  // Entero para DEPTH n y TOP n (acepta $parametro)
  case FCurrent.Kind of
    tkNumber:
      Result := Trunc(StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant));
    tkParameter:
      begin
        Result := ParamValue(FCurrent.Text, FCurrent.Position);
        Next;
      end;
  else
    raise EGQLParserError.CreateAt(AErrorMsg, FCurrent.Position);
  end;
end;

function TGraphParser.ParseValueList: Variant;
var
  Values: TArray<Variant>;
  Value: Variant;
  ValueList: TList<Variant>;
  I: Integer;
begin
  // Formato:  [valor1, valor2, valor3]
  Expect(tkLBracket);
  ValueList := TList<Variant>.Create;
  try
    if FCurrent.Kind <> tkRBracket then
    begin
      while True do
      begin
        // --- PARSEO DE UN VALOR ---
        if not ParseLiteralValue(Value) then
          raise EGQLParserError.CreateAt(AiFmt('rag.gql.list_value', [FCurrent.Text]), FCurrent.Position);
        ValueList.Add(Value);

        // --- SEPARADOR: COMA ---
        if FCurrent.Kind = tkComma then
          Next
        else
          Break; // Fin de la lista
      end;
    end;
    Expect(tkRBracket);

    // --- CONVERSIÓN A VARIANT ARRAY (para compatibilidad) ---
    SetLength(Values, ValueList.Count);
    for I := 0 to ValueList.Count - 1 do
      Values[I] := ValueList[I];

    Result := Values;
  finally
    ValueList.Free;
  end;
end;

function TGraphParser.ParseInList: TGraphExpression;
var
  V: Variant;
begin
  // IN [a, b, c], IN $lista (un VarArray, o un valor suelto) o IN expresion-que-da-lista (labels(n), split(...))
  if FCurrent.Kind = tkLBracket then
    V := ParseValueList
  else if FCurrent.Kind = tkParameter then
  begin
    V := ParamValue(FCurrent.Text, FCurrent.Position);
    Next;
  end
  else
    Exit(ParseAdditive);
  Result := TLiteralExpr.Create(V);
end;

function TGraphParser.ParseLabelAlternatives: string;
begin
  // :Persona  o  :Persona|Empresa  (también :A|:B, como en Cypher)
  Result := FCurrent.Text;
  Expect(tkIdentifier);
  while FCurrent.Kind = tkPipe do
  begin
    Next;
    if FCurrent.Kind = tkColon then
      Next;
    Result := Result + '|' + FCurrent.Text;
    Expect(tkIdentifier);
  end;
end;

function TGraphParser.ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
var
  VarName, LblName: string;
begin
  Expect(tkLParen);
  VarName := '';
  LblName := '';

  // Variable opcional: (p)
  if FCurrent.Kind = tkIdentifier then
    VarName := GetTokenTextAndNext;

  // Etiqueta opcional: (:Persona) o (:Persona|Empresa)
  if FCurrent.Kind = tkColon then
  begin
    Next;
    LblName := ParseLabelAlternatives;
  end;

  // Nodo anónimo: nombre interno único ('#' no lo produce el lexer, así que no choca
  // con variables del usuario). Antes todos compartían la clave '' y se pisaban.
  if VarName = '' then
  begin
    Inc(FAnonCount);
    VarName := '#' + IntToStr(FAnonCount);
  end;

  // Binding o Creación de Variable (en la consulta que se está armando: la principal,
  // o la de un patrón dentro del WHERE, que tiene sus propios patrones)
  Result := AQuery.NodePatternByVariable[VarName];
  if Assigned(Result) then
  begin
    // Reutilizar nodo si existe. Actualizar etiqueta si fue provista.
    if (Result.NodeLabel = '') and (LblName <> '') then
      Result.NodeLabel := LblName;
  end
  else
  begin
    // Nueva variable (o nodo anónimo)
    Result := TMatchNodePattern.Create;
    Result.Variable := VarName;
    Result.NodeLabel := LblName;
    Result.Optional := FCurOptional;
    AQuery.AddNodePattern(Result);
    if Assigned(FCurNewPatterns) then
      FCurNewPatterns.Add(Result);
    if (FPatternDepth = 0) and not VarName.StartsWith('#') and not FScope.Contains(VarName) then
      FScope.Add(VarName);
  end;

  // Propiedades opcionales: {name: 'Juan'}
  if FCurrent.Kind = tkLBrace then
    ParseProperties(Result.Properties);

  Expect(tkRParen);
end;

function TGraphParser.ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
var
  Left, Right: Boolean;
  HopPos: Integer;
begin
  // Detección de flecha izquierda <-
  Left := (FCurrent.Kind = tkArrowLeft);
  if Left then
    Next
  else
    Expect(tkDash);

  Expect(tkLBracket);
  Result := TMatchEdgePattern.Create;
  try
    // Variable de arista opcional [r]
    if FCurrent.Kind = tkIdentifier then
    begin
      Result.Variable := GetTokenTextAndNext;
      if (FPatternDepth = 0) and not FScope.Contains(Result.Variable) then
        FScope.Add(Result.Variable);
    end;

    // Tipo de relación opcional [:KNOWS] o [:KNOWS|LIKES]
    if FCurrent.Kind = tkColon then
    begin
      Next;
      Result.EdgeLabel := ParseLabelAlternatives;
    end;

    // Longitud variable: *, *3, *1..3, *..3, *2..
    if FCurrent.Kind = tkAsterisk then
    begin
      HopPos := FCurrent.Position;
      Next;
      Result.MinHops := 1;
      Result.MaxHops := -1;
      if FCurrent.Kind in [tkNumber, tkParameter] then
      begin
        Result.MinHops := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
        Result.MaxHops := Result.MinHops; // *3 = exactamente 3
      end;
      if FCurrent.Kind = tkDot then
      begin
        Next;
        Expect(tkDot);
        Result.MaxHops := -1;
        if FCurrent.Kind in [tkNumber, tkParameter] then
          Result.MaxHops := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
      end;
      if (Result.MinHops < 0) or ((Result.MaxHops >= 0) and (Result.MaxHops < Result.MinHops)) then
        raise EGQLParserError.CreateAt(AiFmt('rag.gql.hops_range', [Result.MinHops, Result.MaxHops]), HopPos);
    end;

    // Propiedades de arista {since: 2020}
    if FCurrent.Kind = tkLBrace then
      ParseProperties(Result.Properties);

    Expect(tkRBracket);

    // Detección de flecha derecha ->
    Right := (FCurrent.Kind = tkArrowRight);
    if Right then
      Next
    else
      Expect(tkDash);
  except
    Result.Free;
    raise;
  end;

  // Determinar la dirección
  if Left and Right then
    ADirection := gdBoth
  else if Left then
    ADirection := gdIncoming
  else if Right then
    ADirection := gdOutgoing
  else
    ADirection := gdBoth; // Si es solo guiones -[]-
end;

procedure TGraphParser.ParseProperties(AProps: TDictionary<string, Variant>);
var
  Key: string;
  Value: Variant;
begin
  Expect(tkLBrace);
  while FCurrent.Kind <> tkRBrace do
  begin
    Key := FCurrent.Text;
    Expect(tkIdentifier);
    Expect(tkColon);

    // --- Leer el Valor de la Propiedad ---
    if not ParseLiteralValue(Value) then
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.property_value', [FCurrent.Text]), FCurrent.Position);
    AProps.AddOrSetValue(Key, Value);

    if FCurrent.Kind = tkComma then
      Next;
  end;
  Expect(tkRBrace);
end;

procedure TGraphParser.ParseWhereClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkWhere);
  AQuery.WhereClause := ParseExpression;
end;

procedure TGraphParser.ParseMatchClause(AQuery: TGraphMatchQuery; AClauses: TObjectList<TMatchClause>);
var
  SrcNode, DstNode: TMatchNodePattern;
  Edge: TMatchEdgePattern;
  Dir: TGraphDirection;
  PathVar: string;
  PathPos: Integer;
  Def: TGQLPathDef;
begin
  // 0. Variable de camino opcional: p = (a)-[...]->(b)
  PathVar := '';
  if FCurrent.Kind = tkIdentifier then
  begin
    PathPos := FCurrent.Position;
    PathVar := GetTokenTextAndNext;
    Expect(tkEqual);
    // Solo en el MATCH principal (los caminos se arman sobre sus cláusulas)
    if FCurOptional or (FPatternDepth > 0) then
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.path_optional', [PathVar]), PathPos);
  end;

  // 1. Parsear el nodo inicial del patrón: (a)
  SrcNode := ParseNode(AQuery);
  Def.Variable := PathVar;
  Def.StartVar := SrcNode.Variable;
  Def.FirstClause := AClauses.Count;

  // 2. Bucle para relaciones encadenadas.
  // Ejemplo: (a)-[r1]->(b)-[r2]->(c)
  // El bucle continúa mientras el token actual indique el inicio de una arista
  // (un guion '-' o una flecha izquierda '<-').
  while FCurrent.Kind in [tkDash, tkArrowLeft] do
  begin
    // A. Parsear la estructura de la arista -[...]-, ->, <-
    Edge := ParseEdge(Dir);
    Edge.Direction := Dir;

    // B. Parsear el nodo siguiente en la cadena
    try
      DstNode := ParseNode(AQuery);
    except
      Edge.Free;
      raise;
    end;

    // C. Crear la cláusula: "VariableOrigen + PatrónArista + VariableDestino"
    // Nota: TMatchClause toma posesión de la memoria del objeto 'Edge'.
    AClauses.Add(TMatchClause.Create(SrcNode.Variable, Edge, DstNode.Variable));

    // D. El destino (b) se convierte en el origen de la siguiente conexión
    SrcNode := DstNode;
  end;

  if PathVar <> '' then
  begin
    Def.LastClause := AClauses.Count - 1;
    AQuery.Paths.Add(Def);
    if not FScope.Contains(PathVar) then
      FScope.Add(PathVar);
  end;
end;

function TGraphParser.LooksLikePattern: Boolean;
var
  SavePos, SavePrev: Integer;
  SaveTok: TToken;
begin
  // En un WHERE, '(' puede abrir un paréntesis o un patrón: (p)-[:X]->(...), (:Cafe), (p {a: 1}).
  // Se mira adelante sin consumir nada.
  SavePos := FLexer.FPos;
  SaveTok := FCurrent;
  SavePrev := FPrevEnd;
  try
    Result := False;
    Next; // (
    if FCurrent.Kind = tkColon then
      Exit(True);
    if FCurrent.Kind = tkIdentifier then
    begin
      Next;
      if FCurrent.Kind in [tkColon, tkLBrace] then
        Exit(True);
    end;
    if FCurrent.Kind = tkRParen then
    begin
      Next;
      if FCurrent.Kind = tkArrowLeft then
        Exit(True);
      if FCurrent.Kind = tkDash then
      begin
        Next;
        Exit(FCurrent.Kind = tkLBracket); // (n) - 1 es una resta, (n)-[ es una arista
      end;
    end;
  finally
    FLexer.FPos := SavePos;
    FCurrent := SaveTok;
    FPrevEnd := SavePrev;
  end;
end;

function TGraphParser.ParsePatternExpr(ABraced: Boolean): TGraphExpression;
var
  Q: TGraphMatchQuery;
  SaveOptional: Boolean;
  SaveNew: TList<TMatchNodePattern>;
begin
  // Patrón como condición. ABraced: EXISTS { [MATCH] patrón {, patrón} [WHERE expr] }
  Q := TGraphMatchQuery.Create;
  SaveOptional := FCurOptional;
  SaveNew := FCurNewPatterns;
  FCurOptional := False;
  FCurNewPatterns := nil;
  Inc(FPatternDepth);
  try
    try
      if ABraced then
      begin
        Expect(tkLBrace);
        if FCurrent.Kind = tkMatch then
          Next;
      end;
      ParseMatchClause(Q, Q.MatchClauses);
      while ABraced and (FCurrent.Kind = tkComma) do
      begin
        Next;
        ParseMatchClause(Q, Q.MatchClauses);
      end;
      if ABraced then
      begin
        if FCurrent.Kind = tkWhere then
        begin
          Next;
          Q.WhereClause := ParseExpression;
        end;
        Expect(tkRBrace);
      end;
    except
      Q.Free;
      raise;
    end;
  finally
    Dec(FPatternDepth);
    FCurOptional := SaveOptional;
    FCurNewPatterns := SaveNew;
  end;
  Result := TPatternExpr.Create(Q);
end;

// --- PARSER DE EXPRESIONES (WHERE) ---

function TGraphParser.ParseExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseXorExpression;
  try
    while FCurrent.Kind = tkOr do
    begin
      Next;
      L := TBinaryExpr.Create(L, opOr, ParseXorExpression);
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

function TGraphParser.ParseXorExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseAndExpression;
  try
    while IsWord('XOR') do
    begin
      Next;
      L := TBinaryExpr.Create(L, opXor, ParseAndExpression);
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

function TGraphParser.ParseAndExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseNotExpression;
  try
    while FCurrent.Kind = tkAnd do
    begin
      Next;
      L := TBinaryExpr.Create(L, opAnd, ParseNotExpression);
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

function TGraphParser.ParseNotExpression: TGraphExpression;
begin
  // NOT general: NOT (a.x = 1 OR a.y = 2), NOT NOT ...
  if FCurrent.Kind = tkNot then
  begin
    Next;
    Result := TUnaryExpr.Create(uoNot, ParseNotExpression);
  end
  else
    Result := ParseComparison;
end;

function TGraphParser.ParseComparison: TGraphExpression;
var
  L, Lo, Hi: TGraphExpression;
  Op: TBinaryOp;
  Negated, StrictGt: Boolean;
begin
  L := ParseAdditive;
  try
    // --- Operadores IS (IS NULL, IS NOT NULL) ---
    if FCurrent.Kind = tkIs then
    begin
      Next; // Consume IS
      if FCurrent.Kind = tkNot then
      begin
        Next; // Consume NOT
        Expect(tkNull);
        Result := TBinaryExpr.Create(L, opIsNotNull, nil); // Unario: Left es la propiedad
      end
      else if FCurrent.Kind = tkNull then
      begin
        Next; // Consume NULL
        Result := TBinaryExpr.Create(L, opIsNull, nil);
      end
      else
        raise EGQLParserError.CreateAt(AiStr('rag.gql.is_null'), FCurrent.Position);
      Exit;
    end;

    // --- NOT pospuesto: NOT IN, NOT BETWEEN, NOT LIKE, NOT CONTAINS, NOT STARTS WITH... ---
    Negated := FCurrent.Kind = tkNot;
    if Negated then
      Next;

    if FCurrent.Kind = tkIn then
    begin
      Next; // Consume IN
      if Negated then
        Op := opNotIn
      else
        Op := opIn;
      Result := TBinaryExpr.Create(L, Op, ParseInList);
      Exit;
    end;

    if IsWord('BETWEEN') then
    begin
      // x BETWEEN a AND b (ambos extremos incluidos). El AND es parte del BETWEEN.
      Next;
      Lo := ParseAdditive;
      try
        Expect(tkAnd);
        Hi := ParseAdditive;
      except
        Lo.Free;
        raise;
      end;
      Result := TBetweenExpr.Create(L, Lo, Hi);
    end
    else if IsWord('SIMILAR') then
    begin
      // n SIMILAR TO 'texto' [> umbral | >= umbral]: similitud de embeddings (por defecto >= 0.5)
      if not(L is TVariableExpr) then
        raise EGQLParserError.CreateAt(AiStr('rag.gql.similar_var'), FCurrent.Position);
      Next;
      Expect(tkTo); // TO es reservada (SHORTEST PATH ... TO ...)
      Lo := ParseAdditive;
      Hi := nil;
      StrictGt := False;
      try
        if FCurrent.Kind in [tkGreater, tkGreaterEqual] then
        begin
          StrictGt := FCurrent.Kind = tkGreater;
          Next;
          Hi := ParseAdditive;
        end;
      except
        Lo.Free;
        raise;
      end;
      Result := TSimilarExpr.Create(L, Lo, Hi, StrictGt);
    end
    else if IsWord('STARTS') or IsWord('ENDS') then
    begin
      if IsWord('STARTS') then
        Op := opStartsWith
      else
        Op := opEndsWith;
      Next;
      ExpectWord('WITH');
      Result := TBinaryExpr.Create(L, Op, ParseAdditive);
    end
    else if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains, tkLike, tkILike] then
    begin
      case FCurrent.Kind of
        tkEqual:
          Op := opEqual;
        tkNotEqual:
          Op := opNotEqual;
        tkGreater:
          Op := opGreater;
        tkGreaterEqual:
          Op := opGreaterEqual;
        tkLess:
          Op := opLess;
        tkLessEqual:
          Op := opLessEqual;
        tkContains:
          Op := opContains;
        tkLike:
          Op := opLike;
      else
        Op := opILike;
      end;

      // NOT solo tiene sentido delante de los operadores de texto
      if Negated and not(Op in [opContains, opLike, opILike]) then
        raise EGQLParserError.CreateAt(AiStr('rag.gql.not_in'), FCurrent.Position);

      Next; // Consumir el operador
      Result := TBinaryExpr.Create(L, Op, ParseAdditive);
    end
    else if Negated then
      raise EGQLParserError.CreateAt(AiStr('rag.gql.not_in'), FCurrent.Position)
    else
      Result := L;
  except
    L.Free;
    raise;
  end;

  if Negated then
    Result := TUnaryExpr.Create(uoNot, Result);
end;

// --- Aritmética: + - (aditiva) < * / % (multiplicativa) < signo ---

function TGraphParser.ParseAdditive: TGraphExpression;
var
  L: TGraphExpression;
  Op: TBinaryOp;
begin
  L := ParseMultiplicative;
  try
    while FCurrent.Kind in [tkPlus, tkDash] do
    begin
      if FCurrent.Kind = tkPlus then
        Op := opAdd
      else
        Op := opSub;
      Next;
      L := TBinaryExpr.Create(L, Op, ParseMultiplicative);
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

function TGraphParser.ParseMultiplicative: TGraphExpression;
var
  L: TGraphExpression;
  Op: TBinaryOp;
begin
  L := ParseUnary;
  try
    while FCurrent.Kind in [tkAsterisk, tkSlash, tkPercent] do
    begin
      case FCurrent.Kind of
        tkAsterisk:
          Op := opMul;
        tkSlash:
          Op := opDiv;
      else
        Op := opMod;
      end;
      Next;
      L := TBinaryExpr.Create(L, Op, ParseUnary);
    end;
  except
    L.Free;
    raise;
  end;
  Result := L;
end;

function TGraphParser.ParseUnary: TGraphExpression;
var
  Operand: TGraphExpression;
begin
  // Signo negativo: -5, -$limite, -(a.x), -c.precio
  if FCurrent.Kind = tkDash then
  begin
    Next;
    Operand := ParseUnary;
    if (Operand is TLiteralExpr) and VarIsNumeric(TLiteralExpr(Operand).Value) and
      not VarIsType(TLiteralExpr(Operand).Value, varBoolean) then
    begin
      TLiteralExpr(Operand).Value := -TLiteralExpr(Operand).Value;
      Result := Operand;
    end
    else
      Result := TUnaryExpr.Create(uoNegate, Operand);
  end
  else
    Result := ParsePrimary;
end;

function TGraphParser.ParsePrimary: TGraphExpression;
var
  V, P: string;
  Value: Variant;
  TokPos: Integer;
begin
  case FCurrent.Kind of
    tkLParen: // Agrupación: (expresión), o un patrón como condición: (p)-[:X]->(:Y)
      if LooksLikePattern then
        Result := ParsePatternExpr(False)
      else
      begin
        Next;
        Result := ParseExpression;
        try
          Expect(tkRParen);
        except
          Result.Free;
          raise;
        end;
      end;

    // n.nombre (propiedad), lower(...) (función) o n (variable suelta, p. ej. id(n))
    tkIdentifier:
      begin
        TokPos := FCurrent.Position;
        V := GetTokenTextAndNext;
        if FCurrent.Kind = tkDot then
        begin
          Next;
          P := TakePropertyKey;
          Result := TPropertyExpr.Create(V, P);
        end
        else if (FCurrent.Kind = tkLBrace) and SameText(V, 'EXISTS') then
          Result := ParsePatternExpr(True)
        else if (FCurrent.Kind = tkLParen) and (SameText(V, 'min') or SameText(V, 'max') or SameText(V, 'collect')) then
          Result := ParseAggregate(V, TokPos)
        else if FCurrent.Kind = tkLParen then
          Result := ParseFunctionCall(V, TokPos)
        else
          Result := TVariableExpr.Create(V);
      end;

    // count(...), sum(...), avg(...): palabras reservadas
    tkCount, tkSum, tkAvg:
      begin
        TokPos := FCurrent.Position;
        V := GetTokenTextAndNext;
        Result := ParseAggregate(V, TokPos);
      end;

    // labels(n): LABELS es palabra reservada (SHOW LABELS)
    tkLabels:
      begin
        TokPos := FCurrent.Position;
        V := GetTokenTextAndNext;
        if FCurrent.Kind <> tkLParen then
          raise EGQLParserError.CreateAt(AiFmt('rag.gql.where_expr', [V]), TokPos);
        Result := ParseFunctionCall(V, TokPos);
      end;

    // Literales y parámetros
    tkString, tkNumber, tkBoolean, tkNull, tkParameter:
      begin
        ParseLiteralValue(Value);
        Result := TLiteralExpr.Create(Value);
      end;
  else
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.where_expr', [FCurrent.Text]), FCurrent.Position);
  end;
end;

// Catálogo de funciones: nombre canónico (minúsculas), mínimo y máximo de argumentos,
// y si el primer argumento debe ser una variable (funciones de grafo).
type
  TGQLFunctionInfo = record
    Name: string;
    MinArgs, MaxArgs: Integer;
    NeedsVariable: Boolean;
  end;

const
  GQL_FUNCTIONS: array [0 .. 41] of TGQLFunctionInfo = (
    // Texto
    (Name: 'lower'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'upper'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'trim'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'ltrim'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'rtrim'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'substring'; MinArgs: 2; MaxArgs: 3; NeedsVariable: False),
    (Name: 'left'; MinArgs: 2; MaxArgs: 2; NeedsVariable: False), (Name: 'right'; MinArgs: 2; MaxArgs: 2; NeedsVariable: False),
    (Name: 'replace'; MinArgs: 3; MaxArgs: 3; NeedsVariable: False), (Name: 'split'; MinArgs: 2; MaxArgs: 2; NeedsVariable: False),
    (Name: 'reverse'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'size'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'length'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'tostring'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    // Números
    (Name: 'tointeger'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'tofloat'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'abs'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'round'; MinArgs: 1; MaxArgs: 2; NeedsVariable: False),
    (Name: 'floor'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'ceil'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'sqrt'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'sign'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    // Nulos
    (Name: 'coalesce'; MinArgs: 1; MaxArgs: 32; NeedsVariable: False), (Name: 'exists'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    // Grafo (reciben una variable: id(n), type(r))
    (Name: 'id'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True), (Name: 'label'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True),
    (Name: 'labels'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True), (Name: 'type'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True),
    (Name: 'degree'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True), (Name: 'indegree'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True),
    (Name: 'outdegree'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True), (Name: 'keys'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True),
    (Name: 'similarity'; MinArgs: 2; MaxArgs: 2; NeedsVariable: True),
    (Name: 'nodes'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True), (Name: 'relationships'; MinArgs: 1; MaxArgs: 1; NeedsVariable: True),
    // Listas
    (Name: 'head'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'last'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    // Fechas
    (Name: 'date'; MinArgs: 0; MaxArgs: 1; NeedsVariable: False), (Name: 'datetime'; MinArgs: 0; MaxArgs: 1; NeedsVariable: False),
    (Name: 'year'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False), (Name: 'month'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False),
    (Name: 'day'; MinArgs: 1; MaxArgs: 1; NeedsVariable: False));

function GQLCanonicalFunction(const AName: string): string;
begin
  // Sinónimos de Cypher/SQL
  Result := LowerCase(AName);
  if Result = 'tolower' then
    Result := 'lower'
  else if Result = 'toupper' then
    Result := 'upper'
  else if Result = 'toint' then
    Result := 'tointeger'
  else if Result = 'ceiling' then
    Result := 'ceil';
end;

function TGraphParser.ParseFunctionCall(const AName: string; APosition: Integer): TGraphExpression;
var
  Func: TFunctionExpr;
  Info: TGQLFunctionInfo;
  Canon, Range: string;
  Found: Boolean;
  I: Integer;
begin
  Canon := GQLCanonicalFunction(AName);
  Found := False;
  for I := Low(GQL_FUNCTIONS) to High(GQL_FUNCTIONS) do
    if GQL_FUNCTIONS[I].Name = Canon then
    begin
      Info := GQL_FUNCTIONS[I];
      Found := True;
      Break;
    end;
  if not Found then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.unknown_function', [AName, APosition]), APosition);

  Func := TFunctionExpr.Create(Canon);
  try
    Expect(tkLParen);
    if FCurrent.Kind <> tkRParen then
    begin
      Func.Args.Add(ParseAdditive);
      while FCurrent.Kind = tkComma do
      begin
        Next;
        Func.Args.Add(ParseAdditive);
      end;
    end;
    Expect(tkRParen);

    if (Func.Args.Count < Info.MinArgs) or (Func.Args.Count > Info.MaxArgs) then
    begin
      if Info.MinArgs = Info.MaxArgs then
        Range := IntToStr(Info.MinArgs)
      else
        Range := IntToStr(Info.MinArgs) + '-' + IntToStr(Info.MaxArgs);
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.function_args', [AName, Range, Func.Args.Count]), APosition);
    end;

    if Info.NeedsVariable and not(Func.Args[0] is TVariableExpr) then
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.function_var', [AName, Canon]), APosition);
  except
    Func.Free;
    raise;
  end;
  Result := Func;
end;

function TGraphParser.ParseAggregate(const AName: string; APosition: Integer): TGraphExpression;
var
  Func: TAggregateFunc;
  Distinct: Boolean;
  Arg: TGraphExpression;
  U: string;
begin
  // Solo en RETURN, WITH y ORDER BY; y sin anidar (count(sum(x)) no tiene sentido)
  if not FAllowAggregates then
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.aggregate_context', [AName, APosition]), APosition);

  U := LowerCase(AName);
  if U = 'count' then
    Func := agCount
  else if U = 'sum' then
    Func := agSum
  else if U = 'avg' then
    Func := agAvg
  else if U = 'min' then
    Func := agMin
  else if U = 'max' then
    Func := agMax
  else
    Func := agCollect;

  Expect(tkLParen);
  Distinct := IsWord('DISTINCT');
  if Distinct then
    Next;

  Arg := nil;
  if (Func = agCount) and (FCurrent.Kind = tkAsterisk) then
    Next // count(*)
  else
  begin
    FAllowAggregates := False;
    try
      Arg := ParseExpression;
    finally
      FAllowAggregates := True;
    end;
  end;

  try
    Expect(tkRParen);
  except
    Arg.Free;
    raise;
  end;
  Result := TAggregateExpr.Create(Func, Arg, Distinct);
end;

function TGraphParser.ParseProjection(AIsReturn: Boolean): TGQLProjection;
var
  Item: TGQLReturnItem;
  Order: TGQLOrderItem;
  StartPos: Integer;
  Name: string;
  Other: TGQLReturnItem;

  function ParseWithAggregates: TGraphExpression;
  begin
    FAllowAggregates := True;
    try
      Result := ParseExpression;
    finally
      FAllowAggregates := False;
    end;
  end;

begin
  Result := TGQLProjection.Create;
  try
    Result.IsReturn := AIsReturn;

    if IsWord('DISTINCT') then
    begin
      Result.Distinct := True;
      Next;
    end;

    if FCurrent.Kind in [tkEOF, tkSemicolon] then
      raise EGQLParserError.CreateAt(AiStr('rag.gql.return_expr'), FCurrent.Position);

    if FCurrent.Kind = tkAsterisk then
    begin
      // RETURN * / WITH *: todas las variables con nombre visibles en este punto
      Next;
      for Name in FScope do
      begin
        Item := TGQLReturnItem.Create;
        Result.Items.Add(Item);
        Item.Expr := TVariableExpr.Create(Name);
        Item.Name := Name;
      end;
    end
    else
      while True do
      begin
        StartPos := FCurrent.Position;
        Item := TGQLReturnItem.Create;
        Result.Items.Add(Item);
        Item.Expr := ParseWithAggregates;

        if IsWord('AS') then
        begin
          Next;
          Item.Name := FCurrent.Text;
          Expect(tkIdentifier);
        end
        else
          Item.Name := Trim(Copy(FLexer.FText, StartPos, FPrevEnd - StartPos)); // el texto tal cual: c.name, count(*)

        for Other in Result.Items do
          if (Other <> Item) and SameText(Other.Name, Item.Name) then
            raise EGQLParserError.CreateAt(AiFmt('rag.gql.duplicate_column', [Item.Name]), StartPos);

        if FCurrent.Kind = tkComma then
          Next
        else
          Break;
      end;

    // ORDER BY expr [ASC|DESC] {, ...}
    if IsWord('ORDER') then
    begin
      Next;
      ExpectWord('BY');
      while True do
      begin
        Order := TGQLOrderItem.Create;
        Result.OrderBy.Add(Order);
        Order.Expr := ParseWithAggregates;
        if IsWord('DESC') or IsWord('DESCENDING') then
        begin
          Order.Descending := True;
          Next;
        end
        else if IsWord('ASC') or IsWord('ASCENDING') then
          Next;
        if FCurrent.Kind = tkComma then
          Next
        else
          Break;
      end;
    end;

    if IsWord('SKIP') then
    begin
      Next;
      Result.Skip := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
    end;

    if IsWord('LIMIT') then
    begin
      Next;
      Result.Limit := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
    end;

    // WITH ... WHERE: filtra sobre las columnas (n > 1 después de count(*) AS n)
    if (not AIsReturn) and (FCurrent.Kind = tkWhere) then
    begin
      Next;
      Result.Where := ParseExpression;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TGraphParser.IsWriteKeyword: Boolean;
begin
  Result := IsWord('CREATE') or IsWord('MERGE') or IsWord('SET') or IsWord('REMOVE') or IsWord('DELETE') or
    IsWord('DETACH');
end;

procedure TGraphParser.ParseSetItems(AItems: TObjectList<TGQLSetItem>; ARemove: Boolean);
var
  It: TGQLSetItem;
begin
  // SET n.prop = expr, n:Etiqueta, ...   /   REMOVE n.prop, ...
  while True do
  begin
    It := TGQLSetItem.Create;
    AItems.Add(It);
    It.Variable := FCurrent.Text;
    Expect(tkIdentifier);
    if (not ARemove) and (FCurrent.Kind = tkColon) then
    begin
      Next;
      It.NewLabel := FCurrent.Text;
      Expect(tkIdentifier);
    end
    else
    begin
      Expect(tkDot);
      It.Key := TakePropertyKey;
      if not ARemove then
      begin
        Expect(tkEqual);
        It.Expr := ParseExpression;
      end;
    end;
    if FCurrent.Kind = tkComma then
      Next
    else
      Break;
  end;
end;

function TGraphParser.ParseWriteClause: TGQLWriteClause;
var
  C: TMatchClause;
  StartPos: Integer;
begin
  Result := TGQLWriteClause.Create;
  try
    StartPos := FCurrent.Position;
    if IsWord('CREATE') or IsWord('MERGE') then
    begin
      if IsWord('CREATE') then
        Result.Kind := wkCreate
      else
        Result.Kind := wkMerge;
      Next;
      // Los patrones a crear o buscar van en su propia consulta: no participan del MATCH.
      // Las variables ya enlazadas (MATCH, CREATE anterior) se reutilizan al ejecutar.
      Result.Pattern := TGraphMatchQuery.Create;
      ParseMatchClause(Result.Pattern, Result.Pattern.MatchClauses);
      if Result.Kind = wkCreate then
        while FCurrent.Kind = tkComma do
        begin
          Next;
          ParseMatchClause(Result.Pattern, Result.Pattern.MatchClauses);
        end;
      if Result.Pattern.Paths.Count > 0 then
        raise EGQLParserError.CreateAt(AiFmt('rag.gql.path_optional', [Result.Pattern.Paths[0].Variable]), StartPos);
      // Una relación nueva necesita un solo tipo, una flecha y un salto
      for C in Result.Pattern.MatchClauses do
        if (C.EdgePattern.EdgeLabel = '') or C.EdgePattern.EdgeLabel.Contains('|') or C.EdgePattern.IsVariableLength or
          (C.EdgePattern.Direction = gdBoth) then
          raise EGQLParserError.CreateAt(AiStr('rag.gql.create_edge'), StartPos);

      // MERGE ... ON CREATE SET ... ON MATCH SET ...
      if Result.Kind = wkMerge then
        while IsWord('ON') do
        begin
          Next;
          if IsWord('CREATE') then
          begin
            Next;
            ExpectWord('SET');
            ParseSetItems(Result.OnCreate, False);
          end
          else
          begin
            Expect(tkMatch);
            ExpectWord('SET');
            ParseSetItems(Result.OnMatch, False);
          end;
        end;
    end
    else if IsWord('SET') then
    begin
      Result.Kind := wkSet;
      Next;
      ParseSetItems(Result.Items, False);
    end
    else if IsWord('REMOVE') then
    begin
      Result.Kind := wkRemove;
      Next;
      ParseSetItems(Result.Items, True);
    end
    else
    begin
      Result.Kind := wkDelete;
      if IsWord('DETACH') then
      begin
        Result.Detach := True;
        Next;
      end;
      ExpectWord('DELETE');
      while True do
      begin
        Result.DeleteVars.Add(FCurrent.Text);
        Expect(tkIdentifier);
        if FCurrent.Kind = tkComma then
          Next
        else
          Break;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TGraphParser.Parse: TGraphMatchQuery;
var
  TempQuery: TGraphMatchQuery;
  Src, Dst: TMatchNodePattern;
  Stage: TGQLProjection;
  Item: TGQLReturnItem;
  Part: TGQLOptionalMatch;
begin
  Result := nil;
  FCommandType := cmdNone;
  ClearCommandPatterns;

  // 0. EXPLAIN <consulta>: se parsea igual pero el ejecutor solo describe el plan
  FExplain := IsWord('EXPLAIN');
  if FExplain then
    Next;

  // 1. Comandos de Introspección (SHOW...)
  if FCurrent.Kind = tkShow then
  begin
    Next; // Consumir 'SHOW'
    if FCurrent.Kind = tkLabels then
    begin
      FCommandType := cmdShowLabels;
      Next;
    end
    else if FCurrent.Kind = tkEdges then
    begin
      FCommandType := cmdShowEdges;
      Next;
    end
    else if IsWord('SCHEMA') then
    begin
      // Etiquetas con su cantidad y las combinaciones (:A)-[:TIPO]->(:B) que existen
      FCommandType := cmdShowSchema;
      Next;
    end
    else if IsWord('PROPERTIES') then
    begin
      // SHOW PROPERTIES [OF [:]Etiqueta]: claves de metadatos por etiqueta y por tipo de relación
      FCommandType := cmdShowProperties;
      Next;
      if IsWord('OF') then
      begin
        Next;
        if FCurrent.Kind = tkColon then
          Next;
        FCommandLabel := FCurrent.Text;
        Expect(tkIdentifier);
      end;
    end
    else
      raise EGQLParserError.CreateAt(AiStr('rag.gql.show'), FCurrent.Position);
    ExpectEnd;
    Exit;
  end;

  // 2. Comandos de Algoritmos de Grafo
  // ALL SHORTEST PATHS (a) TO (b): todos los caminos de longitud mínima
  if IsWord('ALL') then
  begin
    Next;
    Expect(tkShortest);
    if IsWord('PATHS') then
      Next
    else
      Expect(tkPath);
    FCommandType := cmdAllShortestPaths;
    TempQuery := TGraphMatchQuery.Create;
    try
      Src := ParseNode(TempQuery);
      TempQuery.NodePatterns.Extract(Src);
      FCommandSourcePattern := Src;
      Expect(tkTo);
      Dst := ParseNode(TempQuery);
      TempQuery.NodePatterns.Extract(Dst);
      FCommandTargetPattern := Dst;
    finally
      TempQuery.Free;
    end;
    ExpectEnd;
    Exit;
  end;

  if FCurrent.Kind = tkShortest then
  begin
    Next; // Consumir 'SHORTEST'
    Expect(tkPath);
    FCommandType := cmdShortestPath;
    TempQuery := TGraphMatchQuery.Create;
    try
      Src := ParseNode(TempQuery);
      TempQuery.NodePatterns.Extract(Src);
      FCommandSourcePattern := Src;
      Expect(tkTo);
      Dst := ParseNode(TempQuery);
      TempQuery.NodePatterns.Extract(Dst);
      FCommandTargetPattern := Dst;
    finally
      TempQuery.Free;
    end;
    ExpectEnd;
    Exit;
  end;

  if FCurrent.Kind = tkGet then
  begin
    Next;
    if FCurrent.Kind = tkCentrality then
    begin
      Next;
      if FCurrent.Kind = tkTop then
      begin
        // GET CENTRALITY TOP n: los n nodos más centrales
        Next;
        FCommandType := cmdCentralityTop;
        FCommandLimit := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
      end
      else
      begin
        FCommandType := cmdCentrality;
        TempQuery := TGraphMatchQuery.Create;
        try
          Src := ParseNode(TempQuery);
          TempQuery.NodePatterns.Extract(Src);
          FCommandSourcePattern := Src;
        finally
          TempQuery.Free;
        end;
      end;
    end
    else if FCurrent.Kind = tkDegrees then
    begin
      Next;
      FCommandType := cmdDegrees;
      Expect(tkTop);
      FCommandLimit := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
    end
    else if IsWord('COMMUNITIES') then
    begin
      // GET COMMUNITIES [TOP n]: grupos de nodos muy conectados entre sí
      Next;
      FCommandType := cmdCommunities;
      FCommandLimit := 0;
      if FCurrent.Kind = tkTop then
      begin
        Next;
        FCommandLimit := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
      end;
    end
    else if IsWord('NEIGHBORS') then
    begin
      // GET NEIGHBORS (nodo) [DEPTH n]: el vecindario como texto de contexto
      Next;
      FCommandType := cmdNeighbors;
      TempQuery := TGraphMatchQuery.Create;
      try
        Src := ParseNode(TempQuery);
        TempQuery.NodePatterns.Extract(Src);
        FCommandSourcePattern := Src;
      finally
        TempQuery.Free;
      end;
      FCommandLimit := 1;
      if FCurrent.Kind = tkDepth then
      begin
        Next;
        FCommandLimit := ParseIntegerValue(AiStr('rag.gql.depth'));
      end;
    end
    else
      raise EGQLParserError.CreateAt(AiStr('rag.gql.get'), FCurrent.Position);
    ExpectEnd;
    Exit;
  end;

  // 3. Consultas MATCH Estándar
  Result := TGraphMatchQuery.Create;
  try
    // Una consulta puede empezar con CREATE o MERGE, sin MATCH
    if not(IsWord('CREATE') or IsWord('MERGE')) then
    begin
      if FCurrent.Kind = tkMatch then
        Next; // Consumimos MATCH

      // --- PARSEO DE CLÁUSULAS MATCH (a)-[]->(b), (c)-[]->(d) ---
      ParseMatchClause(Result, Result.MatchClauses);
      while FCurrent.Kind = tkComma do // Soporte para múltiples patrones separados por coma
      begin
        Next; // Consumir la coma
        ParseMatchClause(Result, Result.MatchClauses);
      end;
    end;

    // 4. DEPTH (Opcional)
    if FCurrent.Kind = tkDepth then
    begin
      Next;
      Result.Depth := ParseIntegerValue(AiStr('rag.gql.depth'));
    end;

    // 5. WHERE (Opcional)
    if FCurrent.Kind = tkWhere then
      ParseWhereClause(Result);

    // 5b. OPTIONAL MATCH patrón {, patrón} [WHERE expr]
    while IsWord('OPTIONAL') do
    begin
      Next;
      Expect(tkMatch);
      Part := TGQLOptionalMatch.Create;
      Result.OptionalParts.Add(Part);
      FCurOptional := True;
      FCurNewPatterns := Part.Patterns;
      try
        ParseMatchClause(Result, Part.Clauses);
        while FCurrent.Kind = tkComma do
        begin
          Next;
          ParseMatchClause(Result, Part.Clauses);
        end;
      finally
        FCurOptional := False;
        FCurNewPatterns := nil;
      end;
      if FCurrent.Kind = tkWhere then
      begin
        Next;
        Part.Where := ParseExpression;
      end;
    end;

    // 5c. Escritura: CREATE, MERGE, SET, REMOVE, [DETACH] DELETE (en el orden escrito)
    while IsWriteKeyword do
      Result.Writes.Add(ParseWriteClause);

    // 6. WITH ... (etapas) y RETURN
    while IsWord('WITH') do
    begin
      Next;
      Stage := ParseProjection(False);
      Result.Projections.Add(Stage);
      // Después de un WITH solo se ven sus columnas
      FScope.Clear;
      for Item in Stage.Items do
        FScope.Add(Item.Name);
    end;
    if FCurrent.Kind = tkReturn then
    begin
      Next;
      Result.Projections.Add(ParseProjection(True));
    end
    else if Result.Projections.Count > 0 then
      raise EGQLParserError.CreateAt(AiStr('rag.gql.with_return'), FCurrent.Position);

    for var W in Result.Writes do
      if (W.Kind = wkDelete) and (Result.Projections.Count > 0) then
        raise EGQLParserError.CreateAt(AiStr('rag.gql.delete_return'), FCurrent.Position);

    // Tabla si hay WITH, o un RETURN que no sea solo variables (RETURN c, p sigue dando el
    // texto de contexto del subgrafo, como antes)
    Result.Tabular := Result.Projections.Count > 1;
    if Result.Projections.Count = 1 then
    begin
      Stage := Result.Projections[0];
      Result.Tabular := Stage.Distinct or (Stage.OrderBy.Count > 0) or (Stage.Skip >= 0) or (Stage.Limit >= 0);
      for Item in Stage.Items do
        if not((Item.Expr is TVariableExpr) and (TVariableExpr(Item.Expr).Variable = Item.Name)) then
          Result.Tabular := True;
    end;

    // 7. Fin de la consulta (admite ; final)
    ExpectEnd;
  except
    Result.Free;
    raise;
  end;
end;

end.
