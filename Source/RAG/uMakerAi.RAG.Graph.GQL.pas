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
    tkSemicolon); // ;

  TGraphCommandType = (cmdNone, cmdShowLabels, cmdShowEdges, cmdShortestPath, cmdCentrality, cmdDegrees);

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
    FNodeVariables: TDictionary<string, TMatchNodePattern>;
    FCommandType: TGraphCommandType;
    FCommandSourcePattern: TMatchNodePattern;
    FCommandTargetPattern: TMatchNodePattern;
    FCommandLimit: Integer;

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
    function ParsePrimary: TGraphExpression;
    function ParseValueList: Variant;
    function ParseInList: TGraphExpression;

    procedure ParseMatchClause(AQuery: TGraphMatchQuery);
    procedure ParseWhereClause(AQuery: TGraphMatchQuery);
    procedure ParseReturnClause(AQuery: TGraphMatchQuery);
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
  FNodeVariables := TDictionary<string, TMatchNodePattern>.Create;
  FCommandType := cmdNone;
  FCommandSourcePattern := nil;
  FCommandTargetPattern := nil;
  FCommandLimit := 0;
  FAnonCount := 0;
  Next;
end;

destructor TGraphParser.Destroy;
begin
  ClearCommandPatterns;
  FNodeVariables.Free;
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
  // IN [a, b, c]  o  IN $lista (un VarArray, o un valor suelto)
  if FCurrent.Kind = tkParameter then
  begin
    V := ParamValue(FCurrent.Text, FCurrent.Position);
    Next;
  end
  else
    V := ParseValueList;
  Result := TLiteralExpr.Create(V);
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

  // Etiqueta opcional: (:Persona)
  if FCurrent.Kind = tkColon then
  begin
    Next;
    LblName := FCurrent.Text;
    Expect(tkIdentifier);
  end;

  // Nodo anónimo: nombre interno único ('#' no lo produce el lexer, así que no choca
  // con variables del usuario). Antes todos compartían la clave '' y se pisaban.
  if VarName = '' then
  begin
    Inc(FAnonCount);
    VarName := '#' + IntToStr(FAnonCount);
  end;

  // Binding o Creación de Variable
  if FNodeVariables.TryGetValue(VarName, Result) then
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
    FNodeVariables.Add(VarName, Result);
    AQuery.AddNodePattern(Result);
  end;

  // Propiedades opcionales: {name: 'Juan'}
  if FCurrent.Kind = tkLBrace then
    ParseProperties(Result.Properties);

  Expect(tkRParen);
end;

function TGraphParser.ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
var
  Left, Right: Boolean;
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
      Result.Variable := GetTokenTextAndNext;

    // Tipo de relación opcional [:KNOWS]
    if FCurrent.Kind = tkColon then
    begin
      Next;
      Result.EdgeLabel := FCurrent.Text;
      Expect(tkIdentifier);
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

procedure TGraphParser.ParseMatchClause(AQuery: TGraphMatchQuery);
var
  SrcNode, DstNode: TMatchNodePattern;
  Edge: TMatchEdgePattern;
  Dir: TGraphDirection;
begin
  // 1. Parsear el nodo inicial del patrón: (a)
  SrcNode := ParseNode(AQuery);

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
    AQuery.AddMatchClause(TMatchClause.Create(SrcNode.Variable, Edge, DstNode.Variable));

    // D. El destino (b) se convierte en el origen de la siguiente conexión
    SrcNode := DstNode;
  end;
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
  Negated: Boolean;
begin
  L := ParsePrimary;
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
      Lo := ParsePrimary;
      try
        Expect(tkAnd);
        Hi := ParsePrimary;
      except
        Lo.Free;
        raise;
      end;
      Result := TBetweenExpr.Create(L, Lo, Hi);
    end
    else if IsWord('STARTS') or IsWord('ENDS') then
    begin
      if IsWord('STARTS') then
        Op := opStartsWith
      else
        Op := opEndsWith;
      Next;
      ExpectWord('WITH');
      Result := TBinaryExpr.Create(L, Op, ParsePrimary);
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
      Result := TBinaryExpr.Create(L, Op, ParsePrimary);
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

function TGraphParser.ParsePrimary: TGraphExpression;
var
  V, P: string;
  Value: Variant;
  Operand: TGraphExpression;
begin
  case FCurrent.Kind of
    tkLParen: // Agrupación: (expresión)
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

    // Acceso a propiedad (ej: n.nombre)
    tkIdentifier:
      begin
        V := GetTokenTextAndNext; // Consume variable
        Expect(tkDot); // Consume punto
        P := TakePropertyKey; // Nombre de la propiedad
        Result := TPropertyExpr.Create(V, P);
      end;

    // Literales y parámetros
    tkString, tkNumber, tkBoolean, tkNull, tkParameter:
      begin
        ParseLiteralValue(Value);
        Result := TLiteralExpr.Create(Value);
      end;

    // Signo negativo: -5, -$limite, -(a.x)
    tkDash:
      begin
        Next;
        Operand := ParsePrimary;
        if (Operand is TLiteralExpr) and VarIsNumeric(TLiteralExpr(Operand).Value) and
          not VarIsType(TLiteralExpr(Operand).Value, varBoolean) then
        begin
          TLiteralExpr(Operand).Value := -TLiteralExpr(Operand).Value;
          Result := Operand;
        end
        else
          Result := TUnaryExpr.Create(uoNegate, Operand);
      end;
  else
    raise EGQLParserError.CreateAt(AiFmt('rag.gql.where_expr', [FCurrent.Text]), FCurrent.Position);
  end;
end;

Procedure TGraphParser.ParseReturnClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkReturn);

  // Verificar que haya al menos un elemento
  if FCurrent.Kind in [tkEOF, tkSemicolon] then
    raise EGQLParserError.CreateAt(AiStr('rag.gql.return_expr'), FCurrent.Position);

  while True do
  begin
    // --- CASO A: Funciones agregadas (COUNT, SUM, AVG) ---
    if FCurrent.Kind in [tkCount, tkSum, tkAvg] then
    begin
      Next; // Consumir función
      Expect(tkLParen);

      // argumento puede ser variable (p), propiedad (p.valor) o * (todo)
      if FCurrent.Kind = tkIdentifier then
      begin
        Next; // Consumir identificador (p)
        if FCurrent.Kind = tkDot then
        begin
          Next; // Consumir el punto
          TakePropertyKey;
        end;
      end
      else if FCurrent.Kind = tkAsterisk then
        Next // Consumir *
      else
        raise EGQLParserError.CreateAt(AiStr('rag.gql.function_arg'), FCurrent.Position);

      Expect(tkRParen);
    end
    // --- CASO B: Identificador Simple o Acceso a Propiedad ---
    else if FCurrent.Kind = tkIdentifier then
    begin
      Next; // Consumir el identificador

      if FCurrent.Kind = tkDot then
      begin
        Next; // Consumir el punto
        TakePropertyKey;
      end;
    end
    // --- CASO C: Literales, parámetros y RETURN * ---
    else if FCurrent.Kind in [tkString, tkNumber, tkBoolean, tkNull, tkParameter, tkAsterisk] then
    begin
      Next;
    end
    else
      raise EGQLParserError.CreateAt(AiFmt('rag.gql.invalid_return', [FCurrent.Text]), FCurrent.Position);

    // --- SOPORTE PARA ALIAS "AS" ---
    if IsWord('AS') then
    begin
      Next; // Consumir 'AS'
      Expect(tkIdentifier); // Consumir alias
    end;

    // --- SEPARADOR: Coma ',' ---
    if FCurrent.Kind = tkComma then
      Next // Consumir coma
    else
      Break;
  end; // Fin del WHILE
end;

function TGraphParser.Parse: TGraphMatchQuery;
var
  TempQuery: TGraphMatchQuery;
  Src, Dst: TMatchNodePattern;
begin
  Result := nil;
  FCommandType := cmdNone;
  ClearCommandPatterns;

  // 1. Comandos de Introspección (SHOW...)
  if FCurrent.Kind = tkShow then
  begin
    Next; // Consumir 'SHOW'
    if FCurrent.Kind = tkLabels then
      FCommandType := cmdShowLabels
    else if FCurrent.Kind = tkEdges then
      FCommandType := cmdShowEdges
    else
      raise EGQLParserError.CreateAt(AiStr('rag.gql.show'), FCurrent.Position);
    Next; // Avanzamos después del comando show
    ExpectEnd;
    Exit;
  end;

  // 2. Comandos de Algoritmos de Grafo
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
      FCommandType := cmdCentrality;
      TempQuery := TGraphMatchQuery.Create;
      try
        Src := ParseNode(TempQuery);
        TempQuery.NodePatterns.Extract(Src);
        FCommandSourcePattern := Src;
      finally
        TempQuery.Free;
      end;
    end
    else if FCurrent.Kind = tkDegrees then
    begin
      Next;
      FCommandType := cmdDegrees;
      Expect(tkTop);
      FCommandLimit := ParseIntegerValue(AiFmt('rag.gql.expected', ['tkNumber', FCurrent.Text, FCurrent.Position]));
    end
    else
      raise EGQLParserError.CreateAt(AiStr('rag.gql.get'), FCurrent.Position);
    ExpectEnd;
    Exit;
  end;

  // 3. Consultas MATCH Estándar
  Result := TGraphMatchQuery.Create;
  try
    if FCurrent.Kind = tkMatch then
      Next; // Consumimos MATCH

    // --- PARSEO DE CLÁUSULAS MATCH (a)-[]->(b), (c)-[]->(d) ---
    ParseMatchClause(Result);
    while FCurrent.Kind = tkComma do // Soporte para múltiples patrones separados por coma
    begin
      Next; // Consumir la coma
      ParseMatchClause(Result);
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

    // 6. RETURN (Opcional)
    if FCurrent.Kind = tkReturn then
      ParseReturnClause(Result);

    // 7. Fin de la consulta (admite ; final)
    ExpectEnd;
  except
    Result.Free;
    raise;
  end;
end;

end.
