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
    tkIdentifier, // Variables, etiquetas, claves
    tkString, // 'Texto' o "Texto"
    tkNumber, // 123, 123.45
    tkBoolean, // TRUE, FALSE
    tkNull, // NULL

    // Palabras Reservadas: Estructura de Consulta
    tkMatch, tkWhere, tkReturn, tkShow, tkLabels, tkEdges,

    // Operadores L�gicos
    tkAnd, tkOr, tkNot, // Nuevo: Para NOT IN, IS NOT NULL

    // Operadores de Comparaci�n y Texto (Alineados con TAiFilterCriteria)
    tkEqual, // =
    tkNotEqual, // <>
    tkGreater, // >
    tkGreaterEqual, // >=
    tkLess, // <
    tkLessEqual, // <=
    tkLike, // LIKE (Nuevo)
    tkILike, // ILIKE (Nuevo)
    tkContains, // CONTAINS
    tkIn, // IN
    tkIs, // IS
    tkAsterisk,

    // Funciones de Agregaci�n y Modificadores
    tkCount, tkSum, tkAvg, tkDepth,

    // Palabras Reservadas: Algoritmos de Grafo
    tkShortest, tkPath, tkTo, tkGet, tkCentrality, tkDegrees, tkTop);

  TGraphCommandType = (cmdNone, cmdShowLabels, cmdShowEdges, cmdShortestPath, cmdCentrality, cmdDegrees);

  TToken = record
    Kind: TTokenKind;
    Text: string;
    Position: Integer;
  end;

  { Analizador L�xico (Lexer) }
  TGraphLexer = class
  private
    FText: string;
    FPos: Integer;

    // M�todos internos de navegaci�n
    function Peek: Char;
    function Next: Char;
    procedure SkipWhitespace;
    function IsEOF: Boolean;

    // M�todos de lectura de tipos espec�ficos
    function ReadIdentifier: TToken;
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
    FNodeVariables: TDictionary<string, TMatchNodePattern>;
    FCommandType: TGraphCommandType;
    FCommandSourcePattern: TMatchNodePattern;
    FCommandTargetPattern: TMatchNodePattern;
    FCommandLimit: Integer;

    procedure Next;
    function GetTokenTextAndNext: string;
    procedure Expect(AKind: TTokenKind);

    // M�todos internos de parsing
    function ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
    function ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
    procedure ParseProperties(AProps: TDictionary<string, Variant>);

    // M�todos de parsing de expresiones (nuevos)
    function ParseExpression: TGraphExpression;
    function ParseAndExpression: TGraphExpression;
    function ParseComparison: TGraphExpression;
    function ParsePrimary: TGraphExpression;
    function ParseValueList: Variant;

    procedure ParseMatchClause(AQuery: TGraphMatchQuery);
    procedure ParseWhereClause(AQuery: TGraphMatchQuery);
    procedure ParseReturnClause(AQuery: TGraphMatchQuery);
    procedure ClearCommandPatterns;

  public
    constructor Create(const AText: string);
    destructor Destroy; override;
    function Parse: TGraphMatchQuery;

    property CommandType: TGraphCommandType read FCommandType;
    property CommandSourcePattern: TMatchNodePattern read FCommandSourcePattern;
    property CommandTargetPattern: TMatchNodePattern read FCommandTargetPattern;
    property CommandLimit: Integer read FCommandLimit;
  end;

implementation

{ TGraphLexer }

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

function TGraphLexer.IsEOF: Boolean;
begin
  Result := FPos > Length(FText);
end;

procedure TGraphLexer.SkipWhitespace;
begin
  while (not IsEOF) and Peek.IsWhiteSpace do
    Next;
end;

function TGraphLexer.ReadString: TToken;
var
  Quote: Char;
  Start: Integer;
begin
  Quote := Next; // Consumir comilla de apertura (' o ")
  Start := FPos;

  // Leer hasta encontrar la comilla de cierre o EOF
  while (not IsEOF) and (Peek <> Quote) do
    Next;

  Result.Kind := tkString;
  // Extraer el texto SIN las comillas
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start - 1; // Posici�n del inicio de la cadena

  if not IsEOF then
    Next; // Consumir comilla de cierre
end;

function TGraphLexer.ReadNumber: TToken;
var
  Start: Integer;
  HasDot: Boolean;
begin
  Start := FPos;
  HasDot := False;

  while (not IsEOF) and (Peek.IsDigit or (Peek = '.')) do
  begin
    if Peek = '.' then
    begin
      // Solo permitimos un punto decimal
      if HasDot then
        Break;
      HasDot := True;
    end;
    Next;
  end;

  Result.Kind := tkNumber;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TGraphLexer.ReadIdentifier: TToken;
var
  Start: Integer;
  UpperText: string;
begin
  Start := FPos;

  // Leemos letras, d�gitos o guiones bajos
  while (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_')) do
    Next;

  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;

  // Normalizamos a may�sculas para chequear palabras reservadas
  UpperText := Result.Text.ToUpper;

  // --- MAPEO DE PALABRAS RESERVADAS ---

  // Estructura B�sica
  if UpperText = 'MATCH' then
    Result.Kind := tkMatch
  else if UpperText = 'WHERE' then
    Result.Kind := tkWhere
  else if UpperText = 'RETURN' then
    Result.Kind := tkReturn

    // Operadores L�gicos
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

    // Introspecci�n
  else if UpperText = 'SHOW' then
    Result.Kind := tkShow
  else if UpperText = 'LABELS' then
    Result.Kind := tkLabels
  else if UpperText = 'EDGES' then
    Result.Kind := tkEdges

    // Operadores de Comparaci�n Textual y Listas
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

    // Agregaci�n y Modificadores
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
    '*':
      begin
        Result.Text := '*';
        Next;
        Result.Kind := tkAsterisk; // O usa un token gen�rico si prefieres
      end;

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
          Result.Kind := tkDash; // -
          Result.Text := '-';
        end;
      end;

    // Cadenas de texto
    '''', '"':
      begin
        Result := ReadString;
      end;

  else
    // Identificadores y N�meros
    if Peek.IsLetter or (Peek = '_') then
      Result := ReadIdentifier
    else if Peek.IsDigit then
      Result := ReadNumber
    else
      // Error: Car�cter desconocido
      raise Exception.CreateFmt('Error L�xico: Car�cter inesperado "%s" en posici�n %d', [Peek, FPos]);
  end;
end;


// ================================================
// ============ GRAPH PARSER ======================
// ================================================

{ TGraphParser }

constructor TGraphParser.Create(const AText: string);
begin
  inherited Create;
  FLexer := TGraphLexer.Create(AText);
  FNodeVariables := TDictionary<string, TMatchNodePattern>.Create;
  FCommandType := cmdNone;
  FCommandSourcePattern := nil;
  FCommandTargetPattern := nil;
  FCommandLimit := 0;
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
    raise Exception.CreateFmt('Error Sint�ctico: Se esperaba %s y se encontr� "%s" en posici�n %d', [GetEnumName(TypeInfo(TTokenKind), Ord(AKind)), FCurrent.Text, FCurrent.Position]);
  Next;
end;

function TGraphParser.ParseValueList: Variant;
var
  Values: TArray<Variant>;
  I: Integer;
begin
  // Formato:  [valor1, valor2, valor3]
  Expect(tkLBracket);
  var
  ValueList := TList<Variant>.Create;
  try
    if FCurrent.Kind <> tkRBracket then
    begin
      while True do
      begin
        // --- PARSEO DE UN VALOR ---
        case FCurrent.Kind of
          tkString:
            ValueList.Add(FCurrent.Text);
          tkNumber:
            ValueList.Add(StrToFloat(FCurrent.Text, TFormatSettings.Invariant));
          tkBoolean:
            ValueList.Add(SameText(FCurrent.Text, 'TRUE')); // Case insensitive
        else
          raise Exception.CreateFmt('Error: Valor de lista inv�lido. Se esperaba String, Number o Boolean, se encontr� "%s".', [FCurrent.Text]);
        end;
        Next;

        // --- SEPARADOR: COMA ---
        if FCurrent.Kind = tkComma then
          Next
        else
          Break; // Fin de la lista
      end;
    end;
    Expect(tkRBracket);

    // --- CONVERSI�N A VARIANT ARRAY (para compatibilidad) ---
    SetLength(Values, ValueList.Count);
    for I := 0 to ValueList.Count - 1 do
      Values[I] := ValueList[I];

    Result := Values;
  finally
    ValueList.Free;
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

  // Etiqueta opcional: (:Persona)
  if FCurrent.Kind = tkColon then
  begin
    Next;
    LblName := FCurrent.Text;
    Expect(tkIdentifier);
  end;

  // Binding o Creaci�n de Variable
  if (VarName <> '') and FNodeVariables.TryGetValue(VarName, Result) then
  begin
    // Reutilizar nodo si existe. Actualizar etiqueta si fue provista.
    if (Result.NodeLabel = '') and (LblName <> '') then
      Result.NodeLabel := LblName;
  end
  else
  begin
    // Nueva variable (o nodo an�nimo)
    Result := TMatchNodePattern.Create;
    Result.Variable := VarName;
    Result.NodeLabel := LblName;

    if VarName <> '' then
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
  // Detecci�n de flecha izquierda <-
  Left := (FCurrent.Kind = tkArrowLeft);
  if Left then
    Next
  else
    Expect(tkDash);

  Expect(tkLBracket);
  Result := TMatchEdgePattern.Create;

  // Variable de arista opcional [r]
  if FCurrent.Kind = tkIdentifier then
    Result.Variable := GetTokenTextAndNext;

  // Tipo de relaci�n opcional [:KNOWS]
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

  // Detecci�n de flecha derecha ->
  Right := (FCurrent.Kind = tkArrowRight);
  if Right then
    Next
  else
    Expect(tkDash);

  // Determinar la direcci�n
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
begin
  Expect(tkLBrace);
  while FCurrent.Kind <> tkRBrace do
  begin
    Key := FCurrent.Text;
    Expect(tkIdentifier);
    Expect(tkColon);

    // --- Leer el Valor de la Propiedad ---
    case FCurrent.Kind of
      tkString:
        AProps.Add(Key, GetTokenTextAndNext);
      tkNumber:
        AProps.Add(Key, StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant));
      tkBoolean:
        AProps.Add(Key, SameText(GetTokenTextAndNext, 'true')); // case insensitive
      tkNull:
        begin
          AProps.Add(Key, Null);
          Next;
        end;
    else
      raise Exception.CreateFmt('Error: Valor de propiedad no soportado. Se esperaba String, Number, Boolean o Null, se encontr� "%s".', [FCurrent.Text]);
    end;

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

// --- PARSER DE EXPRESIONES (WHERE) ---

function TGraphParser.ParseExpression: TGraphExpression;
var
  L: TGraphExpression;
begin
  L := ParseAndExpression;
  while FCurrent.Kind = tkOr do
  begin
    Next;
    L := TBinaryExpr.Create(L, opOr, ParseAndExpression);
  end;
  Result := L;
end;

procedure TGraphParser.ParseMatchClause(AQuery: TGraphMatchQuery);
var
  SrcNode, DstNode: TMatchNodePattern;
  Edge: TMatchEdgePattern;
  Dir: TGraphDirection;
begin
  // 1. Parsear el nodo inicial del patr�n: (a)
  SrcNode := ParseNode(AQuery);

  // 2. Bucle para relaciones encadenadas.
  // Ejemplo: (a)-[r1]->(b)-[r2]->(c)
  // El bucle contin�a mientras el token actual indique el inicio de una arista
  // (un gui�n '-' o una flecha izquierda '<-').
  while FCurrent.Kind in [tkDash, tkArrowLeft] do
  begin
    // A. Parsear la estructura de la arista -[...]-, ->, <-
    // Esto nos devuelve el objeto del patr�n de arista y la direcci�n detectada.
    Edge := ParseEdge(Dir);
    Edge.Direction := Dir; // Asignamos la direcci�n al patr�n

    // B. Parsear el nodo siguiente en la cadena
    DstNode := ParseNode(AQuery);

    // C. Crear la cl�usula de emparejamiento (Match Clause)
    // Una cl�usula conecta "VariableOrigen + Patr�nArista + VariableDestino"
    // Nota: TMatchClause toma posesi�n de la memoria del objeto 'Edge'.
    AQuery.AddMatchClause(TMatchClause.Create(SrcNode.Variable, Edge, DstNode.Variable));

    // D. Avanzar el pivote:
    // El nodo que acabamos de encontrar como destino (b) se convierte
    // en el origen para la siguiente posible conexi�n en la cadena.
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
    Next;
    L := TBinaryExpr.Create(L, opAnd, ParseComparison);
  end;
  Result := L;
end;

function TGraphParser.ParseComparison: TGraphExpression;
var
  L: TGraphExpression;
  Op: TBinaryOp;
  InList: Variant;
begin
  L := ParsePrimary;

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
      Result := TBinaryExpr.Create(L, opIsNull, nil); // Unario: Left es la propiedad
    end
    else
      raise Exception.Create('Error: Se esperaba NULL o NOT NULL despu�s de IS.');
    Exit;
  end;

  // --- Operadores NOT (NOT IN) ---
  if FCurrent.Kind = tkNot then
  begin
    Next; // Consume NOT
    if FCurrent.Kind = tkIn then
    begin
      Next; // Consume IN
      InList := ParseValueList;
      Result := TBinaryExpr.Create(L, opNotIn, TLiteralExpr.Create(InList));
      Exit;
    end
    else
      raise Exception.Create('Error Sint�ctico: Uso inv�lido de NOT. Se esperaba NOT IN.');
  end;

  // --- Operadores Binarios (IN, LIKE, =, etc.) ---
  if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains, tkLike, tkILike, tkIn] then
  begin
    // Determinar el operador
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
        Op := opContains; // ya est� en el Lexer
      tkLike:
        Op := opLike;
      tkILike:
        Op := opILike;
      tkIn:
        Op := opIn;
    else
      Op := opEqual; // Default (nunca deber�a llegar aqu�)
    end;
    Next; // Consumir el operador

    // Para IN, el lado derecho es una lista
    if Op = opIn then
    begin
      InList := ParseValueList;
      Result := TBinaryExpr.Create(L, Op, TLiteralExpr.Create(InList));
    end
    else
      {
        Result := TBinaryExpr.Create(L, Op, ParsePrimary); // <-- Original
        Result := TBinaryExpr.Create(L, Op, ParseExpression); // <-- Nueva versi�n
      }
      Result := TBinaryExpr.Create(L, Op, ParsePrimary);
  end
  else
    Result := L;
end;

function TGraphParser.ParsePrimary: TGraphExpression;
var
  V, P: string;
  Negate: Boolean;
begin
  Negate := False;
  case FCurrent.Kind of
    tkLParen: // Agrupaci�n: (expresi�n)
      begin
        Next;
        Result := ParseExpression;
        Expect(tkRParen);
      end;
    // Acceso a propiedad (ej: n.nombre)
    tkIdentifier:
      begin
        // Variable (n)
        V := GetTokenTextAndNext; // Consume variable
        Expect(tkDot); // Consume punto
        P := FCurrent.Text; // Nombre de la propiedad (nombre)
        Expect(tkIdentifier); // Consume nombre de la propiedad
        Result := TPropertyExpr.Create(V, P);
      end;
    // Literales (cadenas, n�meros, booleanos, null)
    tkString:
      Result := TLiteralExpr.Create(GetTokenTextAndNext);
    tkBoolean:
      Result := TLiteralExpr.Create(SameText(GetTokenTextAndNext, 'true'));

    tkNumber:
      begin
        // Aplicar el negativo si exist�a
        if Negate then
          Result := TLiteralExpr.Create(-StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant))
        else
          Result := TLiteralExpr.Create(StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant));
      end;

    tkNull:
      begin
        Next;
        Result := TLiteralExpr.Create(Null);
      end;
  else
    raise Exception.CreateFmt('Error: Se esperaba una expresi�n v�lida en el WHERE. Se encontr� "%s".', [FCurrent.Text]);
  end;
end;

Procedure TGraphParser.ParseReturnClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkReturn);

  // Verificar que haya al menos un elemento
  if FCurrent.Kind = tkEOF then
    raise Exception.Create('Error: Se esperaba una expresi�n despu�s de RETURN.');

  while True do
  begin
    // --- CASO A: Funciones agregadas (COUNT, SUM, AVG) ---
    if FCurrent.Kind in [tkCount, tkSum, tkAvg] then
    begin
      Next; // Consumir funci�n
      Expect(tkLParen);

      // argumento puede ser variable (p), propiedad (p.valor) o * (todo)
      if FCurrent.Kind = tkIdentifier then
      begin
        Next; // Consumir identificador (p)
        if FCurrent.Kind = tkDot then
        begin
          Next; // Consumir el punto
          Expect(tkIdentifier); // Consumir propiedad
        end;
      end
      else if FCurrent.Text = '*' then
        Next // Consumir *
      else
        raise Exception.Create('Error: Se esperaba una variable o propiedad dentro de la funci�n.');

      Expect(tkRParen);
    end
    // --- CASO B: Identificador Simple o Acceso a Propiedad ---
    else if FCurrent.Kind = tkIdentifier then
    begin
      Next; // Consumir el identificador

      if FCurrent.Kind = tkDot then
      begin
        Next; // Consumir el punto
        Expect(tkIdentifier); // Consumir la propiedad
      end;
    end
    // --- CASO C: Literales (String, Number, Boolean, Null) ---
    else if FCurrent.Kind in [tkString, tkNumber, tkBoolean, tkNull] then
    begin
      Next;
    end
    else
      raise Exception.CreateFmt('Error Sint�ctico: Expresi�n de retorno no v�lida "%s".', [FCurrent.Text]);

    // --- SOPORTE PARA ALIAS "AS" ---
    if (FCurrent.Kind = tkIdentifier) and SameText(FCurrent.Text, 'AS') then
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

  // 1. Comandos de Introspecci�n (SHOW...)
  if FCurrent.Kind = tkShow then
  begin
    Next; // Consumir 'SHOW'
    if FCurrent.Kind = tkLabels then
      FCommandType := cmdShowLabels
      // etc.
    else if FCurrent.Kind = tkEdges then
      FCommandType := cmdShowEdges
    else
      raise Exception.Create('Error Sint�ctico: Se esperaba LABELS o EDGES despu�s de SHOW.');
    Next; // Avanzamos despu�s del comando show
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
    Exit;
  end;
  // ... (completar con el resto de comandos como get centrality...)

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
      FCommandLimit := StrToIntDef(GetTokenTextAndNext, 10);
    end
    else
      raise Exception.Create('Error Sint�ctico: Se esperaba CENTRALITY o DEGREES despu�s de GET.');
    Exit;
  end;

  // 3. Consultas MATCH Est�ndar (La m�s compleja)
  Result := TGraphMatchQuery.Create;
  try
    if FCurrent.Kind = tkMatch then
      Next; // Consumimos MATCH

    // --- PARSEO DE CL�USULAS MATCH (a)-[]->(b), (c)-[]->(d) ---
    ParseMatchClause(Result);
    while FCurrent.Kind = tkComma do // Soporte para m�ltiples patrones separados por coma
    begin
      Next; // Consumir la coma
      ParseMatchClause(Result);
    end;

    // 4. DEPTH (Opcional)
    if FCurrent.Kind = tkDepth then
    begin
      Next;
      if FCurrent.Kind = tkNumber then
        Result.Depth := Trunc(StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant))
      else
        raise Exception.Create('Error: Se esperaba un n�mero despu�s de DEPTH.');
    end;

    // 5. WHERE (Opcional)
    if FCurrent.Kind = tkWhere then
      ParseWhereClause(Result);

    // 6. RETURN (Opcional)
    if FCurrent.Kind = tkReturn then
      ParseReturnClause(Result);

    // 7. Verificaci�n del EOF
    if FCurrent.Kind <> tkEOF then
      raise Exception.CreateFmt('Error: Sintaxis incorrecta. Se esperaba EOF, se encontr� "%s" en posici�n %d', [FCurrent.Text, FCurrent.Position]);
  except
    Result.Free;
    raise;
  end;
end;

end.
