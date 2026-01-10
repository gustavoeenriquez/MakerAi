unit uMakerAi.RAG.Graph.Parser;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Variants, System.TypInfo,
  uMakerAi.RAG.Graph.Lexer,
  uMakerAi.RAG.Graph.Core;

type
  { Tipos de comandos soportados por el Parser }
  TGraphCommandType = (cmdNone, cmdShowLabels, cmdShowEdges, cmdShortestPath, cmdCentrality, cmdDegrees);

  { Analizador Sintáctico (Parser) de MakerAI GQL Lite }
  TGraphParser = class
  private
    FLexer: TGraphLexer;
    FCurrent: TToken;
    FNodeVariables: TDictionary<string, TMatchNodePattern>;
    FCommandType: TGraphCommandType;
    FCommandTargetPattern: TMatchNodePattern;
    FCommandLimit: Integer;
    FCommandSourcePattern: TMatchNodePattern;

    procedure Next;
    function GetTokenTextAndNext: string;
    procedure Expect(AKind: TTokenKind);

    // Métodos internos de parsing
    function ParseNode(AQuery: TGraphMatchQuery): TMatchNodePattern;
    function ParseEdge(out ADirection: TGraphDirection): TMatchEdgePattern;
    procedure ParseProperties(AProps: TDictionary<string, Variant>);

    function ParseExpression: TGraphExpression;
    function ParseAndExpression: TGraphExpression;
    function ParseComparison: TGraphExpression;
    function ParsePrimary: TGraphExpression;

    procedure ParseMatchClause(AQuery: TGraphMatchQuery);
    procedure ParseWhereClause(AQuery: TGraphMatchQuery);
    procedure ParseReturnClause(AQuery: TGraphMatchQuery);
    procedure ClearCommandPatterns;

  public
    constructor Create(const AText: string);
    destructor Destroy; override;

    {
      Parsea el texto de entrada.
      - Si es un comando 'SHOW', devuelve nil y establece CommandType.
      - Si es una consulta 'MATCH', devuelve un objeto TGraphMatchQuery (CommandType será cmdNone).
    }
    function Parse: TGraphMatchQuery;

    property CommandType: TGraphCommandType read FCommandType;
    property CommandSourcePattern: TMatchNodePattern read FCommandSourcePattern;
    property CommandTargetPattern: TMatchNodePattern read FCommandTargetPattern;
    property CommandLimit: Integer read FCommandLimit;
  end;

implementation

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
    raise Exception.CreateFmt('Error Sintáctico: Se esperaba %s y se encontró "%s" en posición %d', [GetEnumName(TypeInfo(TTokenKind), Ord(AKind)), FCurrent.Text, FCurrent.Position]);
  Next;
end;

function TGraphParser.Parse: TGraphMatchQuery;
var
  TempQuery: TGraphMatchQuery;
  Src, Dst: TMatchNodePattern;
begin
  Result := nil;
  FCommandType := cmdNone;

  // Limpiamos patrones de comandos anteriores
  ClearCommandPatterns;

  // 1. COMANDOS SHOW (Introspección)
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
    else
      raise Exception.Create('Error Sintáctico: Se esperaba LABELS o EDGES después de SHOW.');
    Exit;
  end;

  // 2. COMANDOS DE ALGORITMOS
  // A. SHORTEST PATH
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

  // B. GET CENTRALITY / DEGREES
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
      raise Exception.Create('Error Sintáctico: Se esperaba CENTRALITY o DEGREES después de GET.');
    Exit;
  end;

  // 3. CONSULTAS MATCH ESTÁNDAR
  Result := TGraphMatchQuery.Create;
  try
    if FCurrent.Kind = tkMatch then
      Next;

    // --- SOPORTE MULTI-PATRÓN: MATCH (a)-[]->(b), (c)-[]->(d) ---
    ParseMatchClause(Result);
    while FCurrent.Kind = tkComma do
    begin
      Next; // Consumir la coma
      ParseMatchClause(Result);
    end;

    // 2. NUEVA CLÁUSULA: DEPTH opcional
    // Ejemplo: MATCH (p:Persona) DEPTH 2 WHERE ...
    if FCurrent.Kind = tkDepth then
    begin
      Next; // Consumir 'DEPTH'
      if FCurrent.Kind = tkNumber then
        Result.Depth := Trunc(StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant))
      else
        raise Exception.Create('Error Sintáctico: Se esperaba un número después de DEPTH.');
    end;

    // Parsear WHERE (Opcional)
    if FCurrent.Kind = tkWhere then
      ParseWhereClause(Result);

    // Parsear RETURN (Opcional)
    if FCurrent.Kind = tkReturn then
      ParseReturnClause(Result);

    // Verificación de final de cadena
    if FCurrent.Kind <> tkEOF then
      raise Exception.CreateFmt('Error Sintáctico: Contenido inesperado "%s" en la posición %d. Revise la sintaxis del RETURN o comas sobrantes.', [FCurrent.Text, FCurrent.Position]);

  except
    Result.Free;
    raise;
  end;
end;

procedure TGraphParser.ParseMatchClause(AQuery: TGraphMatchQuery);
var
  SrcNode, DstNode: TMatchNodePattern;
  Edge: TMatchEdgePattern;
  Dir: TGraphDirection;
begin
  // Primer nodo: (a)
  SrcNode := ParseNode(AQuery);

  // Mientras haya conectores de arista: (a)-... o (a)<-...
  while FCurrent.Kind in [tkDash, tkArrowLeft] do
  begin
    Edge := ParseEdge(Dir);
    Edge.Direction := Dir;

    DstNode := ParseNode(AQuery);
    AQuery.AddMatchClause(TMatchClause.Create(SrcNode.Variable, Edge, DstNode.Variable));

    // El destino actual se convierte en el origen de la siguiente relación
    SrcNode := DstNode;
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

  // Gestión de variables (Binding)
  if (VarName <> '') and FNodeVariables.TryGetValue(VarName, Result) then
  begin
    // Si la variable ya existe, reusamos el objeto.
    // Si ahora se especifica una etiqueta y antes no tenía, la actualizamos.
    if (Result.NodeLabel = '') and (LblName <> '') then
      Result.NodeLabel := LblName;
  end
  else
  begin
    // Nueva variable o nodo anónimo
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
  // Detección de flecha izquierda: <-
  Left := (FCurrent.Kind = tkArrowLeft);
  if Left then
    Next
  else
    Expect(tkDash);

  Expect(tkLBracket);
  Result := TMatchEdgePattern.Create;

  // Variable de arista opcional: [r]
  if FCurrent.Kind = tkIdentifier then
    Result.Variable := GetTokenTextAndNext;

  // Tipo de relación opcional: [:KNOWS]
  if FCurrent.Kind = tkColon then
  begin
    Next;
    Result.EdgeLabel := FCurrent.Text;
    Expect(tkIdentifier);
  end;

  // Propiedades de arista: {since: 2020}
  if FCurrent.Kind = tkLBrace then
    ParseProperties(Result.Properties);

  Expect(tkRBracket);

  // Detección de flecha derecha: ->
  Right := (FCurrent.Kind = tkArrowRight);
  if Right then
    Next
  else
    Expect(tkDash);

  // Determinar dirección final
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

    case FCurrent.Kind of
      tkString:
        AProps.Add(Key, GetTokenTextAndNext);
      tkNumber:
        AProps.Add(Key, StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant));
      tkBoolean:
        AProps.Add(Key, SameText(GetTokenTextAndNext, 'true'));
      tkNull:
        begin
          AProps.Add(Key, Null);
          Next;
        end;
    else
      raise Exception.Create('Valor de propiedad no soportado.');
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
begin
  L := ParsePrimary;

  // Añade tkContains a la lista de tokens válidos para comparación
  if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains] then
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
        Op := opContains; // <-- Mapeo del token al operador
    end;
    Next;
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
        Next;
        Result := ParseExpression;
        Expect(tkRParen);
      end;

    tkIdentifier:
      begin
        // Soporte para n.propiedad
        V := GetTokenTextAndNext;
        Expect(tkDot);
        P := FCurrent.Text;
        Expect(tkIdentifier);
        Result := TPropertyExpr.Create(V, P);
      end;

    tkString:
      Result := TLiteralExpr.Create(GetTokenTextAndNext);

    tkNumber:
      Result := TLiteralExpr.Create(StrToFloat(GetTokenTextAndNext, TFormatSettings.Invariant));

    tkBoolean:
      Result := TLiteralExpr.Create(SameText(GetTokenTextAndNext, 'true'));

    tkNull:
      begin
        Next;
        Result := TLiteralExpr.Create(Null);
      end;
  else
    raise Exception.Create('Error: Se esperaba una expresión válida en el WHERE.');
  end;
end;

procedure TGraphParser.ParseReturnClause(AQuery: TGraphMatchQuery);
begin
  Expect(tkReturn);

  // El RETURN debe tener al menos un elemento antes del fin de archivo
  if FCurrent.Kind = tkEOF then
    raise Exception.Create('Error Sintáctico: Se esperaba una expresión después de RETURN.');

  while True do
  begin
    // --- CASO A: Funciones de Agregación (COUNT, SUM, AVG) ---
    if FCurrent.Kind in [tkCount, tkSum, tkAvg] then
    begin
      Next; // Consumir el nombre de la función
      Expect(tkLParen);

      // El argumento puede ser una variable (p), una propiedad (p.valor) o todo (*)
      if FCurrent.Kind = tkIdentifier then
      begin
        Next; // Consumir identificador (p)
        // Soporte para SUM(p.puntos)
        if FCurrent.Kind = tkDot then
        begin
          Next; // Consumir el punto
          Expect(tkIdentifier); // Consumir la propiedad
        end;
      end
      else if FCurrent.Text = '*' then
      begin
        Next; // Caso especial COUNT(*)
      end
      else
        raise Exception.Create('Error Sintáctico: Argumento inválido en función de agregación. Se esperaba una variable o p.propiedad.');

      Expect(tkRParen);
    end

    // --- CASO B: Identificador simple o Acceso a Propiedad (p o p.name) ---
    else if FCurrent.Kind = tkIdentifier then
    begin
      Next; // Consumir el identificador (la variable, ej: 'p')

      // Si hay un punto, es acceso a propiedad: variable.propiedad (ej: 'p.name')
      if FCurrent.Kind = tkDot then
      begin
        Next; // Consumir el punto '.'
        Expect(tkIdentifier); // Consumir el nombre de la propiedad (ej: 'name')
      end;
    end

    // --- CASO C: Literales (ej: RETURN 'Hola', 100) ---
    else if FCurrent.Kind in [tkString, tkNumber, tkBoolean] then
    begin
      Next;
    end

    // --- CASO D: Palabra NULL ---
    else if FCurrent.Kind = tkNull then
    begin
      Next;
    end
    else
      raise Exception.CreateFmt('Error Sintáctico: Expresión de retorno no válida "%s" en posición %d.', [FCurrent.Text, FCurrent.Position]);

    // --- SOPORTE PARA ALIAS "AS" (Opcional) ---
    // Permite: RETURN p.name AS nombre. Evita error de "contenido inesperado" si se usa el alias.
    if (FCurrent.Kind = tkIdentifier) and (SameText(FCurrent.Text, 'AS')) then
    begin
      Next; // Consumir 'AS'
      Expect(tkIdentifier); // Consumir el nombre del alias
    end;

    // --- MANEJO DE SEPARADORES (COMAS) ---
    if FCurrent.Kind = tkComma then
    begin
      Next; // Consumir la coma ','

      // Verificación de seguridad: no puede haber una coma al final sin nada después
      if (FCurrent.Kind = tkEOF) then
        raise Exception.Create('Error Sintáctico: Se encontró una coma al final de la cláusula RETURN sin una expresión subsiguiente.');
    end
    else
      Break; // No hay más comas, terminamos de parsear el RETURN
  end;
end;

end.
