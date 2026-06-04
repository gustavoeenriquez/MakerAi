unit uMakerAi.RAG.Vectors.VQL;


interface

uses
  System.SysUtils, System.Variants, System.Character, System.TypInfo, System.Generics.Collections,
  uMakerAi.RAG.MetaData;

type
  // Tipos de token
  TVGQLTokenKind = (
    // Control
    tkEOF,

    tkStartsWith, tkEndsWith, tkLike, tkILike, tkIn, tkBetween,
    tkIs, tkNot, tkExists, tkExistsAny, tkExistsAll,

    // Literales
    tkIdentifier, tkString, tkNumber, tkBoolean, tkNull,

    // Delimitadores
    tkLParen, tkRParen, tkColon, tkComma, tkDot,

    // Operadores comparacion
    tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains,

    // Operadores logicos
    tkAnd, tkOr,

    // Palabras reservadas / clausulas
    tkMatch, tkSearch, tkUsing, tkWhere, tkRerank, tkThreshold, tkOptimize, tkReturn,
    tkWith, tkLimit, tkHybrid, tkEmbeddings, tkBm25, tkWeights, tkLanguage, tkFusion,
    tkRrf, tkWeighted, tkRegenerate, tkGlobal, tkSemantic, tkLexical,
    tkReorder, tkAbc, tkText, tkMetadata, tkScore,

    // --- NUEVOS TOKENS v2 ---
    tkOffset,    // OFFSET n  — paginacion
    tkDistinct,  // DISTINCT ON campo — deduplicacion
    tkOn,        // ON (parte de DISTINCT ON / ORDER BY)
    tkOrder,     // ORDER
    tkBy,        // BY (parte de ORDER BY)
    tkAsc,       // ASC  — direccion ascendente
    tkDesc,      // DESC — direccion descendente
    tkExplain,   // EXPLAIN — mostrar plan sin ejecutar
    tkId,        // ID — campo de retorno extendido
    tkRank,      // RANK — campo de retorno extendido
    tkMmr,       // MMR  — modo de optimizacion
    tkModel,     // MODEL — campo de retorno extendido
    tkTop        // TOP — alias de LIMIT (reservado)
  );

  TVGQLToken = record
    Kind: TVGQLTokenKind;
    Text: string;
    Position: Integer;
  end;

  TVGQLLexer = class
  private
    FText: string;
    FPos: Integer;

    function IsEOF: Boolean;
    function Peek: Char;
    function Next: Char;
    procedure SkipWhitespace;

    function ReadIdentifier: TVGQLToken;
    function ReadString: TVGQLToken;
    function ReadNumber: TVGQLToken;

  public
    constructor Create(const AText: string);
    function NextToken: TVGQLToken;
  end;

  //==================================
  //============ AST =================
  //==================================

  // ===== BASE =====
  TASTNode = class
  private
    FPosition: Integer;
  public
    constructor Create(APosition: Integer = 0); virtual;
    property Position: Integer read FPosition;
  end;

  // ===== EXPRESIONES =====
  TExpression = class(TASTNode);

  TLiteralType = (ltString, ltNumber, ltBoolean, ltNull);

  TLiteralExpression = class(TExpression)
  private
    FLiteralType: TLiteralType;
    FValue: string;
  public
    property LiteralType: TLiteralType read FLiteralType write FLiteralType;
    property Value: string read FValue write FValue;
  end;

  TIdentifierExpression = class(TExpression)
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

TBinaryOperator = (
    boAnd, boOr,
    boEqual, boNotEqual,
    boGreater, boGreaterEqual,
    boLess, boLessEqual,
    boContains,
    // --- NUEVOS PARA COORDINAR CON LEXER Y METADATA ---
    boLike, boILike,
    boIn, boNotIn,
    boBetween,
    boStartsWith, boEndsWith,
    boIsNull, boIsNotNull,
    boExists, boExistsAny, boExistsAll
  );

  TBinaryExpression = class(TExpression)
  private
    FLeft: TExpression;
    FOperator: TBinaryOperator;
    FRight: TExpression;
  public
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Operator: TBinaryOperator read FOperator write FOperator;
    property Right: TExpression read FRight write FRight;
  end;


  TInExpression = class(TExpression)
  private
    FLeft: TExpression;
    FValues: TList<TExpression>;
    FIsNot: Boolean;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Values: TList<TExpression> read FValues;
    property IsNot: Boolean read FIsNot write FIsNot; // True si es "NOT IN"
  end;

  // Expresion para: campo BETWEEN min AND max
  TBetweenExpression = class(TExpression)
  private
    FLeft: TExpression;
    FMin: TExpression;
    FMax: TExpression;
  public
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Min: TExpression read FMin write FMin;
    property Max: TExpression read FMax write FMax;
  end;

  // ===== CLAUSULAS =====
  TMatchClause = class(TASTNode)
  private
    FEntityName: string;
    FAlias: string;
  public
    property EntityName: string read FEntityName write FEntityName;
    property Alias: string read FAlias write FAlias;
  end;

  TSearchClause = class(TASTNode)
  private
    FQueryText: string;
  public
    property QueryText: string read FQueryText write FQueryText;
  end;

  TSearchMode = (smHybrid, smEmbeddings, smBM25);
  TFusionMode = (fmWeighted, fmRRF);


  TUsingClause = class(TASTNode)
  private
    FMode: TSearchMode;
    FSemanticWeight: Double;
    FLexicalWeight: Double;
    FFusion: string;
    FLanguage: string;
  public
    constructor Create(APosition: Integer = 0); override;
    property Mode: TSearchMode read FMode write FMode;
    property SemanticWeight: Double read FSemanticWeight write FSemanticWeight;
    property LexicalWeight: Double read FLexicalWeight write FLexicalWeight;
    property Fusion: string read FFusion write FFusion;
    property Language: string read FLanguage write FLanguage;
  end;

  TWhereClause = class(TASTNode)
  private
    FExpression: TExpression;
  public
    destructor Destroy; override;
    property Expression: TExpression read FExpression write FExpression;
  end;

  TRerankClause = class(TASTNode)
  private
    FQuery: string;
    FRegenerate: Boolean;
  public
    property Query: string read FQuery write FQuery;
    property Regenerate: Boolean read FRegenerate write FRegenerate;
  end;

  TThresholdScope = (tsGlobal, tsSemantic, tsLexical);

  TThresholdClause = class(TASTNode)
  private
    FScope: TThresholdScope;
    FValue: Double;
  public
    property Scope: TThresholdScope read FScope write FScope;
    property Value: Double read FValue write FValue;
  end;

  TOptimizeMode = (omNone, omReorderABC, omMMR);

  TOptimizeClause = class(TASTNode)
  private
    FMode: TOptimizeMode;
    FLambda: Double;
  public
    constructor Create(APosition: Integer = 0); override;
    property Mode: TOptimizeMode read FMode write FMode;
    // Lambda parameter for MMR (0..1): higher = more relevant, lower = more diverse
    property Lambda: Double read FLambda write FLambda;
  end;

  TReturnField = (rfText, rfMetadata, rfScore, rfId, rfRank, rfModel);

  TReturnClause = class(TASTNode)
  private
    FFields: TList<TReturnField>;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor Destroy; override;
    property Fields: TList<TReturnField> read FFields;
  end;

  // --- NUEVAS CLAUSULAS v2 ---

  // Direction for ORDER BY fields
  TOrderDirection = (odAsc, odDesc);

  // A single field specification in ORDER BY
  TOrderByField = record
    FieldName: string;
    Direction: TOrderDirection;
  end;

  TDistinctClause = class(TASTNode)
  private
    FField: string;
  public
    property Field: string read FField write FField;
  end;

  TOrderByClause = class(TASTNode)
  private
    FFields: TList<TOrderByField>;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor Destroy; override;
    property Fields: TList<TOrderByField> read FFields;
  end;

  // ===== QUERY PRINCIPAL =====
  TVGQLQuery = class(TASTNode)
  private
    FMatch: TMatchClause;
    FSearch: TSearchClause;
    FUsing: TUsingClause;
    FWhere: TWhereClause;
    FRerank: TRerankClause;
    FThreshold: TThresholdClause;
    FOptimize: TOptimizeClause;
    FReturn: TReturnClause;
    FLimit: Integer;
    // --- Nuevos campos v2 ---
    FOffset: Integer;
    FDistinct: TDistinctClause;
    FOrderBy: TOrderByClause;
    FExplain: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property Match: TMatchClause read FMatch write FMatch;
    property Search: TSearchClause read FSearch write FSearch;
    property UsingClause: TUsingClause read FUsing write FUsing;
    property WhereClause: TWhereClause read FWhere write FWhere;
    property RerankClause: TRerankClause read FRerank write FRerank;
    property ThresholdClause: TThresholdClause read FThreshold write FThreshold;
    property OptimizeClause: TOptimizeClause read FOptimize write FOptimize;
    property ReturnClause: TReturnClause read FReturn write FReturn;
    property Limit: Integer read FLimit write FLimit;
    // Nuevas propiedades v2
    property Offset: Integer read FOffset write FOffset;
    property DistinctClause: TDistinctClause read FDistinct write FDistinct;
    property OrderByClause: TOrderByClause read FOrderBy write FOrderBy;
    property Explain: Boolean read FExplain write FExplain;
  end;


  //=====================================
  //============ PARSER =================
  //=====================================


  EVGQLParserError = class(Exception);

  TVGQLParser = class
  private
    FLexer: TVGQLLexer;
    FCurrent: TVGQLToken;

    procedure Next;
    procedure Expect(AKind: TVGQLTokenKind; const AMsg: string = '');
    function Check(AKind: TVGQLTokenKind): Boolean;
    function Match(AKind: TVGQLTokenKind): Boolean;

    // Parsers de clausulas
    function ParseMatch: TMatchClause;
    function ParseSearch: TSearchClause;
    function ParseUsing: TUsingClause;
    function ParseWhere: TWhereClause;
    function ParseRerank: TRerankClause;
    function ParseThreshold: TThresholdClause;
    function ParseOptimize: TOptimizeClause;
    function ParseReturn: TReturnClause;
    // Nuevos parsers v2
    function ParseDistinct: TDistinctClause;
    function ParseOrderBy: TOrderByClause;

    // Parser de expresiones (precedencia descendente)
    function ParseExpression: TExpression;
    function ParseOr: TExpression;
    function ParseAnd: TExpression;
    function ParseComparison: TExpression;
    function ParsePrimary: TExpression;
  public
    constructor Create(const AText: string);
    destructor Destroy; override;
    function Parse: TVGQLQuery;
  end;


  //======================================
  //============ REQUEST =================
  //======================================


  TVGQLRequest = class
  public
    // MATCH
    Entity: string;
    ALabel: string;

    // SEARCH
    Query: string;

    // USING
    Mode: TSearchMode;
    WeightSemantic: Double;
    WeightLexical: Double;
    Language: string;
    Fusion: TFusionMode;

    // WHERE
    // CAMBIO: Ahora usamos Criteria nativo, no MetaData plano
    Filter: TAiFilterCriteria;

    // RERANK
    RerankQuery: string;
    RerankRegenerate: Boolean;

    // THRESHOLD
    MinGlobal: Double;
    MinSemantic: Double;
    MinLexical: Double;

    // OPTIMIZE
    UseReorderABC: Boolean;
    UseMMR: Boolean;
    MmrLambda: Double;

    // RETURN
    Limit: Integer;
    IncludeMetadata: Boolean;
    IncludeScore: Boolean;
    // Nuevos campos de retorno v2
    IncludeId: Boolean;
    IncludeRank: Boolean;
    IncludeModel: Boolean;

    // NUEVOS campos v2
    Offset: Integer;
    DistinctField: string;
    OrderByFields: TList<TOrderByField>;
    Explain: Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

  //=======================================
  //============ COMPILER =================
  //=======================================

  EVGQLTranslationError = class(Exception);

  { El Compilador transforma el AST (Sintaxis) en un TVGQLRequest (Ejecucion) }
  TVGQLCompiler = class
  private
    FRequest: TVGQLRequest;

    // Metodos de traduccion de clausulas
    procedure TranslateMatch(AClause: TMatchClause);
    procedure TranslateSearch(AClause: TSearchClause);
    procedure TranslateUsing(AClause: TUsingClause);
    procedure TranslateWhere(AClause: TWhereClause);
    procedure TranslateRerank(AClause: TRerankClause);
    procedure TranslateThreshold(AClause: TThresholdClause);
    procedure TranslateOptimize(AClause: TOptimizeClause);
    procedure TranslateReturn(AClause: TReturnClause);
    // Nuevos traductores v2
    procedure TranslateDistinct(AClause: TDistinctClause);
    procedure TranslateOrderBy(AClause: TOrderByClause);

    // Motor de traduccion de expresiones (Recursivo)
    procedure BuildFilter(AExpr: TExpression; ACriteria: TAiFilterCriteria);
    function GetLiteralValue(AExpr: TExpression): Variant;

  public
    function Translate(AQuery: TVGQLQuery): TVGQLRequest;
  end;




implementation

{ TVGQLLexer }

constructor TVGQLLexer.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPos := 1;
end;

function TVGQLLexer.IsEOF: Boolean;
begin
  Result := FPos > Length(FText);
end;

function TVGQLLexer.Peek: Char;
begin
  if IsEOF then
    Result := #0
  else
    Result := FText[FPos];
end;

function TVGQLLexer.Next: Char;
begin
  Result := Peek;
  Inc(FPos);
end;

procedure TVGQLLexer.SkipWhitespace;
begin
  while (not IsEOF) and Peek.IsWhiteSpace do
    Next;
end;

function TVGQLLexer.ReadString: TVGQLToken;
var
  Quote: Char;
  Start: Integer;
begin
  Quote := Next; // ' o "
  Start := FPos;
  while (not IsEOF) and (Peek <> Quote) do
    Next;

  Result.Kind := tkString;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start - 1;

  if not IsEOF then
    Next; // cerrar comilla
end;

function TVGQLLexer.ReadNumber: TVGQLToken;
var
  Start: Integer;
begin
  Start := FPos;
  while (not IsEOF) and (Peek.IsDigit or (Peek = '.')) do
    Next;

  Result.Kind := tkNumber;
  Result.Text := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TVGQLLexer.ReadIdentifier: TVGQLToken;
var
  Start: Integer;
  S, U: string;
begin
  Start := FPos;
  // Permitir letras, numeros y guiones bajos (tipico de identificadores/claves)
  while (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_')) do
    Next;

  S := Copy(FText, Start, FPos - Start);
  U := S.ToUpper; // Para comparacion insensible a mayusculas

  Result.Text := S;
  Result.Position := Start;

  // --- CLAUSULAS PRINCIPALES ---
  if U = 'MATCH' then Result.Kind := tkMatch
  else if U = 'SEARCH' then Result.Kind := tkSearch
  else if U = 'USING' then Result.Kind := tkUsing
  else if U = 'WHERE' then Result.Kind := tkWhere
  else if U = 'RERANK' then Result.Kind := tkRerank
  else if U = 'RETURN' then Result.Kind := tkReturn
  else if U = 'WITH' then Result.Kind := tkWith
  else if U = 'LIMIT' then Result.Kind := tkLimit
  else if U = 'OPTIMIZE' then Result.Kind := tkOptimize
  else if U = 'THRESHOLD' then Result.Kind := tkThreshold

  // --- NUEVAS CLAUSULAS v2 ---
  else if U = 'OFFSET' then Result.Kind := tkOffset
  else if U = 'DISTINCT' then Result.Kind := tkDistinct
  else if U = 'ON' then Result.Kind := tkOn
  else if U = 'ORDER' then Result.Kind := tkOrder
  else if U = 'BY' then Result.Kind := tkBy
  else if U = 'ASC' then Result.Kind := tkAsc
  else if U = 'DESC' then Result.Kind := tkDesc
  else if U = 'EXPLAIN' then Result.Kind := tkExplain
  else if U = 'MMR' then Result.Kind := tkMmr
  else if U = 'TOP' then Result.Kind := tkTop

  // --- OPERADORES LOGICOS ---
  else if U = 'AND' then Result.Kind := tkAnd
  else if U = 'OR' then Result.Kind := tkOr
  else if U = 'NOT' then Result.Kind := tkNot

  // --- OPERADORES DE FILTRADO (Sincronizados con MetaData) ---
  else if U = 'CONTAINS' then Result.Kind := tkContains
  else if U = 'LIKE' then Result.Kind := tkLike
  else if U = 'ILIKE' then Result.Kind := tkILike
  else if U = 'IN' then Result.Kind := tkIn
  else if U = 'BETWEEN' then Result.Kind := tkBetween
  else if U = 'STARTS_WITH' then Result.Kind := tkStartsWith
  else if U = 'ENDS_WITH' then Result.Kind := tkEndsWith
  else if U = 'IS' then Result.Kind := tkIs
  else if U = 'EXISTS' then Result.Kind := tkExists
  else if U = 'EXISTS_ANY' then Result.Kind := tkExistsAny
  else if U = 'EXISTS_ALL' then Result.Kind := tkExistsAll

  // --- CONFIGURACION DE BUSQUEDA / RAG ---
  else if U = 'HYBRID' then Result.Kind := tkHybrid
  else if U = 'EMBEDDINGS' then Result.Kind := tkEmbeddings
  else if U = 'BM25' then Result.Kind := tkBm25
  else if U = 'WEIGHTS' then Result.Kind := tkWeights
  else if U = 'LANGUAGE' then Result.Kind := tkLanguage
  else if U = 'FUSION' then Result.Kind := tkFusion
  else if U = 'RRF' then Result.Kind := tkRrf
  else if U = 'WEIGHTED' then Result.Kind := tkWeighted

  // --- MODOS Y ESTRATEGIAS ---
  else if U = 'REGENERATE' then Result.Kind := tkRegenerate
  else if U = 'GLOBAL' then Result.Kind := tkGlobal
  else if U = 'SEMANTIC' then Result.Kind := tkSemantic
  else if U = 'LEXICAL' then Result.Kind := tkLexical
  else if U = 'REORDER' then Result.Kind := tkReorder
  else if U = 'ABC' then Result.Kind := tkAbc

  // --- CAMPOS DE RETORNO ---
  else if U = 'TEXT' then Result.Kind := tkText
  else if U = 'METADATA' then Result.Kind := tkMetadata
  else if U = 'SCORE' then Result.Kind := tkScore
  // Nuevos campos de retorno v2
  else if U = 'ID' then Result.Kind := tkId
  else if U = 'RANK' then Result.Kind := tkRank
  else if U = 'MODEL' then Result.Kind := tkModel

  // --- LITERALES BOOL / NULL ---
  else if (U = 'TRUE') or (U = 'FALSE') then Result.Kind := tkBoolean
  else if U = 'NULL' then Result.Kind := tkNull

  // --- SI NO ES PALABRA RESERVADA, ES UN IDENTIFICADOR (Nombre de campo, etc) ---
  else
    Result.Kind := tkIdentifier;
end;

function TVGQLLexer.NextToken: TVGQLToken;
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
      Begin
        Next;
        Result.Kind := tkLParen;
        Result.Text := '('
      end;

    ')':
      Begin
        Next;
        Result.Kind := tkRParen;
        Result.Text := ')'
      end;
    ':':
      Begin
        Next;
        Result.Kind := tkColon;
        Result.Text := ':'
      end;
    ',':
      Begin
        Next;
        Result.Kind := tkComma;
        Result.Text := ','
      end;
    '.':
      Begin
        Next;
        Result.Kind := tkDot;
        Result.Text := '.'
      end;
    '=':
      Begin
        Next;
        Result.Kind := tkEqual;
        Result.Text := '='
      end;

    '<':
      begin
        Next;
        if Peek = '>' then
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
          Result.Kind := tkLess;
        Result.Text := '<';
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
          Result.Kind := tkGreater;
        Result.Text := '>';
      end;
    '''', '"':
      Exit(ReadString);
    '0' .. '9':
      Exit(ReadNumber);
  else
    if Peek.IsLetter or (Peek = '_') then
      Exit(ReadIdentifier)
    else
      raise Exception.CreateFmt('Error lexico: caracter inesperado "%s" en posicion %d', [Peek, FPos]);
  end;
end;



  //==================================
  //============ AST =================
  //==================================


{ TASTNode }

constructor TASTNode.Create(APosition: Integer);
begin
  inherited Create;
  FPosition := APosition;
end;

{ TBinaryExpression }

destructor TBinaryExpression.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

{ TUsingClause }

constructor TUsingClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FSemanticWeight := 0.5;
  FLexicalWeight := 0.5;
  FFusion := '';
  FLanguage := '';
end;

{ TWhereClause }

destructor TWhereClause.Destroy;
begin
  FExpression.Free;
  inherited;
end;

{ TOptimizeClause }

constructor TOptimizeClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FMode := omNone;
  FLambda := 0.7;
end;

{ TReturnClause }

constructor TReturnClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FFields := TList<TReturnField>.Create;
end;

destructor TReturnClause.Destroy;
begin
  FFields.Free;
  inherited;
end;

{ TOrderByClause }

constructor TOrderByClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FFields := TList<TOrderByField>.Create;
end;

destructor TOrderByClause.Destroy;
begin
  FFields.Free;
  inherited;
end;

{ TVGQLQuery }

constructor TVGQLQuery.Create;
begin
  inherited Create(0);
  FMatch := nil;
  FSearch := nil;
  FUsing := nil;
  FWhere := nil;
  FRerank := nil;
  FThreshold := nil;
  FOptimize := nil;
  FReturn := nil;
  FLimit := -1;
  FOffset := 0;
  FDistinct := nil;
  FOrderBy := nil;
  FExplain := False;
end;

destructor TVGQLQuery.Destroy;
begin
  FMatch.Free;
  FSearch.Free;
  FUsing.Free;
  FWhere.Free;
  FRerank.Free;
  FThreshold.Free;
  FOptimize.Free;
  FReturn.Free;
  FDistinct.Free;
  FOrderBy.Free;
  inherited;
end;

{ TInExpression }

constructor TInExpression.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FValues := TList<TExpression>.Create;
  FIsNot := False;
end;

destructor TInExpression.Destroy;
var
  Expr: TExpression;
begin
  FLeft.Free;
  // Liberar cada expresion de la lista de valores
  for Expr in FValues do
    Expr.Free;
  FValues.Free;
  inherited;
end;

{ TBetweenExpression }

destructor TBetweenExpression.Destroy;
begin
  FLeft.Free;
  FMin.Free;
  FMax.Free;
  inherited;
end;

  //=====================================
  //============ PARSER =================
  //=====================================

{ TVGQLParser }

constructor TVGQLParser.Create(const AText: string);
begin
  inherited Create;
  FLexer := TVGQLLexer.Create(AText);
  Next; // Cargar primer token
end;

destructor TVGQLParser.Destroy;
begin
  FLexer.Free;
  inherited;
end;

procedure TVGQLParser.Next;
begin
  FCurrent := FLexer.NextToken;
end;

function TVGQLParser.Check(AKind: TVGQLTokenKind): Boolean;
begin
  Result := FCurrent.Kind = AKind;
end;

function TVGQLParser.Match(AKind: TVGQLTokenKind): Boolean;
begin
  Result := Check(AKind);
  if Result then
    Next;
end;

procedure TVGQLParser.Expect(AKind: TVGQLTokenKind; const AMsg: string);
var
  ErrorMsg: string;
begin
  if not Check(AKind) then
  begin
    if AMsg <> '' then
      ErrorMsg := Format('Linea %d: se esperaba %s (%s)', [FCurrent.Position, GetEnumName(TypeInfo(TVGQLTokenKind), Ord(AKind)), AMsg])
    else
      ErrorMsg := Format('Linea %d: se esperaba %s', [FCurrent.Position, GetEnumName(TypeInfo(TVGQLTokenKind), Ord(AKind))]);
    raise EVGQLParserError.Create(ErrorMsg);
  end;
  Next;
end;

{ ===== PARSER PRINCIPAL ===== }

function TVGQLParser.Parse: TVGQLQuery;
begin
  Result := TVGQLQuery.Create;
  try
    // EXPLAIN (opcional, debe ir primero)
    if Match(tkExplain) then
      Result.Explain := True;

    // MATCH (opcional)
    if Check(tkMatch) then
      Result.Match := ParseMatch;

    // SEARCH (obligatorio)
    if Check(tkSearch) then
      Result.Search := ParseSearch
    else
      raise EVGQLParserError.Create('Se requiere clausula SEARCH');

    // USING (opcional)
    if Check(tkUsing) then
      Result.UsingClause := ParseUsing;

    // WHERE (opcional)
    if Check(tkWhere) then
      Result.WhereClause := ParseWhere;

    // RERANK (opcional)
    if Check(tkRerank) then
      Result.RerankClause := ParseRerank;

    // THRESHOLD (opcional)
    if Check(tkThreshold) then
      Result.ThresholdClause := ParseThreshold;

    // OPTIMIZE (opcional)
    if Check(tkOptimize) then
      Result.OptimizeClause := ParseOptimize;

    // DISTINCT ON campo (opcional)
    if Check(tkDistinct) then
      Result.DistinctClause := ParseDistinct;

    // ORDER BY (opcional)
    if Check(tkOrder) then
      Result.OrderByClause := ParseOrderBy;

    // RETURN (opcional)
    if Check(tkReturn) then
      Result.ReturnClause := ParseReturn;

    // LIMIT (opcional)
    if Match(tkLimit) then
    begin
      // Al usar Match(tkLimit), ya saltamos el token "LIMIT"
      // Ahora FCurrent apunta al supuesto numero
      if Check(tkNumber) then
      begin
        Result.Limit := StrToInt(FCurrent.Text);
        Next; // Ahora si, consumimos el numero para pasar al siguiente token
      end
      else
        raise EVGQLParserError.Create('Se requiere un valor numerico para la clausula LIMIT');
    end;

    // OFFSET (opcional) — paginacion
    if Match(tkOffset) then
    begin
      if Check(tkNumber) then
      begin
        Result.Offset := StrToInt(FCurrent.Text);
        Next;
      end
      else
        raise EVGQLParserError.Create('Se requiere un valor numerico para la clausula OFFSET');
    end;

    // Verificar fin de consulta
    Expect(tkEOF, 'fin de consulta');
  except
    Result.Free;
    raise;
  end;
end;

{ ===== MATCH ===== }

function TVGQLParser.ParseMatch: TMatchClause;
begin
  Result := TMatchClause.Create(FCurrent.Position);
  try
    Next; // consumir MATCH
    Expect(tkLParen, '(');

    if Check(tkIdentifier) then
    begin
      Result.EntityName := FCurrent.Text; // Capturar texto ANTES de avanzar
      Next;
    end
    else
      Expect(tkIdentifier, 'nombre de entidad');

    // Alias opcional
    if Match(tkColon) then
    begin
      if Check(tkIdentifier) then
      begin
        Result.Alias := FCurrent.Text;
        Next;
      end
      else
        Expect(tkIdentifier, 'alias');
    end;

    Expect(tkRParen, ')');
  except
    Result.Free;
    raise;
  end;
end;

{ ===== SEARCH ===== }

function TVGQLParser.ParseSearch: TSearchClause;
begin
  Result := TSearchClause.Create(FCurrent.Position);
  try
    Next; // consumir SEARCH
    if Check(tkString) then
    begin
      Result.QueryText := FCurrent.Text; // Capturar el valor del string
      Next; // Avanzar
    end
    else
      Expect(tkString, 'texto de busqueda');
  except
    Result.Free;
    raise;
  end;
end;

{ ===== USING ===== }

function TVGQLParser.ParseUsing: TUsingClause;
begin
  Result := TUsingClause.Create(FCurrent.Position);
  try
    Next; // consumir USING

    case FCurrent.Kind of
      tkHybrid:
        Result.Mode := smHybrid;
      tkEmbeddings:
        Result.Mode := smEmbeddings;
      tkBm25:
        Result.Mode := smBM25;
    else
      raise EVGQLParserError.CreateFmt('Modo USING invalido en %d', [FCurrent.Position]);
    end;
    Next; // Consumir el modo

    // WITH WEIGHTS
    if Match(tkWith) then
    begin
      Expect(tkWeights);
      Expect(tkLParen);
      if Check(tkNumber) then
      begin
        Result.SemanticWeight := StrToFloat(FCurrent.Text, TFormatSettings.Invariant);
        Next;
      end;
      Expect(tkComma);
      if Check(tkNumber) then
      begin
        Result.LexicalWeight := StrToFloat(FCurrent.Text, TFormatSettings.Invariant);
        Next;
      end;
      Expect(tkRParen);
    end;

    // FUSION
   if Match(tkFusion) then
    begin
      // CORRECCION: Aceptamos tkIdentifier O los tokens especificos RRF/WEIGHTED
      if Check(tkIdentifier) or Check(tkRrf) or Check(tkWeighted) then
      begin
        Result.Fusion := FCurrent.Text;
        Next;
      end
      else
        raise EVGQLParserError.CreateFmt('Se esperaba un modo de fusion valido (RRF, WEIGHTED) en la posicion %d', [FCurrent.Position]);
    end;

    // LANGUAGE identificador — opcional, fija el idioma del motor lexico BM25
    if Match(tkLanguage) then
    begin
      // Aceptar cualquier identificador como nombre de idioma
      if Check(tkIdentifier) or Check(tkSemantic) or Check(tkLexical) or Check(tkGlobal) then
      begin
        Result.Language := FCurrent.Text;
        Next;
      end
      else if Check(tkString) then
      begin
        Result.Language := FCurrent.Text;
        Next;
      end
      else
        raise EVGQLParserError.CreateFmt('Se esperaba un identificador de idioma despues de LANGUAGE en posicion %d', [FCurrent.Position]);
    end;

  except
    Result.Free;
    raise;
  end;
end;

{ ===== WHERE ===== }

function TVGQLParser.ParseWhere: TWhereClause;
begin
  Result := TWhereClause.Create(FCurrent.Position);
  try
    Next; // consumir WHERE
    Result.Expression := ParseExpression;
  except
    Result.Free;
    raise;
  end;
end;

{ ===== EXPRESIONES ===== }

function TVGQLParser.ParseExpression: TExpression;
begin
  Result := ParseOr;
end;

function TVGQLParser.ParseOr: TExpression;
var
  Left, Right: TExpression;
  BinExpr: TBinaryExpression;
begin
  Left := ParseAnd;
  Result := Left;

  while Match(tkOr) do
  begin
    Right := ParseAnd;
    BinExpr := TBinaryExpression.Create;
    BinExpr.Left := Left;
    BinExpr.Operator := boOr;
    BinExpr.Right := Right;
    Result := BinExpr;
    Left := Result;
  end;
end;

function TVGQLParser.ParseAnd: TExpression;
var
  Left, Right: TExpression;
  BinExpr: TBinaryExpression;
begin
  Left := ParseComparison;
  Result := Left;

  while Match(tkAnd) do
  begin
    Right := ParseComparison;
    BinExpr := TBinaryExpression.Create;
    BinExpr.Left := Left;
    BinExpr.Operator := boAnd;
    BinExpr.Right := Right;
    Result := BinExpr;
    Left := Result;
  end;
end;

function TVGQLParser.ParseComparison: TExpression;
var
  Left, Right, Min, Max: TExpression;
  InExpr: TInExpression;
  BetExpr: TBetweenExpression;
  BinExpr: TBinaryExpression;
  Op: TBinaryOperator;
  IsNegated: Boolean;
begin
  Left := ParsePrimary;
  Result := Left;

  // 1. Caso: IS [NOT] NULL
  if Match(tkIs) then
  begin
    IsNegated := Match(tkNot);
    Expect(tkNull, 'NULL despues de IS');

    BinExpr := TBinaryExpression.Create;
    BinExpr.Left := Left;
    if IsNegated then
      BinExpr.Operator := boIsNotNull
    else
      BinExpr.Operator := boIsNull;
    BinExpr.Right := nil; // No necesita derecha
    Exit(BinExpr);
  end;

  // 2. Caso: [NOT] IN (val1, val2...)
  IsNegated := Match(tkNot); // Soporte para NOT IN
  if Match(tkIn) then
  begin
    InExpr := TInExpression.Create(FCurrent.Position);
    InExpr.Left := Left;
    InExpr.IsNot := IsNegated;
    Expect(tkLParen, '(');
    repeat
      InExpr.Values.Add(ParsePrimary);
      if not Match(tkComma) then
        Break;
    until False;
    Expect(tkRParen, ')');
    Exit(InExpr);
  end;

  // 3. Caso: BETWEEN min AND max
  if Match(tkBetween) then
  begin
    Min := ParsePrimary;
    Expect(tkAnd, 'AND entre valores de BETWEEN');
    Max := ParsePrimary;

    BetExpr := TBetweenExpression.Create;
    BetExpr.Left := Left;
    BetExpr.Min := Min;
    BetExpr.Max := Max;
    Exit(BetExpr);
  end;

  // 4. Caso: Operadores Binarios Estandar y Texto
  if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains, tkLike, tkILike, tkStartsWith, tkEndsWith] then
  begin
    case FCurrent.Kind of
      tkEqual:
        Op := boEqual;
      tkNotEqual:
        Op := boNotEqual;
      tkGreater:
        Op := boGreater;
      tkGreaterEqual:
        Op := boGreaterEqual;
      tkLess:
        Op := boLess;
      tkLessEqual:
        Op := boLessEqual;
      tkContains:
        Op := boContains;
      tkLike:
        Op := boLike;
      tkILike:
        Op := boILike;
      tkStartsWith:
        Op := boStartsWith;
      tkEndsWith:
        Op := boEndsWith;
    else
      Op := boEqual;
    end;

    Next; // consumir operador
    Right := ParsePrimary;

    BinExpr := TBinaryExpression.Create;
    BinExpr.Left := Left;
    BinExpr.Operator := Op;
    BinExpr.Right := Right;
    Result := BinExpr;
  end;
end;

function TVGQLParser.ParsePrimary: TExpression;
var
  LitExpr: TLiteralExpression;
  IdExpr: TIdentifierExpression;
  FullName: string;
begin
  case FCurrent.Kind of

    // Identificadores y palabras que pueden usarse como nombre de campo
    tkIdentifier, tkText, tkMetadata, tkScore, tkId, tkRank, tkModel:
      begin
        FullName := FCurrent.Text;
        Next;

        while Match(tkDot) do
        begin
          // Tambien permitimos palabras reservadas despues de un punto (ej: meta.text)
          if FCurrent.Kind in [tkIdentifier, tkText, tkMetadata, tkScore, tkId, tkRank, tkModel] then
          begin
            FullName := FullName + '.' + FCurrent.Text;
            Next;
          end
          else
            raise EVGQLParserError.CreateFmt('Se esperaba un nombre en la posicion %d', [FCurrent.Position]);
        end;

        IdExpr := TIdentifierExpression.Create;
        IdExpr.Name := FullName;
        Result := IdExpr;
      end;

    tkString:
      begin
        LitExpr := TLiteralExpression.Create;
        LitExpr.LiteralType := ltString;
        LitExpr.Value := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkNumber:
      begin
        LitExpr := TLiteralExpression.Create;
        LitExpr.LiteralType := ltNumber;
        LitExpr.Value := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkBoolean:
      begin
        LitExpr := TLiteralExpression.Create;
        LitExpr.LiteralType := ltBoolean;
        LitExpr.Value := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkNull:
      begin
        LitExpr := TLiteralExpression.Create;
        LitExpr.LiteralType := ltNull;
        LitExpr.Value := 'NULL';
        Next;
        Result := LitExpr;
      end;

    tkLParen:
      begin
        Next; // consumir (
        Result := ParseExpression;
        Expect(tkRParen, ')');
      end;
  else
    raise EVGQLParserError.CreateFmt('Expresion invalida en posicion %d: token inesperado "%s"', [FCurrent.Position, FCurrent.Text]);
  end;
end;

{ ===== RERANK ===== }

function TVGQLParser.ParseRerank: TRerankClause;
begin
  Result := TRerankClause.Create(FCurrent.Position);
  try
    Next; // Consumir el token 'RERANK'

    // CORRECCION: Verificar primero, asignar, y luego avanzar.
    if Check(tkString) then
    begin
      Result.Query := FCurrent.Text; // Capturamos el texto ACTUAL
      Next; // Avanzamos al siguiente token
    end
    else
    begin
      // Si quieres que el string sea obligatorio:
      Expect(tkString, 'texto para rerank');

      // NOTA: Si quisieras que el string fuera OPCIONAL (para usar la query original),
      // simplemente quitarias el 'else' y el 'Expect'.
    end;

    // REGENERATE opcional
    if Match(tkRegenerate) then
      Result.Regenerate := True;
  except
    Result.Free;
    raise;
  end;
end;

{ ===== THRESHOLD ===== }

function TVGQLParser.ParseThreshold: TThresholdClause;
begin
  Result := TThresholdClause.Create(FCurrent.Position);
  try
    Next; // Consumir 'THRESHOLD'

    // 1. Identificar Scope
    case FCurrent.Kind of
      tkGlobal:   Result.Scope := tsGlobal;
      tkSemantic: Result.Scope := tsSemantic;
      tkLexical:  Result.Scope := tsLexical;
    else
      raise EVGQLParserError.CreateFmt('Scope de THRESHOLD invalido en %d. Use GLOBAL, SEMANTIC o LEXICAL', [FCurrent.Position]);
    end;
    Next; // Consumir el Scope (ej: GLOBAL)

    // 2. Leer el numero ANTES de avanzar
    if Check(tkNumber) then
    begin
      Result.Value := StrToFloat(FCurrent.Text, TFormatSettings.Invariant);
      Next; // Ahora si avanzamos al siguiente token
    end
    else
      raise EVGQLParserError.CreateFmt('Se esperaba un valor numerico para el umbral en la posicion %d', [FCurrent.Position]);

  except
    Result.Free;
    raise;
  end;
end;

{ ===== OPTIMIZE ===== }

function TVGQLParser.ParseOptimize: TOptimizeClause;
begin
  Result := TOptimizeClause.Create(FCurrent.Position);
  try
    Next; // consumir OPTIMIZE

    // MMR(lambda) — Maximal Marginal Relevance
    if Match(tkMmr) then
    begin
      Result.Mode := omMMR;
      // Lambda opcional: MMR(0.7)
      if Match(tkLParen) then
      begin
        if Check(tkNumber) then
        begin
          Result.Lambda := StrToFloat(FCurrent.Text, TFormatSettings.Invariant);
          Next;
        end;
        Expect(tkRParen, ')');
      end;
    end
    // REORDER ABC — optimizacion de ventana de contexto LLM
    else if Match(tkReorder) then
    begin
      Expect(tkAbc, 'ABC');
      Result.Mode := omReorderABC;
    end
    else
      raise EVGQLParserError.CreateFmt('Modo OPTIMIZE invalido en %d. Use: MMR[(lambda)] o REORDER ABC', [FCurrent.Position]);

  except
    Result.Free;
    raise;
  end;
end;

{ ===== RETURN ===== }

function TVGQLParser.ParseReturn: TReturnClause;
begin
  Result := TReturnClause.Create(FCurrent.Position);
  try
    Next; // consumir RETURN

    // Parsear campos
    repeat
      case FCurrent.Kind of
        tkText:
          Result.Fields.Add(rfText);
        tkMetadata:
          Result.Fields.Add(rfMetadata);
        tkScore:
          Result.Fields.Add(rfScore);
        // Nuevos campos de retorno v2
        tkId:
          Result.Fields.Add(rfId);
        tkRank:
          Result.Fields.Add(rfRank);
        tkModel:
          Result.Fields.Add(rfModel);
      else
        raise EVGQLParserError.CreateFmt(
          'Campo RETURN invalido en posicion %d. Use: TEXT, METADATA, SCORE, ID, RANK o MODEL',
          [FCurrent.Position]);
      end;
      Next;

      if not Match(tkComma) then
        Break;
    until False;

    if Result.Fields.Count = 0 then
      raise EVGQLParserError.Create('RETURN requiere al menos un campo');
  except
    Result.Free;
    raise;
  end;
end;

{ ===== DISTINCT ON ===== }

function TVGQLParser.ParseDistinct: TDistinctClause;
begin
  Result := TDistinctClause.Create(FCurrent.Position);
  try
    Next; // consumir DISTINCT
    Expect(tkOn, 'ON despues de DISTINCT');

    // El campo puede ser un identificador o un token de retorno usado como campo
    if FCurrent.Kind in [tkIdentifier, tkText, tkMetadata, tkScore, tkId, tkRank, tkModel] then
    begin
      Result.Field := FCurrent.Text;
      Next;
    end
    else
      raise EVGQLParserError.CreateFmt('Se esperaba un nombre de campo despues de DISTINCT ON en posicion %d', [FCurrent.Position]);
  except
    Result.Free;
    raise;
  end;
end;

{ ===== ORDER BY ===== }

function TVGQLParser.ParseOrderBy: TOrderByClause;
var
  OBField: TOrderByField;
begin
  Result := TOrderByClause.Create(FCurrent.Position);
  try
    Next; // consumir ORDER
    Expect(tkBy, 'BY despues de ORDER');

    // Parsear lista de campos: campo1 [ASC|DESC], campo2 [ASC|DESC], ...
    repeat
      // Nombre del campo
      if FCurrent.Kind in [tkIdentifier, tkText, tkMetadata, tkScore, tkId, tkRank, tkModel] then
      begin
        OBField.FieldName := FCurrent.Text;
        Next;
      end
      else
        raise EVGQLParserError.CreateFmt('Se esperaba un nombre de campo en ORDER BY, posicion %d', [FCurrent.Position]);

      // Direccion opcional (por defecto ASC)
      if Match(tkDesc) then
        OBField.Direction := odDesc
      else
      begin
        Match(tkAsc); // opcional, por defecto ASC
        OBField.Direction := odAsc;
      end;

      Result.Fields.Add(OBField);

      if not Match(tkComma) then
        Break;
    until False;

    if Result.Fields.Count = 0 then
      raise EVGQLParserError.Create('ORDER BY requiere al menos un campo');
  except
    Result.Free;
    raise;
  end;
end;

  //=======================================
  //============ COMPILER =================
  //=======================================

{ TVGQLCompiler }

function TVGQLCompiler.Translate(AQuery: TVGQLQuery): TVGQLRequest;
begin
  if not Assigned(AQuery) then
    raise EVGQLTranslationError.Create('AST de consulta nulo');

  FRequest := TVGQLRequest.Create;
  try
    // 1. MATCH (Entidad y Alias)
    if Assigned(AQuery.Match) then
      TranslateMatch(AQuery.Match);

    // 2. SEARCH (Texto de busqueda - OBLIGATORIO)
    if Assigned(AQuery.Search) then
      TranslateSearch(AQuery.Search)
    else
      raise EVGQLTranslationError.Create('La clausula SEARCH es obligatoria');

    // 3. USING (Modo y pesos)
    if Assigned(AQuery.UsingClause) then
      TranslateUsing(AQuery.UsingClause);

    // 4. WHERE (Filtros de Metadatos complejos)
    if Assigned(AQuery.WhereClause) then
      TranslateWhere(AQuery.WhereClause);

    // 5. RERANK
    if Assigned(AQuery.RerankClause) then
      TranslateRerank(AQuery.RerankClause);

    // 6. THRESHOLD
    if Assigned(AQuery.ThresholdClause) then
      TranslateThreshold(AQuery.ThresholdClause);

    // 7. OPTIMIZE
    if Assigned(AQuery.OptimizeClause) then
      TranslateOptimize(AQuery.OptimizeClause);

    // 8. RETURN
    if Assigned(AQuery.ReturnClause) then
      TranslateReturn(AQuery.ReturnClause);

    // 9. LIMIT
    if AQuery.Limit > 0 then
      FRequest.Limit := AQuery.Limit;

    // 10. OFFSET — paginacion
    FRequest.Offset := AQuery.Offset;

    // 11. DISTINCT ON
    if Assigned(AQuery.DistinctClause) then
      TranslateDistinct(AQuery.DistinctClause);

    // 12. ORDER BY
    if Assigned(AQuery.OrderByClause) then
      TranslateOrderBy(AQuery.OrderByClause);

    // 13. EXPLAIN
    FRequest.Explain := AQuery.Explain;

    Result := FRequest;
    FRequest := nil; // Cedemos la propiedad del objeto al llamador
  except
    on E: Exception do
    begin
      FRequest.Free;
      raise EVGQLTranslationError.CreateFmt('Error de compilacion VGQL: %s', [E.Message]);
    end;
  end;
end;

procedure TVGQLCompiler.TranslateMatch(AClause: TMatchClause);
begin
  FRequest.Entity := AClause.EntityName;
  FRequest.ALabel := AClause.Alias;
end;

procedure TVGQLCompiler.TranslateSearch(AClause: TSearchClause);
begin
  FRequest.Query := AClause.QueryText;
end;

procedure TVGQLCompiler.TranslateUsing(AClause: TUsingClause);
var
  TotalWeight: Double;
begin
  // 1. Mapeo de Modos y Normalizacion de Pesos
  case AClause.Mode of
    TSearchMode.smHybrid:
    begin
      FRequest.Mode := smHybrid;

      // En modo hibrido, nos aseguramos de que los pesos sumen exactamente 1.0
      TotalWeight := AClause.SemanticWeight + AClause.LexicalWeight;

      if TotalWeight > 0 then
      begin
        FRequest.WeightSemantic := AClause.SemanticWeight / TotalWeight;
        FRequest.WeightLexical := AClause.LexicalWeight / TotalWeight;
      end
      else
      begin
        // Fallback si los pesos son cero
        FRequest.WeightSemantic := 0.5;
        FRequest.WeightLexical := 0.5;
      end;
    end;

    TSearchMode.smEmbeddings:
    begin
      FRequest.Mode := smEmbeddings;
      // Si solo usamos embeddings, el peso semantico DEBE ser 100%
      FRequest.WeightSemantic := 1.0;
      FRequest.WeightLexical := 0.0;
    end;

    TSearchMode.smBM25:
    begin
      FRequest.Mode := TSearchMode.smBM25;
      // Si solo usamos BM25, el peso lexico DEBE ser 100%
      FRequest.WeightSemantic := 0.0;
      FRequest.WeightLexical := 1.0;
    end;
  end;

  // 2. Fusion (Solo relevante si el modo es Hybrid)
  if AClause.Fusion <> '' then
  begin
    if SameText(AClause.Fusion, 'RRF') then
      FRequest.Fusion := fmRRF
    else if SameText(AClause.Fusion, 'WEIGHTED') then
      FRequest.Fusion := fmWeighted
    else
      raise EVGQLTranslationError.CreateFmt('Modo de fusion desconocido: %s', [AClause.Fusion]);
  end;

  // 3. Language — sobrescribe el idioma del motor BM25 cuando se especifica en USING
  if AClause.Language <> '' then
    FRequest.Language := AClause.Language;
end;

procedure TVGQLCompiler.TranslateWhere(AClause: TWhereClause);
begin
  if Assigned(AClause.Expression) then
  begin
    FRequest.Filter.Clear;
    FRequest.Filter.LogicalOp := loAnd; // Raiz por defecto AND
    BuildFilter(AClause.Expression, FRequest.Filter);
  end;
end;

{ --- MOTOR DE COMPILACION DE EXPRESIONES --- }

function TVGQLCompiler.GetLiteralValue(AExpr: TExpression): Variant;
var
  Lit: TLiteralExpression;
begin
  if not(AExpr is TLiteralExpression) then
    raise EVGQLTranslationError.Create('Se esperaba un valor literal (String, Number o Boolean)');

  Lit := TLiteralExpression(AExpr);
  case Lit.LiteralType of
    ltString:
      Result := Lit.Value;
    ltNumber:
      Result := StrToFloatDef(Lit.Value.Replace(',', '.'), 0);
    ltBoolean:
      Result := SameText(Lit.Value, 'TRUE');
    ltNull:
      Result := Null;
  else
    Result := Unassigned;
  end;
end;

procedure TVGQLCompiler.BuildFilter(AExpr: TExpression; ACriteria: TAiFilterCriteria);
var
  Bin: TBinaryExpression;
  Bet: TBetweenExpression;
  InExp: TInExpression;
  FieldName: string;
  VArray: Variant;
  i: Integer;
begin
  if not Assigned(AExpr) then
    Exit;

  // 1. GRUPOS LOGICOS (AND / OR)
  if (AExpr is TBinaryExpression) and (TBinaryExpression(AExpr).Operator in [boAnd, boOr]) then
  begin
    Bin := TBinaryExpression(AExpr);

    // Si el operador del nodo coincide con el nivel actual, seguimos en la misma lista
    // Si cambia (ej: un OR dentro de un AND), creamos un subgrupo recursivo
    if ((Bin.Operator = boAnd) and (ACriteria.LogicalOp = loAnd)) or ((Bin.Operator = boOr) and (ACriteria.LogicalOp = loOr)) then
    begin
      BuildFilter(Bin.Left, ACriteria);
      BuildFilter(Bin.Right, ACriteria);
    end
    else
    begin
      // Cambio de logica detectado -> Crear Nodo Grupo
      if Bin.Operator = boAnd then
        BuildFilter(Bin, ACriteria.AddGroup(loAnd))
      else
        BuildFilter(Bin, ACriteria.AddGroup(loOr));
    end;
  end

  // 2. COMPARACION ENTRE (BETWEEN)
  else if AExpr is TBetweenExpression then
  begin
    Bet := TBetweenExpression(AExpr);
    if not(Bet.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create('BETWEEN requiere un identificador de metadato');

    ACriteria.AddBetween(TIdentifierExpression(Bet.Left).Name, GetLiteralValue(Bet.Min), GetLiteralValue(Bet.Max));
  end

  // 3. COMPARACION EN LISTA (IN)
  else if AExpr is TInExpression then
  begin
    InExp := TInExpression(AExpr);
    if not(InExp.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create('IN requiere un identificador de metadato');

    FieldName := TIdentifierExpression(InExp.Left).Name;
    VArray := VarArrayCreate([0, InExp.Values.Count - 1], varVariant);
    for i := 0 to InExp.Values.Count - 1 do
      VArray[i] := GetLiteralValue(InExp.Values[i]);

    if InExp.IsNot then
      ACriteria.Add(FieldName, foNotIn, VArray)
    else
      ACriteria.AddIn(FieldName, VArray);
  end

  // 4. COMPARACIONES BINARIAS ESTANDAR
  else if AExpr is TBinaryExpression then
  begin
    Bin := TBinaryExpression(AExpr);
    if not(Bin.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create('El lado izquierdo de la comparacion debe ser un campo');

    FieldName := TIdentifierExpression(Bin.Left).Name;

    case Bin.Operator of
      boEqual:
        ACriteria.AddEqual(FieldName, GetLiteralValue(Bin.Right));
      boNotEqual:
        ACriteria.Add(FieldName, foNotEqual, GetLiteralValue(Bin.Right));
      boGreater:
        ACriteria.AddGreater(FieldName, GetLiteralValue(Bin.Right));
      boGreaterEqual:
        ACriteria.Add(FieldName, foGreaterOrEqual, GetLiteralValue(Bin.Right));
      boLess:
        ACriteria.AddLess(FieldName, GetLiteralValue(Bin.Right));
      boLessEqual:
        ACriteria.Add(FieldName, foLessOrEqual, GetLiteralValue(Bin.Right));
      boContains:
        ACriteria.Add(FieldName, foContains, GetLiteralValue(Bin.Right));
      boStartsWith:
        ACriteria.Add(FieldName, foStartsWith, GetLiteralValue(Bin.Right));
      boEndsWith:
        ACriteria.Add(FieldName, foEndsWith, GetLiteralValue(Bin.Right));
      boLike:
        ACriteria.Add(FieldName, foLike, GetLiteralValue(Bin.Right));
      boILike:
        ACriteria.Add(FieldName, foILike, GetLiteralValue(Bin.Right));
      boIsNull:
        ACriteria.Add(FieldName, foIsNull, Null);
      boIsNotNull:
        ACriteria.Add(FieldName, foIsNotNull, Null);
    end;
  end;
end;

procedure TVGQLCompiler.TranslateRerank(AClause: TRerankClause);
begin
  FRequest.RerankQuery := AClause.Query;
  FRequest.RerankRegenerate := AClause.Regenerate;
end;

procedure TVGQLCompiler.TranslateThreshold(AClause: TThresholdClause);
begin
  case AClause.Scope of
    tsGlobal:
      FRequest.MinGlobal := AClause.Value;
    tsSemantic:
      FRequest.MinSemantic := AClause.Value;
    tsLexical:
      FRequest.MinLexical := AClause.Value;
  end;
end;

procedure TVGQLCompiler.TranslateOptimize(AClause: TOptimizeClause);
begin
  case AClause.Mode of
    omReorderABC:
    begin
      FRequest.UseReorderABC := True;
      FRequest.UseMMR := False;
    end;
    omMMR:
    begin
      FRequest.UseMMR := True;
      FRequest.UseReorderABC := False;
      FRequest.MmrLambda := AClause.Lambda;
    end;
  else
    FRequest.UseReorderABC := False;
    FRequest.UseMMR := False;
  end;
end;

procedure TVGQLCompiler.TranslateReturn(AClause: TReturnClause);
var
  Field: TReturnField;
begin
  for Field in AClause.Fields do
  begin
    case Field of
      rfMetadata:
        FRequest.IncludeMetadata := True;
      rfScore:
        FRequest.IncludeScore := True;
      // Nuevos campos de retorno v2
      rfId:
        FRequest.IncludeId := True;
      rfRank:
        FRequest.IncludeRank := True;
      rfModel:
        FRequest.IncludeModel := True;
    end;
  end;
end;

procedure TVGQLCompiler.TranslateDistinct(AClause: TDistinctClause);
begin
  FRequest.DistinctField := AClause.Field;
end;

procedure TVGQLCompiler.TranslateOrderBy(AClause: TOrderByClause);
var
  F: TOrderByField;
begin
  for F in AClause.Fields do
    FRequest.OrderByFields.Add(F);
end;

  //======================================
  //============ REQUEST =================
  //======================================

constructor TVGQLRequest.Create;
begin
  Filter := TAiFilterCriteria.Create; // Inicializamos el contenedor de criterios
  OrderByFields := TList<TOrderByField>.Create;
  Mode := smHybrid;
  WeightSemantic := 0.7;
  WeightLexical := 0.3;
  Limit := 10;
  Offset := 0;
  Fusion := fmWeighted;
  Language := 'spanish';
  MmrLambda := 0.7;
end;

destructor TVGQLRequest.Destroy;
begin
  Filter.Free;
  OrderByFields.Free;
  inherited;
end;



end.
