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
    tkTop,       // TOP — alias de LIMIT
    tkSemicolon  // ';' final opcional
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
    function PeekAt(AOffset: Integer): Char;
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
    FNegated: Boolean;
  public
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Operator: TBinaryOperator read FOperator write FOperator;
    property Right: TExpression read FRight write FRight;
    // NOT pospuesto: campo NOT LIKE / NOT ILIKE / NOT CONTAINS / NOT STARTS_WITH / NOT ENDS_WITH
    property Negated: Boolean read FNegated write FNegated;
  end;

  // NOT antepuesto: NOT condicion, NOT ( ... )
  TNotExpression = class(TExpression)
  private
    FOperand: TExpression;
  public
    destructor Destroy; override;
    property Operand: TExpression read FOperand write FOperand;
  end;


  TInExpression = class(TExpression)
  private
    FLeft: TExpression;
    FValues: TList<TExpression>;
    FIsNot: Boolean;
    FListOp: TBinaryOperator;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Values: TList<TExpression> read FValues;
    property IsNot: Boolean read FIsNot write FIsNot; // True si es "NOT IN" (o NOT EXISTS_ANY/ALL)
    // boIn (campo IN (...)), boExistsAny / boExistsAll (lista del campo con alguno / todos)
    property ListOp: TBinaryOperator read FListOp write FListOp;
  end;

  // Expresion para: campo BETWEEN min AND max
  TBetweenExpression = class(TExpression)
  private
    FLeft: TExpression;
    FMin: TExpression;
    FMax: TExpression;
    FNegated: Boolean;
  public
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Min: TExpression read FMin write FMin;
    property Max: TExpression read FMax write FMax;
    property Negated: Boolean read FNegated write FNegated; // NOT BETWEEN
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
    procedure Expect(AKind: TVGQLTokenKind);
    function Check(AKind: TVGQLTokenKind): Boolean;
    function Match(AKind: TVGQLTokenKind): Boolean;
    // Nombre de campo: una palabra (tambien las reservadas: language, order, top...) y .partes
    function ParseFieldName(const AErrorMsg: string): string;

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
    function ParseNot: TExpression;
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
    // Marcan si la clausula venia escrita en el VQL. Sin esto no se puede
    // distinguir un "FUSION WEIGHTED" explicito del valor por defecto, y quien
    // ejecuta el request no sabe si puede imponer su propio default.
    FusionSpecified: Boolean;
    LanguageSpecified: Boolean;
    // USING / OPTIMIZE escritos en el VQL. Sin USING la consulta usa las SearchOptions del
    // componente; con USING, ese modo vale solo para esta consulta (nunca se copia al componente)
    UsingSpecified: Boolean;
    OptimizeSpecified: Boolean;

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

uses
  uMakerAi.Localization;

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

function TVGQLLexer.PeekAt(AOffset: Integer): Char;
begin
  if FPos + AOffset > Length(FText) then
    Result := #0
  else
    Result := FText[FPos + AOffset];
end;

function TVGQLLexer.Next: Char;
begin
  Result := Peek;
  Inc(FPos);
end;

// Espacios y comentarios: -- y // hasta el fin de la linea, /* ... */ en bloque
procedure TVGQLLexer.SkipWhitespace;
var
  Start: Integer;
begin
  while not IsEOF do
  begin
    if Peek.IsWhiteSpace then
      Next
    else if ((Peek = '-') and (PeekAt(1) = '-')) or ((Peek = '/') and (PeekAt(1) = '/')) then
    begin
      while (not IsEOF) and (Peek <> #10) and (Peek <> #13) do
        Next;
    end
    else if (Peek = '/') and (PeekAt(1) = '*') then
    begin
      Start := FPos;
      Inc(FPos, 2);
      while (not IsEOF) and not((Peek = '*') and (PeekAt(1) = '/')) do
        Next;
      if IsEOF then
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.unclosed_comment'), [Start]);
      Inc(FPos, 2);
    end
    else
      Break;
  end;
end;

// Texto entre '...' o "...". La comilla se repite para escribirla: 'O''Brien'. La barra
// invertida es un caracter mas (rutas como 'C:\docs\nuevo' llegan tal cual; en LIKE sigue
// escapando % y _). Antes no habia forma de escribir una comilla y una cadena sin cerrar se
// aceptaba hasta el final de la consulta
function TVGQLLexer.ReadString: TVGQLToken;
var
  Quote, C: Char;
  Start: Integer;
  SB: TStringBuilder;
begin
  Start := FPos;
  Quote := Next;
  SB := TStringBuilder.Create;
  try
    while True do
    begin
      if IsEOF then
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.unclosed_string'), [Start]);
      C := Next;
      if C = Quote then
      begin
        if Peek = Quote then
        begin
          Next;
          SB.Append(Quote);
          Continue;
        end;
        Break;
      end;
      SB.Append(C);
    end;
    Result.Kind := tkString;
    Result.Text := SB.ToString;
    Result.Position := Start;
  finally
    SB.Free;
  end;
end;

function TVGQLLexer.ReadNumber: TVGQLToken;
var
  Start: Integer;
  HasDot: Boolean;
begin
  Start := FPos;
  HasDot := False;
  if Peek = '-' then
    Next; // negativo: -5, -0.25
  while not IsEOF do
  begin
    if Peek.IsDigit then
      Next
    else if (Peek = '.') and not HasDot and (FPos < Length(FText)) and FText[FPos + 1].IsDigit then
    begin
      HasDot := True;
      Next;
    end
    else
      Break;
  end;

  // Un numero pegado a otro punto, a letras o a '_' (1.2.3, 12abc) es un error. Antes '1.2.3'
  // pasaba como numero, el compilador lo volvia 0 y el filtro se aplicaba con ese 0
  if (not IsEOF) and (Peek.IsLetterOrDigit or (Peek = '_') or (Peek = '.')) then
    raise EVGQLParserError.CreateFmt(AiStr('rag.vql.bad_number'), [Copy(FText, Start, FPos - Start + 1), Start]);

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
      begin
        Next;
        Result.Kind := tkLParen;
        Result.Text := '(';
      end;
    ')':
      begin
        Next;
        Result.Kind := tkRParen;
        Result.Text := ')';
      end;
    ':':
      begin
        Next;
        Result.Kind := tkColon;
        Result.Text := ':';
      end;
    ',':
      begin
        Next;
        Result.Kind := tkComma;
        Result.Text := ',';
      end;
    '.':
      begin
        Next;
        Result.Kind := tkDot;
        Result.Text := '.';
      end;
    ';':
      begin
        Next;
        Result.Kind := tkSemicolon;
        Result.Text := ';';
      end;
    '=':
      begin
        Next;
        Result.Kind := tkEqual;
        Result.Text := '=';
      end;
    '!':
      begin
        if PeekAt(1) <> '=' then
          raise EVGQLParserError.CreateFmt(AiStr('rag.vql.lex_error'), [Peek, FPos]);
        Inc(FPos, 2);
        Result.Kind := tkNotEqual; // != como <>
        Result.Text := '!=';
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
    '''', '"':
      Exit(ReadString);
    '0' .. '9':
      Exit(ReadNumber);
    '-':
      if PeekAt(1).IsDigit then
        Exit(ReadNumber)
      else
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.lex_error'), [Peek, FPos]);
  else
    if Peek.IsLetter or (Peek = '_') then
      Exit(ReadIdentifier)
    else
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.lex_error'), [Peek, FPos]);
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

{ TNotExpression }

destructor TNotExpression.Destroy;
begin
  FOperand.Free;
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
  FListOp := boIn;
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

const
  // Palabras que valen como nombre de campo: los identificadores y las palabras de clausula
  // (WHERE language = 'es', ORDER BY top). No los operadores ni los literales
  VQL_WORD_TOKENS = [tkIdentifier, tkMatch, tkSearch, tkUsing, tkWhere, tkRerank, tkThreshold, tkOptimize, tkReturn,
    tkWith, tkLimit, tkHybrid, tkEmbeddings, tkBm25, tkWeights, tkLanguage, tkFusion, tkRrf, tkWeighted, tkRegenerate,
    tkGlobal, tkSemantic, tkLexical, tkReorder, tkAbc, tkText, tkMetadata, tkScore, tkOffset, tkDistinct, tkOn,
    tkOrder, tkBy, tkAsc, tkDesc, tkExplain, tkId, tkRank, tkMmr, tkModel, tkTop];

// Como se nombra un token en un mensaje de error
function VqlTokenDisplay(AKind: TVGQLTokenKind): string;
begin
  case AKind of
    tkEOF:
      Result := AiStr('rag.vql.end_of_query');
    tkIdentifier:
      Result := AiStr('rag.vql.a_name');
    tkString:
      Result := AiStr('rag.vql.a_string');
    tkNumber:
      Result := AiStr('rag.vql.a_number');
    tkLParen:
      Result := '"("';
    tkRParen:
      Result := '")"';
    tkColon:
      Result := '":"';
    tkComma:
      Result := '","';
    tkDot:
      Result := '"."';
    tkSemicolon:
      Result := '";"';
    tkEqual:
      Result := '"="';
    tkNotEqual:
      Result := '"<>"';
    tkGreater:
      Result := '">"';
    tkGreaterEqual:
      Result := '">="';
    tkLess:
      Result := '"<"';
    tkLessEqual:
      Result := '"<="';
    tkStartsWith:
      Result := 'STARTS_WITH';
    tkEndsWith:
      Result := 'ENDS_WITH';
    tkExistsAny:
      Result := 'EXISTS_ANY';
    tkExistsAll:
      Result := 'EXISTS_ALL';
  else
    Result := UpperCase(Copy(GetEnumName(TypeInfo(TVGQLTokenKind), Ord(AKind)), 3, MaxInt));
  end;
end;

procedure TVGQLParser.Expect(AKind: TVGQLTokenKind);
var
  Found: string;
begin
  if not Check(AKind) then
  begin
    if FCurrent.Kind in [tkEOF, tkString] then
      Found := VqlTokenDisplay(FCurrent.Kind)
    else
      Found := '"' + FCurrent.Text + '"';
    raise EVGQLParserError.CreateFmt(AiStr('rag.vql.expected'), [VqlTokenDisplay(AKind), FCurrent.Position, Found]);
  end;
  Next;
end;

function TVGQLParser.ParseFieldName(const AErrorMsg: string): string;
begin
  if not(FCurrent.Kind in VQL_WORD_TOKENS) then
    raise EVGQLParserError.Create(AErrorMsg);
  Result := FCurrent.Text;
  Next;
  while Check(tkDot) do
  begin
    Next;
    if not(FCurrent.Kind in VQL_WORD_TOKENS) then
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.name_expected'), [FCurrent.Position]);
    Result := Result + '.' + FCurrent.Text;
    Next;
  end;
end;

{ ===== PARSER PRINCIPAL ===== }

// EXPLAIN y MATCH van primero y SEARCH es obligatorio; las demas clausulas van en cualquier
// orden y una vez cada una (antes el orden era fijo: un RETURN antes del ORDER BY, lo que un
// modelo escribe a menudo, era error). ';' final opcional
function TVGQLParser.Parse: TVGQLQuery;
var
  N: Integer;
  LimitSeen, OffsetSeen: Boolean;

  procedure CheckOnce(AAlreadySeen: Boolean);
  begin
    if AAlreadySeen then
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.duplicate_clause'), [UpperCase(FCurrent.Text), FCurrent.Position]);
  end;

begin
  Result := TVGQLQuery.Create;
  LimitSeen := False;
  OffsetSeen := False;
  try
    if Match(tkExplain) then
      Result.Explain := True;

    if Check(tkMatch) then
      Result.Match := ParseMatch;

    if Check(tkSearch) then
      Result.Search := ParseSearch
    else
      raise EVGQLParserError.Create(AiStr('rag.vql.search_required'));

    while not(FCurrent.Kind in [tkEOF, tkSemicolon]) do
      case FCurrent.Kind of
        tkUsing:
          begin
            CheckOnce(Assigned(Result.UsingClause));
            Result.UsingClause := ParseUsing;
          end;
        tkWhere:
          begin
            CheckOnce(Assigned(Result.WhereClause));
            Result.WhereClause := ParseWhere;
          end;
        tkRerank:
          begin
            CheckOnce(Assigned(Result.RerankClause));
            Result.RerankClause := ParseRerank;
          end;
        tkThreshold:
          begin
            CheckOnce(Assigned(Result.ThresholdClause));
            Result.ThresholdClause := ParseThreshold;
          end;
        tkOptimize:
          begin
            CheckOnce(Assigned(Result.OptimizeClause));
            Result.OptimizeClause := ParseOptimize;
          end;
        tkDistinct:
          begin
            CheckOnce(Assigned(Result.DistinctClause));
            Result.DistinctClause := ParseDistinct;
          end;
        tkOrder:
          begin
            CheckOnce(Assigned(Result.OrderByClause));
            Result.OrderByClause := ParseOrderBy;
          end;
        tkReturn:
          begin
            CheckOnce(Assigned(Result.ReturnClause));
            Result.ReturnClause := ParseReturn;
          end;
        tkLimit, tkTop:
          begin
            // LIMIT n o TOP n: entero mayor que 0. Antes LIMIT 0 se ignoraba (y salian 10)
            CheckOnce(LimitSeen);
            LimitSeen := True;
            Next;
            if Check(tkNumber) and TryStrToInt(FCurrent.Text, N) and (N > 0) then
            begin
              Result.Limit := N;
              Next;
            end
            else
              raise EVGQLParserError.Create(AiStr('rag.vql.limit_number'));
          end;
        tkOffset:
          begin
            // OFFSET n: paginacion; entero, 0 o mas
            CheckOnce(OffsetSeen);
            OffsetSeen := True;
            Next;
            if Check(tkNumber) and TryStrToInt(FCurrent.Text, N) and (N >= 0) then
            begin
              Result.Offset := N;
              Next;
            end
            else
              raise EVGQLParserError.Create(AiStr('rag.vql.offset_number'));
          end;
      else
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.unexpected_clause'), [FCurrent.Text, FCurrent.Position]);
      end;

    Match(tkSemicolon);
    Expect(tkEOF);
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
    Expect(tkLParen);

    if Check(tkIdentifier) then
    begin
      Result.EntityName := FCurrent.Text; // Capturar texto ANTES de avanzar
      Next;
    end
    else
      Expect(tkIdentifier);

    // Alias opcional
    if Match(tkColon) then
    begin
      if Check(tkIdentifier) then
      begin
        Result.Alias := FCurrent.Text;
        Next;
      end
      else
        Expect(tkIdentifier);
    end;

    Expect(tkRParen);
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
      Expect(tkString);
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
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.using_mode'), [FCurrent.Position]);
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
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.fusion_mode'), [FCurrent.Position]);
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
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.language'), [FCurrent.Position]);
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

// Los niveles de expresion liberan lo ya armado si la parte siguiente falla (antes una
// consulta con error dejaba nodos del arbol vivos: memoria perdida en un servidor de vida larga)
function TVGQLParser.ParseOr: TExpression;
var
  BinExpr: TBinaryExpression;
begin
  Result := ParseAnd;
  while Check(tkOr) do
  begin
    BinExpr := TBinaryExpression.Create(FCurrent.Position);
    BinExpr.Left := Result;
    BinExpr.Operator := boOr;
    Result := BinExpr;
    Next;
    try
      BinExpr.Right := ParseAnd;
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TVGQLParser.ParseAnd: TExpression;
var
  BinExpr: TBinaryExpression;
begin
  Result := ParseNot;
  while Check(tkAnd) do
  begin
    BinExpr := TBinaryExpression.Create(FCurrent.Position);
    BinExpr.Left := Result;
    BinExpr.Operator := boAnd;
    Result := BinExpr;
    Next;
    try
      BinExpr.Right := ParseNot;
    except
      Result.Free;
      raise;
    end;
  end;
end;

// NOT antepuesto (NOT condicion, NOT ( ... )): precedencia entre AND y la comparacion
function TVGQLParser.ParseNot: TExpression;
var
  NotExpr: TNotExpression;
begin
  if Check(tkNot) then
  begin
    NotExpr := TNotExpression.Create(FCurrent.Position);
    Next;
    try
      NotExpr.Operand := ParseNot;
    except
      NotExpr.Free;
      raise;
    end;
    Result := NotExpr;
  end
  else
    Result := ParseComparison;
end;

function TVGQLParser.ParseComparison: TExpression;
var
  Left: TExpression;
  InExpr: TInExpression;
  BetExpr: TBetweenExpression;
  BinExpr: TBinaryExpression;
  Op: TBinaryOperator;
  IsNegated: Boolean;
  NotPos: Integer;
begin
  Left := ParsePrimary;

  // 1. IS [NOT] NULL
  if Check(tkIs) then
  begin
    BinExpr := TBinaryExpression.Create(FCurrent.Position);
    BinExpr.Left := Left;
    try
      Next;
      if Match(tkNot) then
        BinExpr.Operator := boIsNotNull
      else
        BinExpr.Operator := boIsNull;
      Expect(tkNull);
    except
      BinExpr.Free;
      raise;
    end;
    Exit(BinExpr);
  end;

  // 2. NOT pospuesto: NOT IN, NOT LIKE, NOT ILIKE, NOT BETWEEN, NOT CONTAINS, NOT STARTS_WITH,
  // NOT ENDS_WITH, NOT EXISTS_ANY, NOT EXISTS_ALL
  IsNegated := False;
  if Check(tkNot) then
  begin
    NotPos := FCurrent.Position;
    Next;
    if not(FCurrent.Kind in [tkIn, tkBetween, tkLike, tkILike, tkContains, tkStartsWith, tkEndsWith, tkExistsAny, tkExistsAll]) then
    begin
      Left.Free;
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.not_operator'), [NotPos]);
    end;
    IsNegated := True;
  end;

  // 3. Listas: IN (...), EXISTS_ANY (...) y EXISTS_ALL (...) (campo con alguno / todos los valores)
  if FCurrent.Kind in [tkIn, tkExistsAny, tkExistsAll] then
  begin
    InExpr := TInExpression.Create(FCurrent.Position);
    InExpr.Left := Left;
    try
      case FCurrent.Kind of
        tkExistsAny:
          InExpr.ListOp := boExistsAny;
        tkExistsAll:
          InExpr.ListOp := boExistsAll;
      else
        InExpr.ListOp := boIn;
      end;
      InExpr.IsNot := IsNegated;
      Next;
      Expect(tkLParen);
      repeat
        InExpr.Values.Add(ParsePrimary);
      until not Match(tkComma);
      Expect(tkRParen);
    except
      InExpr.Free;
      raise;
    end;
    Exit(InExpr);
  end;

  // 4. BETWEEN min AND max
  if Check(tkBetween) then
  begin
    BetExpr := TBetweenExpression.Create(FCurrent.Position);
    BetExpr.Left := Left;
    BetExpr.Negated := IsNegated;
    try
      Next;
      BetExpr.Min := ParsePrimary;
      Expect(tkAnd);
      BetExpr.Max := ParsePrimary;
    except
      BetExpr.Free;
      raise;
    end;
    Exit(BetExpr);
  end;

  // 5. Operadores binarios y de texto
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
    else
      Op := boEndsWith;
    end;
    BinExpr := TBinaryExpression.Create(FCurrent.Position);
    BinExpr.Left := Left;
    BinExpr.Operator := Op;
    BinExpr.Negated := IsNegated;
    try
      Next;
      BinExpr.Right := ParsePrimary;
    except
      BinExpr.Free;
      raise;
    end;
    Exit(BinExpr);
  end;

  Result := Left;
end;

function TVGQLParser.ParsePrimary: TExpression;
var
  LitExpr: TLiteralExpression;
  IdExpr: TIdentifierExpression;
  BinExpr: TBinaryExpression;
  P: Integer;
begin
  // EXISTS(campo): la clave esta, aunque su valor sea null
  if Check(tkExists) then
  begin
    P := FCurrent.Position;
    Next;
    IdExpr := TIdentifierExpression.Create(P);
    try
      Expect(tkLParen);
      IdExpr.Name := ParseFieldName(AiFmt('rag.vql.name_expected', [FCurrent.Position]));
      Expect(tkRParen);
    except
      IdExpr.Free;
      raise;
    end;
    BinExpr := TBinaryExpression.Create(P);
    BinExpr.Left := IdExpr;
    BinExpr.Operator := boExists;
    Exit(BinExpr);
  end;

  // Nombre de campo, tambien con palabras reservadas (language, order...) y con puntos
  if FCurrent.Kind in VQL_WORD_TOKENS then
  begin
    IdExpr := TIdentifierExpression.Create(FCurrent.Position);
    try
      IdExpr.Name := ParseFieldName(AiFmt('rag.vql.name_expected', [FCurrent.Position]));
    except
      IdExpr.Free;
      raise;
    end;
    Exit(IdExpr);
  end;

  case FCurrent.Kind of
    tkString, tkNumber, tkBoolean:
      begin
        LitExpr := TLiteralExpression.Create(FCurrent.Position);
        case FCurrent.Kind of
          tkString:
            LitExpr.LiteralType := ltString;
          tkNumber:
            LitExpr.LiteralType := ltNumber;
        else
          LitExpr.LiteralType := ltBoolean;
        end;
        LitExpr.Value := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkNull:
      begin
        LitExpr := TLiteralExpression.Create(FCurrent.Position);
        LitExpr.LiteralType := ltNull;
        LitExpr.Value := 'NULL';
        Next;
        Result := LitExpr;
      end;

    tkLParen:
      begin
        Next; // consumir (
        Result := ParseExpression;
        try
          Expect(tkRParen);
        except
          Result.Free;
          raise;
        end;
      end;
  else
    raise EVGQLParserError.CreateFmt(AiStr('rag.vql.invalid_expr'), [FCurrent.Position, FCurrent.Text]);
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
      Expect(tkString);

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
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.threshold_scope'), [FCurrent.Position]);
    end;
    Next; // Consumir el Scope (ej: GLOBAL)

    // 2. Leer el numero ANTES de avanzar
    if Check(tkNumber) then
    begin
      Result.Value := StrToFloat(FCurrent.Text, TFormatSettings.Invariant);
      Next; // Ahora si avanzamos al siguiente token
    end
    else
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.threshold_number'), [FCurrent.Position]);

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
        Expect(tkRParen);
      end;
    end
    // REORDER ABC — optimizacion de ventana de contexto LLM
    else if Match(tkReorder) then
    begin
      Expect(tkAbc);
      Result.Mode := omReorderABC;
    end
    else
      raise EVGQLParserError.CreateFmt(AiStr('rag.vql.optimize_mode'), [FCurrent.Position]);

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
        raise EVGQLParserError.CreateFmt(AiStr('rag.vql.return_invalid'), [FCurrent.Position]);
      end;
      Next;

      if not Match(tkComma) then
        Break;
    until False;

    if Result.Fields.Count = 0 then
      raise EVGQLParserError.Create(AiStr('rag.vql.return_field'));
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
    Expect(tkOn);

    Result.Field := ParseFieldName(AiFmt('rag.vql.distinct_field', [FCurrent.Position]));
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
    Expect(tkBy);

    // Parsear lista de campos: campo1 [ASC|DESC], campo2 [ASC|DESC], ...
    repeat
      OBField.FieldName := ParseFieldName(AiFmt('rag.vql.order_field', [FCurrent.Position]));

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
      raise EVGQLParserError.Create(AiStr('rag.vql.order_required'));
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
    raise EVGQLTranslationError.Create(AiStr('rag.vql.null_ast'));

  FRequest := TVGQLRequest.Create;
  try
    // 1. MATCH (Entidad y Alias)
    if Assigned(AQuery.Match) then
      TranslateMatch(AQuery.Match);

    // 2. SEARCH (Texto de busqueda - OBLIGATORIO)
    if Assigned(AQuery.Search) then
      TranslateSearch(AQuery.Search)
    else
      raise EVGQLTranslationError.Create(AiStr('rag.vql.search_required'));

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
      raise EVGQLTranslationError.CreateFmt(AiStr('rag.vql.compile_error'), [E.Message]);
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
      FRequest.UsingSpecified := True;

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
      FRequest.UsingSpecified := True;
      // Si solo usamos embeddings, el peso semantico DEBE ser 100%
      FRequest.WeightSemantic := 1.0;
      FRequest.WeightLexical := 0.0;
    end;

    TSearchMode.smBM25:
    begin
      FRequest.Mode := TSearchMode.smBM25;
      FRequest.UsingSpecified := True;
      // Si solo usamos BM25, el peso lexico DEBE ser 100%
      FRequest.WeightSemantic := 0.0;
      FRequest.WeightLexical := 1.0;
    end;
  end;

  // 2. Fusion (Solo relevante si el modo es Hybrid)
  if AClause.Fusion <> '' then
  begin
    FRequest.FusionSpecified := True;
    if SameText(AClause.Fusion, 'RRF') then
      FRequest.Fusion := fmRRF
    else if SameText(AClause.Fusion, 'WEIGHTED') then
      FRequest.Fusion := fmWeighted
    else
      raise EVGQLTranslationError.CreateFmt(AiStr('rag.vql.unknown_fusion'), [AClause.Fusion]);
  end;

  // 3. Language — sobrescribe el idioma del motor BM25 cuando se especifica en USING
  if AClause.Language <> '' then
  begin
    FRequest.Language := AClause.Language;
    FRequest.LanguageSpecified := True;
  end;
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
    raise EVGQLTranslationError.Create(AiStr('rag.vql.literal_expected'));

  Lit := TLiteralExpression(AExpr);
  case Lit.LiteralType of
    ltString:
      Result := Lit.Value;
    ltNumber:
      // El lexer solo deja pasar digitos con un punto decimal. Antes StrToFloatDef usaba el
      // separador de Windows: con coma decimal, 42000.5 se volvia 0 y el filtro seguia con ese 0
      Result := StrToFloat(Lit.Value, TFormatSettings.Invariant);
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
  VArray, V: Variant;
  FOp: TFilterOperator;
  i: Integer;
begin
  if not Assigned(AExpr) then
    Exit;

  // 0. NOT ( ... ): subgrupo negado
  if AExpr is TNotExpression then
  begin
    BuildFilter(TNotExpression(AExpr).Operand, ACriteria.AddNotGroup(loAnd));
    Exit;
  end;

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
      raise EVGQLTranslationError.Create(AiStr('rag.vql.between_field'));

    if Bet.Negated then
      ACriteria.AddNot(TIdentifierExpression(Bet.Left).Name, foBetween, GetLiteralValue(Bet.Min), GetLiteralValue(Bet.Max))
    else
      ACriteria.AddBetween(TIdentifierExpression(Bet.Left).Name, GetLiteralValue(Bet.Min), GetLiteralValue(Bet.Max));
  end

  // 3. COMPARACION EN LISTA (IN)
  else if AExpr is TInExpression then
  begin
    InExp := TInExpression(AExpr);
    if not(InExp.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create(AiStr('rag.vql.in_field'));

    FieldName := TIdentifierExpression(InExp.Left).Name;
    VArray := VarArrayCreate([0, InExp.Values.Count - 1], varVariant);
    for i := 0 to InExp.Values.Count - 1 do
      VArray[i] := GetLiteralValue(InExp.Values[i]);

    case InExp.ListOp of
      boExistsAny:
        FOp := foExistsAny;
      boExistsAll:
        FOp := foExistsAll;
    else
      FOp := foIn;
    end;
    if InExp.IsNot and (FOp = foIn) then
      ACriteria.Add(FieldName, foNotIn, VArray)
    else if InExp.IsNot then
      ACriteria.AddNot(FieldName, FOp, VArray)
    else
      ACriteria.Add(FieldName, FOp, VArray);
  end

  // 4. COMPARACIONES BINARIAS ESTANDAR
  else if AExpr is TBinaryExpression then
  begin
    Bin := TBinaryExpression(AExpr);
    if not(Bin.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create(AiStr('rag.vql.compare_field'));

    FieldName := TIdentifierExpression(Bin.Left).Name;

    case Bin.Operator of
      boEqual:
        FOp := foEqual;
      boNotEqual:
        FOp := foNotEqual;
      boGreater:
        FOp := foGreater;
      boGreaterEqual:
        FOp := foGreaterOrEqual;
      boLess:
        FOp := foLess;
      boLessEqual:
        FOp := foLessOrEqual;
      boContains:
        FOp := foContains;
      boStartsWith:
        FOp := foStartsWith;
      boEndsWith:
        FOp := foEndsWith;
      boLike:
        FOp := foLike;
      boILike:
        FOp := foILike;
      boIsNull:
        FOp := foIsNull;
      boIsNotNull:
        FOp := foIsNotNull;
      boExists:
        FOp := foExists;
    else
      raise EVGQLTranslationError.Create(AiStr('rag.vql.condition_expected'));
    end;

    if FOp in [foIsNull, foIsNotNull, foExists] then
      V := Null
    else
      V := GetLiteralValue(Bin.Right);

    if Bin.Negated then
      ACriteria.AddNot(FieldName, FOp, V)
    else
      ACriteria.Add(FieldName, FOp, V);
  end

  // 5. Un campo o un valor solo no es una condicion. Antes "WHERE estado" se ignoraba en
  // silencio y la busqueda salia sin filtro
  else
    raise EVGQLTranslationError.Create(AiStr('rag.vql.condition_expected'));
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
  FRequest.OptimizeSpecified := True;
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
  FusionSpecified := False;
  Language := 'spanish';
  LanguageSpecified := False;
  MmrLambda := 0.7;
end;

destructor TVGQLRequest.Destroy;
begin
  Filter.Free;
  OrderByFields.Free;
  inherited;
end;



end.
