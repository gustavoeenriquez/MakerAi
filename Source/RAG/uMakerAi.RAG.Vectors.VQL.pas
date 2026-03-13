// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.RAG.Vectors.VQL
// Lenguaje VQL: lexer, parser, AST y compilador para búsqueda vectorial.
unit uMakerAi.RAG.Vectors.VQL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Variants, TypInfo,
  fgl,
  uMakerAi.RAG.MetaData;

type
  // Tipos de token
  TVGQLTokenKind = (
    tkEOF,
    tkStartsWith, tkEndsWith, tkLike, tkILike, tkIn, tkBetween,
    tkIs, tkNot, tkExists, tkExistsAny, tkExistsAll,
    tkIdentifier, tkString, tkNumber, tkBoolean, tkNull,
    tkLParen, tkRParen, tkColon, tkComma, tkDot,
    tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkLess, tkLessEqual, tkContains,
    tkAnd, tkOr,
    tkMatch, tkSearch, tkUsing, tkWhere, tkRerank, tkThreshold, tkOptimize,
    tkReturn, tkWith, tkLimit, tkHybrid, tkEmbeddings, tkBm25, tkWeights,
    tkLanguage, tkFusion, tkRrf, tkWeighted, tkRegenerate, tkGlobal,
    tkSemantic, tkLexical, tkReorder, tkAbc, tkText, tkMetadata, tkScore);

  TVGQLToken = record
    Kind    : TVGQLTokenKind;
    Text    : string;
    Position: Integer;
  end;

  TVGQLLexer = class
  private
    FText: string;
    FPos : Integer;
    function IsEOF: Boolean;
    function Peek: Char;
    function Next: Char;
    procedure SkipWhitespace;
    function ReadIdentifier: TVGQLToken;
    function ReadString    : TVGQLToken;
    function ReadNumber    : TVGQLToken;
  public
    constructor Create(const AText: string);
    function NextToken: TVGQLToken;
  end;

  // =========================================================================
  // AST
  // =========================================================================

  TASTNode = class
  private
    FPosition: Integer;
  public
    constructor Create(APosition: Integer = 0); virtual;
    property Position: Integer read FPosition;
  end;

  TExpression = class(TASTNode);

  // Tipo para lista de expresiones (posesiva)
  TExpressionList = specialize TFPGObjectList<TExpression>;

  TLiteralType = (ltString, ltNumber, ltBoolean, ltNull);

  TLiteralExpression = class(TExpression)
  private
    FLiteralType: TLiteralType;
    FValue      : string;
  public
    property LiteralType: TLiteralType read FLiteralType write FLiteralType;
    property Value      : string       read FValue       write FValue;
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
    boLike, boILike,
    boIn, boNotIn,
    boBetween,
    boStartsWith, boEndsWith,
    boIsNull, boIsNotNull,
    boExists, boExistsAny, boExistsAll);

  TBinaryExpression = class(TExpression)
  private
    FLeft    : TExpression;
    FOperator: TBinaryOperator;
    FRight   : TExpression;
  public
    destructor Destroy; override;
    property Left   : TExpression     read FLeft     write FLeft;
    property BinOp  : TBinaryOperator read FOperator write FOperator;
    property Right  : TExpression     read FRight    write FRight;
  end;

  TInExpression = class(TExpression)
  private
    FLeft  : TExpression;
    FValues: TExpressionList;
    FIsNot : Boolean;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor  Destroy; override;
    property Left  : TExpression     read FLeft   write FLeft;
    property Values: TExpressionList read FValues;
    property IsNot : Boolean         read FIsNot  write FIsNot;
  end;

  TBetweenExpression = class(TExpression)
  private
    FLeft: TExpression;
    FMin : TExpression;
    FMax : TExpression;
  public
    destructor Destroy; override;
    property Left: TExpression read FLeft write FLeft;
    property Min : TExpression read FMin  write FMin;
    property Max : TExpression read FMax  write FMax;
  end;

  // ===== Cláusulas =====

  TMatchClause = class(TASTNode)
  private
    FEntityName: string;
    FAlias     : string;
  public
    property EntityName: string read FEntityName write FEntityName;
    property Alias     : string read FAlias      write FAlias;
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
    FMode          : TSearchMode;
    FSemanticWeight: Double;
    FLexicalWeight : Double;
    FFusion        : string;
  public
    constructor Create(APosition: Integer = 0); override;
    property Mode          : TSearchMode read FMode           write FMode;
    property SemanticWeight: Double      read FSemanticWeight  write FSemanticWeight;
    property LexicalWeight : Double      read FLexicalWeight   write FLexicalWeight;
    property Fusion        : string      read FFusion          write FFusion;
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
    FQuery     : string;
    FRegenerate: Boolean;
  public
    property Query     : string  read FQuery      write FQuery;
    property Regenerate: Boolean read FRegenerate write FRegenerate;
  end;

  TThresholdScope = (tsGlobal, tsSemantic, tsLexical);

  TThresholdClause = class(TASTNode)
  private
    FScope: TThresholdScope;
    FValue: Double;
  public
    property Scope: TThresholdScope read FScope write FScope;
    property Value: Double          read FValue write FValue;
  end;

  TOptimizeMode = (omNone, omReorderABC);

  TOptimizeClause = class(TASTNode)
  private
    FMode: TOptimizeMode;
  public
    property Mode: TOptimizeMode read FMode write FMode;
  end;

  TReturnField = (rfText, rfMetadata, rfScore);
  TReturnFieldList = specialize TFPGList<TReturnField>;

  TReturnClause = class(TASTNode)
  private
    FFields: TReturnFieldList;
  public
    constructor Create(APosition: Integer = 0); override;
    destructor  Destroy; override;
    property Fields: TReturnFieldList read FFields;
  end;

  // ===== Query principal =====

  TVGQLQuery = class(TASTNode)
  private
    FMatch    : TMatchClause;
    FSearch   : TSearchClause;
    FUsing    : TUsingClause;
    FWhere    : TWhereClause;
    FRerank   : TRerankClause;
    FThreshold: TThresholdClause;
    FOptimize : TOptimizeClause;
    FReturn   : TReturnClause;
    FLimit    : Integer;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;

    property Match          : TMatchClause     read FMatch     write FMatch;
    property Search         : TSearchClause    read FSearch    write FSearch;
    property UsingClause    : TUsingClause     read FUsing     write FUsing;
    property WhereClause    : TWhereClause     read FWhere     write FWhere;
    property RerankClause   : TRerankClause    read FRerank    write FRerank;
    property ThresholdClause: TThresholdClause read FThreshold write FThreshold;
    property OptimizeClause : TOptimizeClause  read FOptimize  write FOptimize;
    property ReturnClause   : TReturnClause    read FReturn    write FReturn;
    property Limit          : Integer          read FLimit     write FLimit;
  end;

  // =========================================================================
  // Parser
  // =========================================================================

  EVGQLParserError = class(Exception);

  TVGQLParser = class
  private
    FLexer  : TVGQLLexer;
    FCurrent: TVGQLToken;

    procedure Next;
    procedure Expect(AKind: TVGQLTokenKind; const AMsg: string = '');
    function  Check(AKind: TVGQLTokenKind): Boolean;
    function  MatchTok(AKind: TVGQLTokenKind): Boolean;

    function ParseMatch    : TMatchClause;
    function ParseSearch   : TSearchClause;
    function ParseUsing    : TUsingClause;
    function ParseWhere    : TWhereClause;
    function ParseRerank   : TRerankClause;
    function ParseThreshold: TThresholdClause;
    function ParseOptimize : TOptimizeClause;
    function ParseReturn   : TReturnClause;

    function ParseExpression: TExpression;
    function ParseOr        : TExpression;
    function ParseAnd       : TExpression;
    function ParseComparison: TExpression;
    function ParsePrimary   : TExpression;
  public
    constructor Create(const AText: string);
    destructor  Destroy; override;
    function Parse: TVGQLQuery;
  end;

  // =========================================================================
  // Request (resultado del compilador)
  // =========================================================================

  TVGQLRequest = class
  public
    Entity          : string;
    ALabel          : string;
    Query           : string;
    Mode            : TSearchMode;
    WeightSemantic  : Double;
    WeightLexical   : Double;
    Language        : string;
    Fusion          : TFusionMode;
    Filter          : TAiFilterCriteria;
    RerankQuery     : string;
    RerankRegenerate: Boolean;
    MinGlobal       : Double;
    MinSemantic     : Double;
    MinLexical      : Double;
    UseReorderABC   : Boolean;
    Limit           : Integer;
    IncludeMetadata : Boolean;
    IncludeScore    : Boolean;

    constructor Create;
    destructor  Destroy; override;
  end;

  // =========================================================================
  // Compilador AST → TVGQLRequest
  // =========================================================================

  EVGQLTranslationError = class(Exception);

  TVGQLCompiler = class
  private
    FRequest: TVGQLRequest;
    procedure TranslateMatch    (AClause: TMatchClause);
    procedure TranslateSearch   (AClause: TSearchClause);
    procedure TranslateUsing    (AClause: TUsingClause);
    procedure TranslateWhere    (AClause: TWhereClause);
    procedure TranslateRerank   (AClause: TRerankClause);
    procedure TranslateThreshold(AClause: TThresholdClause);
    procedure TranslateOptimize (AClause: TOptimizeClause);
    procedure TranslateReturn   (AClause: TReturnClause);

    procedure BuildFilter(AExpr: TExpression; ACriteria: TAiFilterCriteria);
    function  GetLiteralValue(AExpr: TExpression): Variant;
  public
    function Translate(AQuery: TVGQLQuery): TVGQLRequest;
  end;

implementation

// ============================================================================
// Helpers locales
// ============================================================================

function InvFS: TFormatSettings;
begin
  Result                  := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator:= #0;
end;

function IsWhiteSpace(C: Char): Boolean; inline;
begin
  Result := C in [' ', #9, #10, #11, #12, #13];
end;

function IsLetterOrDigit(C: Char): Boolean; inline;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

function IsLetter(C: Char): Boolean; inline;
begin
  Result := C in ['A'..'Z', 'a'..'z', '_'];
end;

function IsDigit(C: Char): Boolean; inline;
begin
  Result := C in ['0'..'9'];
end;

// ============================================================================
// TVGQLLexer
// ============================================================================

constructor TVGQLLexer.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FPos  := 1;
end;

function TVGQLLexer.IsEOF: Boolean;
begin
  Result := FPos > Length(FText);
end;

function TVGQLLexer.Peek: Char;
begin
  if IsEOF then Result := #0
  else          Result := FText[FPos];
end;

function TVGQLLexer.Next: Char;
begin
  Result := Peek;
  Inc(FPos);
end;

procedure TVGQLLexer.SkipWhitespace;
begin
  while (not IsEOF) and IsWhiteSpace(Peek) do Next;
end;

function TVGQLLexer.ReadString: TVGQLToken;
var
  Quote: Char;
  Start: Integer;
begin
  Quote := Next; // ' o "
  Start := FPos;
  while (not IsEOF) and (Peek <> Quote) do Next;
  Result.Kind     := tkString;
  Result.Text     := Copy(FText, Start, FPos - Start);
  Result.Position := Start - 1;
  if not IsEOF then Next; // cerrar comilla
end;

function TVGQLLexer.ReadNumber: TVGQLToken;
var
  Start: Integer;
begin
  Start := FPos;
  while (not IsEOF) and (IsDigit(Peek) or (Peek = '.')) do Next;
  Result.Kind     := tkNumber;
  Result.Text     := Copy(FText, Start, FPos - Start);
  Result.Position := Start;
end;

function TVGQLLexer.ReadIdentifier: TVGQLToken;
var
  Start: Integer;
  S, U : string;
begin
  Start := FPos;
  while (not IsEOF) and IsLetterOrDigit(Peek) do Next;
  S := Copy(FText, Start, FPos - Start);
  U := UpperCase(S);

  Result.Text     := S;
  Result.Position := Start;

  // Cláusulas principales
  if      U = 'MATCH'      then Result.Kind := tkMatch
  else if U = 'SEARCH'     then Result.Kind := tkSearch
  else if U = 'USING'      then Result.Kind := tkUsing
  else if U = 'WHERE'      then Result.Kind := tkWhere
  else if U = 'RERANK'     then Result.Kind := tkRerank
  else if U = 'RETURN'     then Result.Kind := tkReturn
  else if U = 'WITH'       then Result.Kind := tkWith
  else if U = 'LIMIT'      then Result.Kind := tkLimit
  else if U = 'OPTIMIZE'   then Result.Kind := tkOptimize
  else if U = 'THRESHOLD'  then Result.Kind := tkThreshold
  // Operadores lógicos
  else if U = 'AND'        then Result.Kind := tkAnd
  else if U = 'OR'         then Result.Kind := tkOr
  else if U = 'NOT'        then Result.Kind := tkNot
  // Operadores de filtrado
  else if U = 'CONTAINS'   then Result.Kind := tkContains
  else if U = 'LIKE'       then Result.Kind := tkLike
  else if U = 'ILIKE'      then Result.Kind := tkILike
  else if U = 'IN'         then Result.Kind := tkIn
  else if U = 'BETWEEN'    then Result.Kind := tkBetween
  else if U = 'STARTS_WITH' then Result.Kind := tkStartsWith
  else if U = 'ENDS_WITH'  then Result.Kind := tkEndsWith
  else if U = 'IS'         then Result.Kind := tkIs
  else if U = 'EXISTS'     then Result.Kind := tkExists
  else if U = 'EXISTS_ANY' then Result.Kind := tkExistsAny
  else if U = 'EXISTS_ALL' then Result.Kind := tkExistsAll
  // Configuración de búsqueda
  else if U = 'HYBRID'     then Result.Kind := tkHybrid
  else if U = 'EMBEDDINGS' then Result.Kind := tkEmbeddings
  else if U = 'BM25'       then Result.Kind := tkBm25
  else if U = 'WEIGHTS'    then Result.Kind := tkWeights
  else if U = 'LANGUAGE'   then Result.Kind := tkLanguage
  else if U = 'FUSION'     then Result.Kind := tkFusion
  else if U = 'RRF'        then Result.Kind := tkRrf
  else if U = 'WEIGHTED'   then Result.Kind := tkWeighted
  // Modos y estrategias
  else if U = 'REGENERATE' then Result.Kind := tkRegenerate
  else if U = 'GLOBAL'     then Result.Kind := tkGlobal
  else if U = 'SEMANTIC'   then Result.Kind := tkSemantic
  else if U = 'LEXICAL'    then Result.Kind := tkLexical
  else if U = 'REORDER'    then Result.Kind := tkReorder
  else if U = 'ABC'        then Result.Kind := tkAbc
  // Campos de retorno
  else if U = 'TEXT'       then Result.Kind := tkText
  else if U = 'METADATA'   then Result.Kind := tkMetadata
  else if U = 'SCORE'      then Result.Kind := tkScore
  // Literales
  else if (U = 'TRUE') or (U = 'FALSE') then Result.Kind := tkBoolean
  else if U = 'NULL'       then Result.Kind := tkNull
  else                          Result.Kind := tkIdentifier;
end;

function TVGQLLexer.NextToken: TVGQLToken;
begin
  SkipWhitespace;
  Result.Position := FPos;
  Result.Text     := '';

  if IsEOF then begin
    Result.Kind := tkEOF;
    Exit;
  end;

  case Peek of
    '(': begin Next; Result.Kind := tkLParen;  Result.Text := '('; end;
    ')': begin Next; Result.Kind := tkRParen;  Result.Text := ')'; end;
    ':': begin Next; Result.Kind := tkColon;   Result.Text := ':'; end;
    ',': begin Next; Result.Kind := tkComma;   Result.Text := ','; end;
    '.': begin Next; Result.Kind := tkDot;     Result.Text := '.'; end;
    '=': begin Next; Result.Kind := tkEqual;   Result.Text := '='; end;
    '<': begin
           Next;
           if Peek = '>' then begin
             Next; Result.Kind := tkNotEqual;  Result.Text := '<>';
           end else if Peek = '=' then begin
             Next; Result.Kind := tkLessEqual; Result.Text := '<=';
           end else begin
             Result.Kind := tkLess; Result.Text := '<';
           end;
         end;
    '>': begin
           Next;
           if Peek = '=' then begin
             Next; Result.Kind := tkGreaterEqual; Result.Text := '>=';
           end else begin
             Result.Kind := tkGreater; Result.Text := '>';
           end;
         end;
    '''', '"': Exit(ReadString);
    '0'..'9' : Exit(ReadNumber);
  else
    if IsLetter(Peek) then
      Exit(ReadIdentifier)
    else
      raise Exception.CreateFmt(
        'Error léxico: carácter inesperado "%s" en posición %d', [Peek, FPos]);
  end;
end;

// ============================================================================
// AST — TASTNode
// ============================================================================

constructor TASTNode.Create(APosition: Integer);
begin
  inherited Create;
  FPosition := APosition;
end;

// ============================================================================
// TBinaryExpression
// ============================================================================

destructor TBinaryExpression.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

// ============================================================================
// TInExpression
// ============================================================================

constructor TInExpression.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FValues := TExpressionList.Create(True); // FreeObjects=True
  FIsNot  := False;
end;

destructor TInExpression.Destroy;
begin
  FLeft.Free;
  FValues.Free; // auto-frees contained TExpression objects
  inherited;
end;

// ============================================================================
// TBetweenExpression
// ============================================================================

destructor TBetweenExpression.Destroy;
begin
  FLeft.Free;
  FMin.Free;
  FMax.Free;
  inherited;
end;

// ============================================================================
// TUsingClause
// ============================================================================

constructor TUsingClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FSemanticWeight := 0.5;
  FLexicalWeight  := 0.5;
  FFusion         := '';
end;

// ============================================================================
// TWhereClause
// ============================================================================

destructor TWhereClause.Destroy;
begin
  FExpression.Free;
  inherited;
end;

// ============================================================================
// TReturnClause
// ============================================================================

constructor TReturnClause.Create(APosition: Integer);
begin
  inherited Create(APosition);
  FFields := TReturnFieldList.Create;
end;

destructor TReturnClause.Destroy;
begin
  FFields.Free;
  inherited;
end;

// ============================================================================
// TVGQLQuery
// ============================================================================

constructor TVGQLQuery.Create;
begin
  inherited Create(0);
  FMatch     := nil; FSearch   := nil; FUsing  := nil;
  FWhere     := nil; FRerank   := nil; FThreshold := nil;
  FOptimize  := nil; FReturn   := nil;
  FLimit     := -1;
end;

destructor TVGQLQuery.Destroy;
begin
  FMatch.Free; FSearch.Free;  FUsing.Free;
  FWhere.Free; FRerank.Free;  FThreshold.Free;
  FOptimize.Free; FReturn.Free;
  inherited;
end;

// ============================================================================
// TVGQLParser
// ============================================================================

constructor TVGQLParser.Create(const AText: string);
begin
  inherited Create;
  FLexer := TVGQLLexer.Create(AText);
  Next;
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

function TVGQLParser.MatchTok(AKind: TVGQLTokenKind): Boolean;
begin
  Result := Check(AKind);
  if Result then Next;
end;

procedure TVGQLParser.Expect(AKind: TVGQLTokenKind; const AMsg: string);
var
  ErrorMsg: string;
begin
  if not Check(AKind) then begin
    if AMsg <> '' then
      ErrorMsg := Format('Pos %d: se esperaba %s (%s)',
          [FCurrent.Position, GetEnumName(TypeInfo(TVGQLTokenKind), Ord(AKind)), AMsg])
    else
      ErrorMsg := Format('Pos %d: se esperaba %s',
          [FCurrent.Position, GetEnumName(TypeInfo(TVGQLTokenKind), Ord(AKind))]);
    raise EVGQLParserError.Create(ErrorMsg);
  end;
  Next;
end;

// ============================================================================
// Parse
// ============================================================================

function TVGQLParser.Parse: TVGQLQuery;
begin
  Result := TVGQLQuery.Create;
  try
    if Check(tkMatch)  then Result.Match        := ParseMatch;
    if Check(tkSearch) then Result.Search       := ParseSearch
    else raise EVGQLParserError.Create('Se requiere cláusula SEARCH');
    if Check(tkUsing)     then Result.UsingClause     := ParseUsing;
    if Check(tkWhere)     then Result.WhereClause     := ParseWhere;
    if Check(tkRerank)    then Result.RerankClause    := ParseRerank;
    if Check(tkThreshold) then Result.ThresholdClause := ParseThreshold;
    if Check(tkOptimize)  then Result.OptimizeClause  := ParseOptimize;
    if Check(tkReturn)    then Result.ReturnClause    := ParseReturn;
    if MatchTok(tkLimit) then begin
      if Check(tkNumber) then begin
        Result.Limit := StrToInt(FCurrent.Text);
        Next;
      end else
        raise EVGQLParserError.Create('Se requiere un valor numérico para LIMIT');
    end;
    Expect(tkEOF, 'fin de consulta');
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// ParseMatch
// ============================================================================

function TVGQLParser.ParseMatch: TMatchClause;
begin
  Result := TMatchClause.Create(FCurrent.Position);
  try
    Next; // consumir MATCH
    Expect(tkLParen, '(');
    if Check(tkIdentifier) then begin
      Result.EntityName := FCurrent.Text;
      Next;
    end else
      Expect(tkIdentifier, 'nombre de entidad');
    if MatchTok(tkColon) then begin
      if Check(tkIdentifier) then begin
        Result.Alias := FCurrent.Text;
        Next;
      end else
        Expect(tkIdentifier, 'alias');
    end;
    Expect(tkRParen, ')');
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// ParseSearch
// ============================================================================

function TVGQLParser.ParseSearch: TSearchClause;
begin
  Result := TSearchClause.Create(FCurrent.Position);
  try
    Next; // consumir SEARCH
    if Check(tkString) then begin
      Result.QueryText := FCurrent.Text;
      Next;
    end else
      Expect(tkString, 'texto de búsqueda');
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// ParseUsing
// ============================================================================

function TVGQLParser.ParseUsing: TUsingClause;
var
  FS: TFormatSettings;
begin
  FS     := InvFS;
  Result := TUsingClause.Create(FCurrent.Position);
  try
    Next; // consumir USING
    case FCurrent.Kind of
      tkHybrid    : Result.Mode := smHybrid;
      tkEmbeddings: Result.Mode := smEmbeddings;
      tkBm25      : Result.Mode := smBM25;
    else
      raise EVGQLParserError.CreateFmt(
          'Modo USING inválido en %d', [FCurrent.Position]);
    end;
    Next;

    if MatchTok(tkWith) then begin
      Expect(tkWeights);
      Expect(tkLParen);
      if Check(tkNumber) then begin
        Result.SemanticWeight := StrToFloat(FCurrent.Text, FS);
        Next;
      end;
      Expect(tkComma);
      if Check(tkNumber) then begin
        Result.LexicalWeight := StrToFloat(FCurrent.Text, FS);
        Next;
      end;
      Expect(tkRParen);
    end;

    if MatchTok(tkFusion) then begin
      if Check(tkIdentifier) or Check(tkRrf) or Check(tkWeighted) then begin
        Result.Fusion := FCurrent.Text;
        Next;
      end else
        raise EVGQLParserError.CreateFmt(
            'Se esperaba un modo de fusión válido (RRF, WEIGHTED) en %d',
            [FCurrent.Position]);
    end;
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// ParseWhere + Expression parser
// ============================================================================

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

function TVGQLParser.ParseExpression: TExpression;
begin
  Result := ParseOr;
end;

function TVGQLParser.ParseOr: TExpression;
var
  Left, Right: TExpression;
  BinExpr    : TBinaryExpression;
begin
  Left   := ParseAnd;
  Result := Left;
  while MatchTok(tkOr) do begin
    Right   := ParseAnd;
    BinExpr := TBinaryExpression.Create;
    BinExpr.Left  := Left;
    BinExpr.BinOp := boOr;
    BinExpr.Right := Right;
    Result := BinExpr;
    Left   := Result;
  end;
end;

function TVGQLParser.ParseAnd: TExpression;
var
  Left, Right: TExpression;
  BinExpr    : TBinaryExpression;
begin
  Left   := ParseComparison;
  Result := Left;
  while MatchTok(tkAnd) do begin
    Right   := ParseComparison;
    BinExpr := TBinaryExpression.Create;
    BinExpr.Left  := Left;
    BinExpr.BinOp := boAnd;
    BinExpr.Right := Right;
    Result := BinExpr;
    Left   := Result;
  end;
end;

function TVGQLParser.ParseComparison: TExpression;
var
  Left, Right: TExpression;
  InExpr     : TInExpression;
  BetExpr    : TBetweenExpression;
  BinExpr    : TBinaryExpression;
  Op         : TBinaryOperator;
  IsNegated  : Boolean;
begin
  Left   := ParsePrimary;
  Result := Left;

  // IS [NOT] NULL
  if MatchTok(tkIs) then begin
    IsNegated := MatchTok(tkNot);
    Expect(tkNull, 'NULL después de IS');
    BinExpr          := TBinaryExpression.Create;
    BinExpr.Left  := Left;
    if IsNegated then BinExpr.BinOp := boIsNotNull
    else              BinExpr.BinOp := boIsNull;
    BinExpr.Right := nil;
    Exit(BinExpr);
  end;

  // [NOT] IN
  IsNegated := MatchTok(tkNot);
  if MatchTok(tkIn) then begin
    InExpr        := TInExpression.Create(FCurrent.Position);
    InExpr.Left   := Left;
    InExpr.IsNot  := IsNegated;
    Expect(tkLParen, '(');
    repeat
      InExpr.Values.Add(ParsePrimary);
      if not MatchTok(tkComma) then Break;
    until False;
    Expect(tkRParen, ')');
    Exit(InExpr);
  end;

  // BETWEEN min AND max
  if MatchTok(tkBetween) then begin
    BetExpr      := TBetweenExpression.Create;
    BetExpr.Left := Left;
    BetExpr.Min  := ParsePrimary;
    Expect(tkAnd, 'AND entre valores de BETWEEN');
    BetExpr.Max  := ParsePrimary;
    Exit(BetExpr);
  end;

  // Operadores binarios estándar
  if FCurrent.Kind in [tkEqual, tkNotEqual, tkGreater, tkGreaterEqual,
                       tkLess, tkLessEqual, tkContains, tkLike, tkILike,
                       tkStartsWith, tkEndsWith] then begin
    case FCurrent.Kind of
      tkEqual       : Op := boEqual;
      tkNotEqual    : Op := boNotEqual;
      tkGreater     : Op := boGreater;
      tkGreaterEqual: Op := boGreaterEqual;
      tkLess        : Op := boLess;
      tkLessEqual   : Op := boLessEqual;
      tkContains    : Op := boContains;
      tkLike        : Op := boLike;
      tkILike       : Op := boILike;
      tkStartsWith  : Op := boStartsWith;
      tkEndsWith    : Op := boEndsWith;
    else               Op := boEqual;
    end;
    Next;
    Right            := ParsePrimary;
    BinExpr          := TBinaryExpression.Create;
    BinExpr.Left  := Left;
    BinExpr.BinOp := Op;
    BinExpr.Right := Right;
    Result := BinExpr;
  end;
end;

function TVGQLParser.ParsePrimary: TExpression;
var
  LitExpr : TLiteralExpression;
  IdExpr  : TIdentifierExpression;
  FullName: string;
begin
  case FCurrent.Kind of
    tkIdentifier, tkText, tkMetadata, tkScore:
      begin
        FullName := FCurrent.Text;
        Next;
        while MatchTok(tkDot) do begin
          if FCurrent.Kind in [tkIdentifier, tkText, tkMetadata, tkScore] then begin
            FullName := FullName + '.' + FCurrent.Text;
            Next;
          end else
            raise EVGQLParserError.CreateFmt(
                'Se esperaba un nombre en la posición %d', [FCurrent.Position]);
        end;
        IdExpr      := TIdentifierExpression.Create;
        IdExpr.Name := FullName;
        Result      := IdExpr;
      end;

    tkString:
      begin
        LitExpr             := TLiteralExpression.Create;
        LitExpr.LiteralType := ltString;
        LitExpr.Value       := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkNumber:
      begin
        LitExpr             := TLiteralExpression.Create;
        LitExpr.LiteralType := ltNumber;
        LitExpr.Value       := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkBoolean:
      begin
        LitExpr             := TLiteralExpression.Create;
        LitExpr.LiteralType := ltBoolean;
        LitExpr.Value       := FCurrent.Text;
        Next;
        Result := LitExpr;
      end;

    tkNull:
      begin
        LitExpr             := TLiteralExpression.Create;
        LitExpr.LiteralType := ltNull;
        LitExpr.Value       := 'NULL';
        Next;
        Result := LitExpr;
      end;

    tkLParen:
      begin
        Next;
        Result := ParseExpression;
        Expect(tkRParen, ')');
      end;
  else
    raise EVGQLParserError.CreateFmt(
        'Expresión inválida en posición %d: token inesperado "%s"',
        [FCurrent.Position, FCurrent.Text]);
  end;
end;

// ============================================================================
// ParseRerank, ParseThreshold, ParseOptimize, ParseReturn
// ============================================================================

function TVGQLParser.ParseRerank: TRerankClause;
begin
  Result := TRerankClause.Create(FCurrent.Position);
  try
    Next; // consumir RERANK
    if Check(tkString) then begin
      Result.Query := FCurrent.Text;
      Next;
    end else
      Expect(tkString, 'texto para rerank');
    if MatchTok(tkRegenerate) then
      Result.Regenerate := True;
  except
    Result.Free;
    raise;
  end;
end;

function TVGQLParser.ParseThreshold: TThresholdClause;
var
  FS: TFormatSettings;
begin
  FS     := InvFS;
  Result := TThresholdClause.Create(FCurrent.Position);
  try
    Next; // consumir THRESHOLD
    case FCurrent.Kind of
      tkGlobal  : Result.Scope := tsGlobal;
      tkSemantic: Result.Scope := tsSemantic;
      tkLexical : Result.Scope := tsLexical;
    else
      raise EVGQLParserError.CreateFmt(
          'Scope de THRESHOLD inválido en %d. Use GLOBAL, SEMANTIC o LEXICAL',
          [FCurrent.Position]);
    end;
    Next;
    if Check(tkNumber) then begin
      Result.Value := StrToFloat(FCurrent.Text, FS);
      Next;
    end else
      raise EVGQLParserError.CreateFmt(
          'Se esperaba un valor numérico para THRESHOLD en %d',
          [FCurrent.Position]);
  except
    Result.Free;
    raise;
  end;
end;

function TVGQLParser.ParseOptimize: TOptimizeClause;
begin
  Result := TOptimizeClause.Create(FCurrent.Position);
  try
    Next; // consumir OPTIMIZE
    Expect(tkReorder, 'REORDER');
    Expect(tkAbc,     'ABC');
    Result.Mode := omReorderABC;
  except
    Result.Free;
    raise;
  end;
end;

function TVGQLParser.ParseReturn: TReturnClause;
begin
  Result := TReturnClause.Create(FCurrent.Position);
  try
    Next; // consumir RETURN
    repeat
      case FCurrent.Kind of
        tkText    : Result.Fields.Add(rfText);
        tkMetadata: Result.Fields.Add(rfMetadata);
        tkScore   : Result.Fields.Add(rfScore);
      else
        raise EVGQLParserError.CreateFmt(
            'Campo RETURN inválido en %d. Use: TEXT, METADATA o SCORE',
            [FCurrent.Position]);
      end;
      Next;
      if not MatchTok(tkComma) then Break;
    until False;
    if Result.Fields.Count = 0 then
      raise EVGQLParserError.Create('RETURN requiere al menos un campo');
  except
    Result.Free;
    raise;
  end;
end;

// ============================================================================
// TVGQLRequest
// ============================================================================

constructor TVGQLRequest.Create;
begin
  Filter         := TAiFilterCriteria.Create;
  Mode           := smHybrid;
  WeightSemantic := 0.7;
  WeightLexical  := 0.3;
  Limit          := 10;
  Fusion         := fmWeighted;
  Language       := 'spanish';
end;

destructor TVGQLRequest.Destroy;
begin
  Filter.Free;
  inherited;
end;

// ============================================================================
// TVGQLCompiler
// ============================================================================

function TVGQLCompiler.Translate(AQuery: TVGQLQuery): TVGQLRequest;
begin
  if not Assigned(AQuery) then
    raise EVGQLTranslationError.Create('AST de consulta nulo');

  FRequest := TVGQLRequest.Create;
  try
    if Assigned(AQuery.Match)          then TranslateMatch    (AQuery.Match);
    if Assigned(AQuery.Search)         then TranslateSearch   (AQuery.Search)
    else raise EVGQLTranslationError.Create('La cláusula SEARCH es obligatoria');
    if Assigned(AQuery.UsingClause)    then TranslateUsing    (AQuery.UsingClause);
    if Assigned(AQuery.WhereClause)    then TranslateWhere    (AQuery.WhereClause);
    if Assigned(AQuery.RerankClause)   then TranslateRerank   (AQuery.RerankClause);
    if Assigned(AQuery.ThresholdClause)then TranslateThreshold(AQuery.ThresholdClause);
    if Assigned(AQuery.OptimizeClause) then TranslateOptimize (AQuery.OptimizeClause);
    if Assigned(AQuery.ReturnClause)   then TranslateReturn   (AQuery.ReturnClause);
    if AQuery.Limit > 0 then FRequest.Limit := AQuery.Limit;
    Result   := FRequest;
    FRequest := nil; // cede propiedad al caller
  except
    on E: Exception do begin
      FRequest.Free;
      raise EVGQLTranslationError.CreateFmt('Error de compilación VGQL: %s', [E.Message]);
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
  case AClause.Mode of
    smHybrid: begin
      FRequest.Mode := smHybrid;
      TotalWeight   := AClause.SemanticWeight + AClause.LexicalWeight;
      if TotalWeight > 0 then begin
        FRequest.WeightSemantic := AClause.SemanticWeight / TotalWeight;
        FRequest.WeightLexical  := AClause.LexicalWeight  / TotalWeight;
      end else begin
        FRequest.WeightSemantic := 0.5;
        FRequest.WeightLexical  := 0.5;
      end;
    end;
    smEmbeddings: begin
      FRequest.Mode           := smEmbeddings;
      FRequest.WeightSemantic := 1.0;
      FRequest.WeightLexical  := 0.0;
    end;
    smBM25: begin
      FRequest.Mode           := smBM25;
      FRequest.WeightSemantic := 0.0;
      FRequest.WeightLexical  := 1.0;
    end;
  end;

  if AClause.Fusion <> '' then begin
    if SameText(AClause.Fusion, 'RRF') then
      FRequest.Fusion := fmRRF
    else if SameText(AClause.Fusion, 'WEIGHTED') then
      FRequest.Fusion := fmWeighted
    else
      raise EVGQLTranslationError.CreateFmt(
          'Modo de fusión desconocido: %s', [AClause.Fusion]);
  end;
end;

procedure TVGQLCompiler.TranslateWhere(AClause: TWhereClause);
begin
  if Assigned(AClause.Expression) then begin
    FRequest.Filter.Clear;
    FRequest.Filter.LogicalOp := loAnd;
    BuildFilter(AClause.Expression, FRequest.Filter);
  end;
end;

function TVGQLCompiler.GetLiteralValue(AExpr: TExpression): Variant;
var
  Lit: TLiteralExpression;
  FS : TFormatSettings;
begin
  if not (AExpr is TLiteralExpression) then
    raise EVGQLTranslationError.Create(
        'Se esperaba un valor literal (String, Number o Boolean)');

  Lit := TLiteralExpression(AExpr);
  FS  := InvFS;
  case Lit.LiteralType of
    ltString : Result := Lit.Value;
    ltNumber :
      begin
        try
          Result := StrToFloat(
              StringReplace(Lit.Value, ',', '.', [rfReplaceAll]), FS);
        except
          Result := 0;
        end;
      end;
    ltBoolean: Result := SameText(Lit.Value, 'TRUE');
    ltNull   : Result := Null;
  else
    Result := Unassigned;
  end;
end;

procedure TVGQLCompiler.BuildFilter(AExpr: TExpression; ACriteria: TAiFilterCriteria);
var
  Bin      : TBinaryExpression;
  Bet      : TBetweenExpression;
  InExp    : TInExpression;
  FieldName: string;
  VArray   : Variant;
  I        : Integer;
begin
  if not Assigned(AExpr) then Exit;

  // Grupos lógicos (AND / OR)
  if (AExpr is TBinaryExpression) and
     (TBinaryExpression(AExpr).BinOp in [boAnd, boOr]) then begin
    Bin := TBinaryExpression(AExpr);
    if ((Bin.BinOp = boAnd) and (ACriteria.LogicalOp = loAnd)) or
       ((Bin.BinOp = boOr)  and (ACriteria.LogicalOp = loOr))  then begin
      BuildFilter(Bin.Left,  ACriteria);
      BuildFilter(Bin.Right, ACriteria);
    end else begin
      if Bin.BinOp = boAnd then
        BuildFilter(Bin, ACriteria.AddGroup(loAnd))
      else
        BuildFilter(Bin, ACriteria.AddGroup(loOr));
    end;
  end

  // BETWEEN
  else if AExpr is TBetweenExpression then begin
    Bet := TBetweenExpression(AExpr);
    if not (Bet.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create(
          'BETWEEN requiere un identificador de metadato');
    ACriteria.AddBetween(TIdentifierExpression(Bet.Left).Name,
        GetLiteralValue(Bet.Min), GetLiteralValue(Bet.Max));
  end

  // IN
  else if AExpr is TInExpression then begin
    InExp := TInExpression(AExpr);
    if not (InExp.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create(
          'IN requiere un identificador de metadato');
    FieldName := TIdentifierExpression(InExp.Left).Name;
    VArray    := VarArrayCreate([0, InExp.Values.Count - 1], varVariant);
    for I := 0 to InExp.Values.Count - 1 do
      VArray[I] := GetLiteralValue(TExpression(InExp.Values[I]));
    if InExp.IsNot then
      ACriteria.Add(FieldName, foNotIn, VArray)
    else
      ACriteria.AddIn(FieldName, VArray);
  end

  // Comparaciones binarias estándar
  else if AExpr is TBinaryExpression then begin
    Bin := TBinaryExpression(AExpr);
    if not (Bin.Left is TIdentifierExpression) then
      raise EVGQLTranslationError.Create(
          'El lado izquierdo de la comparación debe ser un campo');
    FieldName := TIdentifierExpression(Bin.Left).Name;
    case Bin.BinOp of
      boEqual       : ACriteria.AddEqual  (FieldName, GetLiteralValue(Bin.Right));
      boNotEqual    : ACriteria.Add       (FieldName, foNotEqual,     GetLiteralValue(Bin.Right));
      boGreater     : ACriteria.AddGreater(FieldName, GetLiteralValue(Bin.Right));
      boGreaterEqual: ACriteria.Add       (FieldName, foGreaterOrEqual, GetLiteralValue(Bin.Right));
      boLess        : ACriteria.AddLess   (FieldName, GetLiteralValue(Bin.Right));
      boLessEqual   : ACriteria.Add       (FieldName, foLessOrEqual,  GetLiteralValue(Bin.Right));
      boContains    : ACriteria.Add       (FieldName, foContains,     GetLiteralValue(Bin.Right));
      boStartsWith  : ACriteria.Add       (FieldName, foStartsWith,   GetLiteralValue(Bin.Right));
      boEndsWith    : ACriteria.Add       (FieldName, foEndsWith,     GetLiteralValue(Bin.Right));
      boLike        : ACriteria.Add       (FieldName, foLike,         GetLiteralValue(Bin.Right));
      boILike       : ACriteria.Add       (FieldName, foILike,        GetLiteralValue(Bin.Right));
      boIsNull      : ACriteria.Add       (FieldName, foIsNull,       Null);
      boIsNotNull   : ACriteria.Add       (FieldName, foIsNotNull,    Null);
    end;
  end;
end;

procedure TVGQLCompiler.TranslateRerank(AClause: TRerankClause);
begin
  FRequest.RerankQuery     := AClause.Query;
  FRequest.RerankRegenerate:= AClause.Regenerate;
end;

procedure TVGQLCompiler.TranslateThreshold(AClause: TThresholdClause);
begin
  case AClause.Scope of
    tsGlobal  : FRequest.MinGlobal   := AClause.Value;
    tsSemantic: FRequest.MinSemantic := AClause.Value;
    tsLexical : FRequest.MinLexical  := AClause.Value;
  end;
end;

procedure TVGQLCompiler.TranslateOptimize(AClause: TOptimizeClause);
begin
  FRequest.UseReorderABC := (AClause.Mode = omReorderABC);
end;

procedure TVGQLCompiler.TranslateReturn(AClause: TReturnClause);
var
  Field: TReturnField;
  I    : Integer;
begin
  for I := 0 to AClause.Fields.Count - 1 do begin
    Field := AClause.Fields[I];
    case Field of
      rfMetadata: FRequest.IncludeMetadata := True;
      rfScore   : FRequest.IncludeScore    := True;
    end;
  end;
end;

end.
