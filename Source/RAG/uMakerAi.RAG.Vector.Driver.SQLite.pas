unit uMakerAi.RAG.Vector.Driver.SQLite;

{
  TAiRAGVectorSQLiteDriver — Driver SQLite para TAiRAGVector (MakerAI 3.3)

  Características:
  - Almacenamiento persistente en archivo .db (SQLite vía FireDAC)
  - Búsqueda vectorial por similitud coseno calculada en Delphi
  - Soporte opcional para sqlite-vec: si VecExtensionPath está definido se usa
    vec_distance_cosine() nativo para búsqueda vectorial más rápida y escalable
  - Búsqueda léxical BM25 vía FTS5 (nativo en SQLite 3.9+, sin extensiones)
  - Búsqueda híbrida (vector + BM25) con fusión RRF o ponderada
  - Filtrado de metadatos vía json_extract()
  - Upsert (DELETE+INSERT) con triggers que mantienen FTS5 sincronizado

  Esquema generado por CreateSchema():
    <tabla>        — entidad, id, model, content, properties(JSON TEXT), embedding(TEXT)
    <tabla>_fts    — virtual FTS5 sincronizada por triggers
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Variants,
  System.StrUtils,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DApt,
  uMakerAi.Embeddings.Core,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData;

type
  // ---------------------------------------------------------------------------
  // TSQLiteFilterBuilder — WHERE clause desde TAiFilterCriteria para SQLite
  // Usa json_extract() en lugar de los operadores JSONB de PostgreSQL.
  // ---------------------------------------------------------------------------
  TSQLiteFilterBuilder = class
  private
    FParamCounter: Integer;
    FParamNames: TList<string>;
    FParamValues: TList<Variant>;
    function BuildPath(const AKey: string): string;
    function Process(ACriteria: TAiFilterCriteria): string;
  public
    constructor Create;
    destructor Destroy; override;
    function BuildSQL(ACriteria: TAiFilterCriteria): string;
    procedure ApplyParams(AQuery: TFDQuery);
  end;

  // ---------------------------------------------------------------------------
  // TAiRAGVectorSQLiteDriver
  // ---------------------------------------------------------------------------
  TAiRAGVectorSQLiteDriver = class(TAiVectorStoreDriverBase)
  private
    FConnection: TFDConnection;
    FTableName: string;
    FCurrentEntidad: string;
    FLanguage: TAiLanguage;
    FVecExtensionPath: string;
    FVecExtLoaded: Boolean;
    FLastSQL: string;

    function NewQuery: TFDQuery;
    function FtsTableName: string;
    function PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
    function GetFtsTokenizer: string;
    procedure SetTableName(const Value: string);
    procedure SetConnection(const Value: TFDConnection);
    procedure TryLoadVecExtension;

    function VectorSearchDelphi(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function VectorSearchVecExt(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function LexicalSearchFTS5(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
    function FuseWeighted(AVec, ALex: TAiRAGVector;
      AVW, ALW: Double; ALimit: Integer): TAiRAGVector;

    // Copia un nodo creando una instancia nueva con misma dimensión que ATarget
    function CopyNode(Src: TAiEmbeddingNode; ATargetDim: Integer): TAiEmbeddingNode;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Add(const ANode: TAiEmbeddingNode; const AEntidad: string = ''); override;
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string;
      ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria;
      Options: TAiSearchOptions): TAiRAGVector; override;
    procedure Delete(const AID: string; const AEntidad: string); override;
    procedure Clear(const AEntidad: string); override;

    procedure CreateSchema(const ABaseTableName: string);
    procedure DropSchema(const ABaseTableName: string);

    function EmbeddingToStr(const AData: TAiEmbeddingData): string;
    function StrToEmbedding(const AStr: string): TAiEmbeddingData;

    property LastSQL: string read FLastSQL;
    property VecExtLoaded: Boolean read FVecExtLoaded;
  published
    property Connection: TFDConnection read FConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property CurrentEntidad: string read FCurrentEntidad write FCurrentEntidad;
    property Language: TAiLanguage read FLanguage write FLanguage default alSpanish;
    // Ruta completa a sqlite_vec.dll/.so — vacío = modo Delphi (brute-force cosine)
    property VecExtensionPath: string read FVecExtensionPath write FVecExtensionPath;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRAGVectorSQLiteDriver]);
end;

// =============================================================================
// TSQLiteFilterBuilder
// =============================================================================

constructor TSQLiteFilterBuilder.Create;
begin
  inherited Create;
  FParamCounter := 0;
  FParamNames  := TList<string>.Create;
  FParamValues := TList<Variant>.Create;
end;

destructor TSQLiteFilterBuilder.Destroy;
begin
  FParamNames.Free;
  FParamValues.Free;
  inherited;
end;

function TSQLiteFilterBuilder.BuildPath(const AKey: string): string;
var
  Parts: TArray<string>;
  I: Integer;
  Path: string;
begin
  if SameText(AKey, 'text') or SameText(AKey, 'content') then Exit('content');
  if SameText(AKey, 'model')  then Exit('model');
  if SameText(AKey, 'id')     then Exit('id');

  if AKey.Contains('.') then
  begin
    Parts := AKey.Split(['.']);
    Path := '$.';
    for I := 0 to High(Parts) do
    begin
      if I > 0 then Path := Path + '.';
      Path := Path + Parts[I];
    end;
    Result := Format('json_extract(properties, ''%s'')', [Path]);
  end
  else
    Result := Format('json_extract(properties, ''$.%s'')', [AKey]);
end;

function TSQLiteFilterBuilder.BuildSQL(ACriteria: TAiFilterCriteria): string;
begin
  FParamCounter := 0;
  FParamNames.Clear;
  FParamValues.Clear;
  Result := Process(ACriteria);
end;

function TSQLiteFilterBuilder.Process(ACriteria: TAiFilterCriteria): string;
var
  I: Integer;
  Criterion: TFilterCriterion;
  Parts: TList<string>;
  LogicStr, SubSQL, Path, PName: string;

  function NextParam: string;
  begin
    Inc(FParamCounter);
    Result := 'p_flt_' + IntToStr(FParamCounter);
  end;

  procedure AddParam(const AName: string; const AValue: Variant);
  begin
    FParamNames.Add(AName);
    FParamValues.Add(AValue);
  end;

begin
  Result := '';
  if (ACriteria = nil) or (ACriteria.Count = 0) then Exit;

  Parts := TList<string>.Create;
  try
    LogicStr := IfThen(ACriteria.LogicalOp = loAnd, ' AND ', ' OR ');

    for I := 0 to ACriteria.Count - 1 do
    begin
      Criterion := ACriteria.Items[I];

      if Criterion.IsGroup then
      begin
        SubSQL := Process(Criterion.SubCriteria);
        if SubSQL <> '' then Parts.Add('(' + SubSQL + ')');
        Continue;
      end;

      Path  := BuildPath(Criterion.Key);
      PName := NextParam;

      case Criterion.Op of
        foEqual:        begin Parts.Add(Format('(%s = :%s)',  [Path, PName])); AddParam(PName, Criterion.Value); end;
        foNotEqual:     begin Parts.Add(Format('(%s <> :%s)', [Path, PName])); AddParam(PName, Criterion.Value); end;
        foGreater:      begin Parts.Add(Format('(%s > :%s)',  [Path, PName])); AddParam(PName, Criterion.Value); end;
        foGreaterOrEqual: begin Parts.Add(Format('(%s >= :%s)', [Path, PName])); AddParam(PName, Criterion.Value); end;
        foLess:         begin Parts.Add(Format('(%s < :%s)',  [Path, PName])); AddParam(PName, Criterion.Value); end;
        foLessOrEqual:  begin Parts.Add(Format('(%s <= :%s)', [Path, PName])); AddParam(PName, Criterion.Value); end;

        foLike, foILike: // SQLite LIKE es case-insensitive para ASCII por defecto
          begin Parts.Add(Format('(%s LIKE :%s)', [Path, PName])); AddParam(PName, Criterion.Value); end;
        foStartsWith:
          begin Parts.Add(Format('(%s LIKE :%s)', [Path, PName])); AddParam(PName, VarToStr(Criterion.Value) + '%'); end;
        foEndsWith:
          begin Parts.Add(Format('(%s LIKE :%s)', [Path, PName])); AddParam(PName, '%' + VarToStr(Criterion.Value)); end;

        foIn, foNotIn:
          begin
            if VarIsArray(Criterion.Value) then
            begin
              var InList := TList<string>.Create;
              try
                var J: Integer;
                for J := VarArrayLowBound(Criterion.Value, 1) to VarArrayHighBound(Criterion.Value, 1) do
                begin
                  var SP := PName + '_in_' + IntToStr(J);
                  InList.Add(':' + SP);
                  AddParam(SP, VarArrayGet(Criterion.Value, [J]));
                end;
                var Op := IfThen(Criterion.Op = foIn, 'IN', 'NOT IN');
                Parts.Add(Format('(%s %s (%s))', [Path, Op, String.Join(',', InList.ToArray)]));
              finally
                InList.Free;
              end;
            end
            else
            begin
              var Op := IfThen(Criterion.Op = foIn, 'IN', 'NOT IN');
              Parts.Add(Format('(%s %s (:%s))', [Path, Op, PName]));
              AddParam(PName, Criterion.Value);
            end;
          end;

        foBetween:
          begin
            Parts.Add(Format('(%s BETWEEN :%s_1 AND :%s_2)', [Path, PName, PName]));
            AddParam(PName + '_1', Criterion.Value);
            AddParam(PName + '_2', Criterion.Value2);
          end;

        foIsNull:    Parts.Add(Format('(%s IS NULL)',     [Path]));
        foIsNotNull: Parts.Add(Format('(%s IS NOT NULL)', [Path]));

        foExists:
          // Verificar que la clave exista en el JSON properties
          Parts.Add(Format('(json_type(properties, ''$.%s'') IS NOT NULL)', [Criterion.Key]));

        foContains:
          // Aproximación: valor exacto en la clave indicada
          begin Parts.Add(Format('(%s = :%s)', [Path, PName])); AddParam(PName, Criterion.Value); end;

      else
        Parts.Add('(1=1)');
      end;
    end;

    if Parts.Count > 0 then
    begin
      Result := Parts[0];
      for I := 1 to Parts.Count - 1 do
        Result := Result + LogicStr + Parts[I];
    end;
  finally
    Parts.Free;
  end;
end;

procedure TSQLiteFilterBuilder.ApplyParams(AQuery: TFDQuery);
var
  I: Integer;
begin
  for I := 0 to FParamNames.Count - 1 do
    AQuery.ParamByName(FParamNames[I]).Value := FParamValues[I];
end;

// =============================================================================
// TAiRAGVectorSQLiteDriver — Infraestructura
// =============================================================================

constructor TAiRAGVectorSQLiteDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableName := 'vector_store';
  FCurrentEntidad := 'DEFAULT';
  FLanguage := alSpanish;
  FVecExtensionPath := '';
  FVecExtLoaded := False;
  FLastSQL := '';
end;

procedure TAiRAGVectorSQLiteDriver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TAiRAGVectorSQLiteDriver.SetConnection(const Value: TFDConnection);
begin
  if FConnection = Value then Exit;
  FConnection := Value;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

procedure TAiRAGVectorSQLiteDriver.SetTableName(const Value: string);
begin
  FTableName := Value.ToLower;
end;

function TAiRAGVectorSQLiteDriver.FtsTableName: string;
begin
  Result := FTableName + '_fts';
end;

function TAiRAGVectorSQLiteDriver.GetFtsTokenizer: string;
begin
  case FLanguage of
    alEnglish: Result := 'porter ascii';
  else
    Result := 'unicode61 remove_diacritics 1';
  end;
end;

function TAiRAGVectorSQLiteDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('TAiRAGVectorSQLiteDriver: propiedad Connection no asignada.');
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

procedure TAiRAGVectorSQLiteDriver.TryLoadVecExtension;
var
  Q: TFDQuery;
begin
  FVecExtLoaded := False;
  if FVecExtensionPath = '' then Exit;
  Q := NewQuery;
  try
    try
      Q.SQL.Text := 'SELECT load_extension(:p)';
      Q.ParamByName('p').AsString := FVecExtensionPath;
      Q.Open;
      Q.Close;
      FVecExtLoaded := True;
    except
      FVecExtLoaded := False; // Silencioso: fallback a Delphi cosine
    end;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Helpers: embedding ↔ string y metadata ↔ JSON
// =============================================================================

function TAiRAGVectorSQLiteDriver.EmbeddingToStr(const AData: TAiEmbeddingData): string;
var
  I: Integer;
  Parts: TArray<string>;
  FS: TFormatSettings;
begin
  if Length(AData) = 0 then Exit('');
  FS := TFormatSettings.Invariant;
  SetLength(Parts, Length(AData));
  for I := 0 to High(AData) do
    Parts[I] := FloatToStr(AData[I], FS);
  Result := String.Join(',', Parts);
end;

function TAiRAGVectorSQLiteDriver.StrToEmbedding(const AStr: string): TAiEmbeddingData;
var
  Parts: TArray<string>;
  I: Integer;
  FS: TFormatSettings;
begin
  Result := nil;
  if AStr = '' then Exit;
  Parts := AStr.Split([',']);
  SetLength(Result, Length(Parts));
  FS := TFormatSettings.Invariant;
  for I := 0 to High(Parts) do
    if not TryStrToFloat(Trim(Parts[I]), Result[I], FS) then
      Result[I] := 0.0;
end;

function TAiRAGVectorSQLiteDriver.PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
var
  J: TJSONObject;
begin
  if not Assigned(AMeta) then Exit('{}');
  J := AMeta.ToJSON;
  try
    Result := J.ToJSON;
  finally
    J.Free;
  end;
end;

function TAiRAGVectorSQLiteDriver.CopyNode(Src: TAiEmbeddingNode; ATargetDim: Integer): TAiEmbeddingNode;
var
  J: TJSONObject;
begin
  Result := TAiEmbeddingNode.Create(ATargetDim);
  try
    Result.Tag   := Src.Tag;
    Result.Text  := Src.Text;
    Result.Model := Src.Model;
    Result.Idx   := Src.Idx;
    if Length(Src.Data) > 0 then
    begin
      Result.Data := Copy(Src.Data);
      Result.SetDataLength(Length(Result.Data));
    end;
    J := Src.MetaData.ToJSON;
    try
      Result.MetaData.FromJSON(J);
    finally
      J.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

// =============================================================================
// Gestión del esquema
// =============================================================================

procedure TAiRAGVectorSQLiteDriver.CreateSchema(const ABaseTableName: string);
var
  Q: TFDQuery;
  TN, FN, Tok: string;
begin
  TN := ABaseTableName.ToLower;
  FN := TN + '_fts';
  FTableName := TN;
  Tok := GetFtsTokenizer;

  Q := NewQuery;
  try
    Q.ExecSQL('PRAGMA journal_mode=WAL');
    Q.ExecSQL('PRAGMA synchronous=NORMAL');

    // Tabla principal
    Q.ExecSQL(
      'CREATE TABLE IF NOT EXISTS ' + TN + ' (' +
      '  entidad    TEXT NOT NULL,' +
      '  id         TEXT NOT NULL,' +
      '  model      TEXT,' +
      '  content    TEXT,' +
      '  properties TEXT NOT NULL DEFAULT ''{}'',' +
      '  embedding  TEXT,' +
      '  PRIMARY KEY (entidad, id)' +
      ')');

    Q.ExecSQL('CREATE INDEX IF NOT EXISTS idx_' + TN + '_ent ON ' + TN + '(entidad)');

    // Tabla FTS5 (virtual)
    Q.ExecSQL(
      'CREATE VIRTUAL TABLE IF NOT EXISTS ' + FN + ' USING fts5(' +
      '  entidad UNINDEXED,' +
      '  id UNINDEXED,' +
      '  content,' +
      '  tokenize=''' + Tok + '''' +
      ')');

    // Triggers para mantener FTS5 sincronizado
    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_ai AFTER INSERT ON ' + TN + ' BEGIN' +
      '  INSERT INTO ' + FN + '(entidad, id, content)' +
      '  VALUES (NEW.entidad, NEW.id, NEW.content);' +
      'END');

    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_ad AFTER DELETE ON ' + TN + ' BEGIN' +
      '  DELETE FROM ' + FN + ' WHERE entidad=OLD.entidad AND id=OLD.id;' +
      'END');

    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_au AFTER UPDATE OF content ON ' + TN + ' BEGIN' +
      '  DELETE FROM ' + FN + ' WHERE entidad=OLD.entidad AND id=OLD.id;' +
      '  INSERT INTO ' + FN + '(entidad, id, content) VALUES (NEW.entidad, NEW.id, NEW.content);' +
      'END');
  finally
    Q.Free;
  end;

  TryLoadVecExtension;
end;

procedure TAiRAGVectorSQLiteDriver.DropSchema(const ABaseTableName: string);
var
  Q: TFDQuery;
  TN: string;
begin
  TN := ABaseTableName.ToLower;
  Q := NewQuery;
  try
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_ai');
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_ad');
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_au');
    Q.ExecSQL('DROP TABLE IF EXISTS '   + TN + '_fts');
    Q.ExecSQL('DROP TABLE IF EXISTS '   + TN);
  finally
    Q.Free;
  end;
end;

// =============================================================================
// CRUD
// =============================================================================

procedure TAiRAGVectorSQLiteDriver.Add(const ANode: TAiEmbeddingNode; const AEntidad: string);
var
  Q: TFDQuery;
  LEnt: string;
begin
  Q := NewQuery;
  try
    LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);

    // Upsert: eliminar primero dispara trigger _ad en FTS5,
    // luego el INSERT dispara _ai — así FTS5 queda siempre consistente.
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent AND id=:id';
    Q.ParamByName('ent').AsString := LEnt;
    Q.ParamByName('id').AsString  := ANode.Tag;
    Q.ExecSQL;

    Q.SQL.Text :=
      'INSERT INTO ' + FTableName +
      ' (entidad, id, model, content, properties, embedding)' +
      ' VALUES (:ent, :id, :model, :content, :prop, :emb)';
    Q.ParamByName('ent').AsString     := LEnt;
    Q.ParamByName('id').AsString      := ANode.Tag;
    Q.ParamByName('model').AsString   := ANode.Model;
    Q.ParamByName('content').AsString := ANode.Text;
    Q.ParamByName('prop').AsString    := PropertiesToJSON(ANode.MetaData);
    Q.ParamByName('emb').AsString     := EmbeddingToStr(ANode.Data);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorSQLiteDriver.Delete(const AID: string; const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent AND id=:id';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ParamByName('id').AsString  := AID;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorSQLiteDriver.Clear(const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Búsqueda vectorial — similitud coseno en Delphi (brute-force, O(n))
// Adecuado para colecciones pequeñas/medianas (≤ 50K nodos).
// =============================================================================

function TAiRAGVectorSQLiteDriver.VectorSearchDelphi(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;

  function CosineSim(const A, B: TAiEmbeddingData): Double;
  var
    Dot, NA, NB: Double;
    I: Integer;
  begin
    Dot := 0; NA := 0; NB := 0;
    if Length(A) <> Length(B) then Exit(0.0);
    for I := 0 to High(A) do
    begin
      Dot := Dot + A[I] * B[I];
      NA  := NA  + A[I] * A[I];
      NB  := NB  + B[I] * B[I];
    end;
    NA := Sqrt(NA); NB := Sqrt(NB);
    if (NA < 1e-12) or (NB < 1e-12) then Exit(0.0);
    Result := Dot / (NA * NB);
  end;

type
  TNodeScore = record
    Node: TAiEmbeddingNode;
    Score: Double;
  end;

var
  Q: TFDQuery;
  SQL: string;
  Candidates: TList<TNodeScore>;
  NS: TNodeScore;
  I: Integer;
  PropStr: string;
  JObj: TJSONObject;
begin
  Result := TAiRAGVector.Create(nil, True);
  Candidates := TList<TNodeScore>.Create;
  Q := NewQuery;
  try
    SQL :=
      'SELECT id, content, model, properties, embedding ' +
      'FROM ' + FTableName +
      ' WHERE entidad=:ent AND (embedding IS NOT NULL) AND (embedding <> '''')';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('ent').AsString := AEntidad;
    if AFilterSQL <> '' then FB.ApplyParams(Q);
    Q.Open;

    while not Q.Eof do
    begin
      NS.Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        NS.Node.Tag   := Q.FieldByName('id').AsString;
        NS.Node.Text  := Q.FieldByName('content').AsString;
        NS.Node.Model := Q.FieldByName('model').AsString;
        NS.Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(NS.Node.Data) > 0 then
          NS.Node.SetDataLength(Length(NS.Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try NS.Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
        end;
        NS.Score := CosineSim(ATarget.Data, NS.Node.Data);
        Candidates.Add(NS);
      except
        NS.Node.Free;
        raise;
      end;
      Q.Next;
    end;

    // Ordenar por score desc
    Candidates.Sort(TComparer<TNodeScore>.Construct(
      function(const A, B: TNodeScore): Integer
      begin
        if A.Score > B.Score then Result := -1
        else if A.Score < B.Score then Result := 1
        else Result := 0;
      end));

    // Tomar top-ALimit respetando min score
    for I := 0 to Candidates.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      if (AMinScore > 0) and (Candidates[I].Score < AMinScore) then Break;
      Candidates[I].Node.Idx := Candidates[I].Score;
      Result.Items.Add(Candidates[I].Node);
      // Marcar como transferido (evitar Free en cleanup)
      var NSMod := Candidates[I];
      NSMod.Node := nil;
      Candidates[I] := NSMod;
    end;

  finally
    // Liberar nodos no transferidos al Result
    for I := 0 to Candidates.Count - 1 do
      Candidates[I].Node.Free;
    Candidates.Free;
    Q.Free;
  end;
end;

// =============================================================================
// Búsqueda vectorial con sqlite-vec (rápida, usa índice vectorial nativo)
// =============================================================================

function TAiRAGVectorSQLiteDriver.VectorSearchVecExt(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: string;
  Node: TAiEmbeddingNode;
  PropStr: string;
  JObj: TJSONObject;
begin
  Result := TAiRAGVector.Create(nil, True);
  Q := NewQuery;
  try
    // sqlite-vec almacena embeddings como BLOB; aquí asumimos la columna
    // 'embedding' ya fue creada como vec_f32 por el usuario, o usamos
    // la versión TEXT con conversión inline.
    // Distancia coseno sqlite-vec: vec_distance_cosine(a, b) ∈ [0, 2]
    // Similitud coseno ≈ 1 - distancia (rango [−1, 1], usamos max(0, ...))
    SQL :=
      'SELECT id, content, model, properties, embedding,' +
      '  max(0.0, 1.0 - vec_distance_cosine(embedding, vec_f32(:qvec))) AS final_score ' +
      'FROM ' + FTableName +
      ' WHERE entidad=:ent';
    if AMinScore > 0 then
      SQL := SQL + ' AND max(0.0, 1.0 - vec_distance_cosine(embedding, vec_f32(:qvec))) >= :min_s';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';
    SQL := SQL +
      ' ORDER BY vec_distance_cosine(embedding, vec_f32(:qvec)) ASC' +
      ' LIMIT :lim';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('ent').AsString  := AEntidad;
    Q.ParamByName('qvec').AsString := '[' + EmbeddingToStr(ATarget.Data) + ']';
    Q.ParamByName('lim').AsInteger := ALimit;
    if AMinScore > 0 then Q.ParamByName('min_s').AsFloat := AMinScore;
    if AFilterSQL <> '' then FB.ApplyParams(Q);

    Q.Open;
    while not Q.Eof do
    begin
      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag   := Q.FieldByName('id').AsString;
        Node.Text  := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx   := Q.FieldByName('final_score').AsFloat;
        Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(Node.Data) > 0 then Node.SetDataLength(Length(Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
        end;
        Result.Items.Add(Node);
      except
        Node.Free;
        raise;
      end;
      Q.Next;
    end;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Búsqueda léxical BM25 vía FTS5
// FTS5 bm25() devuelve valores ≤ 0 (más negativo = más relevante).
// Convertimos a score positivo normalizado al rango [0, 1].
// =============================================================================

function TAiRAGVectorSQLiteDriver.LexicalSearchFTS5(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: string;
  Node: TAiEmbeddingNode;
  PropStr: string;
  JObj: TJSONObject;
  RawScore, MaxRaw, NormScore: Double;
  Candidates: TList<TAiEmbeddingNode>;
  I: Integer;
begin
  Result := TAiRAGVector.Create(nil, True);
  Candidates := TList<TAiEmbeddingNode>.Create;
  Q := NewQuery;
  try
    // JOIN FTS5 con tabla principal para recuperar todos los campos.
    // bm25() negativo → usamos -bm25() para tener valor positivo (mayor = mejor).
    SQL :=
      'SELECT t.id, t.content, t.model, t.properties, t.embedding,' +
      '  (-bm25(' + FtsTableName + ')) AS raw_score ' +
      'FROM ' + FtsTableName + ' AS f ' +
      'JOIN ' + FTableName + ' AS t ON t.entidad=f.entidad AND t.id=f.id ' +
      'WHERE f.' + FtsTableName + ' MATCH :query AND f.entidad=:ent';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';
    SQL := SQL + ' ORDER BY bm25(' + FtsTableName + ') ASC LIMIT :lim';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('query').AsString := ATarget.Text;
    Q.ParamByName('ent').AsString   := AEntidad;
    Q.ParamByName('lim').AsInteger  := ALimit * 2; // extra para normalizar
    if AFilterSQL <> '' then FB.ApplyParams(Q);

    try
      Q.Open;
    except
      // Query FTS inválida (caracteres especiales sin escapar) — devolver vacío
      Exit;
    end;

    // Primera pasada: recopilar candidatos y encontrar máximo raw_score
    MaxRaw := 1e-12; // evitar división por cero
    while not Q.Eof do
    begin
      RawScore := Q.FieldByName('raw_score').AsFloat;
      if RawScore > MaxRaw then MaxRaw := RawScore;

      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag   := Q.FieldByName('id').AsString;
        Node.Text  := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx   := RawScore; // temporal — se normaliza después
        Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(Node.Data) > 0 then Node.SetDataLength(Length(Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
        end;
        Candidates.Add(Node);
      except
        Node.Free;
        raise;
      end;
      Q.Next;
    end;

    // Segunda pasada: normalizar scores y filtrar
    for I := 0 to Candidates.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      NormScore := Candidates[I].Idx / MaxRaw; // [0, 1]
      if (AMinScore > 0) and (NormScore < AMinScore) then Continue;
      Candidates[I].Idx := NormScore;
      Result.Items.Add(Candidates[I]);
      Candidates[I] := nil; // Transferido, no liberar en cleanup
    end;

  finally
    for I := 0 to Candidates.Count - 1 do
      Candidates[I].Free; // Los no transferidos
    Candidates.Free;
    Q.Free;
  end;
end;

// =============================================================================
// Fusión RRF (Reciprocal Rank Fusion)
// =============================================================================

function TAiRAGVectorSQLiteDriver.FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
const
  K = 60;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
  RRF: Double;
begin
  Result := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;

    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));

    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));

    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key], NodeMap[Pairs[I].Key].Dim);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

// =============================================================================
// Fusión ponderada (Weighted Score Fusion)
// =============================================================================

function TAiRAGVectorSQLiteDriver.FuseWeighted(AVec, ALex: TAiRAGVector;
  AVW, ALW: Double; ALimit: Integer): TAiRAGVector;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
begin
  Result := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      var S := AVec.Items[I].Idx * AVW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      var S := ALex.Items[I].Idx * ALW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;

    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));

    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));

    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key], NodeMap[Pairs[I].Key].Dim);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

// =============================================================================
// Search — orquestador principal
// =============================================================================

function TAiRAGVectorSQLiteDriver.Search(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  APrecision: Double;
  AFilter: TAiFilterCriteria;
  Options: TAiSearchOptions): TAiRAGVector;
var
  LEnt: string;
  LOptions: TAiSearchOptions;
  FB: TSQLiteFilterBuilder;
  FilterSQL: string;
  DoVector, DoLexical: Boolean;
  MinVec, MinLex: Double;
  VecRes, LexRes: TAiRAGVector;
  Fused: TAiRAGVector;

  procedure TransferFiltered(Src: TAiRAGVector);
  var
    J: Integer;
    N: TAiEmbeddingNode;
  begin
    for J := 0 to Src.Items.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      if (APrecision > 0) and (Src.Items[J].Idx < APrecision) then Continue;
      N := CopyNode(Src.Items[J], ATarget.Dim);
      Result.Items.Add(N);
    end;
  end;

begin
  Result := TAiRAGVector.Create(nil, True);
  LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);

  LOptions := Options;
  if not Assigned(LOptions) and Assigned(Owner) and (Owner is TAiRAGVector) then
    LOptions := TAiRAGVector(Owner).SearchOptions;

  DoVector  := (Length(ATarget.Data) > 0) and ((LOptions = nil) or LOptions.UseEmbeddings);
  DoLexical := Assigned(LOptions) and LOptions.UseBM25 and (Trim(ATarget.Text) <> '');
  if not (DoVector or DoLexical) then Exit;

  MinVec := IfThen(Assigned(LOptions), LOptions.MinAbsoluteScoreEmbedding, 0.0);
  MinLex := IfThen(Assigned(LOptions), LOptions.MinAbsoluteScoreBM25, 0.0);

  FB := TSQLiteFilterBuilder.Create;
  VecRes := nil;
  LexRes := nil;
  try
    FilterSQL := '';
    if Assigned(AFilter) and (AFilter.Count > 0) then
      FilterSQL := FB.BuildSQL(AFilter);

    if DoVector then
    begin
      try
        if FVecExtLoaded then
          VecRes := VectorSearchVecExt(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB)
        else
          VecRes := VectorSearchDelphi(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB);
      except
        on E: Exception do
        begin
          FreeAndNil(VecRes);
          if FVecExtLoaded then
          begin
            FVecExtLoaded := False; // sqlite-vec falló en runtime → fallback
            VecRes := VectorSearchDelphi(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB);
          end
          else raise;
        end;
      end;
    end;

    if DoLexical then
      LexRes := LexicalSearchFTS5(ATarget, LEnt, ALimit * 3, MinLex, FilterSQL, FB);

    // Fusión
    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
        Fused := FuseRRF(VecRes, LexRes, ALimit)
      else
      begin
        var VW := IfThen(Assigned(LOptions), LOptions.EmbeddingWeight, 0.7);
        var LW := IfThen(Assigned(LOptions), LOptions.BM25Weight, 0.3);
        Fused := FuseWeighted(VecRes, LexRes, VW, LW, ALimit);
      end;
      try
        TransferFiltered(Fused);
      finally
        Fused.Free;
      end;
    end
    else if DoVector and Assigned(VecRes) then
      TransferFiltered(VecRes)
    else if DoLexical and Assigned(LexRes) then
      TransferFiltered(LexRes);

  finally
    FB.Free;
    VecRes.Free;
    LexRes.Free;
  end;
end;

end.
