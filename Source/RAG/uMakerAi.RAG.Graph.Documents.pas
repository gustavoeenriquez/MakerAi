// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.RAG.Graph.Documents;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Generics.Defaults, System.Variants, System.Math,
  uMakerAi.Embeddings.core,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.VQL,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.Chat;

const
  // Claves de metadata para chunks de documento
  DOCMETA_ORDER         = 'doc_order';
  DOCMETA_NAME          = 'doc_part_name';
  DOCMETA_LABEL         = 'doc_part_label';
  DOCMETA_DEPTH         = 'doc_depth';
  DOCMETA_PATH          = 'doc_path';
  DOCMETA_IS_LEAF       = 'doc_is_leaf';
  DOCMETA_DOCUMENT_ID   = 'doc_document_id';
  DOCMETA_DOCUMENT_NAME = 'doc_document_name';
  DOCMETA_ORIGINAL_TEXT = 'doc_original_text';

  // Etiquetas de aristas
  EDGE_EXTRACTED_FROM         = 'EXTRACTED_FROM';
  EDGEPROP_SOURCE_CHUNK_INDEX = 'source_chunk_index';

  // Etiqueta por defecto para nodos de documento
  NODELABEL_DOCUMENT = 'Document';

type

  { TDocumentPart - Representa una parte/secci?n de un documento }
  TDocumentPart = record
    Name: string;
    PartLabel: string;
    Text: string;
    Path: string;
    Depth: Integer;
    Order: Integer;
    Score: Double;
    IsLeaf: Boolean;
    Properties: TJSONObject;
  end;

  { TDocumentContext - Contexto completo de un documento para LLM }
  TDocumentContext = record
    DocumentNode: TAiRagGraphNode;
    MatchedParts: TArray<TDocumentPart>;
    AllParts: TArray<TDocumentPart>;
    Entities: TArray<TAiRagGraphNode>;
    Relations: TArray<TAiRagGraphEdge>;
    function GetFullText: string;
    function GetContextText: string;
  end;

  { TDocumentSearchResult - Resultado de b?squeda agrupado por documento }
  TDocumentSearchResult = record
    DocumentNode: TAiRagGraphNode;
    MatchingChunks: TArray<TAiEmbeddingNode>;
    BestScore: Double;
  end;

  TOnDocumentProgress = procedure(Sender: TObject; Current, Total: Integer;
    var Cancel: Boolean) of object;

  { TAiRagDocumentManager }

  TAiRagDocumentManager = class(TComponent)
  private
    FGraph: TAiRagGraph;
    FChat: TAiChat;
    FChunkSize: Integer;
    FChunkOverlapPct: Integer;
    FExtractionPrompt: TStrings;
    FDefaultDocumentLabel: string;
    FOnProgress: TOnDocumentProgress;

    procedure SetGraph(const Value: TAiRagGraph);
    procedure SetChat(const Value: TAiChat);
    procedure SetExtractionPrompt(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // Helpers internos
    function ChunkText(const AText: string; AChunkSize, AOverlapPct: Integer): TArray<string>;
    function GenerateContextualText(const APath, AText: string): string;
    function GenerateNodeEmbeddingText(const AName, ALabel: string;
      AProperties: TJSONObject): string;
    function ProcessParts(ADocNode: TAiRagGraphNode; APartsArray: TJSONArray;
      const AParentPath: string; ADepth: Integer;
      var AOrder: Integer; var ACancelled: Boolean): Integer;
    procedure AddChunkToDocument(ADocNode: TAiRagGraphNode;
      const AOriginalText, APath, AName, ALabel: string;
      ADepth, AOrder: Integer; AIsLeaf: Boolean;
      AProperties: TJSONObject);
    procedure ApplyProperties(ANode: TAiRagGraphNode; AProps: TJSONObject);
    procedure CheckGraphAssigned;
    procedure CheckEmbeddingsAvailable;
    function BuildChunkVector: TAiRAGVector;
    function BuildChunkVectorForDocument(ADocNode: TAiRagGraphNode): TAiRAGVector;
    function ChunkResultsToDocumentResults(AChunkVector: TAiRAGVector;
      ALimit: Integer): TArray<TDocumentSearchResult>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // --- Ingesta ---
    function AddDocument(const AName, AText: string;
      const ADocLabel: string = '';
      AProperties: TJSONObject = nil): TAiRagGraphNode;
    function AddDocumentFromJSON(const AJSON: string): TAiRagGraphNode; overload;
    function AddDocumentFromJSON(AJSONObj: TJSONObject): TAiRagGraphNode; overload;
    function AddDocumentsFromJSONArray(const AJSON: string): Integer; overload;
    function AddDocumentsFromJSONArray(AArr: TJSONArray): Integer; overload;

    // --- Extracci?n de relaciones (LLM) ---
    function ExtractRelationships(ADocNode: TAiRagGraphNode): Integer;

    // --- Extracci?n de entidades (LLM) - Fase 1 ---
    function ExtractEntitiesOnly(ADocNode: TAiRagGraphNode): Integer;

    // --- Extracci?n de relaciones con contexto (LLM) - Fase 2 ---
    function ExtractRelationshipsWithContext(ADocNode: TAiRagGraphNode): Integer;

    // --- Gesti?n manual de relaciones ---
    function AddEdge(AFromNode, AToNode: TAiRagGraphNode;
      const AEdgeLabel: string; const AEdgeName: string = ''): TAiRagGraphEdge;

    // --- Consulta de entidades ---
    function GetDocumentEntities(ADocNode: TAiRagGraphNode): TArray<TAiRagGraphNode>;

    // --- Recuperaci?n ---
    function GetDocumentContext(ADocNode: TAiRagGraphNode): TDocumentContext; overload;
    function GetDocumentContext(const ADocumentID: string): TDocumentContext; overload;
    function GetDocumentText(ADocNode: TAiRagGraphNode): string;
    function GetDocumentParts(ADocNode: TAiRagGraphNode): TArray<TDocumentPart>;

    // --- B?squeda ---
    function SearchDocuments(const APrompt: string; ALimit: Integer = 5;
      APrecision: Double = 0.5): TArray<TDocumentSearchResult>;
    function SearchDocumentsText(const APrompt: string; ALimit: Integer = 5;
      APrecision: Double = 0.5): string;

    // --- B?squeda VQL (Vector Query Language sobre chunks de documentos) ---
    function ExecuteVQL(const AVgqlQuery: string): string; overload;
    function ExecuteVQL(const AVgqlQuery: string;
      out AResultVector: TAiRAGVector): string; overload;
    function ExecuteDocumentVQL(const AVgqlQuery: string;
      ADocNode: TAiRagGraphNode): string; overload;
    function ExecuteDocumentVQL(const AVgqlQuery: string;
      ADocNode: TAiRagGraphNode;
      out AResultVector: TAiRAGVector): string; overload;
    function SearchDocumentsByVQL(const AVgqlQuery: string;
      ALimit: Integer = 5): TArray<TDocumentSearchResult>;

    // --- B?squeda GQL (Graph Query Language sobre grafo de documentos) ---
    function ExecuteGQL(const AGqlQuery: string): string; overload;
    function ExecuteGQL(const AGqlQuery: string;
      out AResultObjects: TArray<TDictionary<string, TObject>>;
      ADepth: Integer = 0): string; overload;

    // --- Utilidades ---
    function GetAllDocuments: TArray<TAiRagGraphNode>;
    procedure DeleteDocument(ADocNode: TAiRagGraphNode);

  published
    property Graph: TAiRagGraph read FGraph write SetGraph;
    property Chat: TAiChat read FChat write SetChat;
    property ChunkSize: Integer read FChunkSize write FChunkSize default 1000;
    property ChunkOverlapPct: Integer read FChunkOverlapPct write FChunkOverlapPct default 10;
    property ExtractionPrompt: TStrings read FExtractionPrompt write SetExtractionPrompt;
    property DefaultDocumentLabel: string read FDefaultDocumentLabel write FDefaultDocumentLabel;
    property OnProgress: TOnDocumentProgress read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiRagDocumentManager]);
end;

{ TDocumentContext }

function TDocumentContext.GetFullText: string;
var
  SB: TStringBuilder;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    for I := 0 to High(AllParts) do
    begin
      if I > 0 then
        SB.AppendLine;
      SB.Append(AllParts[I].Text);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TDocumentContext.GetContextText: string;
var
  SB: TStringBuilder;
  Part: TDocumentPart;
  Entity: TAiRagGraphNode;
  Rel: TAiRagGraphEdge;
  Pair: TPair<string, Variant>;
  IsFirst: Boolean;
begin
  SB := TStringBuilder.Create;
  try
    // Encabezado del documento
    if DocumentNode <> nil then
    begin
      SB.AppendFormat('### DOCUMENT: %s (%s) ###', [DocumentNode.Name, DocumentNode.NodeLabel]);
      SB.AppendLine;
      SB.AppendLine;
    end;

    // Partes ordenadas
    if Length(AllParts) > 0 then
    begin
      SB.AppendLine('--- DOCUMENT PARTS ---');
      for Part in AllParts do
      begin
        if Part.Path <> '' then
          SB.AppendFormat('[%s] ', [Part.Path]);
        SB.AppendLine(Part.Text);
      end;
      SB.AppendLine;
    end;

    // Entidades extra?das
    if Length(Entities) > 0 then
    begin
      SB.AppendLine('--- ENTITIES ---');
      for Entity in Entities do
      begin
        SB.AppendFormat('- %s (%s)', [Entity.Name, Entity.NodeLabel]);
        if Entity.MetaData.InternalDictionary.Count > 0 then
        begin
          SB.Append(' {');
          IsFirst := True;
          for Pair in Entity.MetaData.InternalDictionary do
          begin
            if not IsFirst then
              SB.Append(', ');
            SB.AppendFormat('%s: %s', [Pair.Key, VarToStr(Pair.Value)]);
            IsFirst := False;
          end;
          SB.Append('}');
        end;
        SB.AppendLine;
      end;
      SB.AppendLine;
    end;

    // Relaciones
    if Length(Relations) > 0 then
    begin
      SB.AppendLine('--- RELATIONSHIPS ---');
      for Rel in Relations do
      begin
        SB.AppendFormat('(%s)-[%s]->(%s)',
          [Rel.FromNode.Name, Rel.EdgeLabel, Rel.ToNode.Name]);
        SB.AppendLine;
      end;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

{ TAiRagDocumentManager }

constructor TAiRagDocumentManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGraph := nil;
  FChat := nil;
  FChunkSize := 1000;
  FChunkOverlapPct := 10;
  FDefaultDocumentLabel := NODELABEL_DOCUMENT;
  FExtractionPrompt := TStringList.Create;

  // Prompt por defecto para extracci?n de relaciones
  FExtractionPrompt.Text :=
    'Analyze the following text and extract all entities and relationships.' + sLineBreak +
    'Return a JSON array of triplets with this format:' + sLineBreak +
    '[' + sLineBreak +
    '  {' + sLineBreak +
    '    "subject": {"name": "EntityA", "nodeLabel": "Type", "properties": {}}, ' + sLineBreak +
    '    "predicate": {"edgeLabel": "RELATION", "name": "description", "properties": {}},' + sLineBreak +
    '    "object": {"name": "EntityB", "nodeLabel": "Type", "properties": {}}' + sLineBreak +
    '  }' + sLineBreak +
    ']' + sLineBreak +
    'Only return the JSON array, no additional text.' + sLineBreak +
    sLineBreak +
    'TEXT:' + sLineBreak;
end;

destructor TAiRagDocumentManager.Destroy;
begin
  FExtractionPrompt.Free;
  inherited;
end;

procedure TAiRagDocumentManager.SetGraph(const Value: TAiRagGraph);
begin
  if FGraph <> Value then
  begin
    FGraph := Value;
    if FGraph <> nil then
      FGraph.FreeNotification(Self);
  end;
end;

procedure TAiRagDocumentManager.SetChat(const Value: TAiChat);
begin
  if FChat <> Value then
  begin
    FChat := Value;
    if FChat <> nil then
      FChat.FreeNotification(Self);
  end;
end;

procedure TAiRagDocumentManager.SetExtractionPrompt(const Value: TStrings);
begin
  FExtractionPrompt.Assign(Value);
end;

procedure TAiRagDocumentManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FGraph then
      FGraph := nil;
    if AComponent = FChat then
      FChat := nil;
  end;
end;

procedure TAiRagDocumentManager.CheckGraphAssigned;
begin
  if FGraph = nil then
    raise Exception.Create('Graph property is not assigned to the document manager.');
end;

procedure TAiRagDocumentManager.CheckEmbeddingsAvailable;
begin
  CheckGraphAssigned;
  if FGraph.Embeddings = nil then
    raise Exception.Create('Graph.Embeddings is not assigned. An embeddings engine is required.');
end;

// ---------------------------------------------------------------------------
// ChunkText - Fragmentaci?n inteligente de texto
// Port del algoritmo de TAiRAGVector.AddItemsFromPlainText
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ChunkText(const AText: string;
  AChunkSize, AOverlapPct: Integer): TArray<string>;
const
  TOLERANCE_PCT = 0.15;
var
  TextTrimmed: string;
  TotalLen, StartPos, IdealEnd, MaxEnd, MinEnd: Integer;
  CutPos, NextStartPos: Integer;
  OverlapChars: Integer;
  ChunkStr: string;
  ResultList: TList<string>;

  function FindSmartCut(LimitStart, LimitEnd: Integer): Integer;
  var
    K: Integer;
    LastPeriod, LastSpace: Integer;
    C: Char;
  begin
    LastPeriod := -1;
    LastSpace := -1;

    for K := LimitEnd downto LimitStart do
    begin
      if K > TotalLen then
        Continue;

      C := TextTrimmed[K];

      // Prioridad 1: Salto de l?nea
      if (C = #10) or (C = #13) then
      begin
        Result := K;
        Exit;
      end;

      // Prioridad 2: Puntuaci?n de fin de frase
      if CharInSet(C, ['.', '?', '!', ';']) and (LastPeriod = -1) then
        LastPeriod := K;

      // Prioridad 3: Espacio
      if (C = ' ') and (LastSpace = -1) then
        LastSpace := K;
    end;

    if LastPeriod <> -1 then
      Result := LastPeriod
    else if LastSpace <> -1 then
      Result := LastSpace
    else
      Result := -1;
  end;

begin
  Result := [];

  if AChunkSize <= 10 then
    raise Exception.Create('Chunk size is too small.');

  if (AOverlapPct < 0) or (AOverlapPct >= 100) then
    raise Exception.Create('Overlap percentage must be between 0 and 99.');

  TextTrimmed := AText.Trim;
  TotalLen := TextTrimmed.Length;

  if TotalLen = 0 then
    Exit;

  OverlapChars := Round(AChunkSize * (AOverlapPct / 100));
  if OverlapChars >= AChunkSize then
    OverlapChars := AChunkSize - 1;

  ResultList := TList<string>.Create;
  try
    StartPos := 1;

    while StartPos <= TotalLen do
    begin
      IdealEnd := StartPos + AChunkSize;

      // Si lo que queda es menor que el chunk, tomamos todo
      if IdealEnd > TotalLen then
      begin
        ChunkStr := Copy(TextTrimmed, StartPos, TotalLen - StartPos + 1).Trim;
        if ChunkStr <> '' then
          ResultList.Add(ChunkStr);
        Break;
      end;

      // Rango de tolerancia
      MinEnd := IdealEnd - Round(AChunkSize * TOLERANCE_PCT);
      MaxEnd := IdealEnd + Round(AChunkSize * TOLERANCE_PCT);

      if MaxEnd > TotalLen then
        MaxEnd := TotalLen;
      if MinEnd < StartPos then
        MinEnd := StartPos;

      CutPos := FindSmartCut(MinEnd, MaxEnd);

      if CutPos = -1 then
        CutPos := IdealEnd;

      ChunkStr := Copy(TextTrimmed, StartPos, CutPos - StartPos + 1).Trim;

      if ChunkStr <> '' then
        ResultList.Add(ChunkStr);

      // C?lculo del siguiente paso con solapamiento
      NextStartPos := (CutPos + 1) - OverlapChars;

      if NextStartPos <= StartPos then
        NextStartPos := StartPos + 1;

      // Ajuste de l?mite de palabra
      if (NextStartPos > 1) and (NextStartPos < TotalLen) then
      begin
        while (NextStartPos > StartPos + 1) and
          (not CharInSet(TextTrimmed[NextStartPos - 1], [' ', #13, #10, '.', ',', ';', ':', '!', '?'])) do
        begin
          Dec(NextStartPos);
        end;
      end;

      StartPos := NextStartPos;
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TAiRagDocumentManager.GenerateContextualText(const APath, AText: string): string;
begin
  if APath.Trim.IsEmpty then
    Result := AText
  else
    Result := APath + ': ' + AText;
end;

function TAiRagDocumentManager.GenerateNodeEmbeddingText(const AName, ALabel: string;
  AProperties: TJSONObject): string;
var
  SB: TStringBuilder;
  Pair: TJSONPair;
  PropValue: string;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendFormat('Entidad: %s. Categoria: %s.', [AName, ALabel]);

    if Assigned(AProperties) then
    begin
      for Pair in AProperties do
      begin
        if Pair.JsonValue is TJSONString then
          PropValue := Pair.JsonValue.Value
        else
          PropValue := Pair.JsonValue.ToJSON;
        SB.AppendFormat(' La propiedad %s tiene el valor %s.', [Pair.JsonString.Value, PropValue]);
      end;
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TAiRagDocumentManager.ApplyProperties(ANode: TAiRagGraphNode; AProps: TJSONObject);
var
  Pair: TJSONPair;
begin
  if (AProps = nil) or (ANode = nil) then
    Exit;

  for Pair in AProps do
    ANode.Properties[Pair.JsonString.Value] := JSONValueToVariant(Pair.JsonValue);
end;

procedure TAiRagDocumentManager.AddChunkToDocument(ADocNode: TAiRagGraphNode;
  const AOriginalText, APath, AName, ALabel: string;
  ADepth, AOrder: Integer; AIsLeaf: Boolean;
  AProperties: TJSONObject);
var
  ContextualText: string;
  EmbData: TAiEmbeddingData;
  Chunk: TAiEmbeddingNode;
  LEmbeddings: TAiEmbeddingsCore;
begin
  LEmbeddings := FGraph.Embeddings;

  // Texto contextualizado para embedding (incluye path jer?rquico)
  ContextualText := GenerateContextualText(APath, AOriginalText);

  // Generar embedding
  EmbData := LEmbeddings.CreateEmbedding(ContextualText, 'user');

  // Crear chunk en el nodo documento
  Chunk := ADocNode.AddChunk(ContextualText, EmbData);

  // Metadata del chunk
  Chunk.MetaData[DOCMETA_ORDER] := AOrder;
  Chunk.MetaData[DOCMETA_NAME] := AName;
  Chunk.MetaData[DOCMETA_LABEL] := ALabel;
  Chunk.MetaData[DOCMETA_DEPTH] := ADepth;
  Chunk.MetaData[DOCMETA_PATH] := APath;
  Chunk.MetaData[DOCMETA_IS_LEAF] := AIsLeaf;
  Chunk.MetaData[DOCMETA_DOCUMENT_ID] := ADocNode.ID;
  Chunk.MetaData[DOCMETA_DOCUMENT_NAME] := ADocNode.Name;
  Chunk.MetaData[DOCMETA_ORIGINAL_TEXT] := AOriginalText;
end;

// ---------------------------------------------------------------------------
// AddDocument - Ingesta de documento desde texto plano
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.AddDocument(const AName, AText: string;
  const ADocLabel: string; AProperties: TJSONObject): TAiRagGraphNode;
var
  DocLabel: string;
  Chunks: TArray<string>;
  I: Integer;
  LEmbeddings: TAiEmbeddingsCore;
  NodeText: string;
  Cancel: Boolean;
begin
  CheckEmbeddingsAvailable;

  LEmbeddings := FGraph.Embeddings;

  if ADocLabel <> '' then
    DocLabel := ADocLabel
  else
    DocLabel := FDefaultDocumentLabel;

  // Crear nodo documento
  Result := FGraph.NewNode(TGUID.NewGuid.ToString, DocLabel, AName);

  // Texto descriptivo para embedding del nodo
  NodeText := GenerateNodeEmbeddingText(AName, DocLabel, AProperties);
  Result.Text := NodeText;
  Result.Model := LEmbeddings.Model;
  Result.Data := LEmbeddings.CreateEmbedding(NodeText, 'user');

  // Aplicar propiedades opcionales
  if Assigned(AProperties) then
    ApplyProperties(Result, AProperties);

  // Guardar texto original completo en TagString para reconstrucci?n
  Result.MetaData.TagString := AText;

  FGraph.BeginUpdate;
  try
    FGraph.AddNode(Result);

    // Fragmentar el texto
    Chunks := ChunkText(AText, FChunkSize, FChunkOverlapPct);
    Cancel := False;

    for I := 0 to High(Chunks) do
    begin
      if Assigned(FOnProgress) then
      begin
        FOnProgress(Self, I + 1, Length(Chunks), Cancel);
        if Cancel then
          Break;
      end;

      AddChunkToDocument(Result, Chunks[I], AName, AName, DocLabel,
        0, I, True, nil);
    end;
  finally
    FGraph.EndUpdate;
  end;
end;

// ---------------------------------------------------------------------------
// AddDocumentFromJSON - Ingesta desde JSON jer?rquico
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.AddDocumentFromJSON(const AJSON: string): TAiRagGraphNode;
var
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue(AJSON);
  try
    if not (JsonValue is TJSONObject) then
      raise Exception.Create('Input JSON is not a valid JSON object.');

    Result := AddDocumentFromJSON(JsonValue as TJSONObject);
  finally
    JsonValue.Free;
  end;
end;

function TAiRagDocumentManager.AddDocumentFromJSON(AJSONObj: TJSONObject): TAiRagGraphNode;
var
  DocName, DocLabel, DocText: string;
  DocProps: TJSONObject;
  DocParts: TJSONArray;
  LEmbeddings: TAiEmbeddingsCore;
  NodeText: string;
  Order: Integer;
  Cancel: Boolean;
  Chunks: TArray<string>;
  I: Integer;
begin
  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  // Extraer campos requeridos
  DocName := AJSONObj.GetValue<string>('name', '');
  DocLabel := AJSONObj.GetValue<string>('label', '');

  if DocName.Trim.IsEmpty then
    raise Exception.Create('Document JSON must have a "name" field.');
  if DocLabel.Trim.IsEmpty then
    DocLabel := FDefaultDocumentLabel;

  DocText := AJSONObj.GetValue<string>('text', '');
  DocProps := AJSONObj.GetValue<TJSONObject>('properties', nil);
  DocParts := AJSONObj.GetValue<TJSONArray>('parts', nil);

  // Crear nodo documento
  Result := FGraph.NewNode(TGUID.NewGuid.ToString, DocLabel, DocName);

  // Texto para embedding del nodo
  NodeText := GenerateNodeEmbeddingText(DocName, DocLabel, DocProps);
  Result.Text := NodeText;
  Result.Model := LEmbeddings.Model;
  Result.Data := LEmbeddings.CreateEmbedding(NodeText, 'user');

  // Propiedades opcionales
  if Assigned(DocProps) then
    ApplyProperties(Result, DocProps);

  // Guardar texto completo original
  if DocText <> '' then
    Result.MetaData.TagString := DocText;

  FGraph.BeginUpdate;
  try
    FGraph.AddNode(Result);

    Order := 0;
    Cancel := False;

    if Assigned(DocParts) and (DocParts.Count > 0) then
    begin
      // Procesar estructura jer?rquica
      ProcessParts(Result, DocParts, DocName, 1, Order, Cancel);
    end
    else if DocText <> '' then
    begin
      // Documento sin partes: fragmentar texto plano
      if DocText.Length <= Round(FChunkSize * 1.5) then
      begin
        AddChunkToDocument(Result, DocText, DocName, DocName, DocLabel,
          0, Order, True, nil);
      end
      else
      begin
        Chunks := ChunkText(DocText, FChunkSize, FChunkOverlapPct);
        for I := 0 to High(Chunks) do
        begin
          if Assigned(FOnProgress) then
          begin
            FOnProgress(Self, I + 1, Length(Chunks), Cancel);
            if Cancel then
              Break;
          end;

          AddChunkToDocument(Result, Chunks[I], DocName, DocName, DocLabel,
            0, I, True, nil);
        end;
      end;
    end;
  finally
    FGraph.EndUpdate;
  end;
end;

function TAiRagDocumentManager.AddDocumentsFromJSONArray(const AJSON: string): Integer;
var
  JsonValue: TJSONValue;
begin
  JsonValue := TJSONObject.ParseJSONValue(AJSON);
  try
    if not (JsonValue is TJSONArray) then
      raise Exception.Create('Input JSON is not a valid JSON array.');

    Result := AddDocumentsFromJSONArray(JsonValue as TJSONArray);
  finally
    JsonValue.Free;
  end;
end;

function TAiRagDocumentManager.AddDocumentsFromJSONArray(AArr: TJSONArray): Integer;
var
  Item: TJSONValue;
  Cancel: Boolean;
begin
  Result := 0;
  Cancel := False;

  for Item in AArr do
  begin
    if not (Item is TJSONObject) then
      Continue;

    if Assigned(FOnProgress) then
    begin
      FOnProgress(Self, Result + 1, AArr.Count, Cancel);
      if Cancel then
        Break;
    end;

    AddDocumentFromJSON(Item as TJSONObject);
    Inc(Result);
  end;
end;

// ---------------------------------------------------------------------------
// ProcessParts - Procesamiento recursivo de partes jer?rquicas
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ProcessParts(ADocNode: TAiRagGraphNode;
  APartsArray: TJSONArray; const AParentPath: string; ADepth: Integer;
  var AOrder: Integer; var ACancelled: Boolean): Integer;
var
  PartValue: TJSONValue;
  PartObj: TJSONObject;
  PartName, PartLabel, PartText, PartPath: string;
  PartProps: TJSONObject;
  SubParts: TJSONArray;
  Chunks: TArray<string>;
  I: Integer;
begin
  Result := 0;

  for PartValue in APartsArray do
  begin
    if ACancelled then
      Break;

    if not (PartValue is TJSONObject) then
      Continue;

    PartObj := PartValue as TJSONObject;

    PartName := PartObj.GetValue<string>('name', '');
    PartLabel := PartObj.GetValue<string>('label', '');
    PartText := PartObj.GetValue<string>('text', '');
    PartProps := PartObj.GetValue<TJSONObject>('properties', nil);
    SubParts := PartObj.GetValue<TJSONArray>('parts', nil);

    if PartName.Trim.IsEmpty then
      Continue;
    if PartLabel.Trim.IsEmpty then
      PartLabel := 'Section';

    // Construir path jer?rquico
    if AParentPath <> '' then
      PartPath := AParentPath + ' > ' + PartName
    else
      PartPath := PartName;

    if Assigned(SubParts) and (SubParts.Count > 0) then
    begin
      // Nodo intermedio con sub-partes
      if PartText <> '' then
      begin
        // Texto descriptivo del nodo intermedio (no es hoja)
        AddChunkToDocument(ADocNode, PartText, PartPath, PartName, PartLabel,
          ADepth, AOrder, False, PartProps);
        Inc(AOrder);
        Inc(Result);
      end;

      // Recursi?n en sub-partes
      Result := Result + ProcessParts(ADocNode, SubParts, PartPath,
        ADepth + 1, AOrder, ACancelled);
    end
    else if PartText <> '' then
    begin
      // Nodo hoja: contiene texto final
      if PartText.Length <= Round(FChunkSize * 1.5) then
      begin
        // Chunk ?nico
        AddChunkToDocument(ADocNode, PartText, PartPath, PartName, PartLabel,
          ADepth, AOrder, True, PartProps);
        Inc(AOrder);
        Inc(Result);
      end
      else
      begin
        // Dividir en m?ltiples chunks
        Chunks := ChunkText(PartText, FChunkSize, FChunkOverlapPct);
        for I := 0 to High(Chunks) do
        begin
          if Assigned(FOnProgress) then
          begin
            FOnProgress(Self, AOrder + 1, -1, ACancelled);
            if ACancelled then
              Break;
          end;

          AddChunkToDocument(ADocNode, Chunks[I], PartPath, PartName, PartLabel,
            ADepth, AOrder, True, PartProps);
          Inc(AOrder);
          Inc(Result);
        end;
      end;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// ExtractRelationships - Extracci?n de relaciones v?a LLM
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExtractRelationships(ADocNode: TAiRagGraphNode): Integer;
var
  Chunk: TAiEmbeddingNode;
  ChunkIdx: Integer;
  Prompt, Response: string;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  TripletValue: TJSONValue;
  TripletObj: TJSONObject;
  SubjectObj, PredicateObj, ObjectObj: TJSONObject;
  SubjectName, SubjectLabel, ObjectName, ObjectLabel: string;
  EdgeLabel, EdgeName: string;
  SubjectNode, ObjectNode: TAiRagGraphNode;
  NewEdge, ExtractedEdge: TAiRagGraphEdge;
  LEmbeddings: TAiEmbeddingsCore;
  TextToEmbed: string;
  Cancel: Boolean;
  EntitySet: TDictionary<string, TAiRagGraphNode>;
  EntityKey: string;
begin
  Result := 0;
  Cancel := False;

  if FChat = nil then
    raise Exception.Create('Chat property is not assigned. LLM is required for relationship extraction.');

  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  EntitySet := TDictionary<string, TAiRagGraphNode>.Create;
  try
    FGraph.BeginUpdate;
    try
      for ChunkIdx := 0 to ADocNode.Chunks.Count - 1 do
      begin
        Chunk := ADocNode.Chunks[ChunkIdx];

        if Assigned(FOnProgress) then
        begin
          FOnProgress(Self, ChunkIdx + 1, ADocNode.Chunks.Count, Cancel);
          if Cancel then
            Break;
        end;

        // Construir prompt con contexto
        Prompt := FExtractionPrompt.Text + Chunk.MetaData.Get(DOCMETA_ORIGINAL_TEXT, Chunk.Text);

        // Llamar al LLM sincr?nicamente
        FChat.Asynchronous := False;
        Response := FChat.AddMessageAndRun(Prompt, 'user', nil);

        if Response.Trim.IsEmpty then
          Continue;

        // Limpiar respuesta: extraer JSON del response
        Response := Response.Trim;
        // Buscar el inicio del array JSON
        var StartIdx := Pos('[', Response);
        var EndIdx := Length(Response);
        // Buscar el ?ltimo ']'
        while (EndIdx > 0) and (Response[EndIdx] <> ']') do
          Dec(EndIdx);

        if (StartIdx > 0) and (EndIdx >= StartIdx) then
          Response := Copy(Response, StartIdx, EndIdx - StartIdx + 1)
        else
          Continue;

        // Parsear respuesta JSON
        JsonValue := TJSONObject.ParseJSONValue(Response);
        if JsonValue = nil then
          Continue;

        try
          if not (JsonValue is TJSONArray) then
            Continue;

          JsonArray := JsonValue as TJSONArray;

          for TripletValue in JsonArray do
          begin
            if not (TripletValue is TJSONObject) then
              Continue;

            TripletObj := TripletValue as TJSONObject;
            SubjectObj := TripletObj.GetValue<TJSONObject>('subject', nil);
            PredicateObj := TripletObj.GetValue<TJSONObject>('predicate', nil);
            ObjectObj := TripletObj.GetValue<TJSONObject>('object', nil);

            if (SubjectObj = nil) or (PredicateObj = nil) or (ObjectObj = nil) then
              Continue;

            // Extraer datos b?sicos
            SubjectName := SubjectObj.GetValue<string>('name', '');
            SubjectLabel := SubjectObj.GetValue<string>('nodeLabel', 'Entity');
            ObjectName := ObjectObj.GetValue<string>('name', '');
            ObjectLabel := ObjectObj.GetValue<string>('nodeLabel', 'Entity');
            EdgeLabel := PredicateObj.GetValue<string>('edgeLabel', 'related_to');
            EdgeName := PredicateObj.GetValue<string>('name', '');

            if SubjectName.Trim.IsEmpty or ObjectName.Trim.IsEmpty then
              Continue;

            // GetOrCreate para Subject
            EntityKey := UpperCase(SubjectName + '|' + SubjectLabel);
            if not EntitySet.TryGetValue(EntityKey, SubjectNode) then
            begin
              SubjectNode := FGraph.FindNodeByName(SubjectName, SubjectLabel);
              if SubjectNode = nil then
              begin
                SubjectNode := FGraph.NewNode(TGUID.NewGuid.ToString, SubjectLabel, SubjectName);
                TextToEmbed := GenerateNodeEmbeddingText(SubjectName, SubjectLabel, SubjectObj.GetValue<TJSONObject>('properties', nil));
                SubjectNode.Text := TextToEmbed;
                SubjectNode.Model := LEmbeddings.Model;
                SubjectNode.Data := LEmbeddings.CreateEmbedding(TextToEmbed, 'user');
                FGraph.AddNode(SubjectNode);
              end;
              EntitySet.Add(EntityKey, SubjectNode);
            end;

            // GetOrCreate para Object
            EntityKey := UpperCase(ObjectName + '|' + ObjectLabel);
            if not EntitySet.TryGetValue(EntityKey, ObjectNode) then
            begin
              ObjectNode := FGraph.FindNodeByName(ObjectName, ObjectLabel);
              if ObjectNode = nil then
              begin
                ObjectNode := FGraph.NewNode(TGUID.NewGuid.ToString, ObjectLabel, ObjectName);
                TextToEmbed := GenerateNodeEmbeddingText(ObjectName, ObjectLabel, ObjectObj.GetValue<TJSONObject>('properties', nil));
                ObjectNode.Text := TextToEmbed;
                ObjectNode.Model := LEmbeddings.Model;
                ObjectNode.Data := LEmbeddings.CreateEmbedding(TextToEmbed, 'user');
                FGraph.AddNode(ObjectNode);
              end;
              EntitySet.Add(EntityKey, ObjectNode);
            end;

            // Crear arista de relaci?n entre entidades
            if FGraph.FindEdge(SubjectNode, ObjectNode, EdgeLabel) = nil then
            begin
              NewEdge := FGraph.NewEdge(SubjectNode, ObjectNode,
                TGUID.NewGuid.ToString, EdgeLabel, EdgeName);
              NewEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;

              TextToEmbed := SubjectNode.Name + ' ' + EdgeLabel + ' ' + ObjectNode.Name;
              NewEdge.Data := LEmbeddings.CreateEmbedding(TextToEmbed, '');

              FGraph.AddEdge(NewEdge);
              Inc(Result);
            end;

            // Crear arista EXTRACTED_FROM (entidad -> documento) si no existe
            if FGraph.FindEdge(SubjectNode, ADocNode, EDGE_EXTRACTED_FROM) = nil then
            begin
              ExtractedEdge := FGraph.NewEdge(SubjectNode, ADocNode,
                TGUID.NewGuid.ToString, EDGE_EXTRACTED_FROM, '');
              ExtractedEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;
              FGraph.AddEdge(ExtractedEdge);
            end;

            if FGraph.FindEdge(ObjectNode, ADocNode, EDGE_EXTRACTED_FROM) = nil then
            begin
              ExtractedEdge := FGraph.NewEdge(ObjectNode, ADocNode,
                TGUID.NewGuid.ToString, EDGE_EXTRACTED_FROM, '');
              ExtractedEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;
              FGraph.AddEdge(ExtractedEdge);
            end;
          end;
        finally
          JsonValue.Free;
        end;
      end;
    finally
      FGraph.EndUpdate;
    end;
  finally
    EntitySet.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExtractEntitiesOnly - Extracci?n de entidades (Fase 1)
// Procesa cada chunk del documento y extrae solo entidades, sin relaciones.
// Dise?ado para procesamiento por fases de documentos grandes.
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExtractEntitiesOnly(ADocNode: TAiRagGraphNode): Integer;
var
  Chunk: TAiEmbeddingNode;
  ChunkIdx: Integer;
  Prompt, Response, ChunkText: string;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  EntityValue: TJSONValue;
  EntityObj: TJSONObject;
  EntityName, EntityLabel: string;
  EntityNode: TAiRagGraphNode;
  LEmbeddings: TAiEmbeddingsCore;
  TextToEmbed: string;
  Cancel: Boolean;
  EntitySet: TDictionary<string, TAiRagGraphNode>;
  EntityKey: string;
  ExtractedEdge: TAiRagGraphEdge;
begin
  Result := 0;
  Cancel := False;

  if FChat = nil then
    raise Exception.Create('Chat property is not assigned. LLM is required for entity extraction.');

  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  EntitySet := TDictionary<string, TAiRagGraphNode>.Create;
  try
    FGraph.BeginUpdate;
    try
      for ChunkIdx := 0 to ADocNode.Chunks.Count - 1 do
      begin
        Chunk := ADocNode.Chunks[ChunkIdx];

        if Assigned(FOnProgress) then
        begin
          FOnProgress(Self, ChunkIdx + 1, ADocNode.Chunks.Count, Cancel);
          if Cancel then
            Break;
        end;

        ChunkText := VarToStr(Chunk.MetaData.Get(DOCMETA_ORIGINAL_TEXT, Chunk.Text));

        Prompt :=
          'Analyze the following text and extract all entities ' +
          '(people, organizations, places, concepts, events, products, etc.).' + sLineBreak +
          'Return a JSON array with this format:' + sLineBreak +
          '[' + sLineBreak +
          '  {"name": "EntityName", "nodeLabel": "Type", "properties": {"key": "value"}}' + sLineBreak +
          ']' + sLineBreak +
          'Rules:' + sLineBreak +
          '- nodeLabel should be a general category (Person, Organization, Place, Concept, Event, Product, etc.)' + sLineBreak +
          '- properties should include relevant attributes mentioned in the text' + sLineBreak +
          '- Only return the JSON array, no additional text.' + sLineBreak +
          sLineBreak +
          'TEXT:' + sLineBreak +
          ChunkText;

        FChat.Asynchronous := False;
        FChat.Messages.Clear;
        Response := FChat.AddMessageAndRun(Prompt, 'user', nil);

        if Response.Trim.IsEmpty then
          Continue;

        // Extraer array JSON de la respuesta
        Response := Response.Trim;
        var StartIdx := Pos('[', Response);
        var EndIdx := Length(Response);
        while (EndIdx > 0) and (Response[EndIdx] <> ']') do
          Dec(EndIdx);

        if (StartIdx > 0) and (EndIdx >= StartIdx) then
          Response := Copy(Response, StartIdx, EndIdx - StartIdx + 1)
        else
          Continue;

        JsonValue := TJSONObject.ParseJSONValue(Response);
        if JsonValue = nil then
          Continue;

        try
          if not (JsonValue is TJSONArray) then
            Continue;

          JsonArray := JsonValue as TJSONArray;

          for EntityValue in JsonArray do
          begin
            if not (EntityValue is TJSONObject) then
              Continue;

            EntityObj := EntityValue as TJSONObject;
            EntityName := EntityObj.GetValue<string>('name', '');
            EntityLabel := EntityObj.GetValue<string>('nodeLabel', 'Entity');

            if EntityName.Trim.IsEmpty then
              Continue;

            // GetOrCreate entidad
            EntityKey := UpperCase(EntityName + '|' + EntityLabel);
            if not EntitySet.TryGetValue(EntityKey, EntityNode) then
            begin
              EntityNode := FGraph.FindNodeByName(EntityName, EntityLabel);
              if EntityNode = nil then
              begin
                EntityNode := FGraph.NewNode(TGUID.NewGuid.ToString, EntityLabel, EntityName);
                TextToEmbed := GenerateNodeEmbeddingText(EntityName, EntityLabel,
                  EntityObj.GetValue<TJSONObject>('properties', nil));
                EntityNode.Text := TextToEmbed;
                EntityNode.Model := LEmbeddings.Model;
                EntityNode.Data := LEmbeddings.CreateEmbedding(TextToEmbed, 'user');
                ApplyProperties(EntityNode, EntityObj.GetValue<TJSONObject>('properties', nil));
                FGraph.AddNode(EntityNode);
                Inc(Result);
              end;
              EntitySet.Add(EntityKey, EntityNode);
            end;

            // Crear arista EXTRACTED_FROM si no existe
            if FGraph.FindEdge(EntityNode, ADocNode, EDGE_EXTRACTED_FROM) = nil then
            begin
              ExtractedEdge := FGraph.NewEdge(EntityNode, ADocNode,
                TGUID.NewGuid.ToString, EDGE_EXTRACTED_FROM, '');
              ExtractedEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;
              FGraph.AddEdge(ExtractedEdge);
            end;
          end;
        finally
          JsonValue.Free;
        end;
      end;
    finally
      FGraph.EndUpdate;
    end;
  finally
    EntitySet.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExtractRelationshipsWithContext - Extracci?n de relaciones con contexto (Fase 2)
// Incluye la lista de entidades conocidas en el prompt para que el LLM pueda
// descubrir relaciones cross-chunk/cross-page.
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExtractRelationshipsWithContext(
  ADocNode: TAiRagGraphNode): Integer;
var
  Chunk: TAiEmbeddingNode;
  ChunkIdx, I: Integer;
  EntityListText, Prompt, Response, ChunkText: string;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  TripletValue: TJSONValue;
  TripletObj: TJSONObject;
  SubjectObj, PredicateObj, ObjectObj: TJSONObject;
  SubjectName, SubjectLabel, ObjectName, ObjectLabel: string;
  EdgeLabel, EdgeName: string;
  SubjectNode, ObjectNode: TAiRagGraphNode;
  NewEdge, ExtractedEdge: TAiRagGraphEdge;
  LEmbeddings: TAiEmbeddingsCore;
  TextToEmbed: string;
  Cancel: Boolean;
  EntitySet: TDictionary<string, TAiRagGraphNode>;
  EntityKey: string;
  SB: TStringBuilder;
  Node: TAiRagGraphNode;
begin
  Result := 0;
  Cancel := False;

  if FChat = nil then
    raise Exception.Create('Chat property is not assigned. LLM is required for relationship extraction.');

  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  // Construir lista de todas las entidades conocidas en el grafo
  SB := TStringBuilder.Create;
  try
    for I := 0 to FGraph.Nodes.Items.Count - 1 do
    begin
      if not (FGraph.Nodes.Items[I] is TAiRagGraphNode) then
        Continue;

      Node := TAiRagGraphNode(FGraph.Nodes.Items[I]);

      // Saltar nodos de documento
      if SameText(Node.NodeLabel, FDefaultDocumentLabel) then
        Continue;
      if (Node.Chunks.Count > 0) and Node.Chunks[0].MetaData.Has(DOCMETA_DOCUMENT_ID) then
        Continue;

      SB.AppendFormat('- %s (%s)', [Node.Name, Node.NodeLabel]);
      SB.AppendLine;
    end;
    EntityListText := SB.ToString;
  finally
    SB.Free;
  end;

  EntitySet := TDictionary<string, TAiRagGraphNode>.Create;
  try
    // Pre-poblar EntitySet con entidades existentes
    for I := 0 to FGraph.Nodes.Items.Count - 1 do
    begin
      if not (FGraph.Nodes.Items[I] is TAiRagGraphNode) then
        Continue;

      Node := TAiRagGraphNode(FGraph.Nodes.Items[I]);
      if SameText(Node.NodeLabel, FDefaultDocumentLabel) then
        Continue;
      if (Node.Chunks.Count > 0) and Node.Chunks[0].MetaData.Has(DOCMETA_DOCUMENT_ID) then
        Continue;

      EntityKey := UpperCase(Node.Name + '|' + Node.NodeLabel);
      if not EntitySet.ContainsKey(EntityKey) then
        EntitySet.Add(EntityKey, Node);
    end;

    FGraph.BeginUpdate;
    try
      for ChunkIdx := 0 to ADocNode.Chunks.Count - 1 do
      begin
        Chunk := ADocNode.Chunks[ChunkIdx];

        if Assigned(FOnProgress) then
        begin
          FOnProgress(Self, ChunkIdx + 1, ADocNode.Chunks.Count, Cancel);
          if Cancel then
            Break;
        end;

        ChunkText := VarToStr(Chunk.MetaData.Get(DOCMETA_ORIGINAL_TEXT, Chunk.Text));

        // Construir prompt con contexto de entidades
        Prompt :=
          'Analyze the following text and extract all relationships between entities.' + sLineBreak +
          sLineBreak;

        if EntityListText <> '' then
        begin
          Prompt := Prompt +
            'Here are the entities already known in the knowledge graph:' + sLineBreak +
            EntityListText + sLineBreak +
            'IMPORTANT: If an entity in the text matches one from the list above, ' +
            'use the EXACT same name and nodeLabel from the list.' + sLineBreak +
            'You may also discover NEW entities not in the list.' + sLineBreak +
            sLineBreak;
        end;

        Prompt := Prompt +
          'Return a JSON array of relationship triplets:' + sLineBreak +
          '[' + sLineBreak +
          '  {' + sLineBreak +
          '    "subject": {"name": "EntityA", "nodeLabel": "Type", "properties": {}},' + sLineBreak +
          '    "predicate": {"edgeLabel": "RELATION", "name": "description", "properties": {}},' + sLineBreak +
          '    "object": {"name": "EntityB", "nodeLabel": "Type", "properties": {}}' + sLineBreak +
          '  }' + sLineBreak +
          ']' + sLineBreak +
          'Only return the JSON array, no additional text.' + sLineBreak +
          sLineBreak +
          'TEXT:' + sLineBreak +
          ChunkText;

        FChat.Asynchronous := False;
        FChat.Messages.Clear;
        Response := FChat.AddMessageAndRun(Prompt, 'user', nil);

        if Response.Trim.IsEmpty then
          Continue;

        // Extraer array JSON de la respuesta
        Response := Response.Trim;
        var StartIdx := Pos('[', Response);
        var EndIdx := Length(Response);
        while (EndIdx > 0) and (Response[EndIdx] <> ']') do
          Dec(EndIdx);

        if (StartIdx > 0) and (EndIdx >= StartIdx) then
          Response := Copy(Response, StartIdx, EndIdx - StartIdx + 1)
        else
          Continue;

        JsonValue := TJSONObject.ParseJSONValue(Response);
        if JsonValue = nil then
          Continue;

        try
          if not (JsonValue is TJSONArray) then
            Continue;

          JsonArray := JsonValue as TJSONArray;

          for TripletValue in JsonArray do
          begin
            if not (TripletValue is TJSONObject) then
              Continue;

            TripletObj := TripletValue as TJSONObject;
            SubjectObj := TripletObj.GetValue<TJSONObject>('subject', nil);
            PredicateObj := TripletObj.GetValue<TJSONObject>('predicate', nil);
            ObjectObj := TripletObj.GetValue<TJSONObject>('object', nil);

            if (SubjectObj = nil) or (PredicateObj = nil) or (ObjectObj = nil) then
              Continue;

            SubjectName := SubjectObj.GetValue<string>('name', '');
            SubjectLabel := SubjectObj.GetValue<string>('nodeLabel', 'Entity');
            ObjectName := ObjectObj.GetValue<string>('name', '');
            ObjectLabel := ObjectObj.GetValue<string>('nodeLabel', 'Entity');
            EdgeLabel := PredicateObj.GetValue<string>('edgeLabel', 'related_to');
            EdgeName := PredicateObj.GetValue<string>('name', '');

            if SubjectName.Trim.IsEmpty or ObjectName.Trim.IsEmpty then
              Continue;

            // GetOrCreate para Subject
            EntityKey := UpperCase(SubjectName + '|' + SubjectLabel);
            if not EntitySet.TryGetValue(EntityKey, SubjectNode) then
            begin
              SubjectNode := FGraph.FindNodeByName(SubjectName, SubjectLabel);
              if SubjectNode = nil then
              begin
                SubjectNode := FGraph.NewNode(TGUID.NewGuid.ToString, SubjectLabel, SubjectName);
                TextToEmbed := GenerateNodeEmbeddingText(SubjectName, SubjectLabel,
                  SubjectObj.GetValue<TJSONObject>('properties', nil));
                SubjectNode.Text := TextToEmbed;
                SubjectNode.Model := LEmbeddings.Model;
                SubjectNode.Data := LEmbeddings.CreateEmbedding(TextToEmbed, 'user');
                FGraph.AddNode(SubjectNode);
              end;
              EntitySet.Add(EntityKey, SubjectNode);
            end;

            // GetOrCreate para Object
            EntityKey := UpperCase(ObjectName + '|' + ObjectLabel);
            if not EntitySet.TryGetValue(EntityKey, ObjectNode) then
            begin
              ObjectNode := FGraph.FindNodeByName(ObjectName, ObjectLabel);
              if ObjectNode = nil then
              begin
                ObjectNode := FGraph.NewNode(TGUID.NewGuid.ToString, ObjectLabel, ObjectName);
                TextToEmbed := GenerateNodeEmbeddingText(ObjectName, ObjectLabel,
                  ObjectObj.GetValue<TJSONObject>('properties', nil));
                ObjectNode.Text := TextToEmbed;
                ObjectNode.Model := LEmbeddings.Model;
                ObjectNode.Data := LEmbeddings.CreateEmbedding(TextToEmbed, 'user');
                FGraph.AddNode(ObjectNode);
              end;
              EntitySet.Add(EntityKey, ObjectNode);
            end;

            // Crear arista de relaci?n
            if FGraph.FindEdge(SubjectNode, ObjectNode, EdgeLabel) = nil then
            begin
              NewEdge := FGraph.NewEdge(SubjectNode, ObjectNode,
                TGUID.NewGuid.ToString, EdgeLabel, EdgeName);
              NewEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;

              TextToEmbed := SubjectNode.Name + ' ' + EdgeLabel + ' ' + ObjectNode.Name;
              NewEdge.Data := LEmbeddings.CreateEmbedding(TextToEmbed, '');

              FGraph.AddEdge(NewEdge);
              Inc(Result);
            end;

            // Aristas EXTRACTED_FROM
            if FGraph.FindEdge(SubjectNode, ADocNode, EDGE_EXTRACTED_FROM) = nil then
            begin
              ExtractedEdge := FGraph.NewEdge(SubjectNode, ADocNode,
                TGUID.NewGuid.ToString, EDGE_EXTRACTED_FROM, '');
              ExtractedEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;
              FGraph.AddEdge(ExtractedEdge);
            end;

            if FGraph.FindEdge(ObjectNode, ADocNode, EDGE_EXTRACTED_FROM) = nil then
            begin
              ExtractedEdge := FGraph.NewEdge(ObjectNode, ADocNode,
                TGUID.NewGuid.ToString, EDGE_EXTRACTED_FROM, '');
              ExtractedEdge.MetaData[EDGEPROP_SOURCE_CHUNK_INDEX] := ChunkIdx;
              FGraph.AddEdge(ExtractedEdge);
            end;
          end;
        finally
          JsonValue.Free;
        end;
      end;
    finally
      FGraph.EndUpdate;
    end;
  finally
    EntitySet.Free;
  end;
end;

// ---------------------------------------------------------------------------
// AddEdge - Crea manualmente una arista entre dos nodos existentes
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.AddEdge(AFromNode, AToNode: TAiRagGraphNode;
  const AEdgeLabel: string; const AEdgeName: string): TAiRagGraphEdge;
var
  TextToEmbed: string;
  LEmbeddings: TAiEmbeddingsCore;
begin
  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  // Verificar si la arista ya existe
  Result := FGraph.FindEdge(AFromNode, AToNode, AEdgeLabel);
  if Result <> nil then
    Exit;

  Result := FGraph.NewEdge(AFromNode, AToNode,
    TGUID.NewGuid.ToString, AEdgeLabel, AEdgeName);

  TextToEmbed := AFromNode.Name + ' ' + AEdgeLabel + ' ' + AToNode.Name;
  Result.Data := LEmbeddings.CreateEmbedding(TextToEmbed, '');

  FGraph.AddEdge(Result);
end;

// ---------------------------------------------------------------------------
// GetDocumentEntities - Retorna las entidades vinculadas a un documento
// via aristas EXTRACTED_FROM
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.GetDocumentEntities(
  ADocNode: TAiRagGraphNode): TArray<TAiRagGraphNode>;
var
  Edge: TAiRagGraphEdge;
  EntList: TList<TAiRagGraphNode>;
  EntitySet: TDictionary<string, Boolean>;
begin
  EntList := TList<TAiRagGraphNode>.Create;
  EntitySet := TDictionary<string, Boolean>.Create;
  try
    for Edge in ADocNode.IncomingEdges do
    begin
      if SameText(Edge.EdgeLabel, EDGE_EXTRACTED_FROM) then
      begin
        if (Edge.FromNode <> nil) and not EntitySet.ContainsKey(Edge.FromNode.ID) then
        begin
          EntList.Add(Edge.FromNode);
          EntitySet.Add(Edge.FromNode.ID, True);
        end;
      end;
    end;
    Result := EntList.ToArray;
  finally
    EntList.Free;
    EntitySet.Free;
  end;
end;

// ---------------------------------------------------------------------------
// GetDocumentContext - Recupera contexto completo del documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.GetDocumentContext(ADocNode: TAiRagGraphNode): TDocumentContext;
var
  Parts: TArray<TDocumentPart>;
  EntList: TList<TAiRagGraphNode>;
  RelList: TList<TAiRagGraphEdge>;
  Edge: TAiRagGraphEdge;
  Entity: TAiRagGraphNode;
  EntitySet: TDictionary<string, Boolean>;
begin
  Result.DocumentNode := ADocNode;

  // Obtener todas las partes ordenadas
  Parts := GetDocumentParts(ADocNode);
  Result.AllParts := Parts;
  Result.MatchedParts := [];

  // Encontrar entidades vinculadas v?a EXTRACTED_FROM
  EntList := TList<TAiRagGraphNode>.Create;
  RelList := TList<TAiRagGraphEdge>.Create;
  EntitySet := TDictionary<string, Boolean>.Create;
  try
    // Buscar aristas EXTRACTED_FROM que apuntan a este documento
    for Edge in ADocNode.IncomingEdges do
    begin
      if SameText(Edge.EdgeLabel, EDGE_EXTRACTED_FROM) then
      begin
        Entity := Edge.FromNode;
        if (Entity <> nil) and (not EntitySet.ContainsKey(Entity.ID)) then
        begin
          EntList.Add(Entity);
          EntitySet.Add(Entity.ID, True);
        end;
      end;
    end;

    Result.Entities := EntList.ToArray;

    // Recolectar relaciones entre las entidades encontradas
    for Entity in EntList do
    begin
      for Edge in Entity.OutgoingEdges do
      begin
        if not SameText(Edge.EdgeLabel, EDGE_EXTRACTED_FROM) then
        begin
          if EntitySet.ContainsKey(Edge.ToNode.ID) then
            RelList.Add(Edge);
        end;
      end;
    end;

    Result.Relations := RelList.ToArray;
  finally
    EntList.Free;
    RelList.Free;
    EntitySet.Free;
  end;
end;

function TAiRagDocumentManager.GetDocumentContext(const ADocumentID: string): TDocumentContext;
var
  Node: TAiRagGraphNode;
begin
  CheckGraphAssigned;
  Node := FGraph.FindNodeByID(ADocumentID);
  if Node = nil then
    raise Exception.CreateFmt('Document node not found with ID: %s', [ADocumentID]);
  Result := GetDocumentContext(Node);
end;

// ---------------------------------------------------------------------------
// GetDocumentText - Reconstruye el texto completo del documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.GetDocumentText(ADocNode: TAiRagGraphNode): string;
var
  SB: TStringBuilder;
  Parts: TArray<TDocumentPart>;
  Part: TDocumentPart;
begin
  // Intentar devolver el texto original almacenado
  if ADocNode.MetaData.TagString <> '' then
  begin
    Result := ADocNode.MetaData.TagString;
    Exit;
  end;

  // Reconstruir desde chunks ordenados
  Parts := GetDocumentParts(ADocNode);
  SB := TStringBuilder.Create;
  try
    for Part in Parts do
    begin
      if SB.Length > 0 then
        SB.AppendLine;
      SB.Append(Part.Text);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// ---------------------------------------------------------------------------
// GetDocumentParts - Mapea chunks a TDocumentPart ordenados
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.GetDocumentParts(ADocNode: TAiRagGraphNode): TArray<TDocumentPart>;
var
  ResultList: TList<TDocumentPart>;
  Chunk: TAiEmbeddingNode;
  Part: TDocumentPart;
begin
  ResultList := TList<TDocumentPart>.Create;
  try
    for Chunk in ADocNode.Chunks do
    begin
      Part := Default(TDocumentPart);
      Part.Name := VarToStr(Chunk.MetaData.Get(DOCMETA_NAME, ''));
      Part.PartLabel := VarToStr(Chunk.MetaData.Get(DOCMETA_LABEL, ''));
      Part.Text := VarToStr(Chunk.MetaData.Get(DOCMETA_ORIGINAL_TEXT, Chunk.Text));
      Part.Path := VarToStr(Chunk.MetaData.Get(DOCMETA_PATH, ''));
      Part.Order := Integer(Chunk.MetaData.Get(DOCMETA_ORDER, 0));
      Part.Depth := Integer(Chunk.MetaData.Get(DOCMETA_DEPTH, 0));
      Part.IsLeaf := Boolean(Chunk.MetaData.Get(DOCMETA_IS_LEAF, True));
      Part.Score := 0;
      Part.Properties := nil;

      ResultList.Add(Part);
    end;

    // Ordenar por DOCMETA_ORDER
    ResultList.Sort(TComparer<TDocumentPart>.Construct(
      function(const Left, Right: TDocumentPart): Integer
      begin
        Result := Left.Order - Right.Order;
      end
    ));

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

// ---------------------------------------------------------------------------
// SearchDocuments - B?squeda sem?ntica agrupada por documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.SearchDocuments(const APrompt: string;
  ALimit: Integer; APrecision: Double): TArray<TDocumentSearchResult>;
var
  LEmbeddings: TAiEmbeddingsCore;
  QueryEmb: TAiEmbeddingData;
  QueryNode: TAiEmbeddingNode;
  DocResults: TDictionary<string, TDocumentSearchResult>;
  Node: TAiRagGraphNode;
  Chunk: TAiEmbeddingNode;
  Similarity: Double;
  DocID: string;
  SR: TDocumentSearchResult;
  ResultList: TList<TDocumentSearchResult>;
  I: Integer;
begin
  CheckEmbeddingsAvailable;
  LEmbeddings := FGraph.Embeddings;

  // Generar embedding de la consulta
  QueryEmb := LEmbeddings.CreateEmbedding(APrompt, 'user');

  QueryNode := TAiEmbeddingNode.Create(Length(QueryEmb));
  try
    QueryNode.Data := QueryEmb;

    DocResults := TDictionary<string, TDocumentSearchResult>.Create;
    try
      // Iterar todos los nodos del grafo
      for I := 0 to FGraph.Nodes.Items.Count - 1 do
      begin
        if not (FGraph.Nodes.Items[I] is TAiRagGraphNode) then
          Continue;

        Node := TAiRagGraphNode(FGraph.Nodes.Items[I]);

        // Solo procesar nodos que tengan chunks de documento
        if Node.Chunks.Count = 0 then
          Continue;

        // Verificar que es un nodo de documento (tiene chunks con DOCMETA_DOCUMENT_ID)
        if not Node.Chunks[0].MetaData.Has(DOCMETA_DOCUMENT_ID) then
          Continue;

        DocID := Node.ID;

        // Buscar el mejor score entre todos los chunks
        for Chunk in Node.Chunks do
        begin
          if Length(Chunk.Data) = 0 then
            Continue;

          Similarity := TAiEmbeddingNode.CosineSimilarity(QueryNode, Chunk);

          if Similarity >= APrecision then
          begin
            if DocResults.TryGetValue(DocID, SR) then
            begin
              // Actualizar si este chunk tiene mejor score
              if Similarity > SR.BestScore then
              begin
                SR.BestScore := Similarity;
                DocResults[DocID] := SR;
              end;
              // Agregar chunk a la lista de matching chunks
              // (necesitamos reconstruir el array)
            end
            else
            begin
              SR.DocumentNode := Node;
              SR.BestScore := Similarity;
              SR.MatchingChunks := [Chunk];
              DocResults.Add(DocID, SR);
            end;
          end;
        end;
      end;

      // Convertir a lista y ordenar por BestScore descendente
      ResultList := TList<TDocumentSearchResult>.Create;
      try
        for SR in DocResults.Values do
          ResultList.Add(SR);

        ResultList.Sort(TComparer<TDocumentSearchResult>.Construct(
          function(const Left, Right: TDocumentSearchResult): Integer
          begin
            if Left.BestScore > Right.BestScore then
              Result := -1
            else if Left.BestScore < Right.BestScore then
              Result := 1
            else
              Result := 0;
          end
        ));

        // Limitar resultados
        if ResultList.Count > ALimit then
          ResultList.Count := ALimit;

        Result := ResultList.ToArray;
      finally
        ResultList.Free;
      end;
    finally
      DocResults.Free;
    end;
  finally
    QueryNode.Free;
  end;
end;

function TAiRagDocumentManager.SearchDocumentsText(const APrompt: string;
  ALimit: Integer; APrecision: Double): string;
var
  Results: TArray<TDocumentSearchResult>;
  SB: TStringBuilder;
  SR: TDocumentSearchResult;
  Context: TDocumentContext;
  I: Integer;
begin
  Results := SearchDocuments(APrompt, ALimit, APrecision);

  SB := TStringBuilder.Create;
  try
    for I := 0 to High(Results) do
    begin
      SR := Results[I];

      if I > 0 then
      begin
        SB.AppendLine;
        SB.AppendLine('---');
        SB.AppendLine;
      end;

      SB.AppendFormat('Document: %s (Score: %.4f)', [SR.DocumentNode.Name, SR.BestScore]);
      SB.AppendLine;

      Context := GetDocumentContext(SR.DocumentNode);
      SB.Append(Context.GetContextText);
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// ---------------------------------------------------------------------------
// BuildChunkVector - Construye un TAiRAGVector temporal con todos los chunks
// de todos los documentos. El caller debe liberar el vector retornado.
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.BuildChunkVector: TAiRAGVector;
var
  I: Integer;
  Node: TAiRagGraphNode;
  Chunk, ClonedChunk: TAiEmbeddingNode;
begin
  CheckGraphAssigned;

  Result := TAiRAGVector.Create(nil, True);
  try
    Result.Embeddings := FGraph.Embeddings;

    for I := 0 to FGraph.Nodes.Items.Count - 1 do
    begin
      if not (FGraph.Nodes.Items[I] is TAiRagGraphNode) then
        Continue;

      Node := TAiRagGraphNode(FGraph.Nodes.Items[I]);
      if Node.Chunks.Count = 0 then
        Continue;
      if not Node.Chunks[0].MetaData.Has(DOCMETA_DOCUMENT_ID) then
        Continue;

      for Chunk in Node.Chunks do
      begin
        ClonedChunk := TAiEmbeddingNode.Create(Length(Chunk.Data));
        ClonedChunk.Text := Chunk.Text;
        ClonedChunk.Data := Copy(Chunk.Data);
        ClonedChunk.Model := Chunk.Model;
        ClonedChunk.Orden := Chunk.Orden;
        ClonedChunk.MetaData.Assign(Chunk.MetaData);
        Result.Items.Add(ClonedChunk);
      end;
    end;

    if Result.Items.Count > 0 then
      Result.BuildIndex;
  except
    Result.Free;
    raise;
  end;
end;

// ---------------------------------------------------------------------------
// BuildChunkVectorForDocument - Construye vector solo con chunks de un documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.BuildChunkVectorForDocument(
  ADocNode: TAiRagGraphNode): TAiRAGVector;
var
  Chunk, ClonedChunk: TAiEmbeddingNode;
begin
  Result := TAiRAGVector.Create(nil, True);
  try
    Result.Embeddings := FGraph.Embeddings;

    for Chunk in ADocNode.Chunks do
    begin
      ClonedChunk := TAiEmbeddingNode.Create(Length(Chunk.Data));
      ClonedChunk.Text := Chunk.Text;
      ClonedChunk.Data := Copy(Chunk.Data);
      ClonedChunk.Model := Chunk.Model;
      ClonedChunk.Orden := Chunk.Orden;
      ClonedChunk.MetaData.Assign(Chunk.MetaData);
      Result.Items.Add(ClonedChunk);
    end;

    if Result.Items.Count > 0 then
      Result.BuildIndex;
  except
    Result.Free;
    raise;
  end;
end;

// ---------------------------------------------------------------------------
// ChunkResultsToDocumentResults - Agrupa resultados de chunks por documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ChunkResultsToDocumentResults(
  AChunkVector: TAiRAGVector; ALimit: Integer): TArray<TDocumentSearchResult>;
var
  DocMap: TDictionary<string, TDocumentSearchResult>;
  Chunk: TAiEmbeddingNode;
  DocID: string;
  SR: TDocumentSearchResult;
  DocNode: TAiRagGraphNode;
  ResultList: TList<TDocumentSearchResult>;
  I: Integer;
begin
  Result := [];
  if (AChunkVector = nil) or (AChunkVector.Items.Count = 0) then
    Exit;

  DocMap := TDictionary<string, TDocumentSearchResult>.Create;
  try
    for I := 0 to AChunkVector.Items.Count - 1 do
    begin
      Chunk := AChunkVector.Items[I];
      DocID := VarToStr(Chunk.MetaData.Get(DOCMETA_DOCUMENT_ID, ''));
      if DocID = '' then
        Continue;

      if not DocMap.TryGetValue(DocID, SR) then
      begin
        DocNode := FGraph.FindNodeByID(DocID);
        if DocNode = nil then
          Continue;

        SR.DocumentNode := DocNode;
        SR.BestScore := Chunk.Idx;
        SR.MatchingChunks := [Chunk];
        DocMap.Add(DocID, SR);
      end
      else
      begin
        if Chunk.Idx > SR.BestScore then
        begin
          SR.BestScore := Chunk.Idx;
          DocMap[DocID] := SR;
        end;
      end;
    end;

    ResultList := TList<TDocumentSearchResult>.Create;
    try
      for SR in DocMap.Values do
        ResultList.Add(SR);

      ResultList.Sort(TComparer<TDocumentSearchResult>.Construct(
        function(const Left, Right: TDocumentSearchResult): Integer
        begin
          if Left.BestScore > Right.BestScore then
            Result := -1
          else if Left.BestScore < Right.BestScore then
            Result := 1
          else
            Result := 0;
        end
      ));

      if ResultList.Count > ALimit then
        ResultList.Count := ALimit;

      Result := ResultList.ToArray;
    finally
      ResultList.Free;
    end;
  finally
    DocMap.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExecuteVQL - Ejecuta una consulta VQL sobre todos los chunks de documentos
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExecuteVQL(const AVgqlQuery: string): string;
var
  TempVec: TAiRAGVector;
begin
  Result := ExecuteVQL(AVgqlQuery, TempVec);
  if Assigned(TempVec) then
    TempVec.Free;
end;

function TAiRagDocumentManager.ExecuteVQL(const AVgqlQuery: string;
  out AResultVector: TAiRAGVector): string;
var
  ChunkVec: TAiRAGVector;
begin
  CheckEmbeddingsAvailable;
  AResultVector := nil;

  ChunkVec := BuildChunkVector;
  try
    Result := ChunkVec.ExecuteVGQL(AVgqlQuery, AResultVector);
  finally
    ChunkVec.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExecuteDocumentVQL - Ejecuta VQL sobre chunks de un documento espec?fico
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExecuteDocumentVQL(const AVgqlQuery: string;
  ADocNode: TAiRagGraphNode): string;
var
  TempVec: TAiRAGVector;
begin
  Result := ExecuteDocumentVQL(AVgqlQuery, ADocNode, TempVec);
  if Assigned(TempVec) then
    TempVec.Free;
end;

function TAiRagDocumentManager.ExecuteDocumentVQL(const AVgqlQuery: string;
  ADocNode: TAiRagGraphNode; out AResultVector: TAiRAGVector): string;
var
  ChunkVec: TAiRAGVector;
begin
  CheckEmbeddingsAvailable;
  AResultVector := nil;

  ChunkVec := BuildChunkVectorForDocument(ADocNode);
  try
    Result := ChunkVec.ExecuteVGQL(AVgqlQuery, AResultVector);
  finally
    ChunkVec.Free;
  end;
end;

// ---------------------------------------------------------------------------
// SearchDocumentsByVQL - VQL que retorna resultados agrupados por documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.SearchDocumentsByVQL(const AVgqlQuery: string;
  ALimit: Integer): TArray<TDocumentSearchResult>;
var
  ChunkVec, ResultVec: TAiRAGVector;
begin
  CheckEmbeddingsAvailable;
  Result := [];

  ChunkVec := BuildChunkVector;
  try
    ChunkVec.ExecuteVGQL(AVgqlQuery, ResultVec);
    try
      Result := ChunkResultsToDocumentResults(ResultVec, ALimit);
    finally
      if Assigned(ResultVec) then
        ResultVec.Free;
    end;
  finally
    ChunkVec.Free;
  end;
end;

// ---------------------------------------------------------------------------
// ExecuteGQL - Ejecuta una consulta MakerGQL sobre el grafo
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.ExecuteGQL(const AGqlQuery: string): string;
begin
  CheckGraphAssigned;
  Result := FGraph.ExecuteMakerGQL(AGqlQuery);
end;

function TAiRagDocumentManager.ExecuteGQL(const AGqlQuery: string;
  out AResultObjects: TArray<TDictionary<string, TObject>>;
  ADepth: Integer): string;
begin
  CheckGraphAssigned;
  Result := FGraph.ExecuteMakerGQL(AGqlQuery, AResultObjects, ADepth);
end;

// ---------------------------------------------------------------------------
// GetAllDocuments - Lista todos los nodos de documento
// ---------------------------------------------------------------------------
function TAiRagDocumentManager.GetAllDocuments: TArray<TAiRagGraphNode>;
begin
  CheckGraphAssigned;
  Result := FGraph.FindNodesByLabel(FDefaultDocumentLabel);
end;

// ---------------------------------------------------------------------------
// DeleteDocument - Elimina un documento y sus entidades hu?rfanas
// ---------------------------------------------------------------------------
procedure TAiRagDocumentManager.DeleteDocument(ADocNode: TAiRagGraphNode);
var
  Edge: TAiRagGraphEdge;
  Entity: TAiRagGraphNode;
  EntitiesToCheck: TList<TAiRagGraphNode>;
  EntEdge: TAiRagGraphEdge;
  HasOtherDocLinks: Boolean;
  EntitiesToDelete: TList<TAiRagGraphNode>;
  EdgesToRemove: TList<TAiRagGraphEdge>;
begin
  CheckGraphAssigned;

  if ADocNode = nil then
    Exit;

  EntitiesToCheck := TList<TAiRagGraphNode>.Create;
  EntitiesToDelete := TList<TAiRagGraphNode>.Create;
  EdgesToRemove := TList<TAiRagGraphEdge>.Create;
  try
    // Encontrar entidades vinculadas v?a EXTRACTED_FROM
    for Edge in ADocNode.IncomingEdges do
    begin
      if SameText(Edge.EdgeLabel, EDGE_EXTRACTED_FROM) and (Edge.FromNode <> nil) then
        EntitiesToCheck.Add(Edge.FromNode);
    end;

    // Evaluar cada entidad: si solo est? vinculada a este documento, eliminarla
    for Entity in EntitiesToCheck do
    begin
      HasOtherDocLinks := False;

      for EntEdge in Entity.OutgoingEdges do
      begin
        if SameText(EntEdge.EdgeLabel, EDGE_EXTRACTED_FROM) and
           (EntEdge.ToNode <> ADocNode) then
        begin
          HasOtherDocLinks := True;
          Break;
        end;
      end;

      if HasOtherDocLinks then
      begin
        // Solo eliminar la arista EXTRACTED_FROM hacia este documento
        for EntEdge in Entity.OutgoingEdges do
        begin
          if SameText(EntEdge.EdgeLabel, EDGE_EXTRACTED_FROM) and
             (EntEdge.ToNode = ADocNode) then
          begin
            EdgesToRemove.Add(EntEdge);
            Break;
          end;
        end;
      end
      else
      begin
        // Entidad hu?rfana: eliminar completamente
        EntitiesToDelete.Add(Entity);
      end;
    end;

    // Ejecutar eliminaciones
    for Edge in EdgesToRemove do
      FGraph.DeleteEdge(Edge);

    for Entity in EntitiesToDelete do
      FGraph.DeleteNode(Entity);

    // Finalmente eliminar el nodo documento (y sus aristas restantes)
    FGraph.DeleteNode(ADocNode);
  finally
    EntitiesToCheck.Free;
    EntitiesToDelete.Free;
    EdgesToRemove.Free;
  end;
end;

end.
