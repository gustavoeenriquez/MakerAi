// MIT License
//
// Copyright (c) 2024 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Memory.Context;

// Smart Context Builder para TAiMemory.
//
// Combina tres fuentes de candidatos y los selecciona dentro de un token budget:
//   1. FTS search(prompt)       → score por relevancia léxica
//   2. Semantic search(prompt)  → score por similitud coseno  [si hay embedder]
//   3. Recall(importance >= 7)  → memorias de alta importancia, siempre incluidas
//
// Ranking combinado: 0.6 * relevance + 0.4 * (importance / 10)
// Token budget: acumula entradas ordenadas por score hasta agotar el límite.
// Estimación: ~4 caracteres por token.

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Math,
  uMakerAi.Memory.Types,
  uMakerAi.Memory.Storage,
  uMakerAi.Embeddings.Core;

type
  TAiMemoryContext = class
  private
    FStorage:      IAiMemoryStorage;
    FEmbedder:     TAiEmbeddingsCore;
    FMinImportance: Integer;

    function FormatEntry(AEntry: TMemoryEntry): string;
    function EstimateTokens(const AText: string): Integer;
  public
    constructor Create(AStorage: IAiMemoryStorage; AEmbedder: TAiEmbeddingsCore = nil);

    // Construye el bloque de contexto para inyectar en el prompt del agente
    function Build(
      const APrompt:    string;
      ATokenBudget:     Integer = 2000;
      AMinImportance:   Integer = 1;
      const ANamespace: string  = 'default'
    ): TMemoryContextResult;
  end;

implementation

// ---------------------------------------------------------------------------

constructor TAiMemoryContext.Create(AStorage: IAiMemoryStorage;
  AEmbedder: TAiEmbeddingsCore);
begin
  inherited Create;
  FStorage       := AStorage;
  FEmbedder      := AEmbedder;
  FMinImportance := 1;
end;

function TAiMemoryContext.EstimateTokens(const AText: string): Integer;
begin
  // Estimación conservadora: ~4 chars por token
  Result := Max(1, Length(AText) div 4);
end;

function TAiMemoryContext.FormatEntry(AEntry: TMemoryEntry): string;
var
  TagPart: string;
begin
  if Length(AEntry.Tags) > 0 then
    TagPart := ' tags:' + AEntry.TagsAsString
  else
    TagPart := '';

  Result := Format('[%s|imp:%d|decay:%.2f%s] %s',
    [MemoryTypeToStr(AEntry.MemoryType), AEntry.Importance,
     AEntry.DecayScore, TagPart, AEntry.Content]);
end;

function TAiMemoryContext.Build(const APrompt: string; ATokenBudget: Integer;
  AMinImportance: Integer; const ANamespace: string): TMemoryContextResult;
type
  TScoredCandidate = record
    Entry:     TMemoryEntry;
    Score:     Double;
    IsOwned:   Boolean;    // True = debe liberarse aquí si no se usa
  end;
var
  FTSList, RecallList, SemanticList: TMemoryEntryList;
  Candidates: TArray<TScoredCandidate>;
  CandCount:  Integer;
  SeenIds:    TDictionary<Integer, Boolean>;
  I, Rank:    Integer;
  Si, Sj:     Integer;
  Stmp:       TScoredCandidate;
  FTSScore:   Double;
  Lines:      TStringList;
  TokensUsed: Integer;
  EntryTokens: Integer;
  Entry:      TMemoryEntry;
  Formatted:  string;
  EmbData:    TAiEmbeddingData;

  procedure AddCandidate(AEntry: TMemoryEntry; AScore: Double; AOwned: Boolean);
  begin
    if SeenIds.ContainsKey(AEntry.Id) then
    begin
      if AOwned then AEntry.Free;
      Exit;
    end;
    SeenIds.Add(AEntry.Id, True);
    if CandCount >= Length(Candidates) then
      SetLength(Candidates, CandCount + 64);
    Candidates[CandCount].Entry   := AEntry;
    Candidates[CandCount].Score   := AScore;
    Candidates[CandCount].IsOwned := AOwned;
    Inc(CandCount);
  end;

begin
  Result := Default(TMemoryContextResult);
  CandCount := 0;
  SetLength(Candidates, 0);
  SeenIds := TDictionary<Integer, Boolean>.Create;
  Lines   := TStringList.Create;
  try
    // ── Fuente 1: FTS léxica ─────────────────────────────────────────────────
    FTSList := FStorage.SearchFTS(APrompt, ANamespace, 20);
    try
      Rank := 0;
      for Entry in FTSList do
      begin
        FTSScore := Max(0.0, 1.0 - (Rank * 0.05));
        var Combined := 0.6 * FTSScore + 0.4 * (Entry.Importance / 10.0);
        AddCandidate(Entry, Combined, True); // Context owns these entries
        Inc(Rank);
      end;
    finally
      FTSList.OwnsObjects := False; // ownership transferred to Candidates
      FTSList.Free;
    end;

    // ── Fuente 2: Semántica (si hay embedder) ────────────────────────────────
    if Assigned(FEmbedder) then
    begin
      try
        EmbData := FEmbedder.CreateEmbedding(APrompt, '');
        if Length(EmbData) > 0 then
        begin
          SemanticList := FStorage.SearchSemantic(EmbData, ANamespace, 15, 0.4);
          try
            for I := 0 to SemanticList.Count - 1 do
            begin
              Entry := SemanticList[I];
              var SemScore := 0.6 * (1.0 - I * 0.06) + 0.4 * (Entry.Importance / 10.0);
              AddCandidate(Entry, SemScore, True); // Context owns semantic entries
            end;
          finally
            SemanticList.Free;
          end;
        end;
      except
        // Si el embedder falla, continúa solo con FTS
      end;
    end;

    // ── Fuente 3: Recall de alta importancia (siempre incluir) ───────────────
    RecallList := FStorage.Recall(7, ANamespace, 10);
    try
      for Entry in RecallList do
        AddCandidate(Entry, 0.9, True); // Context owns recall entries
    finally
      RecallList.OwnsObjects := False; // ownership transferred to Candidates
      RecallList.Free;
    end;

    // ── Ordenar por score descendente — selection sort (n típicamente < 50) ──
    SetLength(Candidates, CandCount);
    for Si := 0 to CandCount - 2 do
      for Sj := Si + 1 to CandCount - 1 do
        if Candidates[Sj].Score > Candidates[Si].Score then
        begin
          Stmp := Candidates[Si]; Candidates[Si] := Candidates[Sj]; Candidates[Sj] := Stmp;
        end;

    // ── Acumular dentro del token budget ─────────────────────────────────────
    TokensUsed := 0;
    SetLength(Result.MemoryIds, 0);
    for I := 0 to High(Candidates) do
    begin
      Entry     := Candidates[I].Entry;
      if Entry.Importance < AMinImportance then Continue;

      Formatted := FormatEntry(Entry);
      EntryTokens := EstimateTokens(Formatted);

      if TokensUsed + EntryTokens > ATokenBudget then
      begin
        Result.Truncated := True;
        Break;
      end;

      Lines.Add(Formatted);
      TokensUsed := TokensUsed + EntryTokens;
      Inc(Result.MemoryCount);

      SetLength(Result.MemoryIds, Result.MemoryCount);
      Result.MemoryIds[Result.MemoryCount - 1] := Entry.Id;

      // Actualizar estadísticas de acceso
      FStorage.UpdateAccessStats(Entry.Id);
    end;

    Result.FormattedText  := Lines.Text.Trim;
    Result.TokenEstimate  := TokensUsed;

  finally
    // Liberar entries propios que no quedaron en Result
    for I := 0 to High(Candidates) do
      if Candidates[I].IsOwned then
        Candidates[I].Entry.Free;
    Lines.Free;
    SeenIds.Free;
  end;
end;

end.
