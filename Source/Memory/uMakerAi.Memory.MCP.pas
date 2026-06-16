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

unit uMakerAi.Memory.MCP;

// MCP Tools para exponer TAiMemory a través del servidor MCP de AiMaker.
//
// Cada tool sigue el patrón TAiMCPToolBase<TParams> de uMakerAi.MCPServer.Core:
//   - Clase de Params con atributos AiMCPSchemaDescription
//   - ExecuteWithParams() → llama TAiMemory y devuelve TJSONObject
//
// Registrar en un TAiMCPServer existente:
//   RegisterMemoryTools(MyMCPServer, MyMemory);
//
// Tools disponibles:
//   memory_store   — guarda una memoria
//   memory_search  — búsqueda FTS/semántica/híbrida
//   memory_recall  — recupera memorias de alta importancia
//   memory_context — Smart Context Builder con token budget
//   memory_delete  — elimina por Id
//   memory_stats   — estadísticas del namespace
//   memory_link    — crea relación entre dos memorias
//   memory_prune   — limpieza de memorias antiguas de baja importancia

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Math, System.StrUtils,
  uMakerAi.MCPServer.Core,
  uMakerAi.Memory,
  uMakerAi.Memory.Types;

// Registra los 8 tools de memoria en un TAiMCPServer
procedure RegisterMemoryTools(AServer: TAiMCPServer; AMemory: TAiMemory);

implementation

// ---------------------------------------------------------------------------
// Params classes — TAiMCPSchemaDescription genera el JSON Schema automáticamente
// ---------------------------------------------------------------------------

type
  TMemStoreParams = class
    [AiMCPSchemaDescription('Contenido de la memoria a guardar')]
    content: string;

    [AiMCPSchemaDescription('Tipo: fact | preference | decision | error_fix | pattern | workflow | summary | custom')]
    [AiMCPOptional]
    memory_type: string;

    [AiMCPSchemaDescription('Importancia de 1 (baja) a 10 (crítica). Default: 5')]
    [AiMCPOptional]
    importance: Integer;

    [AiMCPSchemaDescription('Tags separados por coma. Ej: delphi,fmx,memoria')]
    [AiMCPOptional]
    tags: string;

    [AiMCPSchemaDescription('Días hasta expiración. 0 = sin expiración')]
    [AiMCPOptional]
    ttl_days: Integer;
  end;

  TMemSearchParams = class
    [AiMCPSchemaDescription('Texto a buscar en las memorias')]
    query: string;

    [AiMCPSchemaDescription('Número máximo de resultados. Default: 10')]
    [AiMCPOptional]
    limit: Integer;

    [AiMCPSchemaDescription('Modo de búsqueda: fts | semantic | hybrid. Default: hybrid')]
    [AiMCPOptional]
    mode: string;
  end;

  TMemRecallParams = class
    [AiMCPSchemaDescription('Importancia mínima (1-10). Default: 7')]
    [AiMCPOptional]
    min_importance: Integer;

    [AiMCPSchemaDescription('Número máximo de entradas. Default: 20')]
    [AiMCPOptional]
    limit: Integer;
  end;

  TMemContextParams = class
    [AiMCPSchemaDescription('Prompt actual del agente para seleccionar memorias relevantes')]
    prompt: string;

    [AiMCPSchemaDescription('Presupuesto máximo en tokens. Default: 2000')]
    [AiMCPOptional]
    token_budget: Integer;

    [AiMCPSchemaDescription('Importancia mínima de las memorias a incluir. Default: 1')]
    [AiMCPOptional]
    min_importance: Integer;
  end;

  TMemDeleteParams = class
    [AiMCPSchemaDescription('Id de la memoria a eliminar')]
    id: Integer;
  end;

  TMemStatsParams = class
    // Sin parámetros requeridos — usa el namespace del componente TAiMemory
  end;

  TMemLinkParams = class
    [AiMCPSchemaDescription('Id de la memoria origen')]
    from_id: Integer;

    [AiMCPSchemaDescription('Id de la memoria destino')]
    to_id: Integer;

    [AiMCPSchemaDescription('Tipo de relación: related | caused_by | depends_on | contradicts')]
    [AiMCPOptional]
    relation: string;
  end;

  TMemPruneParams = class
    [AiMCPSchemaDescription('Elimina memorias con importancia menor a este valor. Default: 3')]
    [AiMCPOptional]
    min_importance: Integer;

    [AiMCPSchemaDescription('Elimina memorias con más de N días sin acceso. Default: 90')]
    [AiMCPOptional]
    max_age_days: Integer;
  end;

// ---------------------------------------------------------------------------
// Tool implementations
// ---------------------------------------------------------------------------

type
  TAiMemoryStoreTool = class(TAiMCPToolBase<TMemStoreParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemStoreParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemorySearchTool = class(TAiMCPToolBase<TMemSearchParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemSearchParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryRecallTool = class(TAiMCPToolBase<TMemRecallParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemRecallParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryContextTool = class(TAiMCPToolBase<TMemContextParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemContextParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryDeleteTool = class(TAiMCPToolBase<TMemDeleteParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemDeleteParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryStatsTool = class(TAiMCPToolBase<TMemStatsParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemStatsParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryLinkTool = class(TAiMCPToolBase<TMemLinkParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemLinkParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

  TAiMemoryPruneTool = class(TAiMCPToolBase<TMemPruneParams>)
  private
    FMemory: TAiMemory;
  protected
    function ExecuteWithParams(const Params: TMemPruneParams;
      const Auth: TAiAuthContext): TJSONObject; override;
  public
    constructor Create(AMemory: TAiMemory); reintroduce;
  end;

// ---------------------------------------------------------------------------
// TAiMemoryStoreTool
// ---------------------------------------------------------------------------

constructor TAiMemoryStoreTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory     := AMemory;
  FName       := 'memory_store';
  FDescription:= 'Guarda una memoria persistente. Evita duplicados automáticamente.';
end;

function TAiMemoryStoreTool.ExecuteWithParams(const Params: TMemStoreParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  MemType: TMemoryType;
  Imp:     Integer;
  Id:      Integer;
begin
  MemType := StrToMemoryType(Params.memory_type);
  Imp     := IfThen(Params.importance > 0, Params.importance, 5);
  Id      := FMemory.Store(Params.content, MemType, Imp,
                           Params.tags, nil, Params.ttl_days);
  Result := TAiMCPResponseBuilder.New
    .AddText(Format('{"id":%d,"namespace":"%s","status":"stored"}',
                    [Id, FMemory.Namespace]))
    .Build;
end;

// ---------------------------------------------------------------------------
// TAiMemorySearchTool
// ---------------------------------------------------------------------------

constructor TAiMemorySearchTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_search';
  FDescription := 'Busca memorias por texto (FTS), similitud semántica o híbrido.';
end;

function TAiMemorySearchTool.ExecuteWithParams(const Params: TMemSearchParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  Mode:    TMemorySearchMode;
  Limit:   Integer;
  Results: TMemorySearchResults;
  Arr:     TJSONArray;
  I:       Integer;
  Obj:     TJSONObject;
begin
  Mode  := ms_Hybrid;
  if SameText(Params.mode, 'fts')      then Mode := ms_FTS
  else if SameText(Params.mode, 'semantic') then Mode := ms_Semantic;

  Limit := IfThen(Params.limit > 0, Params.limit, 10);

  Results := FMemory.Search(Params.query, Limit, Mode);

  Arr := TJSONArray.Create;
  for I := 0 to High(Results) do
  begin
    Obj := Results[I].Entry.ToJSON;
    Obj.AddPair('score',      TJSONNumber.Create(Results[I].Score));
    Obj.AddPair('match_type', Results[I].MatchType);
    Arr.AddElement(Obj);
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText(Arr.ToJSON)
    .Build;
  Arr.Free;
end;

// ---------------------------------------------------------------------------
// TAiMemoryRecallTool
// ---------------------------------------------------------------------------

constructor TAiMemoryRecallTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_recall';
  FDescription := 'Recupera memorias de alta importancia ordenadas por relevancia.';
end;

function TAiMemoryRecallTool.ExecuteWithParams(const Params: TMemRecallParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  Entries: TMemoryEntryList;
  Arr:     TJSONArray;
  E:       TMemoryEntry;
  MinImp, Limit: Integer;
begin
  MinImp := IfThen(Params.min_importance > 0, Params.min_importance, 7);
  Limit  := IfThen(Params.limit > 0, Params.limit, 20);

  Arr     := TJSONArray.Create;
  Entries := FMemory.Recall(MinImp, Limit);
  try
    for E in Entries do
      Arr.AddElement(E.ToJSON);
  finally
    Entries.Free;
  end;

  Result := TAiMCPResponseBuilder.New.AddText(Arr.ToJSON).Build;
  Arr.Free;
end;

// ---------------------------------------------------------------------------
// TAiMemoryContextTool
// ---------------------------------------------------------------------------

constructor TAiMemoryContextTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_context';
  FDescription := 'Construye un bloque de contexto con las memorias más relevantes ' +
                  'para el prompt actual, respetando un token budget.';
end;

function TAiMemoryContextTool.ExecuteWithParams(const Params: TMemContextParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  Budget, MinImp: Integer;
  Ctx: TMemoryContextResult;
  Obj: TJSONObject;
begin
  Budget := IfThen(Params.token_budget > 0, Params.token_budget, 2000);
  MinImp := IfThen(Params.min_importance > 0, Params.min_importance, 1);

  Ctx := FMemory.Context(Params.prompt, Budget, MinImp);

  Obj := TJSONObject.Create;
  Obj.AddPair('context',        Ctx.FormattedText);
  Obj.AddPair('memory_count',   TJSONNumber.Create(Ctx.MemoryCount));
  Obj.AddPair('token_estimate', TJSONNumber.Create(Ctx.TokenEstimate));
  Obj.AddPair('truncated',      TJSONBool.Create(Ctx.Truncated));

  Result := TAiMCPResponseBuilder.New.AddText(Obj.ToJSON).Build;
  Obj.Free;
end;

// ---------------------------------------------------------------------------
// TAiMemoryDeleteTool
// ---------------------------------------------------------------------------

constructor TAiMemoryDeleteTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_delete';
  FDescription := 'Elimina una memoria por su Id.';
end;

function TAiMemoryDeleteTool.ExecuteWithParams(const Params: TMemDeleteParams;
  const Auth: TAiAuthContext): TJSONObject;
begin
  FMemory.Delete(Params.id);
  Result := TAiMCPResponseBuilder.New
    .AddText(Format('{"id":%d,"status":"deleted"}', [Params.id]))
    .Build;
end;

// ---------------------------------------------------------------------------
// TAiMemoryStatsTool
// ---------------------------------------------------------------------------

constructor TAiMemoryStatsTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_stats';
  FDescription := 'Devuelve estadísticas del namespace activo: total, importancia ' +
                  'promedio, decay promedio, entrada más antigua/reciente.';
end;

function TAiMemoryStatsTool.ExecuteWithParams(const Params: TMemStatsParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  S: TMemoryStats;
begin
  S      := FMemory.Stats;
  Result := TAiMCPResponseBuilder.New.AddText(S.ToJSON.ToJSON).Build;
end;

// ---------------------------------------------------------------------------
// TAiMemoryLinkTool
// ---------------------------------------------------------------------------

constructor TAiMemoryLinkTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_link';
  FDescription := 'Crea una relación entre dos memorias. ' +
                  'Relaciones estándar: related, caused_by, depends_on, contradicts.';
end;

function TAiMemoryLinkTool.ExecuteWithParams(const Params: TMemLinkParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  Rel: string;
begin
  Rel := IfThen(Params.relation <> '', Params.relation, 'related');
  FMemory.Link(Params.from_id, Params.to_id, Rel);
  Result := TAiMCPResponseBuilder.New
    .AddText(Format('{"from_id":%d,"to_id":%d,"relation":"%s","status":"linked"}',
                    [Params.from_id, Params.to_id, Rel]))
    .Build;
end;

// ---------------------------------------------------------------------------
// TAiMemoryPruneTool
// ---------------------------------------------------------------------------

constructor TAiMemoryPruneTool.Create(AMemory: TAiMemory);
begin
  inherited Create;
  FMemory      := AMemory;
  FName        := 'memory_prune';
  FDescription := 'Elimina memorias antiguas de baja importancia y decay bajo. ' +
                  'Útil para limpiar el namespace periódicamente.';
end;

function TAiMemoryPruneTool.ExecuteWithParams(const Params: TMemPruneParams;
  const Auth: TAiAuthContext): TJSONObject;
var
  MinImp, MaxAge: Integer;
  StatsBefore, StatsAfter: TMemoryStats;
  Deleted: Integer;
begin
  MinImp  := IfThen(Params.min_importance > 0, Params.min_importance, 3);
  MaxAge  := IfThen(Params.max_age_days   > 0, Params.max_age_days,   90);

  StatsBefore := FMemory.Stats;
  FMemory.Prune(MinImp, MaxAge);
  StatsAfter  := FMemory.Stats;
  Deleted     := StatsBefore.TotalCount - StatsAfter.TotalCount;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('{"deleted":%d,"remaining":%d,"status":"pruned"}',
                    [Deleted, StatsAfter.TotalCount]))
    .Build;
end;

// ---------------------------------------------------------------------------
// RegisterMemoryTools — registra los 8 tools en TAiMCPServer
// ---------------------------------------------------------------------------

procedure RegisterMemoryTools(AServer: TAiMCPServer; AMemory: TAiMemory);
begin
  // Cada factory crea una instancia fresca del tool para cada llamada MCP.
  // El AMemory capturado por el closure es el mismo componente compartido.

  AServer.RegisterTool('memory_store',
    function: IAiMCPTool begin Result := TAiMemoryStoreTool.Create(AMemory)   end);

  AServer.RegisterTool('memory_search',
    function: IAiMCPTool begin Result := TAiMemorySearchTool.Create(AMemory)  end);

  AServer.RegisterTool('memory_recall',
    function: IAiMCPTool begin Result := TAiMemoryRecallTool.Create(AMemory)  end);

  AServer.RegisterTool('memory_context',
    function: IAiMCPTool begin Result := TAiMemoryContextTool.Create(AMemory) end);

  AServer.RegisterTool('memory_delete',
    function: IAiMCPTool begin Result := TAiMemoryDeleteTool.Create(AMemory)  end);

  AServer.RegisterTool('memory_stats',
    function: IAiMCPTool begin Result := TAiMemoryStatsTool.Create(AMemory)   end);

  AServer.RegisterTool('memory_link',
    function: IAiMCPTool begin Result := TAiMemoryLinkTool.Create(AMemory)    end);

  AServer.RegisterTool('memory_prune',
    function: IAiMCPTool begin Result := TAiMemoryPruneTool.Create(AMemory)   end);
end;

end.
