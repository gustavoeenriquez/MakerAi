// MIT License
// MakerAI - Sistema de Agentes v3.4
// Adaptador MCP → IAiTool: expone cada herramienta de un servidor MCP
// como un IAiTool intercambiable dentro del TAiToolRegistry.
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

unit uMakerAi.Agents.Tools.MCP;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  uMakerAi.Agents.IAiTool,
  uMakerAi.MCPClient.Core;

type

  { TAiMCPTool ------------------------------------------------------------------
    Envuelve UNA herramienta específica de un servidor MCP como IAiTool.
    El TAiMCPToolFactory crea una instancia por cada herramienta listada en el
    servidor.

    Contrato de memoria:
      - FClient NO es propiedad de esta clase (el owner es el código externo).
      - FSchema es propiedad de esta clase y se libera en el destructor.
      - Execute → el caller es responsable de liberar el TJSONObject retornado.
  }
  TAiMCPTool = class(TInterfacedObject, IAiTool)
  private
    FClient      : TMCPClientCustom;
    FToolName    : String;
    FDescription : String;
    FSchema      : TJSONObject;   // inputSchema del servidor; propiedad de esta clase
  public
    constructor Create(AClient: TMCPClientCustom;
                       const AToolName, ADescription: String;
                       AInputSchema: TJSONObject);  // se clona internamente
    destructor Destroy; override;

    // IAiTool
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

  { TAiMCPToolFactory -----------------------------------------------------------
    Inicializa un TMCPClientCustom y crea un TAiMCPTool por cada herramienta
    que el servidor declara en su respuesta tools/list.

    Uso:
      var Tools: TArray<IAiTool>;
      Tools := TAiMCPToolFactory.CreateFromClient(MyMCPClient);
      // Añadir al registry:
      for var T in Tools do
        Registry.Register(T);

    Notas:
      - Si el cliente no está inicializado, llama Initialize() internamente.
      - Si Initialize falla devuelve un array vacío (no eleva excepción).
      - El caller es responsable de la vida de AClient.
  }
  TAiMCPToolFactory = class
  public
    class function CreateFromClient(AClient: TMCPClientCustom): TArray<IAiTool>;
  end;

implementation

{ TAiMCPTool }

constructor TAiMCPTool.Create(AClient: TMCPClientCustom;
  const AToolName, ADescription: String; AInputSchema: TJSONObject);
begin
  inherited Create;
  FClient      := AClient;
  FToolName    := AToolName;
  FDescription := ADescription;
  // Clonar el schema para no depender del objeto JSON temporal del ListTools
  if Assigned(AInputSchema) then
    FSchema := TJSONObject(AInputSchema.Clone)
  else
  begin
    FSchema := TJSONObject.Create;
    FSchema.AddPair('type', 'object');
    FSchema.AddPair('properties', TJSONObject.Create);
  end;
end;

destructor TAiMCPTool.Destroy;
begin
  FSchema.Free;
  inherited;
end;

function TAiMCPTool.GetName: String;
begin
  Result := FToolName;
end;

function TAiMCPTool.GetDescription: String;
begin
  Result := FDescription;
end;

function TAiMCPTool.GetCategory: String;
begin
  Result := 'MCP';
end;

function TAiMCPTool.GetSchema: TJSONObject;
begin
  // NO liberar — propiedad de esta clase
  Result := FSchema;
end;

function TAiMCPTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  RawResult : TJSONObject;
begin
  Result := nil;
  if not Assigned(FClient) then Exit;
  if not FClient.Available then Exit;

  // CallTool devuelve un TJSONObject; esta clase lo retorna directamente.
  // El caller (TAiToolRegistry / agent node) es responsable de liberarlo.
  RawResult := FClient.CallTool(FToolName, AArgs, nil);
  if not Assigned(RawResult) then Exit;

  // El resultado MCP tiene la forma:
  //   { "content": [ { "type":"text", "text":"..." } ], "isError": false }
  // Lo pasamos tal cual — el agente sabe interpretarlo.
  Result := RawResult;
end;

function TAiMCPTool.IsAvailable: Boolean;
begin
  Result := Assigned(FClient) and FClient.Available and FClient.Enabled;
end;

{ TAiMCPToolFactory }

class function TAiMCPToolFactory.CreateFromClient(
  AClient: TMCPClientCustom): TArray<IAiTool>;
var
  ListResp  : TJSONObject;
  ToolsArr  : TJSONArray;
  ToolEntry : TJSONValue;
  ToolObj   : TJSONObject;
  ToolName  : String;
  ToolDesc  : String;
  InputSch  : TJSONObject;
  ResultList: TList<IAiTool>;
  ResultObj : TJSONObject;
begin
  Result := [];
  if not Assigned(AClient) then Exit;

  // Inicializar si todavía no se hizo
  if not AClient.Initialized then
  begin
    if not AClient.Initialize then
      Exit;
  end;

  if not AClient.Available then Exit;

  ResultList := TList<IAiTool>.Create;
  try
    ListResp := AClient.ListTools;
    if not Assigned(ListResp) then Exit;
    try
      // Estructura esperada: { "result": { "tools": [...] } }
      // Algunos transportes omiten el wrapper "result" y devuelven { "tools": [...] }
      ToolsArr  := nil;
      ResultObj := nil;
      if ListResp.TryGetValue<TJSONObject>('result', ResultObj) then
        ResultObj.TryGetValue<TJSONArray>('tools', ToolsArr)
      else
        ListResp.TryGetValue<TJSONArray>('tools', ToolsArr);

      if not Assigned(ToolsArr) then Exit;

      for ToolEntry in ToolsArr do
      begin
        if not (ToolEntry is TJSONObject) then Continue;
        ToolObj := TJSONObject(ToolEntry);

        if not ToolObj.TryGetValue<String>('name', ToolName) then Continue;
        ToolObj.TryGetValue<String>('description', ToolDesc);

        InputSch := nil;
        ToolObj.TryGetValue<TJSONObject>('inputSchema', InputSch);

        ResultList.Add(TAiMCPTool.Create(AClient, ToolName, ToolDesc, InputSch));
      end;
    finally
      ListResp.Free;
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

end.
