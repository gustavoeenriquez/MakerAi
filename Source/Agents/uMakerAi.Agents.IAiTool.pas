// MIT License
// MakerAI - Sistema de Agentes v3.4
// Interfaz unificada de herramientas para el rediseño del sistema de agentes.
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

unit uMakerAi.Agents.IAiTool;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Rtti;

type

  { IAiTool ----------------------------------------------------------------
    Interfaz unificada para todas las herramientas del sistema de agentes.
    Cualquier herramienta (MCP, función, sub-agente, shell) implementa esta
    interfaz para ser intercambiable dentro del TAiToolRegistry.

    Contrato de memoria:
      - GetSchema   → NO liberar; el objeto es propiedad de la herramienta.
      - Execute     → El llamador es responsable de liberar el TJSONObject
                      retornado (puede ser nil si no hay resultado).
  }
  IAiTool = interface
    ['{3F7A9B2E-C154-4D8A-B3F1-0E9A7C6D5824}']
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    // Retorna el JSON Schema de parámetros. NO liberar.
    function GetSchema: TJSONObject;
    // Ejecuta la herramienta con argumentos JSON. Caller libera el resultado.
    function Execute(const AArgs: TJSONObject): TJSONObject;
    // Indica si la herramienta está disponible para su uso.
    function IsAvailable: Boolean;

    property Name        : String       read GetName;
    property Description : String       read GetDescription;
    property Category    : String       read GetCategory;
  end;

  { TAiToolBase_IAiTool ------------------------------------------------------
    Adaptador que envuelve TAiToolBase (sistema legacy de nodos) como IAiTool,
    permitiendo usarlas en TAiToolRegistry sin modificar el código existente.

    Uso:
      var Tool: IAiTool := TAiToolBase_IAiTool.Create(MyLegacyTool, True);
  }
  TAiToolBase_IAiTool = class(TInterfacedObject, IAiTool)
  private
    FTool     : TObject;   // TAiToolBase — evita dependencia circular
    FOwnsTool : Boolean;
    FSchema   : TJSONObject;
  public
    constructor Create(ATool: TObject; AOwns: Boolean = False);
    destructor  Destroy; override;
    // IAiTool
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

  { TAiNullTool ---------------------------------------------------------------
    Implementación vacía de IAiTool. Útil como placeholder o en tests.
  }
  TAiNullTool = class(TInterfacedObject, IAiTool)
  private
    FName        : String;
    FDescription : String;
  public
    constructor Create(const AName: String = 'null';
                       const ADescription: String = 'No-op tool');
    function GetName: String;
    function GetDescription: String;
    function GetCategory: String;
    function GetSchema: TJSONObject;
    function Execute(const AArgs: TJSONObject): TJSONObject;
    function IsAvailable: Boolean;
  end;

implementation

{ TAiToolBase_IAiTool }

constructor TAiToolBase_IAiTool.Create(ATool: TObject; AOwns: Boolean);
begin
  inherited Create;
  FTool     := ATool;
  FOwnsTool := AOwns;
  FSchema   := nil;
end;

destructor TAiToolBase_IAiTool.Destroy;
begin
  FSchema.Free;
  if FOwnsTool then
    FTool.Free;
  inherited;
end;

function TAiToolBase_IAiTool.GetName: String;
begin
  // Acceso vía RTTI al nombre publicado de TAiToolBase
  if Assigned(FTool) and (FTool is TComponent) then
    Result := TComponent(FTool).Name
  else
    Result := '';
end;

function TAiToolBase_IAiTool.GetDescription: String;
begin
  // Intentamos leer la propiedad Description vía RTTI si existe
  Result := '';
  if not Assigned(FTool) then Exit;
  try
    var Ctx := TRttiContext.Create;
    try
      var RttiType := Ctx.GetType(FTool.ClassType);
      var Prop     := RttiType.GetProperty('Description');
      if Assigned(Prop) then
        Result := Prop.GetValue(FTool).AsString;
    finally
      Ctx.Free;
    end;
  except
    Result := FTool.ClassName;
  end;
end;

function TAiToolBase_IAiTool.GetCategory: String;
begin
  Result := 'Legacy';
end;

function TAiToolBase_IAiTool.GetSchema: TJSONObject;
begin
  if not Assigned(FSchema) then
  begin
    FSchema := TJSONObject.Create;
    FSchema.AddPair('type', 'object');
    FSchema.AddPair('properties', TJSONObject.Create);
  end;
  Result := FSchema;
end;

function TAiToolBase_IAiTool.Execute(const AArgs: TJSONObject): TJSONObject;
var
  Input, Output: String;
begin
  Result := nil;
  if not Assigned(FTool) then Exit;

  // Convierte los args JSON a string plano para TAiToolBase
  if Assigned(AArgs) then
    Input := AArgs.ToJSON
  else
    Input := '';

  Output := '';

  // Llama Execute vía RTTI (firma: Execute(ANode, AInput, var AOutput))
  try
    var Ctx := TRttiContext.Create;
    try
      var RttiType := Ctx.GetType(FTool.ClassType);
      var Method   := RttiType.GetMethod('Execute');
      if Assigned(Method) then
      begin
        var Args: TArray<TValue>;
        SetLength(Args, 3);
        Args[0] := TValue.From<TObject>(nil); // ANode = nil
        Args[1] := Input;
        Args[2] := Output;
        Method.Invoke(FTool, Args);
        Output := Args[2].AsString;
      end;
    finally
      Ctx.Free;
    end;
  except
    Output := '';
  end;

  if Output <> '' then
  begin
    Result := TJSONObject.Create;
    Result.AddPair('result', Output);
  end;
end;

function TAiToolBase_IAiTool.IsAvailable: Boolean;
begin
  Result := Assigned(FTool);
end;

{ TAiNullTool }

constructor TAiNullTool.Create(const AName, ADescription: String);
begin
  inherited Create;
  FName        := AName;
  FDescription := ADescription;
end;

function TAiNullTool.GetName: String;
begin
  Result := FName;
end;

function TAiNullTool.GetDescription: String;
begin
  Result := FDescription;
end;

function TAiNullTool.GetCategory: String;
begin
  Result := 'Null';
end;

function TAiNullTool.GetSchema: TJSONObject;
begin
  Result := nil;
end;

function TAiNullTool.Execute(const AArgs: TJSONObject): TJSONObject;
begin
  Result := nil;
end;

function TAiNullTool.IsAvailable: Boolean;
begin
  Result := False;
end;

end.
