// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.MCPClient.Core
// Stub mínimo para Phase 2 (Tools). Implementación completa en Fase 6.
unit uMakerAi.MCPClient.Core;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpjson;

type
  // Tipos de transporte MCP
  TToolTransportType = (tpStdIo, tpHttp, tpSSE, tpMakerAi);

  // Eventos MCP
  TMCPStatusEvent = procedure(Sender: TObject; const StatusMsg: string) of object;
  TMCPLogEvent    = procedure(Sender: TObject; const Msg: string) of object;
  TMCPStreamMessageEvent = procedure(Sender: TObject; const Msg: string) of object;

  // Clase base de clientes MCP — stub para compilación
  TMCPClientCustom = class(TComponent)
  private
    FInitialized   : Boolean;
    FAvailable     : Boolean;
    FEnabled       : Boolean;
    FTransportType : TToolTransportType;
    FParams        : TStringList;
    FEnvVars       : TStringList;
    FTools         : TStringList;
    FOnLog         : TMCPLogEvent;
    FOnStatusUpdate: TMCPStatusEvent;
    FOnStreamMessage: TMCPStreamMessageEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Inicializa el cliente (carga lista de herramientas del servidor)
    function Initialize: Boolean; virtual;

    // Lista las herramientas disponibles como JSON object
    function ListTools: TJSONObject; virtual;

    // Llama a una herramienta. AMedia es TObject para evitar dep. circular con uMakerAi.Core
    function CallTool(const AName: string; AArgs: TJSONObject;
        AMedia: TObject): TJSONObject; virtual;

    property Initialized   : Boolean read FInitialized;
    property Available     : Boolean read FAvailable;
    property TransportType : TToolTransportType read FTransportType write FTransportType;
    property Params        : TStringList read FParams;
    property EnvVars       : TStringList read FEnvVars;
    property Tools         : TStringList read FTools;

  published
    property Enabled        : Boolean read FEnabled write FEnabled default True;
    property OnLog          : TMCPLogEvent read FOnLog write FOnLog;
    property OnStatusUpdate : TMCPStatusEvent read FOnStatusUpdate write FOnStatusUpdate;
    property OnStreamMessage: TMCPStreamMessageEvent read FOnStreamMessage write FOnStreamMessage;
  end;

implementation

{ TMCPClientCustom }

constructor TMCPClientCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialized    := False;
  FAvailable      := False;
  FEnabled        := True;
  FTransportType  := tpStdIo;
  FParams         := TStringList.Create;
  FEnvVars        := TStringList.Create;
  FTools          := TStringList.Create;
end;

destructor TMCPClientCustom.Destroy;
begin
  FParams.Free;
  FEnvVars.Free;
  FTools.Free;
  inherited;
end;

function TMCPClientCustom.Initialize: Boolean;
begin
  Result := False;
end;

function TMCPClientCustom.ListTools: TJSONObject;
begin
  Result := nil;
end;

function TMCPClientCustom.CallTool(const AName: string; AArgs: TJSONObject;
    AMedia: TObject): TJSONObject;
begin
  Result := nil;
end;

end.
