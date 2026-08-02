// MIT License
//
// MakerAI - Guardrails: politica de seguridad para tool calls (fase 3)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Guardrails;

// -----------------------------------------------------------------------------
// TAiGuardrails: intercepta las ejecuciones de tools ANTES de que ocurran.
//
// Se asigna a TAiFunctions.Guardrails; DoCallFunction consulta CheckToolCall
// antes de despachar cualquier tool (local, MCP o AutoMCP). Si la politica
// bloquea, el tool NO se ejecuta y el LLM recibe un error JSON explicando el
// motivo (puede replantear su plan).
//
// Politica (en orden de evaluacion):
//   1. Enabled=False               -> todo permitido (no-op).
//   2. AllowedTools no vacia       -> whitelist estricta (comodin * soportado).
//   3. BlockedTools                -> blocklist por nombre (comodin *).
//   4. BlockedArgPatterns          -> substrings prohibidos en los argumentos
//                                     JSON (case-insensitive; p.ej. 'rm -rf',
//                                     'DROP TABLE', '..\').
//   5. OnCheckToolCall             -> veto/permiso programatico final.
//
// Cada bloqueo dispara OnBlocked (auditoria) e incrementa BlockedCount.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, System.Masks;

type
  // Veto/permiso programatico. AAllow llega con el veredicto de las listas;
  // el handler puede revertirlo en cualquier direccion y explicar en AReason.
  TAiGuardrailCheckEvent = procedure(Sender: TObject; const AToolName, AArguments: string;
    var AAllow: Boolean; var AReason: string) of object;

  // Auditoria: se dispara por cada tool call bloqueado.
  TAiGuardrailBlockedEvent = procedure(Sender: TObject;
    const AToolName, AArguments, AReason: string) of object;

  TAiGuardrails = class(TComponent)
  private
    FEnabled: Boolean;
    FAllowedTools: TStrings;
    FBlockedTools: TStrings;
    FBlockedArgPatterns: TStrings;
    FOnCheckToolCall: TAiGuardrailCheckEvent;
    FOnBlocked: TAiGuardrailBlockedEvent;
    FBlockedCount: Int64;
    procedure SetAllowedTools(const Value: TStrings);
    procedure SetBlockedTools(const Value: TStrings);
    procedure SetBlockedArgPatterns(const Value: TStrings);
    class function MatchesAny(AList: TStrings; const AValue: string): Boolean; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // True = permitido. Si devuelve False, AReason explica el bloqueo y ya se
    // disparo OnBlocked / se incremento BlockedCount.
    function CheckToolCall(const AToolName, AArguments: string; out AReason: string): Boolean;

    property BlockedCount: Int64 read FBlockedCount;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    // Whitelist estricta (si no esta vacia, solo estos tools pueden ejecutarse).
    // Acepta comodines de mascara: 'file_*', '*_read', etc.
    property AllowedTools: TStrings read FAllowedTools write SetAllowedTools;
    // Blocklist por nombre (comodines de mascara)
    property BlockedTools: TStrings read FBlockedTools write SetBlockedTools;
    // Substrings prohibidos dentro del JSON de argumentos (case-insensitive)
    property BlockedArgPatterns: TStrings read FBlockedArgPatterns write SetBlockedArgPatterns;
    property OnCheckToolCall: TAiGuardrailCheckEvent read FOnCheckToolCall write FOnCheckToolCall;
    property OnBlocked: TAiGuardrailBlockedEvent read FOnBlocked write FOnBlocked;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGuardrails]);
end;

{ TAiGuardrails }

constructor TAiGuardrails.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FAllowedTools := TStringList.Create;
  FBlockedTools := TStringList.Create;
  FBlockedArgPatterns := TStringList.Create;
end;

destructor TAiGuardrails.Destroy;
begin
  FAllowedTools.Free;
  FBlockedTools.Free;
  FBlockedArgPatterns.Free;
  inherited;
end;

procedure TAiGuardrails.SetAllowedTools(const Value: TStrings);
begin
  FAllowedTools.Assign(Value);
end;

procedure TAiGuardrails.SetBlockedTools(const Value: TStrings);
begin
  FBlockedTools.Assign(Value);
end;

procedure TAiGuardrails.SetBlockedArgPatterns(const Value: TStrings);
begin
  FBlockedArgPatterns.Assign(Value);
end;

class function TAiGuardrails.MatchesAny(AList: TStrings; const AValue: string): Boolean;
var
  i: Integer;
  Pattern: string;
begin
  Result := False;
  for i := 0 to AList.Count - 1 do
  begin
    Pattern := Trim(AList[i]);
    if Pattern = '' then
      Continue;
    // TMask soporta * y ? ; comparacion case-insensitive via lowercase
    if MatchesMask(AValue.ToLower, Pattern.ToLower) then
      Exit(True);
  end;
end;

function TAiGuardrails.CheckToolCall(const AToolName, AArguments: string; out AReason: string): Boolean;
var
  i: Integer;
  Pattern: string;
begin
  Result := True;
  AReason := '';

  if not FEnabled then
    Exit;

  // 1. Whitelist estricta
  if (FAllowedTools.Count > 0) and (not MatchesAny(FAllowedTools, AToolName)) then
  begin
    Result := False;
    AReason := Format('tool "%s" is not in the allowed list', [AToolName]);
  end;

  // 2. Blocklist por nombre
  if Result and MatchesAny(FBlockedTools, AToolName) then
  begin
    Result := False;
    AReason := Format('tool "%s" is blocked by policy', [AToolName]);
  end;

  // 3. Patrones prohibidos en los argumentos
  if Result then
    for i := 0 to FBlockedArgPatterns.Count - 1 do
    begin
      Pattern := Trim(FBlockedArgPatterns[i]);
      if (Pattern <> '') and ContainsText(AArguments, Pattern) then
      begin
        Result := False;
        AReason := Format('arguments contain forbidden pattern "%s"', [Pattern]);
        Break;
      end;
    end;

  // 4. Veto/permiso programatico (puede revertir el veredicto de las listas)
  if Assigned(FOnCheckToolCall) then
    FOnCheckToolCall(Self, AToolName, AArguments, Result, AReason);

  if not Result then
  begin
    if AReason = '' then
      AReason := 'blocked by guardrails policy';
    Inc(FBlockedCount);
    if Assigned(FOnBlocked) then
      FOnBlocked(Self, AToolName, AArguments, AReason);
  end;
end;

end.
