unit uMakerAi.Agents.Skill;

// MIT License
//
// Copyright (c) 2026 Gustavo Enríquez - CimaMaker
//
// TAiSkill — configuración reutilizable de un agente LLM.
//
// Encapsula: DriverName, Model, ApiKey, SystemPrompt y una lista de
// herramientas adicionales (ExtraTools) identificadas por nombre en el
// TAiToolRegistry. Puede cargarse desde un JSON inline, un archivo local
// o descargarse del registry PPM (https://ppm.pascalai.org).
//
// Flujo de uso con TLLMNode:
//   Node.Skill := TAiSkill.FromFile('skills/code-reviewer.json');
//   // El nodo aplica el skill como base; sus props explícitas tienen prioridad.
//
// Formato JSON esperado:
//   {
//     "name":         "code-reviewer",
//     "description":  "Revisa código en busca de errores y mejoras",
//     "driverName":   "Claude",
//     "model":        "claude-sonnet-4-6",
//     "apiKey":       "@CLAUDE_API_KEY",
//     "systemPrompt": "Eres un revisor experto...",
//     "extraTools":   ["filesystem", "git"]
//   }
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  System.Net.HttpClient;

type
  { TAiSkill -----------------------------------------------------------------
    Contenedor de configuración reutilizable para nodos LLM y conexiones de chat.

    Reglas de merge al aplicar sobre un nodo/chat:
      - El skill se aplica como BASE (valores por defecto).
      - Las propiedades explícitas del nodo/chat sobrescriben las del skill.
      - ExtraTools es SIEMPRE aditivo: se suman a las tools ya cargadas.

    Gestión de memoria:
      - Cuando se asigna a TLLMNode.Skill, el nodo toma ownership y libera
        el skill en su destructor.
      - Para uso standalone, el caller es responsable de liberar la instancia.
  }
  TAiSkill = class
  private
    FName        : String;
    FDescription : String;
    FDriverName  : String;
    FModel       : String;
    FApiKey      : String;
    FSystemPrompt: String;
    FExtraTools  : TStringList;

    procedure ParseJSON(AJson: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;

    // Limpia todos los campos al estado inicial vacío
    procedure Clear;

    // Carga desde string JSON
    procedure LoadFromJSON(const AJsonStr: String);

    // Carga desde archivo local UTF-8
    procedure LoadFromFile(const APath: String);

    // Descarga desde el registry PPM.
    // AName: nombre del skill en el registry (ej. 'code-reviewer').
    // ABaseUrl: URL base del registry; si vacío usa el default de PPM.
    procedure LoadFromPPM(const AName: String; const ABaseUrl: String = '');

    // Factorías — crean, cargan y devuelven una instancia lista para usar.
    // El caller toma ownership del objeto devuelto.
    class function FromJSON(const AJsonStr: String): TAiSkill;
    class function FromFile(const APath: String): TAiSkill;
    class function FromPPM(const AName: String;
                           const ABaseUrl: String = ''): TAiSkill;

    // Nombre identificador del skill (coincide con el nombre en PPM)
    property Name: String read FName write FName;
    // Descripción legible del skill
    property Description: String read FDescription write FDescription;
    // Driver LLM sugerido ('OpenAI', 'Claude', 'Gemini', etc.)
    property DriverName: String read FDriverName write FDriverName;
    // Modelo sugerido (vacío = default del driver)
    property Model: String read FModel write FModel;
    // API key; soporta sintaxis @ENV_VAR para resolución en runtime
    property ApiKey: String read FApiKey write FApiKey;
    // System prompt del skill — el principal aporte reutilizable
    property SystemPrompt: String read FSystemPrompt write FSystemPrompt;
    // Nombres de herramientas del TAiToolRegistry a activar para este skill
    property ExtraTools: TStringList read FExtraTools;
  end;

implementation

const
  // URL base del registry PPM para descarga de skills.
  // Ruta esperada: {base}/{name}/skill.json
  PPM_SKILL_BASE_URL = 'https://ppm.pascalai.org/api/v1/packages/';

{ TAiSkill }

constructor TAiSkill.Create;
begin
  inherited Create;
  FExtraTools            := TStringList.Create;
  FExtraTools.Duplicates := dupIgnore;
  FExtraTools.CaseSensitive := False;
end;

destructor TAiSkill.Destroy;
begin
  FExtraTools.Free;
  inherited;
end;

procedure TAiSkill.Clear;
begin
  FName         := '';
  FDescription  := '';
  FDriverName   := '';
  FModel        := '';
  FApiKey       := '';
  FSystemPrompt := '';
  FExtraTools.Clear;
end;

procedure TAiSkill.ParseJSON(AJson: TJSONObject);
var
  JArr: TJSONArray;
  I   : Integer;
begin
  if not Assigned(AJson) then Exit;

  FName         := AJson.GetValue<String>('name',         '');
  FDescription  := AJson.GetValue<String>('description',  '');
  FDriverName   := AJson.GetValue<String>('driverName',   '');
  FModel        := AJson.GetValue<String>('model',        '');
  FApiKey       := AJson.GetValue<String>('apiKey',       '');
  FSystemPrompt := AJson.GetValue<String>('systemPrompt', '');

  FExtraTools.Clear;
  if AJson.TryGetValue<TJSONArray>('extraTools', JArr) then
    for I := 0 to JArr.Count - 1 do
      FExtraTools.Add(JArr.Items[I].Value);
end;

procedure TAiSkill.LoadFromJSON(const AJsonStr: String);
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  if not Assigned(JObj) then
    raise Exception.CreateFmt('TAiSkill: JSON inválido — no es un objeto: %s',
                              [Copy(AJsonStr, 1, 80)]);
  try
    Clear;
    ParseJSON(JObj);
  finally
    JObj.Free;
  end;
end;

procedure TAiSkill.LoadFromFile(const APath: String);
begin
  if not TFile.Exists(APath) then
    raise Exception.CreateFmt('TAiSkill: archivo no encontrado: %s', [APath]);
  LoadFromJSON(TFile.ReadAllText(APath, TEncoding.UTF8));
end;

procedure TAiSkill.LoadFromPPM(const AName: String; const ABaseUrl: String);
var
  LClient  : THTTPClient;
  LResponse: IHTTPResponse;
  LBase    : String;
  LUrl     : String;
begin
  LBase := ABaseUrl;
  if LBase = '' then
    LBase := PPM_SKILL_BASE_URL;
  if not LBase.EndsWith('/') then
    LBase := LBase + '/';

  LUrl := LBase + AName + '/skill.json';

  LClient := THTTPClient.Create;
  try
    LResponse := LClient.Get(LUrl);
    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt(
        'TAiSkill.LoadFromPPM: error HTTP %d al descargar skill "%s" desde %s',
        [LResponse.StatusCode, AName, LUrl]);
    LoadFromJSON(LResponse.ContentAsString);
  finally
    LClient.Free;
  end;
end;

// --- Factorías --------------------------------------------------------------

class function TAiSkill.FromJSON(const AJsonStr: String): TAiSkill;
begin
  Result := TAiSkill.Create;
  try
    Result.LoadFromJSON(AJsonStr);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkill.FromFile(const APath: String): TAiSkill;
begin
  Result := TAiSkill.Create;
  try
    Result.LoadFromFile(APath);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkill.FromPPM(const AName: String;
                                const ABaseUrl: String): TAiSkill;
begin
  Result := TAiSkill.Create;
  try
    Result.LoadFromPPM(AName, ABaseUrl);
  except
    Result.Free;
    raise;
  end;
end;

end.
