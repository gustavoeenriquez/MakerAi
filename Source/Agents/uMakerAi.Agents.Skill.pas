unit uMakerAi.Agents.Skill;

// MIT License
//
// Copyright (c) 2026 Gustavo Enríquez - CimaMaker
//
// TAiSkill — configuración reutilizable de un agente LLM.
//
// Encapsula: DriverName, Model, ApiKey, SystemPrompt y una lista de
// herramientas adicionales (ExtraTools) identificadas por nombre en el
// TAiToolRegistry. Acepta dos formatos:
//
//   1) SKILL.md — el formato de las Agent Skills y del registry PPM
//      (frontmatter YAML + instrucciones en Markdown). Lo entiende
//      uMakerAi.Skills.Format; el cuerpo pasa a SystemPrompt, 'model' a
//      Model y 'allowed-tools' a ExtraTools. Como extensión de MakerAI se
//      acepta además la clave 'driver' en el frontmatter.
//
//        Node.Skill := TAiSkill.FromPPM('skill-code-review');
//        Node.Skill := TAiSkill.FromFolder('skills\revisor');   // SKILL.md
//
//   2) JSON propio (archivos locales del desarrollador):
//      {
//        "name":         "code-reviewer",
//        "description":  "Revisa código en busca de errores y mejoras",
//        "driverName":   "Claude",
//        "model":        "claude-sonnet-4-6",
//        "apiKey":       "@CLAUDE_API_KEY",
//        "systemPrompt": "Eres un revisor experto...",
//        "extraTools":   ["filesystem", "git"]
//      }
//
// Seguridad: 'apiKey' SOLO se lee del JSON, que es un archivo del propio
// desarrollador. Un SKILL.md (de disco o del registry) es contenido de
// terceros y nunca aporta credenciales: sin esto, un skill descargado podría
// pedir '@CUALQUIER_VARIABLE' y el nodo enviaría ese secreto al proveedor.
//
// Autor: Gustavo Enríquez
// GitHub: https://github.com/gustavoeenriquez/MakerAi

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils,
  uMakerAi.Skills.Format;

type
  // Formato del que se cargó el skill
  TAiSkillFormat = (sfNone, sfJSON, sfSkillMd);

  { TAiSkill -----------------------------------------------------------------
    Contenedor de configuración reutilizable para nodos LLM y conexiones de chat.

    Reglas de merge al aplicar sobre un TLLMNode (ver TLLMNode.ResolveConfig):
      - DriverName / Model / ApiKey: el valor explícito del nodo gana; si el
        nodo lo deja vacío se usa el del skill. El Model del skill solo se
        usa si pertenece al driver efectivo.
      - SystemPrompt: se CONCATENAN — primero el del skill (las instrucciones
        base) y después el del nodo (ajustes para ese nodo).
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
    FFormat      : TAiSkillFormat;
    FSource      : TAiSkillSource;
    FSourcePath  : String;
    FVersion     : String;

    procedure ParseJSON(AJson: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;

    // Limpia todos los campos al estado inicial vacío
    procedure Clear;

    // Carga desde string JSON (formato propio)
    procedure LoadFromJSON(const AJsonStr: String);

    // Carga desde disco. Una carpeta o un archivo .md se leen como SKILL.md;
    // cualquier otro archivo, como JSON (compatibilidad con versiones previas).
    procedure LoadFromFile(const APath: String);

    // Carga un SKILL.md ya parseado (o su texto)
    procedure LoadFromSkillDoc(ADoc: TAiSkillDoc);
    procedure LoadFromSkillText(const AText: String);

    // Descarga el SKILL.md del registry PPM.
    // AName: nombre del paquete ('skill-code-review'; también acepta
    //   'code-review' y prueba con el prefijo 'skill-').
    // ARegistryUrl: URL del registry; vacío = https://registry.cimamaker.com.
    // AVersion: vacío = la última versión no retirada.
    procedure LoadFromPPM(const AName: String; const ARegistryUrl: String = '';
                          const AVersion: String = '');

    // Factorías — crean, cargan y devuelven una instancia lista para usar.
    // El caller toma ownership del objeto devuelto.
    class function FromJSON(const AJsonStr: String): TAiSkill;
    class function FromFile(const APath: String): TAiSkill;
    // SKILL.md explícito (archivo) o carpeta que lo contiene
    class function FromSkillFile(const APath: String): TAiSkill;
    class function FromFolder(const AFolder: String): TAiSkill;
    class function FromPPM(const AName: String; const ARegistryUrl: String = '';
                           const AVersion: String = ''): TAiSkill;

    // Nombre identificador del skill (coincide con el nombre en PPM)
    property Name: String read FName write FName;
    // Descripción legible del skill
    property Description: String read FDescription write FDescription;
    // Driver LLM sugerido ('OpenAI', 'Claude', 'Gemini', etc.)
    property DriverName: String read FDriverName write FDriverName;
    // Modelo sugerido (vacío = default del driver)
    property Model: String read FModel write FModel;
    // API key; soporta sintaxis @ENV_VAR para resolución en runtime.
    // Solo se carga desde JSON (nunca desde un SKILL.md).
    property ApiKey: String read FApiKey write FApiKey;
    // System prompt del skill — el principal aporte reutilizable
    property SystemPrompt: String read FSystemPrompt write FSystemPrompt;
    // Nombres de herramientas del TAiToolRegistry a activar para este skill.
    // Los nombres que no existen en el registry se ignoran al cargar las tools
    // (p.ej. 'Read'/'Bash' de un skill pensado para Claude Code).
    property ExtraTools: TStringList read FExtraTools;
    // Formato y origen de la última carga
    property Format: TAiSkillFormat read FFormat;
    property Source: TAiSkillSource read FSource;
    property SourcePath: String read FSourcePath;
    // Versión del paquete PPM (vacío si no viene del registry)
    property Version: String read FVersion;
  end;

implementation

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
  FFormat       := sfNone;
  FSource       := ssText;
  FSourcePath   := '';
  FVersion      := '';
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
  JVal: TJSONValue;
begin
  JVal := TJSONObject.ParseJSONValue(AJsonStr);
  if not (JVal is TJSONObject) then
  begin
    JVal.Free;
    raise EAiSkillError.CreateFmt('TAiSkill: JSON inválido — no es un objeto: %s',
                                  [Copy(AJsonStr, 1, 80)]);
  end;
  try
    Clear;
    ParseJSON(TJSONObject(JVal));
    FFormat := sfJSON;
  finally
    JVal.Free;
  end;
end;

procedure TAiSkill.LoadFromSkillDoc(ADoc: TAiSkillDoc);
begin
  Clear;
  if not Assigned(ADoc) then Exit;

  FName         := ADoc.Name;
  FDescription  := ADoc.Description;
  FModel        := ADoc.Model;
  FSystemPrompt := ADoc.Body;
  FExtraTools.Assign(ADoc.AllowedTools);
  // Extensión de MakerAI: el SKILL.md puede fijar el driver
  FDriverName   := ADoc.Extra.Values['driver'];
  if FDriverName = '' then
    FDriverName := ADoc.Extra.Values['drivername'];
  // ApiKey: nunca desde un SKILL.md (ver la nota de seguridad arriba)
  FFormat       := sfSkillMd;
  FSource       := ADoc.Source;
  FSourcePath   := ADoc.SourcePath;
  FVersion      := ADoc.Version;
end;

procedure TAiSkill.LoadFromSkillText(const AText: String);
var
  Doc: TAiSkillDoc;
begin
  Doc := TAiSkillDoc.Parse(AText);
  try
    LoadFromSkillDoc(Doc);
  finally
    Doc.Free;
  end;
end;

procedure TAiSkill.LoadFromFile(const APath: String);
var
  Doc: TAiSkillDoc;
begin
  if TDirectory.Exists(APath) or SameText(TPath.GetExtension(APath), '.md') then
  begin
    Doc := TAiSkillDoc.FromFile(APath);
    try
      LoadFromSkillDoc(Doc);
    finally
      Doc.Free;
    end;
    Exit;
  end;

  if not TFile.Exists(APath) then
    raise EAiSkillError.CreateFmt('TAiSkill: archivo no encontrado: %s', [APath]);
  LoadFromJSON(TFile.ReadAllText(APath, TEncoding.UTF8));
  FSource := ssFile;
  FSourcePath := APath;
end;

procedure TAiSkill.LoadFromPPM(const AName: String; const ARegistryUrl: String;
  const AVersion: String);
var
  Doc: TAiSkillDoc;
begin
  Doc := TAiSkillDoc.FromPPM(AName, AVersion, ARegistryUrl);
  try
    LoadFromSkillDoc(Doc);
  finally
    Doc.Free;
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

class function TAiSkill.FromSkillFile(const APath: String): TAiSkill;
var
  Doc: TAiSkillDoc;
begin
  Result := TAiSkill.Create;
  try
    Doc := TAiSkillDoc.FromFile(APath);
    try
      Result.LoadFromSkillDoc(Doc);
    finally
      Doc.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkill.FromFolder(const AFolder: String): TAiSkill;
begin
  Result := FromSkillFile(AFolder);
end;

class function TAiSkill.FromPPM(const AName: String; const ARegistryUrl: String;
  const AVersion: String): TAiSkill;
begin
  Result := TAiSkill.Create;
  try
    Result.LoadFromPPM(AName, ARegistryUrl, AVersion);
  except
    Result.Free;
    raise;
  end;
end;

end.
