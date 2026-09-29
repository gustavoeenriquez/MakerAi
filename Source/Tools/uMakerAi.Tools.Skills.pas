unit uMakerAi.Tools.Skills;

// MIT License
//
// Copyright (c) 2026 Gustavo Enríquez - CimaMaker
//
// TAiSkills — skills bajo demanda para cualquier chat (estilo Agent Skills).
//
// El modelo NO recibe las instrucciones de todos los skills en el system
// prompt. Recibe solo un catálogo (nombre + cuándo usarlo) dentro de la
// descripción de una función, y carga las instrucciones completas cuando las
// necesita llamando a esa función:
//
//   use_skill(name)              -> instrucciones del skill
//   read_skill_file(name, path)  -> un archivo que acompaña al skill
//                                   (solo skills de carpeta, confinado a ella)
//
// Así se pueden tener decenas de skills disponibles sin pagar sus tokens en
// cada turno. Funciona con cualquier driver con function calling, porque son
// funciones normales de TAiFunctions (y pasan por TAiGuardrails si hay).
//
// Uso:
//   Skills.Functions := AiFunctions1;          // registra use_skill
//   Skills.LoadFromFolder('Skills');           // <dir>/<nombre>/SKILL.md
//   Skills.LoadFromPPM('skill-code-review');   // registry PPM
//   Skills.AddSkill('tono', 'Úsalo al redactar correos', 'Escribe formal...');
//   AiConnection.AiFunctions := AiFunctions1;
//
// Seguridad: un skill es texto que el modelo va a obedecer. Un skill de
// terceros (PPM, carpeta compartida) es una vía de prompt injection: cargar
// solo de fuentes confiables. La respuesta de use_skill marca el origen, y
// OnBeforeUseSkill permite vetar. read_skill_file nunca ejecuta nada, rechaza
// rutas que salgan de la carpeta del skill (.., absolutas, enlaces
// simbólicos) y archivos binarios o más grandes que MaxFileSize.

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils, System.StrUtils,
  uMakerAi.Skills.Format, uMakerAi.Tools.Functions, uMakerAi.Chat.Messages;

const
  AI_SKILLS_USE_FUNCTION  = 'use_skill';
  AI_SKILLS_READ_FUNCTION = 'read_skill_file';

type
  TAiSkills = class;

  TAiSkillItem = class(TCollectionItem)
  private
    FName        : string;
    FDescription : string;
    FInstructions: TStrings;
    FEnabled     : Boolean;
    FFolder      : string;
    FSource      : TAiSkillSource;
    FSourcePath  : string;
    FVersion     : string;
    FModel       : string;
    procedure SetName(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetInstructions(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure InstructionsChanged(Sender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromSkillDoc(ADoc: TAiSkillDoc);

    // Carpeta del skill (solo si se cargó de disco): raíz de read_skill_file
    property Folder: string read FFolder write FFolder;
    property Source: TAiSkillSource read FSource write FSource;
    property SourcePath: string read FSourcePath;
    property Version: string read FVersion;
    // 'model' del frontmatter (informativo: TAiSkills no cambia el modelo)
    property Model: string read FModel;
  published
    property Name: string read FName write SetName;
    // Cuándo usar el skill. Es lo que ve el modelo en el catálogo: escribirla
    // pensando en él ("Úsalo cuando el usuario pida...").
    property Description: string read FDescription write SetDescription;
    property Instructions: TStrings read FInstructions write SetInstructions;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TAiSkillItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TAiSkillItem;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TAiSkillItem;
    property Items[Index: Integer]: TAiSkillItem read GetItem; default;
  end;

  TAiSkillEvent = procedure(Sender: TObject; ASkill: TAiSkillItem) of object;
  TAiSkillAllowEvent = procedure(Sender: TObject; ASkill: TAiSkillItem;
    var Allow: Boolean) of object;
  TAiSkillFileEvent = procedure(Sender: TObject; ASkill: TAiSkillItem;
    const APath: string; var Allow: Boolean) of object;

  TAiSkills = class(TComponent)
  private
    FSkills          : TAiSkillItems;
    FFunctions       : TAiFunctions;
    FUseItem         : TFunctionActionItem;
    FReadItem        : TFunctionActionItem;
    FFolder          : string;
    FUseFunctionName : string;
    FReadFunctionName: string;
    FAllowFileAccess : Boolean;
    FMaxFileSize     : Integer;
    FUpdating        : Integer;
    FOnSkillLoaded   : TAiSkillEvent;
    FOnBeforeUseSkill: TAiSkillAllowEvent;
    FOnBeforeReadFile: TAiSkillFileEvent;
    procedure SetFunctions(const Value: TAiFunctions);
    procedure SetSkills(const Value: TAiSkillItems);
    procedure SetFolder(const Value: string);
    procedure SetUseFunctionName(const Value: string);
    procedure SetReadFunctionName(const Value: string);
    procedure SetAllowFileAccess(const Value: Boolean);
    procedure UnregisterTools;
    function HasFolderSkills: Boolean;
    function BuildUseDescription: string;
    function BuildUseSchema: string;
    function ListSkillFiles(ASkill: TAiSkillItem): TArray<string>;
    procedure DoUseSkillAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure DoReadFileAction(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Carga de skills. Un skill con el mismo nombre que uno existente lo
    // reemplaza. Todas devuelven el item (o la cantidad) y lanzan
    // EAiSkillError ante un fallo.
    function AddSkill(const AName, ADescription, AInstructions: string): TAiSkillItem;
    function LoadFromFile(const APath: string): TAiSkillItem;      // SKILL.md o su carpeta
    function LoadFromFolder(const AFolder: string): Integer;        // <dir>/<nombre>/SKILL.md
    function LoadFromPPM(const AName: string; const ARegistryUrl: string = '';
                         const AVersion: string = ''): TAiSkillItem;
    function Find(const AName: string): TAiSkillItem;

    // Agrupa cambios: el schema de las funciones se regenera una sola vez
    procedure BeginUpdate;
    procedure EndUpdate;

    // Regenera las funciones en Functions (catálogo y enum de nombres).
    // Se llama sola al cargar, agregar o cambiar skills.
    procedure RegisterTools;

    // Lo que devuelven las funciones, sin pasar por el LLM (pruebas, UIs que
    // quieran mostrar el skill, o uso directo).
    function ExecuteUseSkill(const AName: string): string;
    function ExecuteReadFile(const AName, APath: string): string;

    // Catálogo tal como lo ve el modelo
    function Catalog: string;
  published
    property Skills: TAiSkillItems read FSkills write SetSkills;
    // TAiFunctions donde se registran use_skill / read_skill_file
    property Functions: TAiFunctions read FFunctions write SetFunctions;
    // Carpeta de skills que se carga al iniciar (solo en runtime)
    property Folder: string read FFolder write SetFolder;
    property UseFunctionName: string read FUseFunctionName write SetUseFunctionName;
    property ReadFunctionName: string read FReadFunctionName write SetReadFunctionName;
    // Registrar read_skill_file (solo aplica si hay skills de carpeta)
    property AllowFileAccess: Boolean read FAllowFileAccess write SetAllowFileAccess default True;
    // Tamaño máximo de un archivo leído con read_skill_file (bytes)
    property MaxFileSize: Integer read FMaxFileSize write FMaxFileSize default 262144;
    // El modelo cargó un skill (auditoría, UI)
    property OnSkillLoaded: TAiSkillEvent read FOnSkillLoaded write FOnSkillLoaded;
    // Veto antes de entregar un skill
    property OnBeforeUseSkill: TAiSkillAllowEvent read FOnBeforeUseSkill write FOnBeforeUseSkill;
    // Veto antes de entregar un archivo (APath ya resuelto y validado)
    property OnBeforeReadFile: TAiSkillFileEvent read FOnBeforeReadFile write FOnBeforeReadFile;
  end;

procedure Register;

implementation

uses
  uMakerAi.Telemetry;

const
  MAX_LISTED_FILES = 50;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiSkills]);
end;

function SourceLabel(ASource: TAiSkillSource): string;
begin
  case ASource of
    ssFile: Result := 'file';
    ssPPM:  Result := 'ppm';
  else
    Result := 'inline';
  end;
end;

// Prefijo de ruta con la sensibilidad a mayúsculas del sistema de archivos
function PathStartsWith(const APath, APrefix: string): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := StartsText(APrefix, APath);
{$ELSE}
  Result := StartsStr(APrefix, APath);
{$ENDIF}
end;

// TFileAttribute cambia de miembros entre Windows y POSIX: faSymLink existe en
// ambos y faReparsePoint solo en Windows (de ahí el IFDEF)
{$WARN SYMBOL_PLATFORM OFF}
function IsLinkOrReparse(const APath: string): Boolean;
var
  Attrs: TFileAttributes;
begin
  Result := False;
  try
    Attrs := TFile.GetAttributes(APath, False);
  except
    Exit;
  end;
  Result := TFileAttribute.faSymLink in Attrs;
{$IFDEF MSWINDOWS}
  Result := Result or (TFileAttribute.faReparsePoint in Attrs);
{$ENDIF}
end;
{$WARN SYMBOL_PLATFORM DEFAULT}

{ TAiSkillItem }

constructor TAiSkillItem.Create(Collection: TCollection);
begin
  FInstructions := TStringList.Create;
  TStringList(FInstructions).OnChange := InstructionsChanged;
  FEnabled := True;
  FSource := ssText;
  inherited Create(Collection);
end;

destructor TAiSkillItem.Destroy;
begin
  FInstructions.Free;
  inherited;
end;

procedure TAiSkillItem.Assign(Source: TPersistent);
var
  S: TAiSkillItem;
begin
  if Source is TAiSkillItem then
  begin
    S := TAiSkillItem(Source);
    FName := S.FName;
    FDescription := S.FDescription;
    FInstructions.Assign(S.FInstructions);
    FEnabled := S.FEnabled;
    FFolder := S.FFolder;
    FSource := S.FSource;
    FSourcePath := S.FSourcePath;
    FVersion := S.FVersion;
    FModel := S.FModel;
    Changed(False);
  end
  else
    inherited;
end;

procedure TAiSkillItem.LoadFromSkillDoc(ADoc: TAiSkillDoc);
begin
  FName := ADoc.Name;
  FDescription := ADoc.Description;
  FModel := ADoc.Model;
  FSource := ADoc.Source;
  FSourcePath := ADoc.SourcePath;
  FVersion := ADoc.Version;
  if ADoc.Source = ssFile then
    FFolder := TPath.GetDirectoryName(ADoc.SourcePath)
  else
    FFolder := '';
  FInstructions.Text := ADoc.Body; // dispara Changed
end;

function TAiSkillItem.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TAiSkillItem.InstructionsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TAiSkillItem.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TAiSkillItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TAiSkillItem.SetInstructions(const Value: TStrings);
begin
  FInstructions.Assign(Value);
end;

procedure TAiSkillItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Trim(Value);
    Changed(False);
  end;
end;

{ TAiSkillItems }

function TAiSkillItems.Add: TAiSkillItem;
begin
  Result := TAiSkillItem(inherited Add);
end;

function TAiSkillItems.GetItem(Index: Integer): TAiSkillItem;
begin
  Result := TAiSkillItem(inherited Items[Index]);
end;

procedure TAiSkillItems.Update(Item: TCollectionItem);
begin
  inherited;
  if GetOwner is TAiSkills then
    TAiSkills(GetOwner).RegisterTools;
end;

{ TAiSkills }

constructor TAiSkills.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSkills := TAiSkillItems.Create(Self, TAiSkillItem);
  FUseFunctionName := AI_SKILLS_USE_FUNCTION;
  FReadFunctionName := AI_SKILLS_READ_FUNCTION;
  FAllowFileAccess := True;
  FMaxFileSize := 262144;
end;

destructor TAiSkills.Destroy;
begin
  // Las funciones apuntan a métodos de este objeto: sacarlas antes de morir
  Inc(FUpdating);
  UnregisterTools;
  FSkills.Free;
  inherited;
end;

procedure TAiSkills.Loaded;
begin
  inherited;
  if (FFolder <> '') and not (csDesigning in ComponentState) and TDirectory.Exists(FFolder) then
    LoadFromFolder(FFolder)
  else
    RegisterTools;
end;

procedure TAiSkills.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FFunctions) then
  begin
    FFunctions := nil;
    // Los items los destruye la colección de Functions
    FUseItem := nil;
    FReadItem := nil;
  end;
end;

procedure TAiSkills.SetFunctions(const Value: TAiFunctions);
begin
  if FFunctions = Value then
    Exit;
  UnregisterTools;
  if Assigned(FFunctions) then
    FFunctions.RemoveFreeNotification(Self);
  FFunctions := Value;
  if Assigned(FFunctions) then
  begin
    FFunctions.FreeNotification(Self);
    RegisterTools;
  end;
end;

procedure TAiSkills.SetSkills(const Value: TAiSkillItems);
begin
  FSkills.Assign(Value);
end;

procedure TAiSkills.SetFolder(const Value: string);
begin
  if FFolder = Value then
    Exit;
  FFolder := Value;
  // En runtime, fuera de la carga del DFM, se aplica en el acto
  if (FFolder <> '') and not (csLoading in ComponentState) and
     not (csDesigning in ComponentState) and TDirectory.Exists(FFolder) then
    LoadFromFolder(FFolder);
end;

procedure TAiSkills.SetUseFunctionName(const Value: string);
begin
  if (Value = '') or (Value = FUseFunctionName) then
    Exit;
  UnregisterTools;
  FUseFunctionName := Value;
  RegisterTools;
end;

procedure TAiSkills.SetReadFunctionName(const Value: string);
begin
  if (Value = '') or (Value = FReadFunctionName) then
    Exit;
  UnregisterTools;
  FReadFunctionName := Value;
  RegisterTools;
end;

procedure TAiSkills.SetAllowFileAccess(const Value: Boolean);
begin
  if FAllowFileAccess = Value then
    Exit;
  FAllowFileAccess := Value;
  RegisterTools;
end;

procedure TAiSkills.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TAiSkills.EndUpdate;
begin
  if FUpdating > 0 then
    Dec(FUpdating);
  if FUpdating = 0 then
    RegisterTools;
end;

function TAiSkills.Find(const AName: string): TAiSkillItem;
var
  I: Integer;
begin
  for I := 0 to FSkills.Count - 1 do
    if SameText(FSkills[I].Name, Trim(AName)) then
      Exit(FSkills[I]);
  Result := nil;
end;

function TAiSkills.AddSkill(const AName, ADescription, AInstructions: string): TAiSkillItem;
begin
  if Trim(AName) = '' then
    raise EAiSkillError.Create('TAiSkills.AddSkill: el skill necesita un nombre');
  BeginUpdate;
  try
    Result := Find(AName);
    if not Assigned(Result) then
      Result := FSkills.Add;
    Result.Name := AName;
    Result.Description := ADescription;
    Result.Instructions.Text := AInstructions;
  finally
    EndUpdate;
  end;
end;

function TAiSkills.LoadFromFile(const APath: string): TAiSkillItem;
var
  Doc: TAiSkillDoc;
begin
  Doc := TAiSkillDoc.FromFile(APath);
  try
    BeginUpdate;
    try
      Result := Find(Doc.Name);
      if not Assigned(Result) then
        Result := FSkills.Add;
      Result.LoadFromSkillDoc(Doc);
    finally
      EndUpdate;
    end;
  finally
    Doc.Free;
  end;
end;

function TAiSkills.LoadFromFolder(const AFolder: string): Integer;
var
  F: string;
begin
  Result := 0;
  BeginUpdate;
  try
    for F in TAiSkillDoc.FindSkillFiles(AFolder) do
    begin
      LoadFromFile(F);
      Inc(Result);
    end;
  finally
    EndUpdate;
  end;
end;

function TAiSkills.LoadFromPPM(const AName, ARegistryUrl, AVersion: string): TAiSkillItem;
var
  Doc: TAiSkillDoc;
begin
  Doc := TAiSkillDoc.FromPPM(AName, AVersion, ARegistryUrl);
  try
    BeginUpdate;
    try
      Result := Find(Doc.Name);
      if not Assigned(Result) then
        Result := FSkills.Add;
      Result.LoadFromSkillDoc(Doc);
    finally
      EndUpdate;
    end;
  finally
    Doc.Free;
  end;
end;

function TAiSkills.HasFolderSkills: Boolean;
var
  I: Integer;
begin
  for I := 0 to FSkills.Count - 1 do
    if FSkills[I].Enabled and (FSkills[I].Folder <> '') then
      Exit(True);
  Result := False;
end;

function TAiSkills.Catalog: string;
var
  SB: TStringBuilder;
  I: Integer;
  S: TAiSkillItem;
  Desc: string;
begin
  SB := TStringBuilder.Create;
  try
    for I := 0 to FSkills.Count - 1 do
    begin
      S := FSkills[I];
      if not S.Enabled or (S.Name = '') then
        Continue;
      // Una línea por skill: el catálogo viaja en cada turno
      Desc := S.Description.Replace(#13#10, ' ').Replace(#10, ' ').Trim;
      if Desc = '' then
        Desc := '(no description)';
      SB.Append('- ').Append(S.Name).Append(': ').Append(Desc).Append(#10);
    end;
    Result := SB.ToString.TrimRight;
  finally
    SB.Free;
  end;
end;

function TAiSkills.BuildUseDescription: string;
begin
  Result :=
    'Loads the full instructions of a skill. Before doing a task that matches ' +
    'one of these skills, call this function with its name and follow the ' +
    'instructions it returns. Available skills:' + #10 + Catalog;
end;

function TAiSkills.BuildUseSchema: string;
var
  Schema, Props, NameProp: TJSONObject;
  Enum, Req: TJSONArray;
  I: Integer;
begin
  Schema := TJSONObject.Create;
  try
    Schema.AddPair('type', 'object');
    Props := TJSONObject.Create;
    Schema.AddPair('properties', Props);
    NameProp := TJSONObject.Create;
    Props.AddPair('name', NameProp);
    NameProp.AddPair('type', 'string');
    NameProp.AddPair('description', 'Name of the skill to load');
    Enum := TJSONArray.Create;
    for I := 0 to FSkills.Count - 1 do
      if FSkills[I].Enabled and (FSkills[I].Name <> '') then
        Enum.Add(FSkills[I].Name);
    NameProp.AddPair('enum', Enum);
    Req := TJSONArray.Create;
    Req.Add('name');
    Schema.AddPair('required', Req);
    Result := Schema.ToJSON;
  finally
    Schema.Free;
  end;
end;

procedure TAiSkills.UnregisterTools;
var
  Idx: Integer;
begin
  if not Assigned(FFunctions) then
  begin
    FUseItem := nil;
    FReadItem := nil;
    Exit;
  end;
  if Assigned(FUseItem) then
  begin
    Idx := FFunctions.Functions.IndexOf(FUseItem.FunctionName);
    if (Idx >= 0) and (FFunctions.Functions[Idx] = FUseItem) then
      FUseItem.Free;
    FUseItem := nil;
  end;
  if Assigned(FReadItem) then
  begin
    Idx := FFunctions.Functions.IndexOf(FReadItem.FunctionName);
    if (Idx >= 0) and (FFunctions.Functions[Idx] = FReadItem) then
      FReadItem.Free;
    FReadItem := nil;
  end;
end;

procedure TAiSkills.RegisterTools;
var
  Any: Boolean;
  I: Integer;
  P: TFunctionParamsItem;
begin
  // En el IDE no se registran: las funciones se guardarían en el DFM del
  // TAiFunctions con un OnAction que apunta a este componente, no al form.
  if (FUpdating > 0) or not Assigned(FFunctions) or (csLoading in ComponentState) or
     (csDestroying in ComponentState) or (csDesigning in ComponentState) then
    Exit;

  Any := False;
  for I := 0 to FSkills.Count - 1 do
    if FSkills[I].Enabled and (FSkills[I].Name <> '') then
      Any := True;

  // use_skill: se reutiliza el item si ya existe (con el mismo nombre)
  if not Assigned(FUseItem) then
  begin
    FUseItem := FFunctions.Functions.GetFunction(FUseFunctionName);
    if not Assigned(FUseItem) then
      FUseItem := FFunctions.Functions.AddFunction(FUseFunctionName, True, DoUseSkillAction);
  end;
  FUseItem.OnAction := DoUseSkillAction;
  FUseItem.Description.Text := BuildUseDescription;
  FUseItem.RawSchemaJson := BuildUseSchema;
  // Sin skills, la función no se ofrece (un enum vacío es un schema inválido)
  FUseItem.Enabled := Any;

  if FAllowFileAccess and Any and HasFolderSkills then
  begin
    if not Assigned(FReadItem) then
    begin
      FReadItem := FFunctions.Functions.GetFunction(FReadFunctionName);
      if not Assigned(FReadItem) then
        FReadItem := FFunctions.Functions.AddFunction(FReadFunctionName, True, DoReadFileAction);
    end;
    FReadItem.OnAction := DoReadFileAction;
    FReadItem.Description.Text :=
      'Reads a supporting file of a skill loaded with ' + FUseFunctionName + ' ' +
      '(references, templates, examples). Use it only when the skill instructions ' +
      'point to that file. Path is relative to the skill folder.';
    if FReadItem.Parameters.Count = 0 then
    begin
      P := FReadItem.Parameters.Add;
      P.Name := 'name';
      P.Description.Text := 'Name of the skill';
      P.ParamType := ptString;
      P.Required := True;
      P := FReadItem.Parameters.Add;
      P.Name := 'path';
      P.Description.Text := 'Relative path of the file inside the skill folder';
      P.ParamType := ptString;
      P.Required := True;
    end;
    FReadItem.Enabled := True;
  end
  else if Assigned(FReadItem) then
    FReadItem.Enabled := False;
end;

function TAiSkills.ListSkillFiles(ASkill: TAiSkillItem): TArray<string>;
var
  List: TStringList;
  Root, F, Rel: string;
begin
  Result := nil;
  if (ASkill.Folder = '') or not TDirectory.Exists(ASkill.Folder) then
    Exit;
  Root := IncludeTrailingPathDelimiter(TPath.GetFullPath(ASkill.Folder));
  List := TStringList.Create;
  try
    for F in TDirectory.GetFiles(Root, '*', TSearchOption.soAllDirectories) do
    begin
      Rel := Copy(F, Length(Root) + 1, MaxInt).Replace('\', '/');
      if SameText(Rel, AI_SKILL_FILE_NAME) then
        Continue;
      List.Add(Rel);
      if List.Count >= MAX_LISTED_FILES then
        Break;
    end;
    List.Sort;
    Result := List.ToStringArray;
  finally
    List.Free;
  end;
end;

function TAiSkills.ExecuteUseSkill(const AName: string): string;
var
  S: TAiSkillItem;
  Allow: Boolean;
  Files: TArray<string>;
  SB: TStringBuilder;
  Span: TAiSpan;
  Names: TStringList;
  I: Integer;
begin
  S := Find(AName);
  if not Assigned(S) or not S.Enabled then
  begin
    Names := TStringList.Create;
    try
      for I := 0 to FSkills.Count - 1 do
        if FSkills[I].Enabled then
          Names.Add(FSkills[I].Name);
      Exit(Format('Error: skill "%s" not found. Available skills: %s',
        [AName, String.Join(', ', Names.ToStringArray)]));
    finally
      Names.Free;
    end;
  end;

  Allow := True;
  if Assigned(FOnBeforeUseSkill) then
    FOnBeforeUseSkill(Self, S, Allow);
  if not Allow then
    Exit(Format('Error: skill "%s" is not allowed in this session.', [S.Name]));

  Span := AiSpanStart('skill.load ' + S.Name);
  try
    AiSpanAttr(Span, 'skill.name', S.Name);
    AiSpanAttr(Span, 'skill.source', SourceLabel(S.Source));

    SB := TStringBuilder.Create;
    try
      SB.Append('<skill name="').Append(S.Name).Append('" source="')
        .Append(SourceLabel(S.Source)).Append('"');
      if S.Version <> '' then
        SB.Append(' version="').Append(S.Version).Append('"');
      SB.Append('>').Append(#10);
      if S.Source = ssPPM then
        SB.Append('(Third-party skill from the PPM registry.)').Append(#10);
      SB.Append(S.Instructions.Text.Trim).Append(#10);
      Files := ListSkillFiles(S);
      if FAllowFileAccess and (Length(Files) > 0) then
        SB.Append(#10).Append('Supporting files (read them with ').Append(FReadFunctionName)
          .Append(' when the instructions refer to them): ')
          .Append(String.Join(', ', Files)).Append(#10);
      SB.Append('</skill>');
      Result := SB.ToString;
    finally
      SB.Free;
    end;
    AiSpanEnd(Span);
  except
    on E: Exception do
    begin
      AiSpanEnd(Span, E.Message);
      raise;
    end;
  end;

  if Assigned(FOnSkillLoaded) then
    FOnSkillLoaded(Self, S);
end;

function TAiSkills.ExecuteReadFile(const AName, APath: string): string;
var
  S: TAiSkillItem;
  Root, Full, Dir, Rel: string;
  Allow: Boolean;
  Bytes: TBytes;
  I: Integer;
begin
  if not FAllowFileAccess then
    Exit('Error: file access is disabled.');
  S := Find(AName);
  if not Assigned(S) or not S.Enabled then
    Exit(Format('Error: skill "%s" not found.', [AName]));
  if S.Folder = '' then
    Exit(Format('Error: skill "%s" has no folder with supporting files.', [S.Name]));

  Rel := Trim(APath).Replace('/', PathDelim).Replace('\', PathDelim);
  if (Rel = '') or TPath.IsPathRooted(Rel) or Rel.StartsWith(PathDelim) or
     (Pos(':', Rel) > 0) then
    Exit('Error: the path must be relative to the skill folder.');

  Root := IncludeTrailingPathDelimiter(TPath.GetFullPath(S.Folder));
  Full := TPath.GetFullPath(TPath.Combine(Root, Rel));
  // GetFullPath resuelve los '..': si el resultado salió de la carpeta, fuera
  if not PathStartsWith(Full, Root) or (Length(Full) <= Length(Root)) then
    Exit('Error: the path is outside the skill folder.');
  if not TFile.Exists(Full) then
    Exit(Format('Error: file "%s" not found in skill "%s".', [APath, S.Name]));

  // Un enlace simbólico dentro de la carpeta podría apuntar a cualquier lado:
  // se revisa el archivo y cada carpeta intermedia.
  Dir := Full;
  while (Length(Dir) > Length(Root)) do
  begin
    if IsLinkOrReparse(Dir) then
      Exit('Error: links are not allowed in skill folders.');
    Dir := ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(Dir));
  end;

  if TFile.GetSize(Full) > FMaxFileSize then
    Exit(Format('Error: file is larger than %d bytes.', [FMaxFileSize]));

  Allow := True;
  if Assigned(FOnBeforeReadFile) then
    FOnBeforeReadFile(Self, S, Full, Allow);
  if not Allow then
    Exit('Error: reading this file is not allowed.');

  Bytes := TFile.ReadAllBytes(Full);
  for I := 0 to Length(Bytes) - 1 do
    if Bytes[I] = 0 then
      Exit('Error: binary files cannot be read.');
  Result := TEncoding.UTF8.GetString(Bytes);
  if (Result <> '') and (Result[1] = #$FEFF) then
    Delete(Result, 1, 1);
end;

procedure TAiSkills.DoUseSkillAction(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Handled := True;
  try
    ToolCall.Response := ExecuteUseSkill(ToolCall.Params.Values['name']);
  except
    on E: Exception do
      ToolCall.Response := 'Error: ' + E.Message;
  end;
end;

procedure TAiSkills.DoReadFileAction(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Handled := True;
  try
    ToolCall.Response := ExecuteReadFile(ToolCall.Params.Values['name'],
      ToolCall.Params.Values['path']);
  except
    on E: Exception do
      ToolCall.Response := 'Error: ' + E.Message;
  end;
end;

end.
