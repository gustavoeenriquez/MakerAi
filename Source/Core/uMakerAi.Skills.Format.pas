unit uMakerAi.Skills.Format;

// MIT License
//
// Copyright (c) 2026 Gustavo Enríquez - CimaMaker
//
// Formato SKILL.md: un skill es un archivo Markdown con un bloque de
// frontmatter YAML al inicio (entre dos líneas '---') y las instrucciones en
// el cuerpo. Es el formato de las Agent Skills de Claude y el que publica el
// registry PPM (tipo 'skill').
//
//   ---
//   name: code-review
//   description: Revisa código en busca de errores. Úsalo cuando...
//   allowed-tools:
//     - Read
//     - Grep
//   model: claude-opus-4-6
//   ---
//   Eres un revisor de código experto...
//
// Esta unidad es la ÚNICA que entiende el formato. La usan TAiPrompts
// (Core), TAiSkill/TLLMNode (Agents) y TAiSkills (Tools), para que un mismo
// skill sirva igual en un chat, en un nodo de agente o como plantilla, venga
// de una carpeta local o del registry.
//
// Del YAML se entiende lo que usan los skills reales: escalares (con o sin
// comillas, con comentario '#' al final), listas '- x', listas inline
// '[a, b]' o 'a, b' y bloques '|' / '>'. No es un parser YAML completo.

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON,
  System.Net.HttpClient, System.Net.URLClient;

const
  AI_SKILL_FILE_NAME       = 'SKILL.md';
  AI_PPM_DEFAULT_REGISTRY  = 'https://registry.cimamaker.com';
  AI_PPM_SKILL_PREFIX      = 'skill-';

type
  EAiSkillError = class(Exception);

  // De dónde salió el skill. Importa para la seguridad: un skill de PPM es
  // contenido de terceros (no se le aceptan credenciales, ver TAiSkill).
  TAiSkillSource = (ssText, ssFile, ssPPM);

  TAiSkillDoc = class
  private
    FName           : string;
    FDescription    : string;
    FModel          : string;
    FBody           : string;
    FSourcePath     : string;
    FVersion        : string;
    FSource         : TAiSkillSource;
    FHasFrontmatter : Boolean;
    FAllowedTools   : TStringList;
    FExtra          : TStringList;
    procedure ParseFrontmatter(const ALines: TArray<string>; AEnd: Integer);
    procedure SetScalar(const AKey, AValue: string);
    procedure SetList(const AKey: string; AValues: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(ASource: TAiSkillDoc);

    // Parsea el texto completo de un SKILL.md. Sin frontmatter, todo el texto
    // es el cuerpo (un Markdown suelto también es un skill válido).
    procedure LoadFromText(const AText: string);

    // APath puede ser el archivo .md o una carpeta que contenga SKILL.md.
    // Si el frontmatter no trae 'name', se usa el nombre de la carpeta
    // (o del archivo sin extensión).
    procedure LoadFromFile(const APath: string);

    // Descarga el skill del registry PPM. AVersion vacío = la última versión
    // no retirada. Si AName no existe y no empieza con 'skill-', se reintenta
    // con el prefijo ('code-review' -> 'skill-code-review').
    procedure LoadFromPPM(const AName: string; const AVersion: string = '';
                          const ARegistryUrl: string = '');

    // Factorías: el llamador libera el objeto devuelto.
    class function Parse(const AText: string): TAiSkillDoc;
    class function FromFile(const APath: string): TAiSkillDoc;
    class function FromPPM(const AName: string; const AVersion: string = '';
                           const ARegistryUrl: string = ''): TAiSkillDoc;

    // Rutas de los SKILL.md de una carpeta: el de la propia carpeta (si hay)
    // y el de cada subcarpeta directa. Es la forma habitual de un directorio
    // de skills: skills/<nombre>/SKILL.md. Orden alfabético.
    class function FindSkillFiles(const AFolder: string): TArray<string>;

    // True si el texto parece una página HTML: un servidor web que responde
    // 200 con su página en vez de un 404 no debe confundirse con un skill.
    class function LooksLikeHtml(const AText: string): Boolean;

    property Name          : string         read FName        write FName;
    property Description   : string         read FDescription write FDescription;
    property Model         : string         read FModel       write FModel;
    // Instrucciones (Markdown sin el frontmatter)
    property Body          : string         read FBody        write FBody;
    // Nombres de herramientas declarados en 'allowed-tools'
    property AllowedTools  : TStringList    read FAllowedTools;
    // Resto de claves escalares del frontmatter (Name=Value, claves en minúscula)
    property Extra         : TStringList    read FExtra;
    property Source        : TAiSkillSource read FSource      write FSource;
    // Archivo local o URL del registry de donde se cargó
    property SourcePath    : string         read FSourcePath  write FSourcePath;
    // Versión del paquete PPM (vacío en skills locales)
    property Version       : string         read FVersion     write FVersion;
    property HasFrontmatter: Boolean        read FHasFrontmatter;
  end;

  // Cliente mínimo del registry PPM (solo lectura).
  TAiPPMClient = class
  public
    // GET a una URL del registry. Devuelve el cuerpo con status 200 y lanza
    // EAiSkillError en cualquier otro caso, con el 'message' del registry si
    // lo trae. AStatus recibe el código HTTP (0 si falló la conexión).
    class function Get(const AUrl: string; out AStatus: Integer): string;

    // Metadatos del paquete (/v1/packages/<name>). El llamador libera.
    class function GetPackage(const ARegistryUrl, AName: string): TJSONObject;
    // Igual, pero devuelve nil si el paquete no existe (404) en vez de lanzar.
    class function TryGetPackage(const ARegistryUrl, AName: string): TJSONObject;

    // Mayor versión (semver) no retirada del paquete; '' si no hay.
    class function ResolveVersion(const ARegistryUrl, AName: string): string;

    // Compara dos versiones semver 'x.y.z' (el sufijo -pre se ignora).
    class function CompareVersions(const A, B: string): Integer;

    // Registry efectivo: ARegistryUrl sin '/' final, o el default.
    class function RegistryOrDefault(const ARegistryUrl: string): string;
  end;

implementation

uses
  System.StrUtils, System.NetEncoding;

const
  PPM_TIMEOUT_MS = 20000;

{ helpers }

// Quita comillas envolventes, o el comentario '#' final si no hay comillas.
function CleanScalar(const AValue: string): string;
var
  V: string;
  P: Integer;
begin
  V := Trim(AValue);
  if (Length(V) >= 2) and
     (((V[1] = '"') and (V[Length(V)] = '"')) or
      ((V[1] = '''') and (V[Length(V)] = ''''))) then
    Exit(Copy(V, 2, Length(V) - 2));

  // En YAML un comentario empieza con ' #' (el '#' pegado es parte del valor)
  P := Pos(' #', V);
  if P > 0 then
    V := Trim(Copy(V, 1, P - 1));
  Result := V;
end;

function IndentOf(const ALine: string): Integer;
begin
  Result := 0;
  while (Result < Length(ALine)) and (ALine[Result + 1] = ' ') do
    Inc(Result);
end;

// 'a, b' o '[a, b]' -> lista
procedure SplitInlineList(const AValue: string; AList: TStrings);
var
  V, Item: string;
begin
  V := Trim(AValue);
  if V.StartsWith('[') and V.EndsWith(']') then
    V := Copy(V, 2, Length(V) - 2);
  for Item in V.Split([',']) do
    if CleanScalar(Item) <> '' then
      AList.Add(CleanScalar(Item));
end;

{ TAiSkillDoc }

constructor TAiSkillDoc.Create;
begin
  inherited Create;
  FAllowedTools := TStringList.Create;
  FExtra := TStringList.Create;
end;

destructor TAiSkillDoc.Destroy;
begin
  FAllowedTools.Free;
  FExtra.Free;
  inherited;
end;

procedure TAiSkillDoc.Clear;
begin
  FName := '';
  FDescription := '';
  FModel := '';
  FBody := '';
  FSourcePath := '';
  FVersion := '';
  FSource := ssText;
  FHasFrontmatter := False;
  FAllowedTools.Clear;
  FExtra.Clear;
end;

procedure TAiSkillDoc.Assign(ASource: TAiSkillDoc);
begin
  if not Assigned(ASource) then
  begin
    Clear;
    Exit;
  end;
  FName := ASource.FName;
  FDescription := ASource.FDescription;
  FModel := ASource.FModel;
  FBody := ASource.FBody;
  FSourcePath := ASource.FSourcePath;
  FVersion := ASource.FVersion;
  FSource := ASource.FSource;
  FHasFrontmatter := ASource.FHasFrontmatter;
  FAllowedTools.Assign(ASource.FAllowedTools);
  FExtra.Assign(ASource.FExtra);
end;

procedure TAiSkillDoc.SetScalar(const AKey, AValue: string);
begin
  if AKey = 'name' then
    FName := AValue
  else if AKey = 'description' then
    FDescription := AValue
  else if AKey = 'model' then
    FModel := AValue
  else if (AKey = 'allowed-tools') or (AKey = 'allowed_tools') then
  begin
    FAllowedTools.Clear;
    SplitInlineList(AValue, FAllowedTools);
  end
  else
    FExtra.Values[AKey] := AValue;
end;

procedure TAiSkillDoc.SetList(const AKey: string; AValues: TStrings);
begin
  if (AKey = 'allowed-tools') or (AKey = 'allowed_tools') then
    FAllowedTools.Assign(AValues)
  else
    // Las listas que no conocemos se guardan como 'a, b' para no perderlas
    FExtra.Values[AKey] := String.Join(', ', AValues.ToStringArray);
end;

procedure TAiSkillDoc.ParseFrontmatter(const ALines: TArray<string>; AEnd: Integer);
var
  I, J, ColonPos, BaseIndent: Integer;
  Line, Key, Value, Style: string;
  Items: TStringList;
  Block: TStringBuilder;
begin
  Items := TStringList.Create;
  try
    I := 1;
    while I < AEnd do
    begin
      Line := ALines[I];
      if (Trim(Line) = '') or Trim(Line).StartsWith('#') then
      begin
        Inc(I);
        Continue;
      end;

      ColonPos := Line.IndexOf(':');
      // Solo claves de nivel superior (sin sangría); lo demás se consume
      // abajo como parte de la clave anterior
      if (ColonPos <= 0) or (IndentOf(Line) > 0) then
      begin
        Inc(I);
        Continue;
      end;

      Key := Trim(Line.Substring(0, ColonPos)).ToLower;
      Value := Trim(Line.Substring(ColonPos + 1));

      if (Value = '|') or (Value = '>') or (Value = '|-') or (Value = '>-') then
      begin
        // Bloque: las líneas con sangría que siguen. '|' conserva los saltos,
        // '>' los pliega en espacios.
        Style := Value;
        Block := TStringBuilder.Create;
        try
          J := I + 1;
          BaseIndent := -1;
          while (J < AEnd) and ((Trim(ALines[J]) = '') or (IndentOf(ALines[J]) > 0)) do
          begin
            if Trim(ALines[J]) <> '' then
            begin
              if BaseIndent < 0 then
                BaseIndent := IndentOf(ALines[J]);
              if Block.Length > 0 then
              begin
                if Style.StartsWith('|') then Block.Append(#10) else Block.Append(' ');
              end;
              Block.Append(Copy(ALines[J], BaseIndent + 1, MaxInt).TrimRight);
            end
            else if Style.StartsWith('|') and (Block.Length > 0) then
              Block.Append(#10);
            Inc(J);
          end;
          SetScalar(Key, Block.ToString.Trim);
        finally
          Block.Free;
        end;
        I := J;
        Continue;
      end;

      if Value = '' then
      begin
        // Posible lista '- x' en las líneas siguientes
        Items.Clear;
        J := I + 1;
        while (J < AEnd) and ((Trim(ALines[J]) = '') or Trim(ALines[J]).StartsWith('- ')
               or (Trim(ALines[J]) = '-')) do
        begin
          if Trim(ALines[J]).StartsWith('- ') then
            Items.Add(CleanScalar(Trim(ALines[J]).Substring(2)));
          Inc(J);
        end;
        if Items.Count > 0 then
          SetList(Key, Items)
        else
          SetScalar(Key, '');
        I := J;
        Continue;
      end;

      // Escalar plano. En YAML puede continuar en líneas con sangría
      // (descripciones largas partidas): se pliegan con espacios.
      J := I + 1;
      while (J < AEnd) and (Trim(ALines[J]) <> '') and (IndentOf(ALines[J]) > 0)
            and not Trim(ALines[J]).StartsWith('- ') do
      begin
        Value := Value + ' ' + Trim(ALines[J]);
        Inc(J);
      end;
      SetScalar(Key, CleanScalar(Value));
      I := J;
    end;
  finally
    Items.Free;
  end;
end;

procedure TAiSkillDoc.LoadFromText(const AText: string);
var
  Text: string;
  Lines: TArray<string>;
  I, FmEnd: Integer;
  Buf: TStringBuilder;
begin
  Clear;
  Text := AText;
  // BOM que haya sobrevivido a la lectura
  if (Text <> '') and (Text[1] = #$FEFF) then
    Delete(Text, 1, 1);

  Lines := Text.Replace(#13#10, #10).Replace(#13, #10).Split([#10]);

  FmEnd := -1;
  if (Length(Lines) > 1) and (Trim(Lines[0]) = '---') then
    for I := 1 to High(Lines) do
      if Trim(Lines[I]) = '---' then
      begin
        FmEnd := I;
        Break;
      end;

  if FmEnd < 0 then
  begin
    // Sin frontmatter (o sin cierre): todo es cuerpo
    FBody := Trim(Text.Replace(#13#10, #10));
    Exit;
  end;

  FHasFrontmatter := True;
  ParseFrontmatter(Lines, FmEnd);

  Buf := TStringBuilder.Create;
  try
    for I := FmEnd + 1 to High(Lines) do
    begin
      if I > FmEnd + 1 then
        Buf.Append(#10);
      Buf.Append(Lines[I]);
    end;
    FBody := Trim(Buf.ToString);
  finally
    Buf.Free;
  end;
end;

procedure TAiSkillDoc.LoadFromFile(const APath: string);
var
  LFile: string;
begin
  if TDirectory.Exists(APath) then
    LFile := TPath.Combine(APath, AI_SKILL_FILE_NAME)
  else
    LFile := APath;

  if not TFile.Exists(LFile) then
    raise EAiSkillError.CreateFmt('Skill no encontrado: %s', [LFile]);

  LoadFromText(TFile.ReadAllText(LFile, TEncoding.UTF8));
  FSource := ssFile;
  FSourcePath := LFile;

  if FName = '' then
  begin
    if SameText(TPath.GetFileName(LFile), AI_SKILL_FILE_NAME) then
      FName := TPath.GetFileName(ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(LFile)))
    else
      FName := TPath.GetFileNameWithoutExtension(LFile);
  end;
end;

procedure TAiSkillDoc.LoadFromPPM(const AName, AVersion, ARegistryUrl: string);
var
  Registry, PkgName, LVersion, Url, Body, TypeStr: string;
  Pkg: TJSONObject;
  Status: Integer;
begin
  if Trim(AName) = '' then
    raise EAiSkillError.Create('LoadFromPPM: falta el nombre del skill');

  Registry := TAiPPMClient.RegistryOrDefault(ARegistryUrl);
  PkgName := Trim(AName);

  // Metadatos: confirman que existe, que es un skill y dan la versión.
  // Solo un 404 dispara el reintento con prefijo; un fallo de red se informa
  // tal cual en vez de disfrazarse de "no encontrado".
  Pkg := TAiPPMClient.TryGetPackage(Registry, PkgName);
  if not Assigned(Pkg) and not StartsText(AI_PPM_SKILL_PREFIX, PkgName) then
  begin
    // 'code-review' -> 'skill-code-review' (convención de nombres de PPM)
    Pkg := TAiPPMClient.TryGetPackage(Registry, AI_PPM_SKILL_PREFIX + PkgName);
    if Assigned(Pkg) then
      PkgName := AI_PPM_SKILL_PREFIX + PkgName;
  end;
  if not Assigned(Pkg) then
    raise EAiSkillError.CreateFmt('Skill "%s" no encontrado en el registry PPM (%s)',
      [AName, Registry]);
  try
    TypeStr := '';
    Pkg.TryGetValue<string>('package.type', TypeStr);
    if (TypeStr <> '') and not SameText(TypeStr, 'skill') then
      raise EAiSkillError.CreateFmt(
        'El paquete PPM "%s" es de tipo "%s", no un skill', [PkgName, TypeStr]);
  finally
    Pkg.Free;
  end;

  LVersion := Trim(AVersion);
  if LVersion = '' then
    LVersion := TAiPPMClient.ResolveVersion(Registry, PkgName);
  if LVersion = '' then
    raise EAiSkillError.CreateFmt('El skill PPM "%s" no tiene versiones disponibles', [PkgName]);

  Url := Format('%s/v1/packages/%s/%s/skill',
    [Registry, TNetEncoding.URL.Encode(PkgName), TNetEncoding.URL.Encode(LVersion)]);
  Body := TAiPPMClient.Get(Url, Status);

  LoadFromText(Body);
  FSource := ssPPM;
  FSourcePath := Url;
  FVersion := LVersion;
  if FName = '' then
    FName := PkgName;
end;

class function TAiSkillDoc.Parse(const AText: string): TAiSkillDoc;
begin
  Result := TAiSkillDoc.Create;
  try
    Result.LoadFromText(AText);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkillDoc.FromFile(const APath: string): TAiSkillDoc;
begin
  Result := TAiSkillDoc.Create;
  try
    Result.LoadFromFile(APath);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkillDoc.FromPPM(const AName, AVersion, ARegistryUrl: string): TAiSkillDoc;
begin
  Result := TAiSkillDoc.Create;
  try
    Result.LoadFromPPM(AName, AVersion, ARegistryUrl);
  except
    Result.Free;
    raise;
  end;
end;

class function TAiSkillDoc.FindSkillFiles(const AFolder: string): TArray<string>;
var
  List: TStringList;
  Dir, F: string;
begin
  Result := nil;
  if not TDirectory.Exists(AFolder) then
    Exit;
  List := TStringList.Create;
  try
    F := TPath.Combine(AFolder, AI_SKILL_FILE_NAME);
    if TFile.Exists(F) then
      List.Add(F);
    for Dir in TDirectory.GetDirectories(AFolder) do
    begin
      F := TPath.Combine(Dir, AI_SKILL_FILE_NAME);
      if TFile.Exists(F) then
        List.Add(F);
    end;
    List.Sort;
    Result := List.ToStringArray;
  finally
    List.Free;
  end;
end;

class function TAiSkillDoc.LooksLikeHtml(const AText: string): Boolean;
var
  Head: string;
begin
  Head := Copy(TrimLeft(AText), 1, 64).ToLower;
  Result := Head.StartsWith('<!doctype html') or Head.StartsWith('<html');
end;

{ TAiPPMClient }

class function TAiPPMClient.RegistryOrDefault(const ARegistryUrl: string): string;
begin
  Result := Trim(ARegistryUrl);
  if Result = '' then
    Result := AI_PPM_DEFAULT_REGISTRY;
  while Result.EndsWith('/') do
    Result := Copy(Result, 1, Length(Result) - 1);
end;

class function TAiPPMClient.Get(const AUrl: string; out AStatus: Integer): string;
var
  Client: THTTPClient;
  Resp: IHTTPResponse;
  Msg, CType: string;
  Err: TJSONValue;
begin
  AStatus := 0;
  Client := THTTPClient.Create;
  try
    Client.ConnectionTimeout := PPM_TIMEOUT_MS;
    Client.ResponseTimeout := PPM_TIMEOUT_MS;
    try
      Resp := Client.Get(AUrl);
    except
      on E: Exception do
        raise EAiSkillError.CreateFmt('No se pudo conectar con el registry PPM (%s): %s',
          [AUrl, E.Message]);
    end;
    AStatus := Resp.StatusCode;
    Result := Resp.ContentAsString(TEncoding.UTF8);
    CType := Resp.MimeType.ToLower;

    if AStatus <> 200 then
    begin
      Msg := '';
      Err := TJSONObject.ParseJSONValue(Result);
      try
        if Err is TJSONObject then
          TJSONObject(Err).TryGetValue<string>('message', Msg);
      finally
        Err.Free;
      end;
      if Msg = '' then
        Msg := 'HTTP ' + IntToStr(AStatus);
      raise EAiSkillError.CreateFmt('Registry PPM: %s (%s)', [Msg, AUrl]);
    end;

    // Un 200 con HTML es la página del sitio, no el recurso pedido (URL mal
    // armada o host equivocado). Antes esto terminaba en "JSON inválido".
    if CType.Contains('text/html') or TAiSkillDoc.LooksLikeHtml(Result) then
      raise EAiSkillError.CreateFmt(
        'La URL devolvió una página HTML, no un recurso del registry PPM: %s ' +
        '(revisa la URL del registry)', [AUrl]);
  finally
    Client.Free;
  end;
end;

class function TAiPPMClient.GetPackage(const ARegistryUrl, AName: string): TJSONObject;
var
  Body: string;
  Status: Integer;
  V: TJSONValue;
begin
  Body := Get(Format('%s/v1/packages/%s',
    [RegistryOrDefault(ARegistryUrl), TNetEncoding.URL.Encode(AName)]), Status);
  V := TJSONObject.ParseJSONValue(Body);
  if not (V is TJSONObject) then
  begin
    V.Free;
    raise EAiSkillError.CreateFmt('Respuesta inesperada del registry PPM para "%s"', [AName]);
  end;
  Result := TJSONObject(V);
end;

class function TAiPPMClient.TryGetPackage(const ARegistryUrl, AName: string): TJSONObject;
var
  Body: string;
  Status: Integer;
  V: TJSONValue;
begin
  Result := nil;
  Status := 0;
  try
    Body := Get(Format('%s/v1/packages/%s',
      [RegistryOrDefault(ARegistryUrl), TNetEncoding.URL.Encode(AName)]), Status);
  except
    on E: EAiSkillError do
    begin
      if Status = 404 then
        Exit;
      raise;
    end;
  end;
  V := TJSONObject.ParseJSONValue(Body);
  if not (V is TJSONObject) then
  begin
    V.Free;
    raise EAiSkillError.CreateFmt('Respuesta inesperada del registry PPM para "%s"', [AName]);
  end;
  Result := TJSONObject(V);
end;

class function TAiPPMClient.CompareVersions(const A, B: string): Integer;

  function Parts(const S: string): TArray<Integer>;
  var
    Core: string;
    Items: TArray<string>;
    I: Integer;
  begin
    Core := Trim(S);
    if Core.StartsWith('v') or Core.StartsWith('V') then
      Delete(Core, 1, 1);
    I := Core.IndexOfAny(['-', '+']);
    if I >= 0 then
      Core := Core.Substring(0, I);
    Items := Core.Split(['.']);
    SetLength(Result, 3);
    for I := 0 to 2 do
      if I < Length(Items) then
        Result[I] := StrToIntDef(Items[I], 0)
      else
        Result[I] := 0;
  end;

var
  PA, PB: TArray<Integer>;
  I: Integer;
begin
  PA := Parts(A);
  PB := Parts(B);
  for I := 0 to 2 do
    if PA[I] <> PB[I] then
      Exit(PA[I] - PB[I]);
  Result := 0;
end;

class function TAiPPMClient.ResolveVersion(const ARegistryUrl, AName: string): string;
var
  Pkg, Ver: TJSONObject;
  Versions: TJSONArray;
  Yanked: TJSONValue;
  V: string;
  I: Integer;
begin
  Result := '';
  Pkg := GetPackage(ARegistryUrl, AName);
  try
    if not Pkg.TryGetValue<TJSONArray>('package.versions', Versions) then
      Exit;
    // La mayor por semver: no se confía en el orden en que llegan
    for I := 0 to Versions.Count - 1 do
    begin
      if not (Versions.Items[I] is TJSONObject) then
        Continue;
      Ver := TJSONObject(Versions.Items[I]);
      Yanked := Ver.FindValue('yanked');
      if Assigned(Yanked) and (Yanked is TJSONTrue) then
        Continue;
      if Ver.TryGetValue<string>('version', V) and
         ((Result = '') or (CompareVersions(V, Result) > 0)) then
        Result := V;
    end;
  finally
    Pkg.Free;
  end;
end;

end.
