// MIT License
//
// Copyright (c) <year> <copyright holders>
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
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


unit uMakerAi.Prompts;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.HttpClient, System.NetEncoding, System.RegularExpressions;

const
  PPM_DEFAULT_REGISTRY = 'https://registry.pascalai.org';

type
  TAiPromptItem = Class(TCollectionItem)
  Private
    fNombre          : String;
    FString          : TStrings;
    fDescripcion     : String;
    FSkillModel      : String;
    FSkillAllowedTools: String;
    function GetString: TStrings;
  Protected
    Procedure SetStrings(aValue: TStrings);
    function GetDisplayName: string; Override;
  Public
    constructor Create(Collection: TCollection); Override;
    destructor Destroy; Override;
  Published
    Property Nombre     : String  read fNombre      Write fNombre;
    Property Descripcion: String  read fDescripcion Write fDescripcion;
    Property Strings    : TStrings Read GetString   Write SetStrings;
    // Metadatos del skill (vacíos si el item es un prompt, no un skill)
    // Modelo recomendado por el skill (frontmatter YAML: model: claude-opus-4-6)
    Property SkillModel      : String read FSkillModel       write FSkillModel;
    // Herramientas permitidas, separadas por coma (frontmatter: allowed-tools)
    Property SkillAllowedTools: String read FSkillAllowedTools write FSkillAllowedTools;
  End;

  TAiPrompts = class(TComponent)
  private
    FItems         : TCollection;
    FPPMRegistryUrl: String;
    function PPMHttpGet(const AUrl: String): String;
    function ConvertPPMTemplate(const AText: String): String;
    function ResolvePPMVersion(const AName, AVersion: String): String;
    // Parsea el frontmatter YAML de un skill.
    // Devuelve True si encontró los delimitadores ---.
    // ABody recibe el cuerpo Markdown sin el bloque de frontmatter.
    function ParseSkillFrontmatter(const AContent: String;
      out AModel, AAllowedTools, ABody: String): Boolean;
  protected
  public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function IndexOf(Nombre: String): Integer;
    Function GetNombre(Index: Integer): String;
    Function GetString(Nombre: String): String;
    Function GetStringByIndex(Index: Integer): String;
    Function AddString(Nombre, Data: String): TAiPromptItem;
    Function GetTemplate(Nombre: String; Params: Array of String): String; Overload;
    Function GetTemplate(Nombre: String; Params: TStringList): String; Overload;
    Function GetTemplate(Nombre: String; Params: TJSonObject): String; Overload;

    // Integración con PPM (registry público de prompts y skills)
    // SearchPPM: busca en el registry. El llamador es responsable de liberar el TJSONObject.
    // AType puede ser 'prompt', 'skill', 'pai', etc.
    function SearchPPM(const AQuery: String; const AType: String = 'prompt';
      APage: Integer = 1; APerPage: Integer = 20): TJSONObject;

    // LoadFromPPM: descarga un prompt (/raw) y lo agrega a Items.
    // Convierte placeholders {{var}} → <#var>. Si ya existe, lo actualiza.
    // AVersion vacío = resuelve la última versión disponible.
    function LoadFromPPM(const AName: String; const AVersion: String = ''): TAiPromptItem;

    // LoadSkillFromPPM: descarga un skill (/skill), parsea el frontmatter YAML
    // y almacena el cuerpo Markdown en Strings.Text (sin frontmatter).
    // SkillModel y SkillAllowedTools quedan disponibles en el item resultante.
    // AVersion vacío = resuelve la última versión disponible.
    function LoadSkillFromPPM(const AName: String; const AVersion: String = ''): TAiPromptItem;

  published
    Property Items: TCollection Read FItems Write FItems;
    // URL del registry PPM. Por defecto: https://registry.pascalai.org
    property PPMRegistryUrl: String read FPPMRegistryUrl write FPPMRegistryUrl;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiPrompts]);
end;

constructor TAiPromptItem.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FString := TStringList.Create;
End;

Procedure TAiPromptItem.SetStrings(aValue: TStrings);
Begin
  FString.Assign(aValue);
End;

function TAiPromptItem.GetDisplayName: string;
Begin
  Result := fNombre;
End;

function TAiPromptItem.GetString: TStrings;
begin
  Result := FString;
end;

// *************************************************************
// *************************************************************

Constructor TAiPrompts.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FItems := TCollection.Create(TAiPromptItem);
  FPPMRegistryUrl := PPM_DEFAULT_REGISTRY;
End;

Function TAiPrompts.IndexOf(Nombre: String): Integer;
Var
  I: Integer;
  Item: TAiPromptItem;
Begin
  Result := -1;
  For I := 0 to FItems.Count - 1 do
  Begin
    Item := TAiPromptItem(FItems.Items[I]);
    If (Item <> Nil) and (AnsiUpperCase(Nombre) = AnsiUpperCase(Item.Nombre)) then
    Begin
      Result := I;
      Break;
    End;
  End;
End;

Function TAiPrompts.GetString(Nombre: String): String;
Var
  I: Integer;
  Item: TAiPromptItem;
Begin
  Result := '';
  I := IndexOf(Nombre);
  If I >= 0 then
  Begin
    Item := TAiPromptItem(FItems.Items[I]);
    Result := Item.Strings.Text;
  End;
End;

function TAiPrompts.GetTemplate(Nombre: String; Params: TStringList): String;
Var
  I: Integer;
  Res, Nom, Val: String;
begin
  Res := GetString(Nombre);

  For I := 0 to Params.Count - 1 do
  Begin
    Nom := Params.Names[I];
    Val := Params.Values[Nom];
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;
end;

function TAiPrompts.GetTemplate(Nombre: String; Params: array of String): String;
Var
  I, P: Integer;
  Res, S, Nom, Val: String;
begin
  Res := GetString(Nombre);
  For I := 0 to Length(Params) - 1 do
  Begin
    S := Params[I];
    P := Pos('=',S);
    If  p <= 0 then
      Raise Exception.Create('Los parámetros deben tener la forma Nombre=Valor');

    Nom := Copy(S, 1, P - 1);
    Val := Copy(S, P + 1, Length(S));
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;
end;

function TAiPrompts.GetTemplate(Nombre: String; Params: TJSonObject): String;
Var
  Pair : TJSonPair;
  Res, Nom, Val: String;
begin
  Res := GetString(Nombre);
  For Pair in Params do
  Begin
    Nom := Pair.JsonString.Value;
    Val := Pair.JsonValue.Value;
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;
end;

Function TAiPrompts.AddString(Nombre, Data: String): TAiPromptItem;
Var
  Item: TAiPromptItem;
Begin
  Item := TAiPromptItem(FItems.Add);
  Item.Nombre := Nombre;
  Item.Strings.Text := Data;
  Result := Item;
End;

destructor TAiPromptItem.Destroy;
Begin
  FString.Free;
  Inherited;
End;

Destructor TAiPrompts.Destroy;
Begin
  FItems.Free;
  Inherited;
End;

Function TAiPrompts.GetNombre(Index: Integer): String;
Begin
  Result := '';
  If (Index >= 0) and (Index < FItems.Count) then
    Result := TAiPromptItem(FItems.Items[Index]).Nombre;
End;

Function TAiPrompts.GetStringByIndex(Index: Integer): String;
Begin
  Result := '';
  If (Index >= 0) and (Index < FItems.Count) then
    Result := TAiPromptItem(FItems.Items[Index]).Strings.Text;
End;

// ---------------------------------------------------------------------------
// PPM Integration
// ---------------------------------------------------------------------------

function TAiPrompts.PPMHttpGet(const AUrl: String): String;
var
  LClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  Result := '';
  LClient := THTTPClient.Create;
  try
    try
      LResponse := LClient.Get(AUrl);
      if LResponse.StatusCode = 200 then
        Result := LResponse.ContentAsString(TEncoding.UTF8);
    except
      // Error de red: retorna vacío
    end;
  finally
    LClient.Free;
  end;
end;

function TAiPrompts.ConvertPPMTemplate(const AText: String): String;
begin
  // Convierte placeholders PPM {{varname}} → formato MakerAI <#varname>
  Result := TRegEx.Replace(AText, '\{\{(\w+)\}\}', '<#$1>');
end;

function TAiPrompts.ResolvePPMVersion(const AName, AVersion: String): String;
var
  LBody: String;
  LJson, LPackage, LVer: TJSONObject;
  LVersions: TJSONArray;
  LYankedVal: TJSONValue;
  I: Integer;
begin
  Result := AVersion;
  if Result <> '' then
    Exit;

  LBody := PPMHttpGet(FPPMRegistryUrl + '/v1/packages/' + AName);
  if LBody = '' then
    Exit;

  LJson := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
  if not Assigned(LJson) then
    Exit;
  try
    if LJson.TryGetValue<TJSONObject>('package', LPackage) and
       LPackage.TryGetValue<TJSONArray>('versions', LVersions) then
    begin
      for I := 0 to LVersions.Count - 1 do
      begin
        LVer := LVersions.Items[I] as TJSONObject;
        LYankedVal := LVer.FindValue('yanked');
        if not (Assigned(LYankedVal) and (LYankedVal is TJSONTrue)) then
        begin
          LVer.TryGetValue<String>('version', Result);
          Break;
        end;
      end;
    end;
  finally
    LJson.Free;
  end;
end;

function TAiPrompts.SearchPPM(const AQuery: String; const AType: String;
  APage, APerPage: Integer): TJSONObject;
var
  LUrl, LBody: String;
begin
  Result := nil;
  LUrl := Format('%s/v1/search?q=%s&type=%s&page=%d&per_page=%d',
    [FPPMRegistryUrl,
     TNetEncoding.URL.Encode(AQuery),
     AType,
     APage,
     APerPage]);
  LBody := PPMHttpGet(LUrl);
  if LBody <> '' then
    Result := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
end;

function TAiPrompts.LoadFromPPM(const AName: String; const AVersion: String): TAiPromptItem;
var
  LVersion, LUrl, LBody: String;
  LIdx: Integer;
begin
  Result := nil;
  LVersion := ResolvePPMVersion(AName, AVersion);
  if LVersion = '' then
    Exit;

  LUrl := Format('%s/v1/packages/%s/%s/raw', [FPPMRegistryUrl, AName, LVersion]);
  LBody := PPMHttpGet(LUrl);
  if LBody = '' then
    Exit;

  // Reusar item existente si ya hay uno con el mismo nombre
  LIdx := IndexOf(AName);
  if LIdx >= 0 then
    Result := TAiPromptItem(FItems.Items[LIdx])
  else
  begin
    Result := TAiPromptItem(FItems.Add);
    Result.Nombre := AName;
  end;
  Result.Descripcion := 'PPM: ' + AName + ' v' + LVersion;
  Result.Strings.Text := ConvertPPMTemplate(LBody);
end;

// ---------------------------------------------------------------------------
// ParseSkillFrontmatter
// Extrae model y allowed-tools del bloque YAML entre los delimitadores ---.
// Soporta allowed-tools en formato lista (- Read) e inline (Read,Glob).
// Retorna True si se encontró el bloque frontmatter.
// ---------------------------------------------------------------------------
function TAiPrompts.ParseSkillFrontmatter(const AContent: String;
  out AModel, AAllowedTools, ABody: String): Boolean;
var
  Lines    : TArray<String>;
  I, FmEnd : Integer;
  Line, Key: String;
  Val      : String;
  ColonPos : Integer;
  InTools  : Boolean;
  Tools    : TStringList;
  LBuf     : TStringBuilder;
begin
  AModel        := '';
  AAllowedTools := '';
  ABody         := AContent;
  Result        := False;

  Lines := AContent.Split([#13#10, #10]);
  if (Length(Lines) < 2) or (Trim(Lines[0]) <> '---') then
    Exit;

  FmEnd   := -1;
  InTools := False;
  Tools   := TStringList.Create;
  try
    for I := 1 to High(Lines) do
    begin
      Line := Lines[I];

      if Trim(Line) = '---' then
      begin
        FmEnd := I;
        Break;
      end;

      // Elemento de lista bajo allowed-tools: "  - Read"
      if InTools and Trim(Line).StartsWith('- ') then
      begin
        Tools.Add(Trim(Trim(Line).Substring(2)));
        Continue;
      end;
      InTools := False;

      ColonPos := Line.IndexOf(':');
      if ColonPos > 0 then
      begin
        Key := Trim(Line.Substring(0, ColonPos)).ToLower;
        Val := Trim(Line.Substring(ColonPos + 1));

        if Key = 'model' then
          AModel := Val
        else if Key = 'allowed-tools' then
        begin
          if Val = '' then
            InTools := True       // formato lista en las líneas siguientes
          else
            AAllowedTools := Val; // formato inline: "Read, Glob, Grep"
        end;
      end;
    end;

    // Construir string de tools desde la lista
    if Tools.Count > 0 then
    begin
      AAllowedTools := Tools[0];
      for I := 1 to Tools.Count - 1 do
        AAllowedTools := AAllowedTools + ',' + Tools[I];
    end;

    // Cuerpo = todo lo que viene después del --- de cierre
    if FmEnd >= 0 then
    begin
      LBuf := TStringBuilder.Create;
      try
        for I := FmEnd + 1 to High(Lines) do
        begin
          if I > FmEnd + 1 then LBuf.Append(#10);
          LBuf.Append(Lines[I]);
        end;
        ABody := LBuf.ToString.TrimLeft([#13, #10]);
      finally
        LBuf.Free;
      end;
    end;

    Result := FmEnd >= 0;
  finally
    Tools.Free;
  end;
end;

// ---------------------------------------------------------------------------
// LoadSkillFromPPM
// ---------------------------------------------------------------------------
function TAiPrompts.LoadSkillFromPPM(const AName: String;
  const AVersion: String): TAiPromptItem;
var
  LVersion, LUrl, LBody       : String;
  LModel, LAllowedTools, LBody2: String;
  LIdx                        : Integer;
begin
  Result := nil;
  LVersion := ResolvePPMVersion(AName, AVersion);
  if LVersion = '' then
    Exit;

  LUrl  := Format('%s/v1/packages/%s/%s/skill', [FPPMRegistryUrl, AName, LVersion]);
  LBody := PPMHttpGet(LUrl);
  if LBody = '' then
    Exit;

  // Parsear frontmatter: extrae model, allowed-tools y el cuerpo Markdown
  ParseSkillFrontmatter(LBody, LModel, LAllowedTools, LBody2);

  // Reusar item existente o crear uno nuevo
  LIdx := IndexOf(AName);
  if LIdx >= 0 then
    Result := TAiPromptItem(FItems.Items[LIdx])
  else
  begin
    Result := TAiPromptItem(FItems.Add);
    Result.Nombre := AName;
  end;
  Result.Descripcion       := 'PPM skill: ' + AName + ' v' + LVersion;
  Result.Strings.Text      := LBody2;
  Result.SkillModel        := LModel;
  Result.SkillAllowedTools := LAllowedTools;
end;

end.
