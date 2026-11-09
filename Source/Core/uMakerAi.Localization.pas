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
// - GitHub: https://github.com/gustavoeenriquez/
//
// -----------------------------------------------------------------------------
// Localización de los textos del framework (v4.0).
//
// Cada texto que ve el usuario o el desarrollador (menús de ChatUI, mensajes
// de excepción, prompts internos) se pide por clave:
//
//   FCtxCopyMsg.Text := AiStr('chatui.copy_message');
//   raise Exception.Create(AiFmt('chat.driver_not_found', [ADriver]));
//
// Convención de claves: '<módulo>.<nombre>' en minúsculas con '_'.
//
// Idiomas:
//   - El inglés ('en') es la tabla base y debe tener TODAS las claves.
//   - Cada idioma vive en su propia unidad (uMakerAi.Lang.En, uMakerAi.Lang.Es,
//     uMakerAi.Lang.Pt)
//     con un arreglo de pares clave/texto. Esta unidad registra las incluidas.
//   - Un idioma propio se agrega desde la aplicación sin tocar el framework:
//       TAiLocale.RegisterTable('pt', ['chatui.copy_message', 'Copiar mensagem', ...]);
//     Registrar sobre un idioma existente agrega o reemplaza claves.
//
// Idioma activo (TAiLocale.Language):
//   - Por defecto: variable de entorno MAKERAI_LANG, si no el idioma de la
//     interfaz del sistema operativo (Windows: idioma de la UI; Linux/macOS:
//     LC_ALL / LC_MESSAGES / LANG; Android: Locale.getDefault). iOS: 'en'
//     salvo MAKERAI_LANG o asignación explícita.
//   - Se cambia en ejecución con TAiLocale.Language := 'es'. Asignar '' vuelve
//     a la detección automática.
//   - Búsqueda: idioma completo ('pt-br') -> idioma primario ('pt') -> 'en' ->
//     la clave misma (así un texto faltante se ve en vez de quedar vacío).
//
// Los textos ya creados (por ejemplo el menú de un control) no se actualizan
// solos al cambiar el idioma: los controles que lo necesiten comparan
// TAiLocale.ChangeCount con el valor que tenían al armar sus textos.
// -----------------------------------------------------------------------------

unit uMakerAi.Localization;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs;

const
  // Idioma base: su tabla debe tener todas las claves
  AI_LANG_BASE = 'en';
  // Variable de entorno que fuerza el idioma (útil en servidores y pruebas)
  AI_LANG_ENV_VAR = 'MAKERAI_LANG';

type
  TAiLocale = class
  private
    class var FTables: TObjectDictionary<string, TDictionary<string, string>>;
    class var FLock: TCriticalSection;
    class var FLanguage: string;    // idioma efectivo, normalizado
    class var FExplicit: Boolean;   // True si lo fijó la aplicación
    class var FChangeCount: Integer;
    class constructor Create;
    class destructor Destroy;
    class function GetLanguage: string; static;
    class procedure SetLanguage(const AValue: string); static;
    // Sin lock: el llamador ya lo tomó
    class function TryFind(const ALang, AKey: string; out AText: string): Boolean; static;
    class function Resolve(const ALang, AKey: string): string; static;
  public
    // Agrega (o reemplaza) textos de un idioma. APairs = [clave1, texto1, clave2, texto2, ...]
    class procedure RegisterTable(const ALang: string; const APairs: array of string); static;

    // Texto de la clave en el idioma activo (con la cadena de respaldo)
    class function Text(const AKey: string): string; static;
    // Texto de la clave en un idioma dado (con la cadena de respaldo)
    class function TextIn(const ALang, AKey: string): string; static;
    // True si el idioma dado tiene la clave en su propia tabla (sin respaldo)
    class function HasKey(const ALang, AKey: string): Boolean; static;

    // 'es_CO.UTF-8' -> 'es-co'; 'PT-BR' -> 'pt-br'; '' -> ''
    class function NormalizeLanguage(const ALang: string): string; static;
    // 'pt-br' -> 'pt'
    class function PrimaryLanguage(const ALang: string): string; static;
    // Idioma del sistema operativo, normalizado ('' si no se pudo saber)
    class function DetectSystemLanguage: string; static;

    class function RegisteredLanguages: TArray<string>; static;
    class function KeysOf(const ALang: string): TArray<string>; static;
    // Problemas de las tablas frente al inglés: claves faltantes o sobrantes y
    // marcadores de Format distintos. Vacío = todo en orden.
    class function Validate: TArray<string>; static;
    // Marcadores de Format de un texto en orden: 'Error %d: %s' -> '%d %s'
    class function FormatSpecifiers(const AText: string): string; static;

    class property Language: string read GetLanguage write SetLanguage;
    // Aumenta cada vez que cambia el idioma o se registra una tabla
    class function ChangeCount: Integer; static;
  end;

// Atajos para el código del framework
function AiStr(const AKey: string): string;
function AiFmt(const AKey: string; const AArgs: array of const): string;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.Helpers,
{$ENDIF}
  System.Generics.Defaults,
  uMakerAi.Lang.En, uMakerAi.Lang.Es, uMakerAi.Lang.Pt;

function AiStr(const AKey: string): string;
begin
  Result := TAiLocale.Text(AKey);
end;

function AiFmt(const AKey: string; const AArgs: array of const): string;
begin
  try
    Result := Format(TAiLocale.Text(AKey), AArgs);
  except
    on EConvertError do
      // Una traducción con marcadores mal puestos no debe tapar el error
      // original: se cae al texto base
      Result := Format(TAiLocale.TextIn(AI_LANG_BASE, AKey), AArgs);
  end;
end;

{ TAiLocale }

class constructor TAiLocale.Create;
begin
  FLock := TCriticalSection.Create;
  FTables := TObjectDictionary<string, TDictionary<string, string>>.Create([doOwnsValues]);
end;

class destructor TAiLocale.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FLock);
end;

class function TAiLocale.NormalizeLanguage(const ALang: string): string;
var
  P: Integer;
begin
  Result := Trim(ALang);
  // 'es_CO.UTF-8@euro' -> 'es_CO'
  P := Pos('.', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
  P := Pos('@', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
  Result := LowerCase(StringReplace(Result, '_', '-', [rfReplaceAll]));
  // 'C' y 'POSIX' no dicen nada del idioma
  if (Result = 'c') or (Result = 'posix') then
    Result := '';
end;

class function TAiLocale.PrimaryLanguage(const ALang: string): string;
var
  P: Integer;
begin
  Result := NormalizeLanguage(ALang);
  P := Pos('-', Result);
  if P > 0 then
    Result := Copy(Result, 1, P - 1);
end;

class function TAiLocale.DetectSystemLanguage: string;
{$IF DEFINED(MSWINDOWS)}
var
  UiLang: LANGID;
  Lcid: Cardinal;
  Buf: array [0 .. 15] of Char;
  Lang, Country: string;
begin
  Result := '';
  UiLang := GetUserDefaultUILanguage;
  Lcid := MAKELCID(UiLang, SORT_DEFAULT);
  if GetLocaleInfo(Lcid, LOCALE_SISO639LANGNAME, Buf, Length(Buf)) > 0 then
  begin
    Lang := Buf;
    Country := '';
    if GetLocaleInfo(Lcid, LOCALE_SISO3166CTRYNAME, Buf, Length(Buf)) > 0 then
      Country := Buf;
    if Country <> '' then
      Result := NormalizeLanguage(Lang + '-' + Country)
    else
      Result := NormalizeLanguage(Lang);
  end;
end;
{$ELSEIF DEFINED(ANDROID)}
begin
  Result := '';
  try
    Result := NormalizeLanguage(JStringToString(TJLocale.JavaClass.getDefault.toString));
  except
    Result := '';
  end;
end;
{$ELSE}
var
  VarName, V: string;
begin
  Result := '';
  // Orden de precedencia de POSIX para los mensajes
  for VarName in TArray<string>.Create('LC_ALL', 'LC_MESSAGES', 'LANG') do
  begin
    V := NormalizeLanguage(GetEnvironmentVariable(VarName));
    if V <> '' then
      Exit(V);
  end;
end;
{$IFEND}

class function TAiLocale.GetLanguage: string;
begin
  FLock.Enter;
  try
    Result := FLanguage;
  finally
    FLock.Leave;
  end;
end;

class procedure TAiLocale.SetLanguage(const AValue: string);
var
  NewLang: string;
  IsExplicit: Boolean;
begin
  NewLang := NormalizeLanguage(AValue);
  IsExplicit := NewLang <> '';
  if not IsExplicit then
  begin
    // '' = volver a la detección automática
    NewLang := NormalizeLanguage(GetEnvironmentVariable(AI_LANG_ENV_VAR));
    if NewLang = '' then
      NewLang := DetectSystemLanguage;
    if NewLang = '' then
      NewLang := AI_LANG_BASE;
  end;
  FLock.Enter;
  try
    FExplicit := IsExplicit;
    if NewLang <> FLanguage then
    begin
      FLanguage := NewLang;
      Inc(FChangeCount);
    end;
  finally
    FLock.Leave;
  end;
end;

class function TAiLocale.ChangeCount: Integer;
begin
  FLock.Enter;
  try
    Result := FChangeCount;
  finally
    FLock.Leave;
  end;
end;

class procedure TAiLocale.RegisterTable(const ALang: string; const APairs: array of string);
var
  Lang: string;
  Table: TDictionary<string, string>;
  I: Integer;
begin
  Lang := NormalizeLanguage(ALang);
  if Lang = '' then
    raise EArgumentException.Create('TAiLocale.RegisterTable: language code is empty');
  if Odd(Length(APairs)) then
    raise EArgumentException.CreateFmt('TAiLocale.RegisterTable(%s): pairs array has an odd length (%d)',
      [Lang, Length(APairs)]);
  FLock.Enter;
  try
    if not FTables.TryGetValue(Lang, Table) then
    begin
      Table := TDictionary<string, string>.Create;
      FTables.Add(Lang, Table);
    end;
    I := 0;
    while I < Length(APairs) do
    begin
      Table.AddOrSetValue(APairs[I], APairs[I + 1]);
      Inc(I, 2);
    end;
    Inc(FChangeCount);
  finally
    FLock.Leave;
  end;
end;

class function TAiLocale.TryFind(const ALang, AKey: string; out AText: string): Boolean;
var
  Table: TDictionary<string, string>;
begin
  Result := (ALang <> '') and FTables.TryGetValue(ALang, Table) and Table.TryGetValue(AKey, AText);
end;

class function TAiLocale.Resolve(const ALang, AKey: string): string;
var
  Primary: string;
begin
  FLock.Enter;
  try
    if TryFind(ALang, AKey, Result) then
      Exit;
    Primary := PrimaryLanguage(ALang);
    if (Primary <> ALang) and TryFind(Primary, AKey, Result) then
      Exit;
    if TryFind(AI_LANG_BASE, AKey, Result) then
      Exit;
    Result := AKey;
  finally
    FLock.Leave;
  end;
end;

class function TAiLocale.Text(const AKey: string): string;
begin
  Result := Resolve(GetLanguage, AKey);
end;

class function TAiLocale.TextIn(const ALang, AKey: string): string;
begin
  Result := Resolve(NormalizeLanguage(ALang), AKey);
end;

class function TAiLocale.HasKey(const ALang, AKey: string): Boolean;
var
  Dummy: string;
begin
  FLock.Enter;
  try
    Result := TryFind(NormalizeLanguage(ALang), AKey, Dummy);
  finally
    FLock.Leave;
  end;
end;

class function TAiLocale.RegisteredLanguages: TArray<string>;
begin
  FLock.Enter;
  try
    Result := FTables.Keys.ToArray;
  finally
    FLock.Leave;
  end;
  TArray.Sort<string>(Result);
end;

class function TAiLocale.KeysOf(const ALang: string): TArray<string>;
var
  Table: TDictionary<string, string>;
begin
  Result := nil;
  FLock.Enter;
  try
    if FTables.TryGetValue(NormalizeLanguage(ALang), Table) then
      Result := Table.Keys.ToArray;
  finally
    FLock.Leave;
  end;
  TArray.Sort<string>(Result);
end;

class function TAiLocale.FormatSpecifiers(const AText: string): string;
var
  I, J: Integer;
  SB: TStringBuilder;
begin
  // Recorre los marcadores de Format: %[índice:][-][ancho][.precisión]tipo
  SB := TStringBuilder.Create;
  try
    I := 1;
    while I <= Length(AText) do
    begin
      if AText[I] = '%' then
      begin
        if (I < Length(AText)) and (AText[I + 1] = '%') then
        begin
          Inc(I, 2); // '%%' es un '%' literal
          Continue;
        end;
        J := I + 1;
        while (J <= Length(AText)) and CharInSet(AText[J], ['0' .. '9', ':', '-', '.', '*']) do
          Inc(J);
        if J <= Length(AText) then
        begin
          if SB.Length > 0 then
            SB.Append(' ');
          SB.Append(LowerCase(Copy(AText, I, J - I + 1)));
        end;
        I := J + 1;
      end
      else
        Inc(I);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TAiLocale.Validate: TArray<string>;
var
  Problems: TList<string>;
  BaseKeys, LangKeys: TArray<string>;
  Lang, Key, BaseText, LangText: string;
begin
  Problems := TList<string>.Create;
  try
    BaseKeys := KeysOf(AI_LANG_BASE);
    if Length(BaseKeys) = 0 then
      Problems.Add('base language "' + AI_LANG_BASE + '" has no table');
    for Lang in RegisteredLanguages do
    begin
      if Lang = AI_LANG_BASE then
        Continue;
      for Key in BaseKeys do
      begin
        if not HasKey(Lang, Key) then
          Problems.Add(Lang + ': missing ' + Key)
        else
        begin
          BaseText := TextIn(AI_LANG_BASE, Key);
          LangText := TextIn(Lang, Key);
          if FormatSpecifiers(BaseText) <> FormatSpecifiers(LangText) then
            Problems.Add(Format('%s: %s has specifiers "%s", base has "%s"',
              [Lang, Key, FormatSpecifiers(LangText), FormatSpecifiers(BaseText)]));
        end;
      end;
      LangKeys := KeysOf(Lang);
      for Key in LangKeys do
        if not HasKey(AI_LANG_BASE, Key) then
          Problems.Add(Lang + ': extra ' + Key + ' (not in base)');
    end;
    Result := Problems.ToArray;
  finally
    Problems.Free;
  end;
end;

initialization
  TAiLocale.RegisterTable('en', AiLangEn);
  TAiLocale.RegisterTable('es', AiLangEs);
  TAiLocale.RegisterTable('pt', AiLangPt); // portugués de Brasil; pt-BR y pt-PT caen aquí
  TAiLocale.Language := ''; // detección automática

end.
