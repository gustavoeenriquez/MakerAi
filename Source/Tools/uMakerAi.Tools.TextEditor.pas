// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

{
  -------------------------------------------------------------------------------
  TAiTextEditorTool - Componente de Edición de Texto para IA
  -------------------------------------------------------------------------------

  ADVERTENCIA SOBRE EL MODO DE OPERACIÓN:

  Este componente está diseñado con un sistema de eventos para virtualizar la
  entrada y salida (I/O).

  1. MODO POR DEFECTO (Acceso a Disco):
  Si NO se asignan los eventos (OnLoadFile, OnSaveFile, etc.) o si el parámetro
  "Handled" se deja en False, el componente ejecutará las operaciones directamente
  sobre el SISTEMA DE ARCHIVOS FÍSICO del sistema operativo.

  2. MODO INTERCEPTADO (Memoria/UI/DB):
  Para evitar el acceso al disco (ej. para editar un TMemo o un registro de BD),
  el programador debe asignar los eventos correspondientes, realizar la lógica
  personalizada y establecer explícitamente:
  Handled := True;

  Esto detiene la ejecución de la lógica predeterminada de archivos.
  -------------------------------------------------------------------------------
}

unit uMakerAi.Tools.TextEditor;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils, System.StrUtils, uMakerAi.Utils.DiffUpdater;

type

  // Evento para leer contenido (ej: desde DB o Memoria)
  TAiFileReadEvent = procedure(Sender: TObject; const Path: string; var Content: string; var Handled: Boolean) of object;
  // Evento para escribir contenido
  TAiFileWriteEvent = procedure(Sender: TObject; const Path: string; const Content: string; var Handled: Boolean) of object;
  // Evento para verificar existencia
  TAiFileCheckEvent = procedure(Sender: TObject; const Path: string; var Exists: Boolean; var Handled: Boolean) of object;
  // Evento para gestión de directorios
  TAiDirEvent = procedure(Sender: TObject; const Path: string; var Handled: Boolean) of object;
  // Evento genérico antes de ejecutar un comando (permite override total de la lógica)
  TAiCommandEvent = procedure(Sender: TObject; const Command, Path: string; Args: TJSONObject; var Result: string; var Handled: Boolean) of object;

  TAiTextEditorTool = class(TComponent)
  private
    FOnLoadFile: TAiFileReadEvent;
    FOnSaveFile: TAiFileWriteEvent;
    FOnFileExists: TAiFileCheckEvent;
    FOnEnsureDirectory: TAiDirEvent;
    FOnBeforeCommand: TAiCommandEvent;

    function CountOccurrences(const Text, SubText: string): Integer;
  protected
    // --- Métodos Virtuales con soporte de Eventos ---
    function LoadFileContent(const Path: string): string; virtual;
    procedure SaveFileContent(const Path: string; const Content: string); virtual;
    function FileExists(const Path: string): Boolean; virtual;
    function EnsureDirectory(const Path: string): Boolean; virtual;
    function ValidatePath(const APath: string): Boolean; virtual;

    // --- Comandos Específicos ---
    function Cmd_View(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_Create(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_StrReplace(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_Insert(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_ApplyDiff(const Path: string; const jArgs: TJSONObject): string; virtual;

  public
    // Ejecuta la herramienta recibiendo un JSON string
    function Execute(const JsonArguments: string): string; virtual;

  published
    // --- Eventos Publicados ---
    property OnLoadFile: TAiFileReadEvent read FOnLoadFile write FOnLoadFile;
    property OnSaveFile: TAiFileWriteEvent read FOnSaveFile write FOnSaveFile;
    property OnFileExists: TAiFileCheckEvent read FOnFileExists write FOnFileExists;
    property OnEnsureDirectory: TAiDirEvent read FOnEnsureDirectory write FOnEnsureDirectory;
    property OnBeforeCommand: TAiCommandEvent read FOnBeforeCommand write FOnBeforeCommand;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiTextEditorTool]);
end;

{ TAiTextEditorTool }

function TAiTextEditorTool.Execute(const JsonArguments: string): string;
var
  jArgs: TJSONObject;
  Cmd, Path: string;
  Handled: Boolean;
begin
  jArgs := TJSONObject.ParseJSONValue(JsonArguments) as TJSONObject;
  if not Assigned(jArgs) then
    Exit('Error: Argumentos inválidos (JSON mal formado).');

  try
    try
      // 1. Extraer parámetros comunes
      Cmd := jArgs.GetValue<string>('command');
      Path := jArgs.GetValue<string>('path');

      // Evento: Permitir al usuario interceptar el comando completamente
      Handled := False;
      if Assigned(FOnBeforeCommand) then
        FOnBeforeCommand(Self, Cmd, Path, jArgs, Result, Handled);

      if Handled then
        Exit; // El usuario ya generó el Result

      // 2. Validar ruta (Seguridad básica)
      if not ValidatePath(Path) then
        Exit('Error: Ruta inválida o vacía.');

      if (Cmd <> 'create') and (not FileExists(Path)) then
        Exit('Error: El archivo no existe en la ruta: ' + Path);

      // 3. Despachar comando
      if Cmd = 'view' then
        Result := Cmd_View(Path, jArgs)
      else if Cmd = 'create' then
        Result := Cmd_Create(Path, jArgs)
      else if Cmd = 'str_replace' then
        Result := Cmd_StrReplace(Path, jArgs)
      else if Cmd = 'insert' then
        Result := Cmd_Insert(Path, jArgs)
      else if Cmd = 'apply_diff' then
        Result := Cmd_ApplyDiff(Path, jArgs)
      else
        Result := 'Error: Comando desconocido "' + Cmd + '"';

    except
      on E: Exception do
        Result := 'Error ejecutando editor (' + Cmd + '): ' + E.Message;
    end;
  finally
    jArgs.Free;
  end;
end;

// --- Implementación de I/O con Eventos ---

function TAiTextEditorTool.LoadFileContent(const Path: string): string;
var
  Handled: Boolean;
begin
  Handled := False;
  Result := '';
  // 1. Intentar evento
  if Assigned(FOnLoadFile) then
    FOnLoadFile(Self, Path, Result, Handled);

  // 2. Si no fue manejado, usar disco
  if not Handled then
  begin
    if TFile.Exists(Path) then
      Result := TFile.ReadAllText(Path, TEncoding.UTF8)
    else
      raise EFileNotFoundException.Create('Archivo no encontrado: ' + Path);
  end;
end;

procedure TAiTextEditorTool.SaveFileContent(const Path: string; const Content: string);
var
  Handled: Boolean;
begin
  Handled := False;
  // 1. Intentar evento
  if Assigned(FOnSaveFile) then
    FOnSaveFile(Self, Path, Content, Handled);

  // 2. Si no fue manejado, escribir a disco
  if not Handled then
    TFile.WriteAllText(Path, Content, TEncoding.UTF8);
end;

function TAiTextEditorTool.FileExists(const Path: string): Boolean;
var
  Handled: Boolean;
begin
  Handled := False;
  Result := False;
  // 1. Intentar evento
  if Assigned(FOnFileExists) then
    FOnFileExists(Self, Path, Result, Handled);

  // 2. Si no fue manejado, verificar disco
  if not Handled then
    Result := TFile.Exists(Path);
end;

function TAiTextEditorTool.EnsureDirectory(const Path: string): Boolean;
var
  Handled: Boolean;
  Dir: string;
begin
  Handled := False;
  Result := True; // Asumimos éxito por defecto

  // 1. Intentar evento
  if Assigned(FOnEnsureDirectory) then
    FOnEnsureDirectory(Self, Path, Handled);

  // 2. Si no fue manejado, crear en disco
  if not Handled then
  begin
    Dir := TPath.GetDirectoryName(Path);
    if (Dir <> '') and (not TDirectory.Exists(Dir)) then
    begin
      try
        TDirectory.CreateDirectory(Dir);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAiTextEditorTool.ValidatePath(const APath: string): Boolean;
begin
  // Validación básica. Se puede sobrecargar para lógica más compleja de seguridad.
  Result := Trim(APath) <> '';
end;

function TAiTextEditorTool.CountOccurrences(const Text, SubText: string): Integer;
var
  P, Offset: Integer;
begin
  Result := 0;
  if (Text = '') or (SubText = '') then
    Exit;

  Offset := 1;
  P := Pos(SubText, Text, Offset);
  while P > 0 do
  begin
    Inc(Result);
    Offset := P + Length(SubText);
    P := Pos(SubText, Text, Offset);
  end;
end;

// --- Comandos Específicos ---

function TAiTextEditorTool.Cmd_View(const Path: string; const jArgs: TJSONObject): string;
var
  FullText: string;
  Lines: TStringList;
  ViewRange: TJSonArray;
  StartL, EndL, i: Integer;
  ResBuilder: TStringBuilder;
begin
  FullText := LoadFileContent(Path);
  Lines := TStringList.Create;
  try
    Lines.Text := FullText; // TStringList maneja saltos de línea automáticamente

    // Verificar si piden un rango específico [start_line, end_line]
    if jArgs.TryGetValue<TJSonArray>('view_range', ViewRange) and (ViewRange.Count = 2) then
    begin
      StartL := ViewRange.Items[0].GetValue<Integer> - 1; // Claude usa base 1 -> TStringList base 0
      EndL := ViewRange.Items[1].GetValue<Integer> - 1;

      if StartL < 0 then
        StartL := 0;
      if EndL >= Lines.Count then
        EndL := Lines.Count - 1;

      if StartL > EndL then
        Exit('Error: Rango de vista inválido (Start > End) o fuera de límites.');

      ResBuilder := TStringBuilder.Create;
      try
        for i := StartL to EndL do
          ResBuilder.AppendLine(Lines[i]);
        Result := ResBuilder.ToString;
      finally
        ResBuilder.Free;
      end;
    end
    else
    begin
      // Ver todo el archivo
      Result := FullText;
    end;
  finally
    Lines.Free;
  end;
end;

function TAiTextEditorTool.Cmd_Create(const Path: string; const jArgs: TJSONObject): string;
var
  Content: string;
begin
  if FileExists(Path) then
    Exit('Error: El archivo ya existe. Usa "str_replace" o "insert" para modificarlo.');

  Content := jArgs.GetValue<string>('file_text', '');

  if not EnsureDirectory(Path) then
    Exit('Error: No se pudo crear el directorio para el archivo.');

  SaveFileContent(Path, Content);
  Result := 'Archivo creado exitosamente.';
end;

function TAiTextEditorTool.Cmd_StrReplace(const Path: string; const jArgs: TJSONObject): string;
var
  OldStr, NewStr, FileContent: string;
  Occurrences: Integer;
begin
  OldStr := jArgs.GetValue<string>('old_str');
  NewStr := jArgs.GetValue<string>('new_str');

  // Cargar contenido (Dispara OnLoadFile -> Lee del Memo)
  FileContent := LoadFileContent(Path);

  // 1. VALIDACIÓN SEGÚN DOCS: Conteo exacto
  Occurrences := CountOccurrences(FileContent, OldStr);

  if Occurrences = 0 then
    // Mensaje oficial de la documentación para "No matches"
    Exit('Error: No match found for replacement. Please check your text and try again.')
  else if Occurrences > 1 then
    // Mensaje oficial de la documentación para "Multiple matches"
    Exit('Error: Found ' + IntToStr(Occurrences) + ' matches for replacement text. Please provide more context to make a unique match.');

  // 2. EJECUCIÓN: Reemplazo en memoria
  FileContent := StringReplace(FileContent, OldStr, NewStr, []);

  // 3. PERSISTENCIA: Dispara OnSaveFile -> Actualiza el Memo
  SaveFileContent(Path, FileContent);

  // Respuesta de éxito estándar
  Result := 'Successfully replaced text at exactly one location.';
end;

function TAiTextEditorTool.Cmd_Insert(const Path: string; const jArgs: TJSONObject): string;
var
  NewStr, FileContent: string;
  InsertLine: Integer;
  Lines: TStringList;
begin
  // CORRECCIÓN: Intentar leer 'insert_text' primero, si no existe, probar 'new_str'
  If Not jArgs.TryGetValue<string>('insert_text', NewStr) then
    if NewStr = '' then
      NewStr := jArgs.GetValue<string>('new_str');

  // Claude envía la línea DESPUÉS de la cual insertar (base 0 o 1 dependiendo del modelo, usualmente 0 es inicio)
  InsertLine := jArgs.GetValue<Integer>('insert_line');

  FileContent := LoadFileContent(Path);
  Lines := TStringList.Create;
  try
    Lines.Text := FileContent;

    // Validación de seguridad para evitar crashes
    if (InsertLine < 0) or (InsertLine > Lines.Count) then
      Exit('Error: Número de línea ' + IntToStr(InsertLine) + ' fuera de rango.');

    if InsertLine = 0 then
      Lines.Insert(0, NewStr)
    else if InsertLine >= Lines.Count then
      Lines.Add(NewStr)
    else
      Lines.Insert(InsertLine, NewStr);

    SaveFileContent(Path, Lines.Text);
    Result := 'Texto insertado exitosamente.';
  finally
    Lines.Free;
  end;
end;

function TAiTextEditorTool.Cmd_ApplyDiff(const Path: string; const jArgs: TJSONObject): string;
var
  DiffText, OriginalContent, NewContent, ErrorMsg: string;
  Applier: TDiffApplier;
begin
  DiffText := jArgs.GetValue<string>('diff_text');
  if DiffText = '' then
    Exit('Error: "diff_text" no puede estar vacío.');

  OriginalContent := LoadFileContent(Path);

  Applier := TDiffApplier.Create;
  try
    if Applier.Apply(OriginalContent, DiffText, NewContent, ErrorMsg) then
    begin
      SaveFileContent(Path, NewContent);
      Result := 'Diff aplicado exitosamente.';
    end
    else
      Result := 'Error al aplicar diff: ' + ErrorMsg;
  finally
    Applier.Free;
  end;
end;

end.
