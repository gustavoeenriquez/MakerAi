// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Tools.TextEditor
// Editor de texto con I/O virtualizado mediante eventos (disco o memoria).
unit uMakerAi.Tools.TextEditor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  uMakerAi.Utils.DiffUpdater;

type

  // Evento para leer contenido (ej: desde DB o Memoria)
  TAiFileReadEvent = procedure(Sender: TObject; const Path: string;
      var Content: string; var Handled: Boolean) of object;
  // Evento para escribir contenido
  TAiFileWriteEvent = procedure(Sender: TObject; const Path: string;
      const Content: string; var Handled: Boolean) of object;
  // Evento para verificar existencia
  TAiFileCheckEvent = procedure(Sender: TObject; const Path: string;
      var Exists: Boolean; var Handled: Boolean) of object;
  // Evento para gestión de directorios
  TAiDirEvent = procedure(Sender: TObject; const Path: string;
      var Handled: Boolean) of object;
  // Evento genérico antes de ejecutar un comando
  TAiCommandEvent = procedure(Sender: TObject; const Command, Path: string;
      Args: TJSONObject; var Result: string; var Handled: Boolean) of object;

  TAiTextEditorTool = class(TComponent)
  private
    FOnLoadFile: TAiFileReadEvent;
    FOnSaveFile: TAiFileWriteEvent;
    FOnFileExists: TAiFileCheckEvent;
    FOnEnsureDirectory: TAiDirEvent;
    FOnBeforeCommand: TAiCommandEvent;

    function CountOccurrences(const Text, SubText: string): Integer;
  protected
    function LoadFileContent(const Path: string): string; virtual;
    procedure SaveFileContent(const Path: string; const Content: string); virtual;
    function DoFileExists(const Path: string): Boolean; virtual;
    function EnsureDirectory(const Path: string): Boolean; virtual;
    function ValidatePath(const APath: string): Boolean; virtual;

    function Cmd_View(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_Create(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_StrReplace(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_Insert(const Path: string; const jArgs: TJSONObject): string; virtual;
    function Cmd_ApplyDiff(const Path: string; const jArgs: TJSONObject): string; virtual;

  public
    function Execute(const JsonArguments: string): string; virtual;

  published
    property OnLoadFile: TAiFileReadEvent read FOnLoadFile write FOnLoadFile;
    property OnSaveFile: TAiFileWriteEvent read FOnSaveFile write FOnSaveFile;
    property OnFileExists: TAiFileCheckEvent
        read FOnFileExists write FOnFileExists;
    property OnEnsureDirectory: TAiDirEvent
        read FOnEnsureDirectory write FOnEnsureDirectory;
    property OnBeforeCommand: TAiCommandEvent
        read FOnBeforeCommand write FOnBeforeCommand;
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
  jVal: TJSONData;
  jArgs: TJSONObject;
  Cmd, Path: string;
  Handled: Boolean;
begin
  jVal := GetJSON(JsonArguments);
  if not (jVal is TJSONObject) then
  begin
    jVal.Free;
    Result := 'Error: Argumentos inválidos (JSON mal formado).';
    Exit;
  end;

  jArgs := TJSONObject(jVal);
  try
    try
      Cmd := jArgs.Get('command', '');
      Path := jArgs.Get('path', '');

      Handled := False;
      if Assigned(FOnBeforeCommand) then
        FOnBeforeCommand(Self, Cmd, Path, jArgs, Result, Handled);

      if Handled then Exit;

      if not ValidatePath(Path) then
      begin
        Result := 'Error: Ruta inválida o vacía.';
        Exit;
      end;

      if (Cmd <> 'create') and (not DoFileExists(Path)) then
      begin
        Result := 'Error: El archivo no existe en la ruta: ' + Path;
        Exit;
      end;

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

function TAiTextEditorTool.LoadFileContent(const Path: string): string;
var
  Handled: Boolean;
  SS: TStringStream;
begin
  Handled := False;
  Result := '';

  if Assigned(FOnLoadFile) then
    FOnLoadFile(Self, Path, Result, Handled);

  if not Handled then
  begin
    if SysUtils.FileExists(Path) then
    begin
      SS := TStringStream.Create('');
      try
        SS.LoadFromFile(Path);
        Result := SS.DataString;
      finally
        SS.Free;
      end;
    end
    else
      raise EFileNotFoundException.Create('Archivo no encontrado: ' + Path);
  end;
end;

procedure TAiTextEditorTool.SaveFileContent(const Path: string;
    const Content: string);
var
  Handled: Boolean;
  SS: TStringStream;
begin
  Handled := False;

  if Assigned(FOnSaveFile) then
    FOnSaveFile(Self, Path, Content, Handled);

  if not Handled then
  begin
    SS := TStringStream.Create(Content);
    try
      SS.SaveToFile(Path);
    finally
      SS.Free;
    end;
  end;
end;

function TAiTextEditorTool.DoFileExists(const Path: string): Boolean;
var
  Handled: Boolean;
begin
  Handled := False;
  Result := False;

  if Assigned(FOnFileExists) then
    FOnFileExists(Self, Path, Result, Handled);

  if not Handled then
    Result := SysUtils.FileExists(Path);
end;

function TAiTextEditorTool.EnsureDirectory(const Path: string): Boolean;
var
  Handled: Boolean;
  Dir: string;
begin
  Handled := False;
  Result := True;

  if Assigned(FOnEnsureDirectory) then
    FOnEnsureDirectory(Self, Path, Handled);

  if not Handled then
  begin
    Dir := ExtractFileDir(Path);
    if (Dir <> '') and not DirectoryExists(Dir) then
    begin
      try
        ForceDirectories(Dir);
      except
        Result := False;
      end;
    end;
  end;
end;

function TAiTextEditorTool.ValidatePath(const APath: string): Boolean;
begin
  Result := Trim(APath) <> '';
end;

function TAiTextEditorTool.CountOccurrences(const Text,
    SubText: string): Integer;
var
  P, Offset: Integer;
begin
  Result := 0;
  if (Text = '') or (SubText = '') then Exit;

  Offset := 1;
  P := Pos(SubText, Text, Offset);
  while P > 0 do
  begin
    Inc(Result);
    Offset := P + Length(SubText);
    P := Pos(SubText, Text, Offset);
  end;
end;

function TAiTextEditorTool.Cmd_View(const Path: string;
    const jArgs: TJSONObject): string;
var
  FullText: string;
  Lines: TStringList;
  ViewRange: TJSONArray;
  StartL, EndL, I: Integer;
  Res: string;
  D: TJSONData;
begin
  FullText := LoadFileContent(Path);
  Lines := TStringList.Create;
  try
    Lines.Text := FullText;

    D := jArgs.Find('view_range');
    if (D <> nil) and (D is TJSONArray) and (TJSONArray(D).Count = 2) then
    begin
      ViewRange := TJSONArray(D);
      StartL := ViewRange.Items[0].AsInteger - 1;
      EndL := ViewRange.Items[1].AsInteger - 1;

      if StartL < 0 then StartL := 0;
      if EndL >= Lines.Count then EndL := Lines.Count - 1;

      if StartL > EndL then
      begin
        Result := 'Error: Rango de vista inválido (Start > End) o fuera de límites.';
        Exit;
      end;

      Res := '';
      for I := StartL to EndL do
        Res := Res + Lines[I] + LineEnding;
      Result := Res;
    end
    else
      Result := FullText;
  finally
    Lines.Free;
  end;
end;

function TAiTextEditorTool.Cmd_Create(const Path: string;
    const jArgs: TJSONObject): string;
var
  Content: string;
begin
  if DoFileExists(Path) then
  begin
    Result := 'Error: El archivo ya existe. Usa "str_replace" o "insert" para modificarlo.';
    Exit;
  end;

  Content := jArgs.Get('file_text', '');

  if not EnsureDirectory(Path) then
  begin
    Result := 'Error: No se pudo crear el directorio para el archivo.';
    Exit;
  end;

  SaveFileContent(Path, Content);
  Result := 'Archivo creado exitosamente.';
end;

function TAiTextEditorTool.Cmd_StrReplace(const Path: string;
    const jArgs: TJSONObject): string;
var
  OldStr, NewStr, FileContent: string;
  Occurrences: Integer;
begin
  OldStr := jArgs.Get('old_str', '');
  NewStr := jArgs.Get('new_str', '');

  FileContent := LoadFileContent(Path);
  Occurrences := CountOccurrences(FileContent, OldStr);

  if Occurrences = 0 then
    Result := 'Error: No match found for replacement. Please check your text and try again.'
  else if Occurrences > 1 then
    Result := 'Error: Found ' + IntToStr(Occurrences) +
        ' matches for replacement text. Please provide more context to make a unique match.'
  else
  begin
    FileContent := StringReplace(FileContent, OldStr, NewStr, []);
    SaveFileContent(Path, FileContent);
    Result := 'Successfully replaced text at exactly one location.';
  end;
end;

function TAiTextEditorTool.Cmd_Insert(const Path: string;
    const jArgs: TJSONObject): string;
var
  NewStr, FileContent: string;
  InsertLine: Integer;
  Lines: TStringList;
  D: TJSONData;
begin
  NewStr := '';
  D := jArgs.Find('insert_text');
  if D <> nil then
    NewStr := D.AsString
  else
  begin
    D := jArgs.Find('new_str');
    if D <> nil then
      NewStr := D.AsString
    else
    begin
      Result := 'Error: No insert text provided.';
      Exit;
    end;
  end;

  InsertLine := jArgs.Get('insert_line', 0);

  FileContent := LoadFileContent(Path);
  Lines := TStringList.Create;
  try
    Lines.Text := FileContent;

    if (InsertLine < 0) or (InsertLine > Lines.Count) then
    begin
      Result := 'Error: Número de línea ' + IntToStr(InsertLine) +
          ' fuera de rango.';
      Exit;
    end;

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

function TAiTextEditorTool.Cmd_ApplyDiff(const Path: string;
    const jArgs: TJSONObject): string;
var
  DiffText, OriginalContent, NewContent, ErrorMsg: string;
  Applier: TDiffApplier;
begin
  DiffText := jArgs.Get('diff_text', '');
  if DiffText = '' then
  begin
    Result := 'Error: "diff_text" no puede estar vacío.';
    Exit;
  end;

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
