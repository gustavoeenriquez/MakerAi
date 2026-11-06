// IT License
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

// Demo 041 - Generación de video con Google Veo (TAiVeoGenerator).
//
// La conversación usa la UI de chat actual: TAIChatView + TAIChatInput.
// - El avance de Veo se ve como un paso dentro de la burbuja del asistente.
// - El video generado se guarda en Documentos\VeoDemo y aparece como adjunto;
//   un clic sobre él lo abre con el reproductor del sistema.
// - Extender un video: se adjunta un .mp4 generado por esta app. Sus metadatos
//   (el identificador de Veo, sin el contenido) se guardan junto al video con
//   el hash MD5 del archivo como nombre, así que se encuentran aunque el video
//   se haya renombrado o movido (TAIChatInput entrega una copia temporal del
//   adjunto, no la ruta original).

unit uVeoDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.JSON, System.Hash,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.ListBox,
  uMakerAi.Core, uMakerAi.Utils.System, uMakerAi.Gemini.Veo,
  AIChat.Types, AIChat.Control.FMX, AIChat.Input;

type

  TVeoGenerationMode = (gmAuto, // El procedimiento decide automáticamente (comportamiento por defecto)
    gmFrames, // Forzar el modo de interpolación (primer/último fotograma)
    gmReferences // Forzar el modo de imágenes de referencia
    );

  TForm11 = class(TForm)
    AiVeo: TAiVeoGenerator;
    MainLayout: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Splitter1: TSplitter;
    Modo: TGroupBox;
    rbAuto: TRadioButton;
    rbInter: TRadioButton;
    rbReference: TRadioButton;
    cbModel: TComboBox;
    Model: TLabel;
    Label2: TLabel;
    cbAspectRatio: TComboBox;
    Label3: TLabel;
    cbResolution: TComboBox;
    Label1: TLabel;
    cbPersonGeneration: TComboBox;
    Label4: TLabel;
    cbDuration: TComboBox;
    Layout3: TLayout;
    Layout4: TLayout;
    Memo1: TMemo;
    Layout5: TLayout;
    LayoutLog: TLayout;
    Label5: TLabel;
    Label6: TLabel;
    memoLog: TMemo;
    ChatView1: TAIChatView;
    Splitter2: TSplitter;
    Layout6: TLayout;
    ChatInput1: TAIChatInput;
    LayoutNegativePrompt: TLayout;
    Label7: TLabel;
    MemoNegativePrompt: TMemo;
    Label8: TLabel;
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ChatView1AttachmentOpen(Sender: TObject; AMsg: TAIChatMessage; AAttach: TAIChatAttachment);
    procedure AiVeoSuccess(Sender: TObject; ResultVideo: TAiMediaFile);
    procedure AiVeoProgress(Sender: TObject; const StatusMessage: string);
    procedure AiVeoError(Sender: TObject; const ErrorMessage: string);
  private
    FProgressIdx: Integer;  // burbuja del asistente que muestra el avance (-1 = ninguna)
    FStartTick: UInt64;     // para la duración del paso
    FVideoHash: string;     // MD5 del video adjunto cuando la tarea es extenderlo
    procedure ExecuteVeoTask(const APrompt: string; aMediaFiles: TAiMediaFiles; aMode: TVeoGenerationMode = gmAuto);
    function GetSelectedVeoMode: TVeoGenerationMode;
    procedure Log(const AText: string);
    procedure BeginProgress(const ACaption: string);
    procedure EndProgress(AOk: Boolean; const AText: string);
    class function VideoFolder: string; static;
    class function HashOfStream(AStream: TStream): string; static;
  public
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

function AllMediaFilesAreImages(aMediaFiles: TAiMediaFiles): Boolean;
var
  MF: TAiMediaFile;
begin
  Result := True;
  if aMediaFiles.Count = 0 then
    Exit(False); // No hay archivos, por lo que no "todos son imágenes"
  for MF in aMediaFiles do
  begin
    if MF.FileCategory <> TAiFileCategory.Tfc_Image then
      Exit(False); // Encontramos uno que no es imagen, fallamos
  end;
end;

{ TForm11 }

class function TForm11.VideoFolder: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'VeoDemo');
  ForceDirectories(Result);
end;

class function TForm11.HashOfStream(AStream: TStream): string;
begin
  AStream.Position := 0;
  Result := THashMD5.GetHashString(AStream);
  AStream.Position := 0;
end;

procedure TForm11.Log(const AText: string);
begin
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + AText);
  memoLog.GoToTextEnd;
end;

function TForm11.GetSelectedVeoMode: TVeoGenerationMode;
begin
  Result := gmAuto;
  if rbInter.IsChecked then
    Result := gmFrames;
  if rbReference.IsChecked then
    Result := gmReferences;
end;

procedure TForm11.BeginProgress(const ACaption: string);
begin
  FStartTick := TThread.GetTickCount64;
  ChatView1.BeginAssistantMessage;
  FProgressIdx := ChatView1.MessageCount - 1;
  ChatView1.AddStep(FProgressIdx, 'veo', ACaption);
end;

procedure TForm11.EndProgress(AOk: Boolean; const AText: string);
begin
  if (FProgressIdx < 0) or (FProgressIdx >= ChatView1.MessageCount) then
    Exit;
  ChatView1.FinishStep(FProgressIdx, 'veo', AOk, TThread.GetTickCount64 - FStartTick);
  ChatView1.SetMessageText(FProgressIdx, AText);
  ChatView1.FinishMessage(FProgressIdx);
  FProgressIdx := -1;
end;

procedure TForm11.AiVeoError(Sender: TObject; const ErrorMessage: string);
begin
  Log('ERROR: ' + ErrorMessage);
  EndProgress(False, 'No se pudo generar el video: ' + ErrorMessage);
  ChatInput1.Busy := False;
end;

procedure TForm11.AiVeoProgress(Sender: TObject; const StatusMessage: string);
begin
  Log(StatusMessage);
  if FProgressIdx >= 0 then
    ChatView1.SetMessageText(FProgressIdx, StatusMessage);
end;

procedure TForm11.AiVeoSuccess(Sender: TObject; ResultVideo: TAiMediaFile);
var
  LVideoPath, LMetaPath: string;
  LJson: TJSONObject;
  LShown: TAiMediaFiles;
  LFile: TAiMediaFile;
begin
  // El componente entrega el video y no lo vuelve a usar: es nuestro.
  try
    // 1. Guardar el .mp4 en Documentos\VeoDemo
    LVideoPath := TPath.Combine(VideoFolder, 'veo_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.mp4');
    ResultVideo.SaveToFile(LVideoPath);
    Log('Video guardado en: ' + LVideoPath);

    // 2. Guardar los metadatos (sin el base64) con el hash del video como
    //    nombre: es lo que permite extenderlo después (ver ExecuteVeoTask).
    LMetaPath := TPath.Combine(VideoFolder, HashOfStream(ResultVideo.Content) + '.veometa');
    LJson := ResultVideo.ToJsonObject;
    try
      LJson.RemovePair('base64').Free;
      TFile.WriteAllText(LMetaPath, LJson.ToJSON);
    finally
      LJson.Free;
    end;

    // 3. Mostrarlo en el chat como adjunto del asistente
    EndProgress(True, 'Video listo. Haz clic en el adjunto para reproducirlo.' + sLineBreak +
      '`' + LVideoPath + '`');
    LShown := TAiMediaFiles.Create;
    try
      LFile := TAiMediaFile.Create;
      LFile.LoadFromFile(LVideoPath);
      LShown.Add(LFile);
      ChatView1.AddAssistantMessage('', LShown);
    finally
      LShown.Free;
    end;
  finally
    ResultVideo.Free;
    ChatInput1.Busy := False;
  end;
end;

procedure TForm11.ChatView1AttachmentOpen(Sender: TObject; AMsg: TAIChatMessage; AAttach: TAIChatAttachment);
begin
  // Los adjuntos que envió el usuario eran copias temporales del input y ya
  // pueden no existir; los videos generados viven en Documentos\VeoDemo.
  if TFile.Exists(AAttach.LocalPath) then
  begin
    if not TUtilsSystem.ShellOpenFile(AAttach.LocalPath) then
      ShowMessage('No hay una aplicación para abrir ' + TPath.GetExtension(AAttach.LocalPath));
  end
  else
    ShowMessage('El archivo ya no está disponible: ' + AAttach.DisplayName);
end;

procedure TForm11.ExecuteVeoTask(const APrompt: string; aMediaFiles: TAiMediaFiles; aMode: TVeoGenerationMode = gmAuto);
var
  LVideoToExtend: TAiMediaFile;
  LMetaPath: string;
  LJsonValue: TJSONValue;
begin
  // CASO 1: Hay un solo archivo y es un VIDEO. La única opción es extenderlo.
  if (aMediaFiles.Count = 1) and (aMediaFiles[0].FileCategory = TAiFileCategory.Tfc_Video) then
  begin
    LMetaPath := TPath.Combine(VideoFolder, FVideoHash + '.veometa');
    if not TFile.Exists(LMetaPath) then
      raise Exception.Create('Este video no lo generó esta aplicación (no hay metadatos para él en ' +
        VideoFolder + '). Solo se pueden extender videos generados y guardados por el demo.');

    LVideoToExtend := TAiMediaFile.Create;
    try
      LJsonValue := TJSONObject.ParseJSONValue(TFile.ReadAllText(LMetaPath));
      try
        if not (LJsonValue is TJSONObject) then
          raise Exception.Create('El archivo de metadatos no es un objeto JSON válido.');
        LVideoToExtend.LoadFromJsonObject(TJSONObject(LJsonValue));
      finally
        LJsonValue.Free;
      end;
      if LVideoToExtend.UrlMedia.IsEmpty then
        raise Exception.Create('Los metadatos no traen el identificador "urlMedia" de Veo.');

      Log('Metadatos encontrados. Extendiendo el video...');
      AiVeo.ExtendVideo(APrompt, LVideoToExtend); // el componente trabaja sobre una copia
    finally
      LVideoToExtend.Free;
    end;
  end

  // CASO 2: Hay uno o más archivos y TODOS son IMÁGENES.
  else if (aMediaFiles.Count > 0) and AllMediaFilesAreImages(aMediaFiles) then
  begin
    case aMediaFiles.Count of
      1: // Con una sola imagen, la única opción es Imagen-a-Video
        begin
          Log('1 imagen: modo Imagen a Video...');
          AiVeo.GenerateFromImage(APrompt, aMediaFiles[0]);
        end;

      2: // Con dos imágenes, puede ser Interpolación o Referencias.
        if aMode = gmReferences then
        begin
          Log('2 imágenes: modo Imágenes de referencia (forzado)...');
          AiVeo.GenerateWithReferences(APrompt, aMediaFiles.ToMediaFileArray);
        end
        else
        begin
          Log('2 imágenes: modo Interpolación (primer y último fotograma)...');
          AiVeo.GenerateFromFrames(APrompt, aMediaFiles[0], aMediaFiles[1]);
        end;

      3: // Con tres imágenes solo cabe Referencias.
        begin
          if aMode = gmFrames then
            raise Exception.Create('La interpolación necesita exactamente 2 imágenes; llegaron 3.');
          Log('3 imágenes: modo Imágenes de referencia...');
          AiVeo.GenerateWithReferences(APrompt, aMediaFiles.ToMediaFileArray);
        end;
    else
      raise Exception.Create('Veo acepta como máximo 3 imágenes de referencia; llegaron ' +
        aMediaFiles.Count.ToString + '.');
    end;
  end

  // CASO 3: NO hay archivos multimedia. La única opción es Texto-a-Video.
  else if aMediaFiles.Count = 0 then
  begin
    if aMode <> gmAuto then
      raise Exception.Create('Los modos Interpolación y Referencias necesitan imágenes adjuntas.');
    Log('Sin adjuntos: modo Texto a Video...');
    AiVeo.GenerateFromText(APrompt);
  end

  // CASO 4: Combinación no válida (ej. imágenes y videos, o archivos de texto).
  else
    raise Exception.Create('Combinación de archivos no soportada: adjunta solo imágenes (1 a 3) o un solo video.');
end;

procedure TForm11.ChatInput1Cancel(Sender: TObject);
begin
  // La operación de Veo sigue en el servidor; aquí solo se libera la interfaz.
  EndProgress(False, 'Cancelado.');
  ChatInput1.Busy := False;
end;

procedure TForm11.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
begin
  memoLog.Lines.Clear;
  FProgressIdx := -1;

  AiVeo.Model := TVeoModel(cbModel.ItemIndex);
  AiVeo.AspectRatio := TVeoAspectRatio(cbAspectRatio.ItemIndex);
  AiVeo.Resolution := TVeoResolution(cbResolution.ItemIndex);
  AiVeo.PersonGeneration := TVeoPersonGeneration(cbPersonGeneration.ItemIndex);
  AiVeo.DurationSeconds := StrToIntDef(cbDuration.Text, 8);
  AiVeo.NegativePrompt := Trim(MemoNegativePrompt.Lines.Text);

  ChatView1.AddUserMessage(APrompt, aMediaFiles);

  // Para extender solo hace falta el hash del video (busca sus metadatos);
  // el contenido se descarta para no mandar megas a ningún lado.
  FVideoHash := '';
  if (aMediaFiles.Count = 1) and (aMediaFiles[0].FileCategory = TAiFileCategory.Tfc_Video) then
  begin
    FVideoHash := HashOfStream(aMediaFiles[0].Content);
    aMediaFiles[0].Content.Clear;
  end;

  try
    BeginProgress('Generando con Veo');
    ExecuteVeoTask(APrompt, aMediaFiles, GetSelectedVeoMode);
    MemoNegativePrompt.Lines.Clear;
  except
    on E: Exception do
    begin
      Log('ERROR: ' + E.Message);
      EndProgress(False, E.Message);
      ChatInput1.Busy := False;
    end;
  end;
end;

end.
