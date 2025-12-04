// IT License
//
// Copyright (c) <year> <copyright holders>
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

unit uVeoDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.JSON, System.Rtti,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Platform,

  uMakerAi.Gemini.Veo, uMakerAi.UI.ChatInput, FMX.Layouts, uMakerAi.UI.ChatList,
  uMakerAi.Core, uMakerAi.utils.System,

  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Menus;

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
    SaveDialog1: TSaveDialog;
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
    ChatList1: TChatList;
    Splitter2: TSplitter;
    Layout6: TLayout;
    ChatInput1: TChatInput;
    LayoutNegativePrompt: TLayout;
    Label7: TLabel;
    MemoNegativePrompt: TMemo;
    Label8: TLabel;
    PopupMenu1: TPopupMenu;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    MnuPlay: TMenuItem;
    AniIndicator1: TAniIndicator;
    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatInput1SlideChange(Sender: TObject; Data: TImageData);
    procedure AiVeoSuccess(Sender: TObject; ResultVideo: TAiMediaFile);
    procedure AiVeoProgress(Sender: TObject; const StatusMessage: string);
    procedure AiVeoError(Sender: TObject; const ErrorMessage: string);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure MnuCopyClick(Sender: TObject);
    Function CopyToClipBoard(AMediaFile: TAiMediaFile): Boolean;
    procedure MnuSaveClick(Sender: TObject);
    procedure MnuPlayClick(Sender: TObject);
  private
    procedure ExecuteVeoTask(const APrompt: string; aMediaFiles: TAiMediaFiles; aMode: TVeoGenerationMode = gmAuto);
    function AllFilesAreImages: Boolean;
    function GetSelectedVeoMode: TVeoGenerationMode;
    procedure UpdateModeSelectorUI;
  public
  end;

var
  Form11: TForm11;

implementation

{$R *.fmx}

function TForm11.GetSelectedVeoMode: TVeoGenerationMode;
begin
  // El ItemIndex del TRadioGroup se corresponde directamente con el orden de nuestro enum

  Result := gmAuto;

  if rbAuto.IsChecked then
    Result := gmAuto;
  if rbInter.IsChecked then
    Result := gmFrames;
  if rbReference.IsChecked then
    Result := gmReferences;
end;

procedure TForm11.MnuCopyClick(Sender: TObject);
var
  LMediaFile: TAiMediaFile;
begin
  LMediaFile := ChatList1.ActiveMediaFile;

  if (LMediaFile <> nil) then
  begin
    // Simplemente llamamos al método. ¡Toda la lógica compleja está encapsulada!
    if CopyToClipBoard(LMediaFile) then
    begin
      // Opcional: Notificar al usuario que la copia fue exitosa
      // ShowMessage('Contenido copiado al portapapeles.');
    end
    else
    begin
      // Opcional: Notificar si el tipo de archivo no es copiable
      showMessage('Este tipo de archivo no se puede copiar al portapapeles.');
    end;
  end;

end;

procedure TForm11.MnuPlayClick(Sender: TObject);
var
  LMediaFile: TAiMediaFile;
  LTempPath: string;
  LFileStream: TFileStream;
  AppService: IFMXApplicationService;
begin
  // 1. Obtener el archivo activo desde el TChatList
  LMediaFile := ChatList1.ActiveMediaFile;

  if LMediaFile = nil then
  begin
    showMessage('Error: No se ha seleccionado ningún archivo.');
    Exit;
  end;

  // 2. Verificar que el archivo tenga contenido para guardar
  if (LMediaFile.Content = nil) or (LMediaFile.Content.size = 0) then
  begin
    showMessage('El archivo seleccionado está vacío.');
    Exit;
  end;

  // 3. Construir la ruta del archivo temporal
  try
    LTempPath := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, LMediaFile.FileName);

    // 4. Guardar el contenido del stream en el archivo temporal
    LMediaFile.Content.Position := 0; // Siempre rebobinar antes de leer
    LFileStream := TFileStream.Create(LTempPath, fmCreate);
    try
      LFileStream.CopyFrom(LMediaFile.Content, LMediaFile.Content.size);
    finally
      LFileStream.Free;
    end;
    LMediaFile.Content.Position := 0; // Dejarlo listo para futuras lecturas

    // 5. Usar el servicio de la plataforma para abrir el archivo
    if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, IInterface(AppService)) then
    begin
      if not TUtilsSystem.ShellOpenFile(LTempPath) then
      begin
        showMessage('No se pudo abrir el archivo. Verifique si tiene una aplicación instalada para este tipo de archivo (' + System.IOUtils.TPath.GetExtension(LMediaFile.FileName) + ').');
      end;
    end
    else
    begin
      showMessage('El servicio para abrir archivos no está disponible en esta plataforma.');
    end;

  except
    on E: Exception do
    begin
      // Capturar cualquier error durante la creación del archivo
      showMessage('Ocurrió un error al intentar abrir el archivo: ' + E.Message);
    end;
  end;

end;

procedure TForm11.MnuSaveClick(Sender: TObject);
var
  LMediaFile: TAiMediaFile;
begin
  LMediaFile := ChatList1.ActiveMediaFile;

  if (LMediaFile <> nil) then
  begin
    SaveDialog1.FileName := LMediaFile.FileName;
    If SaveDialog1.Execute then
      LMediaFile.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm11.UpdateModeSelectorUI;
var
  attachments: TArray<TImageData>;
  attachmentCount: Integer;
  areAllImages: Boolean;
begin
  // 1. Obtener la lista de anexos y su cantidad.
  attachments := ChatInput1.GetAttachments;
  attachmentCount := Length(attachments);

  // 2. Determinar si todos los archivos en la lista son imágenes.
  // Llamamos a nuestra función refactorizada que no necesita parámetros.
  areAllImages := AllFilesAreImages;

  // 3. Aplicar la lógica de UI basada en la cantidad de archivos y si son imágenes.

  // Caso A: Hay exactamente 2 archivos y TODOS son imágenes.
  if (attachmentCount = 2) and areAllImages then
  begin
    // Con 2 imágenes, todas las opciones son potencialmente válidas.
    rbAuto.Enabled := True;
    rbInter.Enabled := True;
    rbReference.Enabled := True;
  end
  // Caso B: Hay exactamente 3 archivos y TODOS son imágenes.
  else if (attachmentCount = 3) and areAllImages then
  begin
    // Con 3 imágenes, la interpolación no es posible.
    // Habilitamos las opciones correspondientes y seleccionamos 'Auto' por defecto.
    rbAuto.Enabled := True;
    rbAuto.IsChecked := True; // Seleccionamos 'Auto'
    rbInter.Enabled := False; // Deshabilitamos 'Interpolar'
    rbReference.Enabled := True;
  end
  // Caso C: Cualquier otra situación (0, 1, 4+ archivos, o si hay archivos que no son imágenes).
  else
  begin
    // En cualquier otro caso, deshabilitamos todas las opciones especiales.
    // 'Auto' queda seleccionado por defecto pero deshabilitado.
    rbAuto.Enabled := False;
    rbAuto.IsChecked := True;
    rbInter.Enabled := False;
    rbReference.Enabled := False;
  end;
end;

// Helper function to check if all files in the list are images
procedure TForm11.AiVeoError(Sender: TObject; const ErrorMessage: string);
begin
  memoLog.Lines.Add(ErrorMessage);
  showMessage(ErrorMessage);
end;

procedure TForm11.AiVeoProgress(Sender: TObject; const StatusMessage: string);
begin
  ChatInput1.AddStatus(StatusMessage);
  memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': ' + StatusMessage);
  memoLog.GoToLineEnd;
end;

procedure TForm11.AiVeoSuccess(Sender: TObject; ResultVideo: TAiMediaFile);
var
  LSaveDialog: TSaveDialog;
  LBaseFileName, LMetaDataFileName: string;
  LJson: TJSONObject;
  MediaFiles: TAiMediaFiles;
begin

  ChatInput1.Busy := False;
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;

  MediaFiles := TAiMediaFiles.Create;
  MediaFiles.Add(ResultVideo);

  ChatList1.AddBubble('Result:', 'Model', MediaFiles, False);

  memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': Video downloaded successfully.');

  LSaveDialog := TSaveDialog.Create(Self);
  try
    LSaveDialog.Title := 'Save Generated Video';
    LSaveDialog.Filter := 'MPEG-4 Video (*.mp4)|*.mp4';
    LSaveDialog.DefaultExt := 'mp4';

    if LSaveDialog.Execute then
    begin
      LBaseFileName := LSaveDialog.FileName;

      // Guardar el archivo de video (.mp4)
      try
        ResultVideo.SaveToFile(LBaseFileName);
        memoLog.Lines.Add('Video file saved to: ' + LBaseFileName);
      except
        on E: Exception do
        begin
          showMessage('Failed to save video file: ' + E.Message);
          Exit;
        end;
      end;

      // Crear el nombre del archivo de metadatos
      LMetaDataFileName := TPath.ChangeExtension(LBaseFileName, '.veometa');

      // --- INICIO DE LA NUEVA LÓGICA ---
      // 1. Obtener el objeto JSON completo (incluyendo el Base64)
      LJson := ResultVideo.ToJsonObject;
      try
        // 2. Eliminar el par clave/valor "base64" del objeto JSON
        LJson.RemovePair('base64');
        memoLog.Lines.Add('Metadata prepared (Base64 content excluded).');

        // 3. Guardar el JSON modificado (ya sin el Base64) en el archivo
        TFile.WriteAllText(LMetaDataFileName, LJson.ToJSON);
        memoLog.Lines.Add('Metadata saved to: ' + LMetaDataFileName);
      finally
        LJson.Free; // Liberar el objeto JSON
      end;
      // --- FIN DE LA NUEVA LÓGICA ---
    end
    else
    begin
      memoLog.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ': Video generation successful, but file saving was cancelled by the user.');
    end;
  finally
    LSaveDialog.Free;
  end;
end;

function TForm11.AllFilesAreImages: Boolean;
var
  attachments: TArray<TImageData>;
  imageData: TImageData;
  fileExtension: string;
  contentType: TContentType;
begin
  // 1. Obtener la lista de todos los anexos del componente.
  attachments := ChatInput1.GetAttachments;

  // 2. Si la lista está vacía, no se cumple la condición "todos son imágenes".
  if Length(attachments) = 0 then
  begin
    Result := False;
    Exit;
  end;

  // 3. Asumimos que todos los archivos son imágenes hasta que se demuestre lo contrario.
  Result := True;

  // 4. Iterar sobre cada uno de los anexos.
  for imageData in attachments do
  begin
    // 5. Extraer la extensión del nombre del archivo de forma segura.
    fileExtension := TPath.GetExtension(imageData.FileName);

    // 6. LLAMADA CLAVE: Usar la función pública del componente para determinar el tipo de contenido
    // basado en la lógica interna del componente y su propiedad ValidExtensions.
    contentType := ChatInput1.GetContentTypeFromValidExtensions(fileExtension);

    // 7. Validar si el tipo de contenido retornado es de imagen.
    if contentType <> ctImage then
    begin
      // Si encontramos UN SOLO archivo que no es clasificado como imagen,
      // la condición general falla. Establecemos el resultado a False
      // y salimos del bucle y de la función inmediatamente.
      Result := False;
      Exit;
    end;
  end;

  // 8. Si el bucle se completa sin encontrar ningún archivo que no sea imagen,
  // la asunción inicial (Result := True) es correcta y se retorna.
end;

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

procedure TForm11.ExecuteVeoTask(const APrompt: string; aMediaFiles: TAiMediaFiles; aMode: TVeoGenerationMode = gmAuto);
var
  LVideoFileToExtend: TAiMediaFile;
  LMetaDataFileName: string;
  LJsonValue: TJSONValue;
  LJsonObj: TJSONObject;
  LErrorMsg: string;
begin
  // --- LÓGICA DE DECISIÓN MUTUAMENTE EXCLUYENTE ---

  // CASO 1: Hay un solo archivo y es un VIDEO. La única opción es extenderlo.
  if (aMediaFiles.Count = 1) and (aMediaFiles[0].FileCategory = TAiFileCategory.Tfc_Video) then
  begin
    memoLog.Lines.Add('Detected 1 video. Checking for metadata to extend...');

    // 1. Determinar la ruta del archivo de metadatos esperado (ej. 'mi_video.veometa')
    LMetaDataFileName := TPath.ChangeExtension(aMediaFiles[0].FullFileName, '.veometa');

    // 2. Verificar si el archivo de metadatos existe.
    if not TFile.Exists(LMetaDataFileName) then
    begin
      LErrorMsg := 'Error: Metadata file (' + TPath.GetFileName(LMetaDataFileName) + ') not found.' + sLineBreak + sLineBreak + 'This video cannot be extended because its original VEO identifier is missing. ' +
        'Only videos previously generated and saved by this application can be extended.';
      memoLog.Lines.Add(LErrorMsg);
      showMessage(LErrorMsg);
      Exit; // Salir de la función, no se puede continuar.
    end;

    // 3. Si existe, proceder a cargarlo y usarlo.
    LVideoFileToExtend := TAiMediaFile.Create;
    try
      try
        // 3.1. Leer el contenido del archivo de texto.
        var
        LJsonText := TFile.ReadAllText(LMetaDataFileName);

        // 3.2. Parsear el texto a un objeto JSON.
        LJsonValue := TJSONObject.ParseJSONValue(LJsonText);

        // 3.3. Asegurarse de que el parseo resultó en un objeto y no en otro tipo de valor JSON.
        if not Assigned(LJsonValue) or not(LJsonValue is TJSONObject) then
          raise Exception.Create('Metadata file is not a valid JSON object.');

        LJsonObj := LJsonValue as TJSONObject;

        // 3.4. Usar el método existente para cargar toda la data del JSON en nuestro objeto.
        LVideoFileToExtend.LoadFromJsonObject(LJsonObj);

        // 3.5. Una validación final para asegurarnos de que el dato más importante (UrlMedia) está presente.
        if LVideoFileToExtend.UrlMedia.IsEmpty then
          raise Exception.Create('Metadata file does not contain the required "urlMedia" identifier.');

      except
        on E: Exception do
        begin
          LErrorMsg := 'Error processing metadata file: ' + E.Message;
          memoLog.Lines.Add(LErrorMsg);
          showMessage(LErrorMsg);
          Exit; // Salir si hay un error con los metadatos.
        end;
      end;

      // 4. Si todo ha ido bien, LVideoFileToExtend ahora es un objeto válido.
      memoLog.Lines.Add('Metadata loaded. Initiating video extension...');
      AiVeo.ExtendVideo(APrompt, LVideoFileToExtend);

    finally
      LVideoFileToExtend.Free; // El objeto solo vive durante esta llamada.
    end;
  end

  // CASO 2: Hay uno o más archivos y TODOS son IMÁGENES.
  else if (aMediaFiles.Count > 0) and AllMediaFilesAreImages(aMediaFiles) then
  begin
    // Ahora decidimos qué hacer con estas imágenes
    case aMediaFiles.Count of
      1: // Con una sola imagen, la única opción es Imagen-a-Video
        begin
          memoLog.Lines.Add('Detected 1 image. Starting mode: Image to Video...');
          AiVeo.GenerateFromImage(APrompt, aMediaFiles[0]);
        end;

      2: // Con dos imágenes, puede ser Interpolación o Referencias.
        begin
          // Usamos el parámetro aMode para decidir. Por defecto (gmAuto), asumimos Interpolación.
          if (aMode = gmAuto) or (aMode = gmFrames) then
          begin
            memoLog.Lines.Add('Detected 2 images. Starting mode: Interpolation (Frames)...');
            AiVeo.GenerateFromFrames(APrompt, aMediaFiles[0], aMediaFiles[1]);
          end
          else // aMode = gmReferences
          begin
            memoLog.Lines.Add('Detected 2 images. Forcing mode: Reference Images...');
            AiVeo.GenerateWithReferences(APrompt, aMediaFiles.ToMediaFileArray);
          end;
        end;

      3: // Con tres imágenes, puede ser Interpolación (si se fuerza) o Referencias.
        begin
          // Por defecto (gmAuto), asumimos Referencias, que es el caso de uso más común para 3 imágenes.
          if (aMode = gmAuto) or (aMode = gmReferences) then
          begin
            memoLog.Lines.Add('Detected 3 images. Starting mode: Reference Images...');
            AiVeo.GenerateWithReferences(APrompt, aMediaFiles.ToMediaFileArray);
          end
          else // aMode = gmFrames
          begin
            // El usuario está forzando un modo que no es compatible con 3 imágenes
            raise Exception.Create('Interpolation (Frames) mode only supports exactly 2 images. 3 were provided.');
          end;
        end
    else
      // Si hay más de 3 imágenes, la única opción es Referencias, pero la API tiene un límite.
      if aMediaFiles.Count > 3 then
      begin
        raise Exception.Create('VEO only supports a maximum of 3 reference images. ' + aMediaFiles.Count.ToString + ' were provided.');
      end;
    end;
  end

  // CASO 3: NO hay archivos multimedia. La única opción es Texto-a-Video.
  else if (aMediaFiles.Count = 0) then
  begin
    memoLog.Lines.Add('No files detected. Starting mode: Text to Video...');
    AiVeo.GenerateFromText(APrompt);
  end

  // CASO 4: Combinación no válida (ej. imágenes y videos, o archivos de texto).
  else
  begin
    LErrorMsg := 'Unsupported file combination for video generation. Please provide only images or a single video.';
    memoLog.Lines.Add('ERROR: ' + LErrorMsg);
    // Opcional: raise Exception.Create(LErrorMsg);
  end;
end;

procedure TForm11.ChatInput1Cancel(Sender: TObject);
begin
  ChatInput1.Busy := False;
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;
end;

procedure TForm11.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
Var
  Model: TVeoModel; // = (vmCustom, vmVeo3_1, vmVeo3_1_Fast, vmVeo3_0, vmVeo3_0_Fast, vmVeo2_0);
  Ratio: TVeoAspectRatio; // = (arDefault, ar16x9, ar9x16);
  Resolution: TVeoResolution; // = (vrDefault, vr720p, vr1080p);
  Person: TVeoPersonGeneration; // = (pgDefault, pgAllowAll, pgAllowAdult, pgDontAllow);
  Duration: Integer;
  NegativePrompt: String;

begin

  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
  memoLog.Lines.Clear;

  Model := TVeoModel(cbModel.ItemIndex); // = (vmCustom, vmVeo3_1, vmVeo3_1_Fast, vmVeo3_0, vmVeo3_0_Fast, vmVeo2_0);
  Ratio := TVeoAspectRatio(cbAspectRatio.ItemIndex); // = (arDefault, ar16x9, ar9x16);
  Resolution := TVeoResolution(cbResolution.ItemIndex); // = (vrDefault, vr720p, vr1080p);
  Person := TVeoPersonGeneration(cbPersonGeneration.ItemIndex); // = (pgDefault, pgAllowAll, pgAllowAdult, pgDontAllow);
  Duration := StrToIntDef(cbDuration.Text, 8);
  NegativePrompt := Trim(MemoNegativePrompt.Lines.Text);

  AiVeo.Model := Model;
  AiVeo.AspectRatio := Ratio;
  AiVeo.Resolution := Resolution;
  AiVeo.PersonGeneration := Person;
  AiVeo.DurationSeconds := Duration;
  AiVeo.NegativePrompt := NegativePrompt;

  if (aMediaFiles.Count = 1) and (aMediaFiles[0].FileCategory = TAiFileCategory.Tfc_Video) then
  begin
    // El TAiMediaFile todavía tiene el FullFileName, que es lo que necesitamos.
    // Simplemente vaciamos el TMemoryStream para liberar RAM.
    aMediaFiles[0].Content.Clear;
    memoLog.Lines.Add('[Optimization] Video content cleared from memory for extension task.');
  end;

  try
    ChatList1.AddBubble(APrompt, 'user', aMediaFiles, True);

    ExecuteVeoTask(APrompt, aMediaFiles, GetSelectedVeoMode);

    MemoNegativePrompt.Lines.Clear;

  except
    on E: Exception do
      showMessage('Error: ' + E.Message);
  end;

end;

procedure TForm11.ChatInput1SlideChange(Sender: TObject; Data: TImageData);
Begin
  UpdateModeSelectorUI;
end;

function TForm11.CopyToClipBoard(AMediaFile: TAiMediaFile): Boolean;
var
  ClipboardSvc: IFMXClipboardService;
  LBitmap: FMX.Graphics.TBitmap;
  LText: string;
begin
  Result := False;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipboardSvc)) then
    Exit; // No hay servicio de portapapeles disponible

  // Antes de cualquier operación, asegurarnos de que el stream esté al principio.
  if Assigned(AMediaFile.Content) then
    AMediaFile.Content.Position := 0;

  // La lógica de copiado depende de la categoría del archivo
  case AMediaFile.FileCategory of
    Tfc_Image:
      begin
        if (AMediaFile.Content = nil) or (AMediaFile.Content.size = 0) then
          Exit;
        LBitmap := FMX.Graphics.TBitmap.Create;
        try
          LBitmap.LoadFromStream(AMediaFile.Content);
          ClipboardSvc.SetClipboard(LBitmap); // FMX sabe cómo poner un TBitmap en el portapapeles
          Result := True;
        finally
          LBitmap.Free;
        end;
      end;

    Tfc_Text, tfc_ExtracttextFile:
      begin
        if (AMediaFile.Content = nil) or (AMediaFile.Content.size = 0) then
          Exit;

        // Leemos el contenido del stream como texto UTF-8 (el más común)
        var
        LReader := TStreamReader.Create(AMediaFile.Content, TEncoding.UTF8);
        try
          LText := LReader.ReadToEnd;
          // Usamos TValue.From<string> para pasarlo al portapapeles como texto plano
          ClipboardSvc.SetClipboard(TValue.From<string>(LText));
          Result := True;
        finally
          LReader.Free;
        end;
      end;

    // Para otros tipos de archivo (PDF, DOC, Audio, etc.), el portapapeles estándar
    // no tiene un formato "nativo" para ellos. La mejor opción es copiar
    // la RUTA del archivo si existe, o no hacer nada.
    // En este caso, como el contenido está en un TMemoryStream, no hay una ruta
    // que podamos copiar que otra aplicación pueda entender.
    // Por lo tanto, para estos tipos, no hacemos nada y el método devuelve False.
  else
    Result := False;
  end;

  // Volvemos a rebobinar el stream por si se necesita reutilizar después.
  if Assigned(AMediaFile.Content) then
    AMediaFile.Content.Position := 0;

end;

end.
