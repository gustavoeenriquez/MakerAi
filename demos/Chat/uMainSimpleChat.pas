unit uMainSimpleChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.IOUtils, System.Net.HttpClient, System.NetEncoding,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.Layouts, REST.Json, System.Json, FMX.ListBox, FMX.TabControl,
  FMX.Edit, FMX.Platform, FMX.ClipBoard, FMX.Surfaces, FMX.ComboEdit,
  uAiOpenChat, uOpenAi, FMX.Menus, uAiToolFunctions,
  uAiOpenAssistant, uAiAnthropic, uAiGroq, uAiOllama;

type
  TImageData = Class
    FileName: String;
    Checked: TCheckBox;
    Item: TListBoxItem;
    BitMap: TBitMap;
  End;

  TForm64 = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Rectangle1: TRectangle;
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    MemoChat: TMemo;
    Label1: TLabel;
    OpenChat: TAiOpenChat;
    LayChat: TLayout;
    Layout4: TLayout;
    Splitter1: TSplitter;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    LblTotaltokens: TLabel;
    EditTotalTokens: TEdit;
    ListBoxItem2: TListBoxItem;
    LblPrompTokens: TLabel;
    EditPromptTokens: TEdit;
    ListBoxItem3: TListBoxItem;
    LblCompletionTokens: TLabel;
    EditCompletionTokens: TEdit;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem5: TListBoxItem;
    ChAsincrono: TCheckBox;
    ListBoxItem6: TListBoxItem;
    Label7: TLabel;
    ComboModels: TComboEdit;
    ListBoxItem4: TListBoxItem;
    Label5: TLabel;
    EditMaxTokens: TEdit;
    ListBoxItem7: TListBoxItem;
    Label2: TLabel;
    EditSeed: TEdit;
    ListBoxItem8: TListBoxItem;
    Label3: TLabel;
    TrackTopP: TTrackBar;
    ListBoxItem9: TListBoxItem;
    Label4: TLabel;
    TrackTemperature: TTrackBar;
    Splitter2: TSplitter;
    ListBoxItem10: TListBoxItem;
    Label6: TLabel;
    EditUrl: TComboEdit;
    ListBoxItem11: TListBoxItem;
    ChJSonFormat: TCheckBox;
    Layout5: TLayout;
    SpeedButton1: TSpeedButton;
    Image1: TImage;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    SpeedButton2: TSpeedButton;
    Image2: TImage;
    SpeedButton6: TSpeedButton;
    Image6: TImage;
    ListBoxItem12: TListBoxItem;
    ChUseTools: TCheckBox;
    OllamaChat: TAiOllamaChat;
    GroqChat: TAiGroqChat;
    ClaudChat: TAiClaudeChat;
    Label8: TLabel;
    ComboEngines: TComboEdit;
    Layout6: TLayout;
    ListImages: TListBox;
    Layout7: TLayout;
    ListaUsados: TListBox;
    Label9: TLabel;
    Layout8: TLayout;
    SpeedButton3: TSpeedButton;
    Image3: TImage;
    SpeedButton4: TSpeedButton;
    Image4: TImage;
    SpeedButton5: TSpeedButton;
    Image5: TImage;
    OpenDialogImage: TOpenDialog;
    SpeedButton7: TSpeedButton;
    Image7: TImage;
    Label10: TLabel;
    AiVision1: TAiVision;
    procedure AiOpenChat1ReceiveData(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
    procedure BtnPlayClick(Sender: TObject);
    procedure AiOpenChat1ReceiveDataEnd(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
    procedure ChAsincronoChange(Sender: TObject);
    procedure EditMaxTokensTyping(Sender: TObject);
    procedure ComboModelsChange(Sender: TObject);
    procedure TrackTopPTracking(Sender: TObject);
    procedure TrackTemperatureTracking(Sender: TObject);
    procedure EditSeedTyping(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditUrlChange(Sender: TObject);
    procedure ChJSonFormatChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure AiOpenChat1Functions0GetFechaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AiOpenChat1Functions2GetClimaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure ChUseToolsChange(Sender: TObject);
    procedure GroqChatFunctions0GetFechaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure ComboEnginesChange(Sender: TObject);
    procedure ListImagesDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure ListImagesDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure ClaudChatProcessMediaFile(const Sender: TObject; Prompt: string;
      MediaFile: TAiMediaFile; var Respuesta: string; var aProcesado: Boolean);
  private
    Procedure UpdateMemo(Text: String);
    Procedure InitData;
    Procedure AddMsgImages(Msg: TAiOpenChatMessage);
    Function DownLoadFromUrl(sUrl: String): TMemoryStream;
    Procedure AddImageToSlide(FileName: String; BitMap: TBitMap);
    Procedure CapturarPantalla(BitMap: TBitMap);
  public
    GlChat: TAiOpenChat;
  end;

var
  Form64: TForm64;

implementation

{$R *.fmx}

function ScaleRectangle(MaxSize: Double; X, Y: Double; var ScaledX, ScaledY: Double): Boolean;
var
  AspectRatio: Double;
begin

  // Inicialmente, los resultados son iguales a los valores originales
  ScaledX := X;
  ScaledY := Y;

  // Si ambos lados son menores o iguales a 500, no se necesita escalar
  if (X <= MaxSize) and (Y <= MaxSize) then
  begin
    ScaledX := X;
    ScaledY := Y;
    Result := False; // No se ha escalado
    Exit;
  end;

  // Calcular la relación de aspecto
  AspectRatio := X / Y;

  if X > Y then
  begin
    // Escalar por el ancho
    ScaledX := MaxSize;
    ScaledY := MaxSize / AspectRatio;
  end
  else
  begin
    // Escalar por el alto
    ScaledY := MaxSize;
    ScaledX := MaxSize * AspectRatio;
  end;

  Result := True; // Se ha escalado
end;

procedure TForm64.AddMsgImages(Msg: TAiOpenChatMessage);
Var
  I: Integer;
  ImageData: TImageData;
  Item: TListBoxItem;
  St: TMemoryStream;
begin
  For I := 0 to ListImages.Count - 1 do
  Begin
    Item := ListImages.ListItems[I];
    ImageData := TImageData(Item.TagObject);
    If ImageData.Checked.IsChecked then
    Begin
      St := TMemoryStream.Create;
      ImageData.BitMap.SaveToStream(St);
      St.Position := 0;
      Msg.LoadMediaFromStream(ImageData.FileName, St);
    End;
  End;

  For I := ListImages.Count - 1 downto 0 do
  Begin
    Item := ListImages.ListItems[I];
    ImageData := TImageData(Item.TagObject);
    If ImageData.Checked.IsChecked then
    Begin
      ListaUsados.AddObject(ImageData.Item);
      ImageData.Checked.IsChecked := False;
    End;
  End;
end;

procedure TForm64.AiOpenChat1Functions0GetFechaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := FormatDateTime('DD/MM/YYYY hh:nn:ss', Now);
  Handled := True;
end;

procedure TForm64.AiOpenChat1Functions2GetClimaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := '23 grados';
end;

procedure TForm64.AiOpenChat1ReceiveData(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
begin
  UpdateMemo(Text);
end;

procedure TForm64.AiOpenChat1ReceiveDataEnd(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
begin

  // Si no es asincrono se puede obtener el resultado aquí
  // o directamente en el llamado de la consulta en el botón de BtnPlay  Res := AiOpenChat1.AddMessageAndRun(MemoPrompt.Text, 'user');
  // If AiOpenChat1.Asynchronous = False then
  // UpdateMemo(Text);

  EditTotalTokens.Text := FormatFloat('###,##0', GlChat.Total_tokens);
  EditPromptTokens.Text := FormatFloat('###,##0', GlChat.Prompt_tokens);
  EditCompletionTokens.Text := FormatFloat('###,##0', GlChat.Completion_tokens);

  If GlChat.Asynchronous then
    UpdateMemo('');

  MemoChat.Lines.Add('');

  BtnPlay.StyleLookup := 'playtoolbutton';
  BtnPlay.ApplyStyleLookup;

end;

procedure TForm64.BtnPlayClick(Sender: TObject);
Var
  Res: String;
  Msg: TAiOpenChatMessage;
  ListaImagenes: TStringList;
begin
  If GlChat.Busy and GlChat.Asynchronous then
    GlChat.Abort
  Else
  Begin
    // Si está en modo asincrono el resultado lo obtiene en el evento ondatarecieve
    // En el método OnDataRecieveEnd se obtiene el resultado final tanto si es Asincrono o no.
    UpdateMemo('User : ' + MemoPrompt.Text);
    MemoChat.Lines.Add('');

    If GlChat.Asynchronous = True then
    Begin
      BtnPlay.StyleLookup := 'pausetoolbutton';

      Msg := GlChat.NewMessage(MemoPrompt.Text, 'user');
      AddMsgImages(Msg);

      Res := GlChat.Run(Msg);


      // Res := GlChat.AddMessageAndRun(MemoPrompt.Text, 'user', '', '', Lista);

      If (GlChat.Tool_Active) then
        UpdateMemo('Asistant : ' + Res);

    End
    Else
    // Si NO está en modo asincrono el resultado lo obtiene directamente en la funcion
    // o también lo puede obtener en el evento ondatarecieveend
    Begin
      Cursor := crHourGlass;
      Try
        BtnPlay.StyleLookup := 'pausetoolbutton';

        Msg := GlChat.NewMessage(MemoPrompt.Text, 'user');

        // If EditImagen.Text <> '' then
        // Msg.VisionUrls.Add(EditImagen.Text);

        { //esta es la forma de adiconar imágenes ya sea url o base64
          Msg.VisionUrls.Add('https://static.miscota.com/media/1/photos/products/114332/rascador-para-gatos-con-casita_1_g.jpeg');

          Var
          ST: TMemoryStream := TMemoryStream.Create;
          ST.LoadFromFile('c:\temp\gato.jpg');
          ST.Position := 0;
          Msg.AddStreamImage(ST);
        }

        AddMsgImages(Msg);

        Res := GlChat.Run(Msg);

        { Var
          Lista: TStringList := TStringList.Create;

          If EditImagen.Text <> '' then
          Lista.Add(EditImagen.Text);
        }

        // Res := AiOpenChat1.AddMessageAndRun(MemoPrompt.Text, 'user', '', '', Lista);
        UpdateMemo('Asistant : ' + Res);
        MemoChat.Lines.Add('');
      Finally
        Cursor := crDefault;
        BtnPlay.StyleLookup := 'playtoolbutton';
      End;
    End;
    MemoPrompt.Lines.Clear;
  End;
end;

procedure TForm64.Button1Click(Sender: TObject);
Var
  Item: TListBoxItem;
begin
  Item := ListImages.ListItems[0];
  ListaUsados.AddObject(Item);

end;

procedure TForm64.CapturarPantalla(BitMap: TBitMap);
begin
  ShowMessage('Falta por implementar la captura de pantalla o una sección de la pantalla');
end;

procedure TForm64.ChAsincronoChange(Sender: TObject);
begin
  GlChat.Asynchronous := ChAsincrono.IsChecked;

  // El conteo de tokens solo funciona cuando no es asincrónico
  LblCompletionTokens.Enabled := Not GlChat.Asynchronous;
  LblPrompTokens.Enabled := Not GlChat.Asynchronous;
  LblTotaltokens.Enabled := Not GlChat.Asynchronous;
end;

procedure TForm64.ChJSonFormatChange(Sender: TObject);
begin

  If ChJSonFormat.IsChecked then
  Begin
    GlChat.Response_format := TAiOpenChatResponseFormat.tiaChatRfJson;
    GlChat.AddMessageAndRun('a partir de ahora las respuestas serán en formato json', 'system',[]);
  End
  Else
  Begin
    GlChat.Response_format := TAiOpenChatResponseFormat.tiaChatRfText;
    GlChat.AddMessageAndRun('a partir de ahora las respuestas serán en formato de texto libre', 'system',[]);
  End;
end;

procedure TForm64.ChUseToolsChange(Sender: TObject);
begin
  GlChat.Tool_Active := ChUseTools.IsChecked;
end;

procedure TForm64.ClaudChatProcessMediaFile(const Sender: TObject;
  Prompt: string; MediaFile: TAiMediaFile; var Respuesta: string;
  var aProcesado: Boolean);
begin
   Respuesta := 'La imágen anexa se reemplaza por la descripción que te daré a continuación, describe a partir de esta descripcion y no hagas comentarios sobre si la imagen existe o no:'+sLineBreak+'La imagen anexa muestra un globo de helio rojo en un fondo blanco';
   aProcesado := True;
end;

procedure TForm64.ComboEnginesChange(Sender: TObject);
begin

  If ComboEngines.Text = 'OpenAi' then
    GlChat := OpenChat;

  If ComboEngines.Text = 'Anthropic' then
    GlChat := ClaudChat;

  If ComboEngines.Text = 'Groq' then
    GlChat := GroqChat;

  If ComboEngines.Text = 'Ollama' then
    GlChat := OllamaChat;

  InitData;
end;

procedure TForm64.ComboModelsChange(Sender: TObject);
begin
  GlChat.Model := ComboModels.Text;
end;

function TForm64.DownLoadFromUrl(sUrl: String): TMemoryStream;
Var
  Client: THTTPClient;
  Res: IHTTPResponse;
  Response: TStringStream;
begin

  Result := TMemoryStream.Create;
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);

  Try
    Client.ContentType := 'application/octet-stream';

    Res := Client.Get(sUrl, Response);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result.LoadFromStream(Response);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

procedure TForm64.EditMaxTokensTyping(Sender: TObject);
begin
  GlChat.Max_Tokens := StrToIntDef(EditMaxTokens.Text, 300);
end;

procedure TForm64.EditSeedTyping(Sender: TObject);
begin
  GlChat.Seed := StrToIntDef(EditSeed.Text, 0);
end;

procedure TForm64.EditUrlChange(Sender: TObject);
begin
  GlChat.Url := EditUrl.Text;
end;

procedure TForm64.FormCreate(Sender: TObject);
Var
  St: TStringStream;
  jVal: TJSonValue;
  JArr: TJSonArray;
  Engine, ApiKey, Url, FileName: String;
begin
  LayChat.Enabled := False;
  ListImages.AllowDrag := True;

  FileName := ExtractFilePath(ParamStr(0)) + 'config.ini';

  If Not TFile.Exists(FileName) then
    Raise Exception.Create('el archivo "' + FileName + '" no se encuentra');

  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    St.LoadFromFile(FileName);

    JArr := TJSonArray(TJSONObject.ParseJSONValue(St.DataString));
    ComboEngines.Items.Clear;

    For jVal in JArr do
    Begin
      Engine := jVal.GetValue<String>('engine');
      ApiKey := jVal.GetValue<String>('apikey');
      Url := jVal.GetValue<String>('url');

      if Engine = 'openai' then
      Begin
        OpenChat.ApiKey := ApiKey;
        OpenChat.Url := Url;
        ComboEngines.Items.Add('OpenAi');
      End;

      if Engine = 'anthropic' then
      Begin
        ClaudChat.ApiKey := ApiKey;
        ClaudChat.Url := Url;
        ComboEngines.Items.Add('Anthropic');
      End;

      if Engine = 'groq' then
      Begin
        GroqChat.ApiKey := ApiKey;
        GroqChat.Url := Url;
        ComboEngines.Items.Add('Groq');
      End;

      if Engine = 'ollama' then
      Begin
        OllamaChat.ApiKey := ApiKey;
        OllamaChat.Url := Url;
        ComboEngines.Items.Add('Ollama');
      End;
    End;
    If ComboEngines.Count > 0 then
    Begin
      ComboEngines.ItemIndex := 0;
      ComboEngines.OnChange(Nil);
    End;
  Finally
    St.Free;
  End;
end;

procedure TForm64.GroqChatFunctions0GetFechaAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
  Handled := True;

end;

procedure TForm64.InitData;
Var
  Res, JObj: TJSONObject;
  Val: TJSonValue;
  Data: TJSonArray;
  S: String;
begin

  ComboModels.Items.Text := GlChat.GetModels.Text;
  ComboModels.ItemIndex := 0;
  ChAsincrono.IsChecked := GlChat.Asynchronous;
  ChUseTools.IsChecked := GlChat.Tool_Active;
  EditMaxTokens.Text := GlChat.Max_Tokens.ToString;
  // ComboModels.ItemIndex := ComboModels.Items.IndexOf(AiOpenChat1.Model);
  TrackTemperature.Value := GlChat.Temperature * 10;
  TrackTopP.Value := GlChat.Top_p * 10;
  EditSeed.Text := GlChat.Seed.ToString;
  LayChat.Enabled := True;
  ChJSonFormat.IsChecked := GlChat.Response_format = TAiOpenChatResponseFormat.tiaChatRfJson;
  EditUrl.Text := GlChat.Url;
end;

procedure TForm64.AddImageToSlide(FileName: String; BitMap: TBitMap);
Var
  X, Y, X1, Y1: Double;
  Image: TImage;
  Check: TCheckBox;
  Item: TListBoxItem;
  ImageData: TImageData;
begin
  X := BitMap.Width;
  Y := BitMap.Height;
  ScaleRectangle(150, X, Y, X1, Y1);

  Image := TImage.Create(Self);
  Image.BitMap := BitMap.CreateThumbnail(Trunc(X1), Trunc(Y1));
  Image.Align := TAlignLayout.Client;
  Image.WrapMode := TImageWrapMode.Fit;
  Image.Margins := TBounds.Create(TRectf.Create(3, 3, 3, 3));
  Image.HitTest := False;
  Image.Locked := True;
  Check := TCheckBox.Create(Self);
  Check.Align := TAlignLayout.Bottom;
  Check.Text := ExtractFileName(FileName);
  Check.IsChecked := True;

  Item := TListBoxItem.Create(Self);
  Item.AddObject(Image);
  Item.AddObject(Check);
  Item.Width := ListImages.Height;
  Item.Height := ListImages.Height;

  ImageData := TImageData.Create;
  ImageData.FileName := FileName;
  ImageData.Checked := Check;
  ImageData.Item := Item;
  ImageData.BitMap := BitMap;

  Item.TagObject := ImageData;

  ListImages.AddObject(Item);
end;

procedure TForm64.ListImagesDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
Var
  FileName, Ext: String;
  I: Integer;
  BitMap: TBitMap;
  St: TMemoryStream;
begin

  If Length(Data.Files) > 0 then
  Begin
    For I := 0 to Length(Data.Files) - 1 do
    Begin
      FileName := Data.Files[I];
      Ext := ExtractFileExt(LowerCase(FileName));
      If (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp') then
      Begin
        If TFile.Exists(FileName) then
        Begin
          BitMap := TBitMap.Create;
          BitMap.LoadFromFile(FileName);

          AddImageToSlide(FileName, BitMap);
        End;
      End;
    End;
  End
  Else
  Begin
    FileName := Data.Data.AsString;
    If (Copy(FileName, 1, 7) = 'http://') or (Copy(FileName, 1, 8) = 'https://') then
    Begin
      Ext := Copy(FileName, Length(FileName) - 3, 4);
      If (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp') then
      Begin
        BitMap := TBitMap.Create;
        St := DownLoadFromUrl(FileName);
        BitMap.LoadFromStream(St);
        AddImageToSlide(FileName, BitMap);
      End;
    End;
  End;

end;

procedure TForm64.ListImagesDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
Var
  Ext: String;
  Kind: TTypeKind;
  S: String;
begin
  Operation := TDragOperation.None;

  // Si son archivos
  If (Length(Data.Files) > 0) then
  Begin
    Ext := ExtractFileExt(LowerCase(Data.Files[0]));
    If (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp') then
      Operation := TDragOperation.Copy;
  End
  Else
  Begin
    Kind := Data.Data.Kind;
    S := Data.Data.AsString;
    If (Copy(S, 1, 7) = 'http://') or (Copy(S, 1, 8) = 'https://') then
    Begin
      Ext := Copy(S, Length(S) - 3, 4);
      If (Ext = '.jpg') or (Ext = '.jpeg') or (Ext = '.png') or (Ext = '.bmp') then
      Begin
        Operation := TDragOperation.Move;
      End;
    End;
  End;
end;

procedure TForm64.SpeedButton1Click(Sender: TObject);
begin
  If SaveDialog1.Execute then
    GlChat.Messages.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm64.SpeedButton2Click(Sender: TObject);
begin
  If OpenDialog1.Execute then
    GlChat.Messages.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm64.SpeedButton3Click(Sender: TObject);
var
  ClipboardService: IFMXClipboardService;
  BitMap: TBitMap;
  Kind: TTypeKind;
  Sur: TBitMapSurface;
  S: String;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipboardService)) then
  begin

    if ClipboardService.GetClipboard.Kind = tkClass then
    begin

      if ClipboardService.GetClipboard.AsObject is TBitMapSurface then
      begin
        // Convierte el contenido del portapapeles a un TBitmap
        Sur := TBitMapSurface(ClipboardService.GetClipboard.AsObject);

        BitMap := TBitMap.Create;
        BitMap.SetSize(Sur.Width, Sur.Height);
        BitMap.Assign(Sur);
        AddImageToSlide('', BitMap);
      end
    end
    else
    begin
      ShowMessage('El portapapeles no contiene una imagen.');
    end;

  end
  else
  begin
    ShowMessage('El servicio de portapapeles no está disponible.');
  end;
end;

procedure TForm64.SpeedButton4Click(Sender: TObject);
Var
  BitMap: TBitMap;
begin
  If OpenDialogImage.Execute then
  Begin
    BitMap := TBitMap.Create;
    BitMap.LoadFromFile(OpenDialogImage.FileName);
    AddImageToSlide(ExtractFileName(OpenDialogImage.FileName), BitMap);
  End;
end;

procedure TForm64.SpeedButton5Click(Sender: TObject);
Var
  I: Integer;
  ImageData: TImageData;
  Item: TListBoxItem;
begin
  For I := ListImages.Count - 1 downto 0 do
  Begin
    Item := ListImages.ListItems[I];
    ImageData := TImageData(Item.TagObject);
    If ImageData.Checked.IsChecked then
    Begin
      ListaUsados.AddObject(ImageData.Item);
      ImageData.Checked.IsChecked := False;
    End;
  End;
end;

procedure TForm64.SpeedButton6Click(Sender: TObject);
begin
  GlChat.Messages.Clear;
end;

procedure TForm64.TrackTemperatureTracking(Sender: TObject);
begin
  GlChat.Top_p := TrackTopP.Value / 10;
end;

procedure TForm64.TrackTopPTracking(Sender: TObject);
begin
  GlChat.Temperature := TrackTemperature.Value / 10;
end;

procedure TForm64.UpdateMemo(Text: String);
begin
  MemoChat.BeginUpdate;
  Try
    MemoChat.Lines.Text := MemoChat.Lines.Text + Text;
    MemoChat.SelStart := Length(MemoChat.Text);
  Finally
    MemoChat.EndUpdate;
  End;
end;

end.
