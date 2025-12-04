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

unit uMainChatFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.Rtti, System.JSON, System.Net.HttpClient, System.Threading, System.ZLib,
  System.StrUtils, System.TypInfo, System.Math, System.Generics.Collections,
  System.Net.HttpClientComponent,

  uMakerAi.UI.ChatBubble, uMakerAi.UI.ChatInput, uMakerAi.Core, uMakerAi.Chat,
  uMakerAi.Utils.System, uMakerAi.UI.ChatList, uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi, uMakerAi.Chat.Initializations,
  uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Claude, uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Kimi, uMakerAi.Chat.LMStudio, uMakerAi.OpenAi.Sora,
  uMakerAi.Chat.GenericLLM, uMakerAi.Utils.ScreenCapture,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Layouts,
  FMX.Styles.Objects, FMX.Platform, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Menus, FMX.ListBox,

  uMakerAi.Chat.Mistral, uMakerAi.Chat.Ollama, uMakerAi.Chat.Groq, uMakerAi.Chat.Grok, uMakerAi.Whisper,
  uMakerAi.Utils.VoiceMonitor, System.Actions, FMX.ActnList, uMakerAi.RAG.Vectors, uMakerAi.Tools.Functions,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings, uMakerAi.MCPServer.Core, uMakerAi.MCPServer.Http,

  System.ImageList, FMX.ImgList, FMX.TabControl, uMakerAi.Tools.Shell, uMakerAi.Tools.TextEditor, uMakerAi.Utils.DiffUpdater;

type
  TForm2 = class(TForm)
    PopupMenu1: TPopupMenu;
    MnuCopy: TMenuItem;
    MnuSave: TMenuItem;
    MnuPlay: TMenuItem;
    SaveDialog1: TSaveDialog;
    AiConn: TAiChatConnection;
    MainLayout: TLayout;
    MainChatLayout: TLayout;
    ChatList1: TChatList;
    Layout3: TLayout;
    Layout2: TLayout;

    ComboDriver: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboModels: TComboBox;
    ListBox1: TListBox;
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
    ListBoxItem11: TListBoxItem;
    ChJSonFormat: TCheckBox;
    ListBoxItem12: TListBoxItem;
    ChUseTools: TCheckBox;
    ListBoxItem4: TListBoxItem;
    Label5: TLabel;
    EditMaxTokens: TEdit;
    ListBoxItem9: TListBoxItem;
    LblTemperature: TLabel;
    TrackTemperature: TTrackBar;
    ListBoxItem8: TListBoxItem;
    Label4: TLabel;
    EditCompletionModel: TEdit;
    ListBoxItem10: TListBoxItem;
    ChWebSearch: TCheckBox;
    ListBoxItem13: TListBoxItem;
    ChCodeInterpreter: TCheckBox;
    ListBoxItem14: TListBoxItem;
    ChExtractFiles: TCheckBox;
    ListBoxItem15: TListBoxItem;
    Label6: TLabel;
    EditThinkingLevel: TComboBox;
    ListBoxItem16: TListBoxItem;
    Label7: TLabel;
    EditVoices: TEdit;
    ListBoxItem7: TListBoxItem;
    Label3: TLabel;
    EditChatMediaSupports: TEdit;
    ListBoxItem17: TListBoxItem;
    Label8: TLabel;
    EditNativeInputFiles: TEdit;
    ListBoxItem18: TListBoxItem;
    Label9: TLabel;
    EditNativeOutputFiles: TEdit;
    ListBoxItem19: TListBoxItem;
    Label10: TLabel;
    EditVoiceFormat: TEdit;
    Rectangle1: TRectangle;
    Label11: TLabel;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    Rectangle2: TRectangle;
    Label12: TLabel;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    Rectangle3: TRectangle;
    Label13: TLabel;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    Rectangle4: TRectangle;
    Label14: TLabel;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    Rectangle5: TRectangle;
    Label15: TLabel;
    ListBoxItem6: TListBoxItem;
    ModelCapabilitiesCombo: TCalloutRectangle;
    ChModelImage: TCheckBox;
    ChModelVideo: TCheckBox;
    ChModelAudio: TCheckBox;
    ChModelPDF: TCheckBox;
    AIVoiceMonitor1: TAIVoiceMonitor;
    AIWhisper1: TAIWhisper;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    Rectangle6: TRectangle;
    Label16: TLabel;
    ListBoxItem20: TListBoxItem;
    ChEnableListening: TCheckBox;
    ListBoxItem21: TListBoxItem;
    ChEnableWordDetection: TCheckBox;
    ListBoxItem22: TListBoxItem;
    Label17: TLabel;
    EditWakeWord: TEdit;
    ListBoxItem23: TListBoxItem;
    ChSendAudioToIA: TCheckBox;
    MenuBar1: TMenuBar;
    MnuFile: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MnuSaveToFile: TMenuItem;
    MnuLoadFromFile: TMenuItem;
    MnuNewChat: TMenuItem;
    ToolBar1: TToolBar;
    BtnNewChat: TButton;
    BtnSaveDataBase: TButton;
    BtnOpenDataBase: TButton;
    BtnSaveChat: TButton;
    BtnOpenChat: TButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    ac_NewChat: TAction;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    AiFunctions1: TAiFunctions;
    AiRAGVector1: TAiRAGVector;
    ac_LoadChatFromFile: TAction;
    ac_SaveChatToFile: TAction;
    ac_LoadKnowledgeFromFile: TAction;
    ac_SaveKnowledgeToFile: TAction;
    ac_AddItemToDatabase: TAction;
    ac_Config: TAction;
    BtnConfig: TButton;
    Image6: TImage;
    BtnAddItemToDatabase: TButton;
    Image7: TImage;
    BtnAbout: TButton;
    Image8: TImage;
    BtnHelp: TButton;
    Image9: TImage;
    ac_help: TAction;
    ac_about: TAction;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    AiOpenAiEmbeddings1: TAiOpenAiEmbeddings;
    Layout1: TLayout;
    LblLog: TLabel;
    ChatInput1: TChatInput;
    AniIndicator1: TAniIndicator;
    ac_LoadMCPConfig: TAction;
    BtnLoadMcpServerConfig: TButton;
    Image10: TImage;
    BtnShowBilling: TButton;
    Image11: TImage;
    AiOpenChat1: TAiOpenChat;
    Layout10: TLayout;
    MemoThinking: TMemo;
    Splitter1: TSplitter;
    ChModelAny: TCheckBox;
    ListBoxItem24: TListBoxItem;
    Label19: TLabel;
    EditThinkingTokens: TEdit;
    LayoutArtefacts: TLayout;
    TabControl1: TTabControl;
    TabUsageItem: TTabItem;
    TabShellTool: TTabItem;
    TabEditTool: TTabItem;
    MemoTokenUsageHistory: TMemo;
    MemoShellTool: TMemo;
    AiShell1: TAiShell;
    ListBoxItem25: TListBoxItem;
    ChShellTool: TCheckBox;
    ListBoxItem26: TListBoxItem;
    ChEditTool: TCheckBox;
    MemoEditTool: TMemo;
    SplitterArtefacts: TSplitter;
    Rectangle7: TRectangle;
    ListBoxItem27: TListBoxItem;
    Label22: TLabel;
    EditURL: TEdit;
    AiTextEditorTool1: TAiTextEditorTool;
    Layout4: TLayout;
    MemoEditCommandLog: TMemo;
    Splitter3: TSplitter;
    TabChatParams: TTabItem;
    MemoUsedChatParams: TMemo;
    Button2: TButton;
    Image12: TImage;
    Rectangle8: TRectangle;
    Label25: TLabel;
    Rectangle9: TRectangle;
    Label26: TLabel;
    Rectangle10: TRectangle;
    Label24: TLabel;
    Rectangle11: TRectangle;
    Label21: TLabel;
    Rectangle12: TRectangle;
    Label23: TLabel;
    Rectangle13: TRectangle;
    Label20: TLabel;
    Layout5: TLayout;
    EditCaption: TEdit;
    Edit1: TEdit;
    BtnJSonShema: TSpeedButton;

    procedure ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
    procedure ChatList1MediaFileDblClick(Sender: TObject; const ABubble: TChatBubble; const AMediaFile: TAiMediaFile);
    procedure ChatList1BeforeAddBubble(Sender: TObject; ABubble: TChatBubble);
    procedure MnuCopyClick(Sender: TObject);
    procedure MnuSaveClick(Sender: TObject);
    procedure MnuPlayClick(Sender: TObject);
    procedure AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure AiConnError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const aResponse: IHTTPResponse);
    procedure FormCreate(Sender: TObject);
    procedure ComboDriverChange(Sender: TObject);
    procedure ComboModelsChange(Sender: TObject);
    procedure TrackTemperatureTracking(Sender: TObject);
    procedure ChatInput1Cancel(Sender: TObject);
    procedure ChModelImageChange(Sender: TObject);
    procedure EditChatMediaSupportsExit(Sender: TObject);
    procedure EditChatMediaSupportsEnter(Sender: TObject);
    procedure ChatInput1TranscriptText(Sender: TObject; aFragmentStream: TMemoryStream; out aTransriptText: string);
    procedure ChEnableListeningChange(Sender: TObject);
    procedure ChEnableWordDetectionChange(Sender: TObject);
    procedure EditWakeWordChange(Sender: TObject);
    procedure ac_NewChatExecute(Sender: TObject);
    procedure ac_LoadChatFromFileExecute(Sender: TObject);
    procedure ac_SaveChatToFileExecute(Sender: TObject);
    procedure ac_LoadKnowledgeFromFileExecute(Sender: TObject);
    procedure ac_SaveKnowledgeToFileExecute(Sender: TObject);
    procedure ac_AddItemToDatabaseExecute(Sender: TObject);
    procedure ac_ConfigExecute(Sender: TObject);
    procedure ac_helpExecute(Sender: TObject);
    procedure ac_aboutExecute(Sender: TObject);
    procedure BtnNewChatClick(Sender: TObject);
    procedure BtnOpenChatClick(Sender: TObject);
    procedure BtnSaveChatClick(Sender: TObject);
    procedure BtnOpenDataBaseClick(Sender: TObject);
    procedure BtnSaveDataBaseClick(Sender: TObject);
    procedure BtnAddItemToDatabaseClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
    procedure BtnConfigClick(Sender: TObject);
    procedure AiFunctions1Functions0GetSystemDateTimeAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AiFunctions1Functions1AddInfoToRAGAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure AiFunctions1Functions2SearchInRagAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
    procedure ac_LoadMCPConfigExecute(Sender: TObject);
    procedure BtnLoadMcpServerConfigClick(Sender: TObject);
    procedure AiConnCallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
    procedure BtnShowBillingClick(Sender: TObject);
    procedure AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
    procedure ChShellToolChange(Sender: TObject);
    procedure ChEditToolChange(Sender: TObject);
    procedure AiShell1ConsoleLog(Sender: TObject; const Command, StdOut, StdErr: string; ExitCode: Integer);
    procedure AiTextEditorTool1BeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject; var Result: string; var Handled: Boolean);
    procedure AiTextEditorTool1EnsureDirectory(Sender: TObject; const Path: string; var Handled: Boolean);
    procedure AiTextEditorTool1FileExists(Sender: TObject; const Path: string; var Exists, Handled: Boolean);
    procedure AiTextEditorTool1LoadFile(Sender: TObject; const Path: string; var Content: string; var Handled: Boolean);
    procedure AiTextEditorTool1SaveFile(Sender: TObject; const Path, Content: string; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure BtnJSonShemaClick(Sender: TObject);
  private
    FLastBubble: TChatBubble;
    FPrompt_tokens: Integer;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;
    FThinking_tokens: Integer;
    FActiveModelEdit: TEdit;
    FLastNewText: String;
    FJSonShema: String; // Almacena temporalmente el jsonshemma
    FNoImage: Integer; // Temporal para la captura de la imagen;

    Function CopyToClipBoard(AMediaFile: TAiMediaFile): Boolean;
    Procedure AssignModel(Const DriverName, ModelName: String);
    Procedure InitModelCapabilitiesCombo(Edit: TEdit);
    Procedure AssignAiConnParams;
    function PackString(const Texto: string): TMemoryStream; // para comprimir los archivos de salida
    function UnpackString(StreamComprimido: TStream): string; // para descomprimir los archivos de salida
    Function PackStream(StreamOrigen: TStream): TMemoryStream;
    Function UnpackStream(StreamComprimido: TStream): TMemoryStream;
    procedure SaveCombinedChatToStream(AContainerStream: TStream; AChatUIStream, AHistoryStream: TStream);
    procedure LoadCombinedChatFromStream(AContainerStream: TStream; out AChatUIStream: TStream; out AHistoryStream: TStream);
    procedure SaveRAGToStream(AContainerStream: TStream; ASourceDataStream: TStream);
    function LoadRAGFromStream(AContainerStream: TStream): TMemoryStream;
    Procedure AddLog(Value: String);
    procedure LoadMCPClientsFromJSON(AJsonString: string; AFunctions: TAiFunctions);
    function WebSearchToString(AWebSearch: TAiWebSearch): String; // Recupera los detalles de la búsqueda web
  public
    Procedure ShowArtefacts(Visible: Boolean);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses uMemoPropertiesEdit;

const
  // Opciones de versionado del formato creado para este modelo.
  MKCHAT_MAGIC_HEADER: array [0 .. 3] of AnsiChar = 'MKCH';
  MKCHAT_FORMAT_VERSION: Word = 1;

  MKRAG_MAGIC_HEADER: array [0 .. 3] of AnsiChar = 'MKRG';
  MKRAG_FORMAT_VERSION: Word = 1;

procedure LogPruebas(const Mensaje: string);
var
  Archivo: TextFile;
  RutaLog: string;
begin
  // ---------------------------------------------------------------------------------
  // -------- OPCIÓN DESHABILITADA ES SOLO UN LOG DE PRUEBAS--------------------------
  // ---------------------------------------------------------------------------------

  { RutaLog := 'd:\videos\logPruebas.txt';

    try
    AssignFile(Archivo, RutaLog);

    // Si el archivo existe, lo abre para agregar; si no, lo crea
    if FileExists(RutaLog) then
    Append(Archivo)
    else
    Rewrite(Archivo);

    // Escribe la línea con fecha/hora
    // WriteLn(Archivo, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Mensaje);
    WriteLn(Archivo, Mensaje);

    finally
    CloseFile(Archivo);
    end;
  }
end;

procedure TForm2.ac_aboutExecute(Sender: TObject);
Var
  Bm: TBitMap;
  FileName: String;
begin
  // ---------------------------------------------------------------------------------
  // -------- OPCIÓN DESHABILITADA ES SOLO UN LOG DE PRUEBAS--------------------------
  // ---------------------------------------------------------------------------------

  {  Inc(FNoImage);
  Bm := TScreenCapture.CaptureFullScreen;
  FileName := Edit1.Text + ':' + '_' + FNoImage.ToString + '_' + EditCaption.Text;
  FileName := StringReplace(FileName, ' ', '_', [rfReplaceAll]);
  FileName := StringReplace(FileName, ':', '_', [rfReplaceAll]);
  FileName := 'D:\Videos\' + FileName + '.png';
  TScreenCapture.SaveToFile(Bm, FileName)
}
end;

procedure TForm2.ac_AddItemToDatabaseExecute(Sender: TObject);
begin
  //
end;

procedure TForm2.ac_ConfigExecute(Sender: TObject);
begin
  //
end;

procedure TForm2.ac_helpExecute(Sender: TObject);
begin
  //
end;

procedure TForm2.ac_LoadChatFromFileExecute(Sender: TObject);
var
  LUIStream, LHistoryStream: TStream;
  LFileStream: TFileStream;
begin
  OpenDialog1.DefaultExt := '.mkchat';
  OpenDialog1.Filter := 'MKChat Files (*.mkchat)|*.mkchat|All Files (*.*)|*.*';
  OpenDialog1.Title := 'Load Chat Session';

  if OpenDialog1.Execute then
  begin
    LUIStream := nil;
    LHistoryStream := nil;
    LFileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // 1. Usar nuestro procedimiento de ayuda para desempaquetar
      LoadCombinedChatFromStream(LFileStream, LUIStream, LHistoryStream);

      // 2. Cargar cada parte en su componente correspondiente
      if Assigned(LUIStream) then
      begin
        LUIStream.Position := 0;
        ChatList1.LoadFromStream(LUIStream);
      end;

      if Assigned(LHistoryStream) then
      begin
        LHistoryStream.Position := 0;
        // Asumiendo que AiConn.Messages tiene un método LoadFromStream
        AiConn.Messages.LoadFromStream(LHistoryStream);
      end;

    finally
      // LoadCombinedChatFromStream crea nuevas instancias, debemos liberarlas
      LUIStream.Free;
      LHistoryStream.Free;
      LFileStream.Free;
    end;
  end;
end;

procedure TForm2.ac_LoadKnowledgeFromFileExecute(Sender: TObject);
var
  LDataStream: TMemoryStream;
  LFileStream: TFileStream;
begin
  OpenDialog1.DefaultExt := '.mkrag';
  OpenDialog1.Filter := 'MKRAG Files (*.mkrag)|*.mkrag|All Files (*.*)|*.*';
  OpenDialog1.Title := 'Load RAG Vector Data';

  if OpenDialog1.Execute then
  begin
    LDataStream := nil;
    LFileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      // 1. Usar nuestra función de ayuda para leer y descomprimir
      LDataStream := LoadRAGFromStream(LFileStream);

      // 2. Si todo fue bien, LDataStream contiene los datos listos para usar
      if Assigned(LDataStream) then
      begin
        AiRAGVector1.LoadFromStream(LDataStream);
        ShowMessage('Knowledge database loaded "' + OpenDialog1.FileName + '"');
      end;
    finally
      // Liberamos los streams. LDataStream fue creado por LoadRAGFromStream.
      LDataStream.Free;
      LFileStream.Free;
    end;
  end;
end;

procedure TForm2.ac_LoadMCPConfigExecute(Sender: TObject);
var
  LJsonContent: string;
begin
  // 1. Configurar el diálogo para buscar archivos de configuración MCP
  OpenDialog1.DefaultExt := '.mcpconf';
  OpenDialog1.Filter := 'MCP Config Files (*.mcpconf)|*.mcpconf|JSON Files (*.json)|*.json|All Files (*.*)|*.*';
  OpenDialog1.Title := 'Load MCP Server Configuration';

  // 2. Mostrar el diálogo y verificar si el usuario seleccionó un archivo
  if OpenDialog1.Execute then
  begin
    try
      // 3. Leer todo el contenido del archivo a una cadena de texto.
      // TFile.ReadAllText es la forma más simple y segura de hacerlo.
      // Se encarga de abrir, leer y cerrar el archivo automáticamente.
      LJsonContent := TFile.ReadAllText(OpenDialog1.FileName);

      // 4. Llamar a nuestra función de procesamiento con el contenido del archivo
      // Asumimos que tu componente TAiFunctions se llama 'AiFunctions1'
      LoadMCPClientsFromJSON(LJsonContent, AiFunctions1);

      // 5. Notificar al usuario (opcional, pero recomendado)
      AddLog('Configuración MCP cargada exitosamente desde: ' + OpenDialog1.FileName);
      ShowMessage('MCP configuration loaded successfully!');

    except
      on E: Exception do
      begin
        // En caso de un error (archivo no encontrado, JSON mal formado, etc.)
        AddLog('ERROR: Falló la carga del archivo de configuración MCP. ' + E.Message);
        ShowMessage('Failed to load MCP configuration file.' + sLineBreak + E.ClassName + ': ' + E.Message);
      end;
    end;
  end;
end;

procedure TForm2.ac_NewChatExecute(Sender: TObject);
begin
  AiConn.NewChat;
  ChatList1.Clear;
  AiRAGVector1.Clear;
  MemoThinking.Lines.Clear;
end;

procedure TForm2.ac_SaveChatToFileExecute(Sender: TObject);
var
  LUIStream: TMemoryStream;
  LHistoryStream: TMemoryStream;
  LFileStream: TFileStream;
begin
  SaveDialog1.DefaultExt := '.mkchat';
  SaveDialog1.Filter := 'MKChat Files (*.mkchat)|*.mkchat|All Files (*.*)|*.*';
  SaveDialog1.Title := 'Save Chat Session';

  if SaveDialog1.Execute then
  begin
    LUIStream := TMemoryStream.Create;
    try
      LHistoryStream := TMemoryStream.Create;
      try
        // 1. Guardar cada parte en su propio stream en memoria
        ChatList1.SaveToStream(LUIStream);
        AiConn.Messages.SaveToStream(LHistoryStream);

        // 2. Crear el archivo de destino
        LFileStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          // 3. Usar nuestro procedimiento de ayuda para empaquetarlos
          SaveCombinedChatToStream(LFileStream, LUIStream, LHistoryStream);
        finally
          LFileStream.Free;
        end;
      finally
        LHistoryStream.Free;
      end;
    finally
      LUIStream.Free;
    end;
  end;
end;

procedure TForm2.ac_SaveKnowledgeToFileExecute(Sender: TObject);
var
  LSourceStream: TMemoryStream;
  LFileStream: TFileStream;
begin
  SaveDialog1.DefaultExt := '.mkrag';
  SaveDialog1.Filter := 'MKRAG Files (*.mkrag)|*.mkrag|All Files (*.*)|*.*';
  SaveDialog1.Title := 'Save RAG Vector Data';

  if SaveDialog1.Execute then
  begin
    LSourceStream := TMemoryStream.Create;
    try
      // 1. Obtener los datos RAG en un stream
      AiRAGVector1.SaveToStream(LSourceStream);

      LFileStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
      try
        // 2. Usar nuestra función de ayuda para guardar en el formato correcto
        SaveRAGToStream(LFileStream, LSourceStream);
      finally
        LFileStream.Free;
      end;
    finally
      LSourceStream.Free;
    end;
  end;
end;

procedure TForm2.AddLog(Value: String);
begin
  TThread.Queue(nil,
    procedure
    begin
      LblLog.Text := 'Log: ' + Value;
    end);

end;

procedure TForm2.AiConnCallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
Var
  FunctionName: String;
begin
  FunctionName := AiToolCall.name;
  // LLamado por defecto a la función siempre se ejectua para cualquier función

end;

procedure TForm2.AiConnError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const aResponse: IHTTPResponse);
begin

  TThread.Queue(nil,
    procedure
    begin
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
      ChatInput1.Busy := False;
      ShowMessage(ErrorMsg);
    end);
end;

procedure TForm2.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin

  if Not ChAsincrono.IsChecked then
    Exit;

  If aText = '' then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      If Assigned(FLastBubble) then
        FLastBubble.AppendText(aText)
      Else
        FLastBubble := ChatList1.AddBubble(aText, aRole, Nil);

      ChatList1.RecalcSize;
    end);
end;

procedure TForm2.AiConnReceiveDataEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin

  TThread.Queue(nil,
    procedure
    begin
      ChatInput1.Busy := False;
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
    end);

  MemoTokenUsageHistory.Lines.Add(Format('Model: %s, Input: %d, Output %d, Total: %d', [aMsg.Model, aMsg.Prompt_tokens, aMsg.Completion_tokens, aMsg.Total_tokens]));

  FPrompt_tokens := FPrompt_tokens + aMsg.Prompt_tokens;
  FCompletion_tokens := FCompletion_tokens + aMsg.Completion_tokens;
  FTotal_tokens := FTotal_tokens + aMsg.Total_tokens;
  FThinking_tokens := FThinking_tokens + aMsg.Thinking_tokens;

  EditPromptTokens.Text := Format('%d', [FPrompt_tokens]);
  EditCompletionTokens.Text := Format('%d', [FCompletion_tokens]);
  EditTotalTokens.Text := Format('%d', [FTotal_tokens]);
  EditThinkingTokens.Text := Format('%d', [FThinking_tokens]);
  EditCompletionModel.Text := aMsg.Model;

  // aMsg.Model;

  TThread.Queue(nil,
    procedure
    begin
      if Not ChAsincrono.IsChecked then
        FLastBubble := ChatList1.AddBubble(aText, aRole, aMsg.MediaFiles)
      else
      Begin

        If aMsg.MediaFiles.Count > 0 then
        Begin
          // If Assigned(FLastBubble) then
          // FLastBubble.AddContent('', aMsg.MediaFiles)
          // Else
          FLastBubble := ChatList1.AddBubble('', aRole, aMsg.MediaFiles);
        End;
      End;

      // Adiciona el detalle de la búsqueda web, info adicional de fuentes
      If Assigned(aMsg.WebSearchResponse) and (aMsg.WebSearchResponse.annotations.Count > 0) then
      Begin
        Var
        WebRes := WebSearchToString(aMsg.WebSearchResponse);
        If Assigned(FLastBubble) then
          FLastBubble.AppendText(WebRes);
      End;

      // Adiciona la transcripción en (traanscripciones o generación de imagenes)

      For Var MF in aMsg.MediaFiles do
      Begin

        If MF.Transcription <> '' then
        Begin
          If Assigned(FLastBubble) then
            FLastBubble.AppendText(MF.Transcription)
          else
            FLastBubble := ChatList1.AddBubble(MF.Transcription, aRole, Nil);
        End;
      End;

      FLastBubble := Nil;
    end);

  TTask.Run(
    procedure
    begin
      Try
        Sleep(2000);

        LogPruebas('');
        LogPruebas('Result: ' + aRole);
        LogPruebas('   ' + aText);
        LogPruebas('');
        LogPruebas('-----------------------------------------------------------------------------------------------');

        ac_aboutExecute(Nil);
      Except

      End;
    end);

end;

procedure TForm2.AiConnReceiveThinking(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      MemoThinking.BeginUpdate;
      Try
        MemoThinking.Lines.Text := MemoThinking.Lines.Text + aText;
        MemoThinking.GoToTextEnd;
      Finally
        MemoThinking.EndUpdate;
      End;
    end);
end;

procedure TForm2.AiFunctions1Functions0GetSystemDateTimeAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  ToolCall.Response := FormatDateTime('YYYY-MM-DD  hh:nn:ss', Now);
end;

procedure TForm2.AiFunctions1Functions1AddInfoToRAGAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
Var
  InfoToSave: String;
begin
  AddLog('Adicionando a RAG..');
  Try
    InfoToSave := Trim(ToolCall.Params.Values['InfoToSave']);
    If InfoToSave <> '' then
    Begin
      Try
        AiRAGVector1.AddItem(InfoToSave);
        ToolCall.Response := 'Ok';

        AddLog('');

      Except
        ON E: Exception do
        Begin
          ToolCall.Response := E.Message;
          AddLog('Adicionando a RAG Error');

        End;
      End;
    End
    Else
      ToolCall.Response := 'El parámetro InfoToSave no puede estar vacío';
  Finally
    AddLog('');
  End;
end;

procedure TForm2.AiFunctions1Functions2SearchInRagAction(Sender: TObject; FunctionAction: TFunctionActionItem; FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
Var
  SearchText: String;
begin
  AddLog('Consultando RAG..');
  Try

    SearchText := Trim(ToolCall.Params.Values['SearchText']);
    If SearchText <> '' then
    Begin
      Try
        ToolCall.Response := AiRAGVector1.SearchText(SearchText, 10, 0.3); // 03 es mas permisivo para el indice que maneja RAG
      Except
        ON E: Exception do
        Begin
          ToolCall.Response := E.Message;
        End;
      End;
    End
    Else
      ToolCall.Response := 'El parámetro SearchText no puede estar vacío';
  Finally
    AddLog('');
  End;
end;

procedure TForm2.AiShell1ConsoleLog(Sender: TObject; const Command, StdOut, StdErr: string; ExitCode: Integer);
begin
  // Capturamos los datos necesarios para el hilo principal
  // TThread.Queue toma un método anónimo
  TThread.Queue(nil,
    procedure
    begin
      // --- AQUI DENTRO YA ESTAMOS EN EL HILO PRINCIPAL (UI) ---

      // 1. Pintar Comando
      MemoShellTool.Lines.Add(Format('root@makerai:~$ %s', [Command]));

      // 2. Pintar Salidas
      if StdOut <> '' then
      Begin
        Var
        aStdOut := StringReplace(StdOut, #$A, #$D#$A, [rfReplaceAll]);
        MemoShellTool.Lines.Add(aStdOut);
      End;

      if StdErr <> '' then
        MemoShellTool.Lines.Add('ERROR: ' + StdErr);

      // 3. Scroll al final (Versión VCL)
      MemoShellTool.GoToTextEnd;
    end);
end;

procedure TForm2.AssignAiConnParams;

// Función auxiliar para agregar elementos a la cadena del SET limpiamente
  procedure AddToSet(var ASetString: String; const AValue: String);
  begin
    if ASetString = '' then
      ASetString := AValue
    else
      ASetString := ASetString + ',' + AValue;
  end;

Var
  InputMediaFiles, OutputMediaFiles, ChatMediaSuports, Opt, sUrl: String;
begin
  AiConn.Params.BeginUpdate; // Importante para rendimiento si hay muchos cambios
  try
    AiConn.Params.Clear;

    sUrl := Trim(EditURL.Text);

    If sUrl <> '' then
      AiConn.Params.Values['Url'] := sUrl;

    // 1. BOOLEANOS:
    // BoolToStr(Valor, True) devuelve los strings 'True' o 'False'.
    // Esto es correcto. Al hacer Clear, si aquí pasas 'False', la RTTI leerá 'False' y actualizará el objeto.
    AiConn.Params.Values['Tool_Active'] := BoolToStr(ChUseTools.IsChecked, True);
    AiConn.Params.Values['Asynchronous'] := BoolToStr(ChAsincrono.IsChecked, True);

    // Otros valores simples
    AiConn.Params.Values['Max_Tokens'] := StrToIntDef(EditMaxTokens.Text, 8000).ToString;
    AiConn.Params.Values['Temperature'] := FloatToStr(TrackTemperature.Value / 10); // FloatToStr maneja mejor el punto decimal según locale
    AiConn.Params.Values['ResponseTimeOut'] := '600000';
    AiConn.Params.Values['Voice'] := EditVoices.Text;
    AiConn.Params.Values['Voice_Format'] := EditVoiceFormat.Text;

    // 2. ENUMERADOS (El error grave estaba aquí):
    // Si no está chequeado, DEBES enviar el valor por defecto explícitamente.
    // Si lo omites, el objeto mantiene el valor anterior.
    If ChJSonFormat.IsChecked then
    Begin
      If FJSonShema <> '' then
        AiConn.Params.Values['Response_format'] := 'tiaChatRfJsonSchema'
      Else
        AiConn.Params.Values['Response_format'] := 'tiaChatRfJson';
    End
    Else
      AiConn.Params.Values['Response_format'] := 'tiaChatRfText';

    If (FJSonShema <> '') and ChJSonFormat.IsChecked then
      AiConn.Params.Values['JsonSchema'] := FJSonShema;

    // Para ThinkingLevel, no envíes cadena vacía '', envía el Default.
    // Si la RTTI intenta convertir '' a un Enum, fallará o no hará nada.
    If EditThinkingLevel.ItemIndex >= 0 then
      AiConn.Params.Values['ThinkingLevel'] := GetEnumName(TypeInfo(TAiThinkingLevel), Ord(EditThinkingLevel.ItemIndex))
    Else
      AiConn.Params.Values['ThinkingLevel'] := 'tlDefault'; // <--- CORRECCIÓN: Asigna el valor base del Enum

    // 3. SETS (Conjuntos):
    InputMediaFiles := '';
    OutputMediaFiles := '';
    ChatMediaSuports := '';

    // Lógica ChatMediaSuports

    If ChExtractFiles.IsChecked then
      AddToSet(OutputMediaFiles, 'tfc_ExtracttextFile');

    If ChWebSearch.IsChecked then
      AddToSet(ChatMediaSuports, 'Tcm_WebSearch');
    If ChCodeInterpreter.IsChecked then
      AddToSet(ChatMediaSuports, 'Tcm_code_interpreter');

    Opt := LowerCase(EditChatMediaSupports.Text);

    If Opt.Contains('image') then
      AddToSet(ChatMediaSuports, 'Tcm_Image');
    If Opt.Contains('audio') then
      AddToSet(ChatMediaSuports, 'Tcm_Audio');
    If Opt.Contains('video') then
      AddToSet(ChatMediaSuports, 'Tcm_Video');
    If Opt.Contains('pdf') then
      AddToSet(ChatMediaSuports, 'tcm_pdf');
    If Opt.Contains('any') then
      AddToSet(ChatMediaSuports, 'Tcm_Any');

    // Lógica NativeInputFiles
    Opt := LowerCase(EditNativeInputFiles.Text);
    If Opt.Contains('image') then
      AddToSet(InputMediaFiles, 'Tfc_Image');
    If Opt.Contains('audio') then
      AddToSet(InputMediaFiles, 'Tfc_Audio');
    If Opt.Contains('video') then
      AddToSet(InputMediaFiles, 'Tfc_Video');
    If Opt.Contains('pdf') then
      AddToSet(InputMediaFiles, 'Tfc_pdf');
    If Opt.Contains('any') then
      AddToSet(InputMediaFiles, 'Tfc_Any');

    // Lógica NativeOutputFiles
    Opt := LowerCase(EditNativeOutputFiles.Text);
    If Opt.Contains('image') then
      AddToSet(OutputMediaFiles, 'Tfc_Image');
    If Opt.Contains('audio') then
      AddToSet(OutputMediaFiles, 'Tfc_Audio');
    If Opt.Contains('video') then
      AddToSet(OutputMediaFiles, 'Tfc_Video');
    If Opt.Contains('pdf') then
      AddToSet(OutputMediaFiles, 'Tfc_pdf');

    If ChShellTool.IsChecked then
      AddToSet(ChatMediaSuports, 'tcm_Shell');

    If ChEditTool.IsChecked then
      AddToSet(ChatMediaSuports, 'tcm_TextEditor');

    // Asignación final con formato de Set [a,b,c]
    AiConn.Params.Values['NativeInputFiles'] := '[' + InputMediaFiles + ']';
    AiConn.Params.Values['NativeOutputFiles'] := '[' + OutputMediaFiles + ']';
    AiConn.Params.Values['ChatMediaSupports'] := '[' + ChatMediaSuports + ']';

    // Estos son los parámetros con los que se ejecuta la petición actual
    MemoUsedChatParams.Lines.Assign(AiConn.Params);
    MemoUsedChatParams.Lines.Add('');
    MemoUsedChatParams.Lines.Add('------------------------------------------------------------');
    MemoUsedChatParams.Lines.Add('-- These are properties on TAIChatConnection, not params. --');
    MemoUsedChatParams.Lines.Add('------------------------------------------------------------');
    MemoUsedChatParams.Lines.Add('');
    MemoUsedChatParams.Lines.Add('drivename=' + AiConn.DriverName);
    MemoUsedChatParams.Lines.Add('model=' + AiConn.Model);
    MemoUsedChatParams.Lines.Values['ApiKey'] := '';

    MemoUsedChatParams.Lines.Insert(0, 'ApiKey=@' + UpperCase(ComboDriver.Text) + '_API_KEY');

  finally
    AiConn.Params.EndUpdate;
  end;

end;

procedure TForm2.AssignModel(const DriverName, ModelName: String);

// Función auxiliar para reconstruir el texto de la interfaz (Image, Audio, Video...)
// basado en el contenido del parámetro raw (tcm_Image, tfc_Audio, etc.)
  function BuildMediaString(const AParamValue: String): String;
  var
    LConf: String;
  begin
    Result := '';
    LConf := LowerCase(AParamValue);

    // Usamos Contains para detectar la presencia.
    // Nota: Esto asume que no hay conflictos de nombres (ej: 'image' vs 'imagery')
    if LConf.Contains('image') then
      Result := Result + ',Image';
    if LConf.Contains('audio') then
      Result := Result + ',Audio';
    if LConf.Contains('video') then
      Result := Result + ',Video';
    if LConf.Contains('pdf') then
      Result := Result + ',Pdf';
    if LConf.Contains('any') then
      Result := Result + ',Any'; // Agregado 'Any' que usabas en el guardado

    // Quitar la primera coma si existe
    if (Result.Length > 0) and (Result[1] = ',') then
      Delete(Result, 1, 1);
  end;

Var
  Max_Tokens: Integer;
  Tool_Active, Asynchronous, JSonFormat: Boolean;
  Temperature: Double;
  LEnumIndex: Integer;
begin
  // 1. ¡CRÍTICO! NO hacer AiConn.Params.Clear aquí.
  // Primero asignamos el driver y modelo para que el componente cargue sus DEFAULTS internos a Params.
  AiConn.DriverName := DriverName;
  AiConn.Model := ModelName;

  // Ahora leemos de Params (que ya tiene los defaults del modelo cargado)

  // 2. Valores Simples
  Max_Tokens := StrToIntDef(AiConn.Params.Values['Max_Tokens'], 8000);

  EditURL.Text := AiConn.Params.Values['Url'];

  // Ojo con la temperatura: Guardamos Float, recuperamos para TrackBar (Integer)
  // Usamos StrToFloatDef para manejar '0.7' o '1' correctamente segun locale.
  Temperature := StrToFloatDef(AiConn.Params.Values['Temperature'], 0.7);

  Tool_Active := StrToBoolDef(AiConn.Params.Values['Tool_Active'], False);
  Asynchronous := StrToBoolDef(AiConn.Params.Values['Asynchronous'], False);

  // Comparamos sin importar mayúsculas/minúsculas
  JSonFormat := SameText(AiConn.Params.Values['Response_format'], 'tiaChatRfJson') or SameText(AiConn.Params.Values['Response_format'], 'tiaChatRfJsonSchema');
  FJSonShema := AiConn.Params.Values['JsonSchema'];

  // 3. Asignar a la Interfaz (UI)
  EditMaxTokens.Text := Max_Tokens.ToString;
  TrackTemperature.Value := Temperature * 10; // Si es 0.7 -> 7
  ChAsincrono.IsChecked := Asynchronous;
  ChUseTools.IsChecked := Tool_Active;
  ChJSonFormat.IsChecked := JSonFormat;

  EditVoices.Text := AiConn.Params.Values['Voice'];
  EditVoiceFormat.Text := AiConn.Params.Values['Voice_Format'];

  // 4. Thinking Level
  // GetEnumValue retorna -1 si no encuentra el texto.
  LEnumIndex := GetEnumValue(TypeInfo(TAiThinkingLevel), AiConn.Params.Values['ThinkingLevel']);
  if LEnumIndex = -1 then
    EditThinkingLevel.ItemIndex := 0 // Default (tlDefault) si no existe el param
  else
    EditThinkingLevel.ItemIndex := LEnumIndex;

  // 5. Native Input Files (Reutilizando lógica con la función auxiliar)
  EditNativeInputFiles.Text := BuildMediaString(AiConn.Params.Values['NativeInputFiles']);

  // 6. Native Output Files
  EditNativeOutputFiles.Text := BuildMediaString(AiConn.Params.Values['NativeOutputFiles']);

  // Para el texto de capacidades multimedia, leemos del param string
  EditChatMediaSupports.Text := BuildMediaString(AiConn.Params.Values['ChatMediaSupports']);
end;

procedure TForm2.BtnNewChatClick(Sender: TObject);
begin
  ac_NewChatExecute(Sender);
end;

procedure TForm2.BtnConfigClick(Sender: TObject);
begin
  ac_ConfigExecute(Sender);
end;

procedure TForm2.BtnAddItemToDatabaseClick(Sender: TObject);
begin
  ac_AddItemToDatabaseExecute(Sender);
end;

procedure TForm2.BtnSaveDataBaseClick(Sender: TObject);
begin
  ac_SaveKnowledgeToFileExecute(Sender);
end;

procedure TForm2.BtnShowBillingClick(Sender: TObject);
begin
  ShowArtefacts(Not LayoutArtefacts.Visible);
  TabControl1.ActiveTab := TabUsageItem;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  ShowArtefacts(Not LayoutArtefacts.Visible);
  TabControl1.ActiveTab := TabChatParams;
end;

procedure TForm2.BtnOpenDataBaseClick(Sender: TObject);
begin
  ac_LoadKnowledgeFromFileExecute(Sender);
end;

procedure TForm2.BtnSaveChatClick(Sender: TObject);
begin
  ac_SaveChatToFileExecute(Sender);
end;

procedure TForm2.BtnOpenChatClick(Sender: TObject);
begin
  ac_LoadChatFromFileExecute(Sender);
end;

procedure TForm2.BtnAboutClick(Sender: TObject);
begin
  ac_aboutExecute(Sender);
end;

procedure TForm2.BtnHelpClick(Sender: TObject);
begin
  ac_helpExecute(Sender);
end;

procedure TForm2.BtnLoadMcpServerConfigClick(Sender: TObject);
begin
  ac_LoadMCPConfigExecute(Self);
end;

procedure TForm2.ChatInput1Cancel(Sender: TObject);
begin
  AiConn.Abort;
  TThread.Queue(nil,
    procedure
    begin
      ChatInput1.Busy := False;
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
    end);
end;

procedure TForm2.ChatInput1SendEvent(Sender: TObject; APrompt: string; aMediaFiles: TAiMediaFiles; aAudioStream: TMemoryStream);
Var
  MF: TAiMediaFile;
begin

  // Estos son los parámetros con los que se ejecuta la petición actual
  AssignAiConnParams;

  // ---------------------------------------------------------------------------------
  // -------- OPCIÓN DESHABILITADA ES SOLO UN LOG DE PRUEBAS--------------------------
  // ---------------------------------------------------------------------------------
  LogPruebas('');
  LogPruebas('Prueba: ' + EditCaption.Text);
  LogPruebas('');
  LogPruebas('Parámetros:');

  For Var I := 0 to MemoUsedChatParams.Lines.Count - 1 do
  Begin
    Var
    S := MemoUsedChatParams.Lines[I];
    LogPruebas('   ' + S);
  End;

  LogPruebas('');
  LogPruebas('Prompt:');
  LogPruebas('   ' + APrompt);
  // ---------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------

  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  // Limpia el memoThinking para el proximo llamado
  MemoThinking.Lines.Clear;

  If ChSendAudioToIA.IsChecked then
  Begin
    // Si hay un audio grabado por el control se adiciona, solo si la comunicación es solo audio ya que el texto se transcribió en APrompt
    If Assigned(aAudioStream) and (aAudioStream.size > 100) then
    Begin
      MF := TAiMediaFile.Create;
      MF.LoadFromStream('miaudio.wav', aAudioStream);
      aMediaFiles.Add(MF);
    End;
  End
  Else
  Begin

  End;

  FLastBubble := Nil;
  ChatList1.AddBubble(APrompt, 'Usuario', aMediaFiles, False);

  TTask.Run(
    procedure
    begin
      Try
        // El resultado se obtiene en el evento para obtener los mediafiles si los hay
        AiConn.AddMessageAndRun(APrompt, 'user', aMediaFiles.ToMediaFileArray);

        TThread.Queue(nil,
          procedure
          begin
            Try

            Finally
            End;
          end);

      Except
        On E: Exception do
        Begin

        End;
      End;
    end);

end;

procedure TForm2.ChatInput1TranscriptText(Sender: TObject; aFragmentStream: TMemoryStream; out aTransriptText: string);
begin
  if Assigned(aFragmentStream) and (aFragmentStream.size > 100) then
    aTransriptText := Trim(AIWhisper1.Transcription(aFragmentStream, 'dummy.wav', 'user'));
end;

procedure TForm2.ChatList1BeforeAddBubble(Sender: TObject; ABubble: TChatBubble);
begin
  ABubble.MoreOptionsVisible := False;
  ABubble.HeaderVisible := False;
end;

procedure TForm2.ChatList1MediaFileDblClick(Sender: TObject; const ABubble: TChatBubble; const AMediaFile: TAiMediaFile);
begin
  ShowMessage(AMediaFile.FileName);
end;

procedure TForm2.ChEditToolChange(Sender: TObject);
begin
  ShowArtefacts(ChEditTool.IsChecked);

  If ChEditTool.IsChecked then
    TabControl1.ActiveTab := TabEditTool;
end;

procedure TForm2.ChEnableListeningChange(Sender: TObject);
begin
  ChatInput1.MicVisible := ChEnableListening.IsChecked;
end;

procedure TForm2.ChEnableWordDetectionChange(Sender: TObject);
begin
  AIVoiceMonitor1.WakeWordActive := ChEnableWordDetection.IsChecked;
end;

procedure TForm2.ChModelImageChange(Sender: TObject);
Var
  List: TStringList;
begin
  List := TStringList.Create;
  Try
    if ChModelImage.IsChecked then
      List.Add('Image');
    if ChModelAudio.IsChecked then
      List.Add('Audio');
    if ChModelVideo.IsChecked then
      List.Add('Video');
    if ChModelPDF.IsChecked then
      List.Add('PDF');
    if ChModelAny.IsChecked then
      List.Add('ANY');

    FActiveModelEdit.Text := List.CommaText;
  Finally
    List.Free;
  End;
end;

procedure TForm2.ChShellToolChange(Sender: TObject);
begin
  ShowArtefacts(ChShellTool.IsChecked);

  If ChShellTool.IsChecked then
    TabControl1.ActiveTab := TabShellTool;
end;

procedure TForm2.ComboDriverChange(Sender: TObject);
Var
  DriverName: String;
  List: TStringList;
begin
  If Assigned(ComboDriver.Selected) then
  Begin
    DriverName := Trim(ComboDriver.Text);

    Edit1.Text := DriverName;

    AiConn.DriverName := DriverName;
    Cursor := crHourGlass;
    EditURL.Text := '';

    List := AiConn.GetModels;
    Try
      List.Sort;
      ComboModels.Items.Text := List.Text;
    Finally
      List.Free;
      Cursor := crDefault;
    End;
  End;

end;

procedure TForm2.ComboModelsChange(Sender: TObject);
Var
  DriverName, ModelName: String;
begin
  If Assigned(ComboModels.Selected) then
  Begin
    DriverName := ComboDriver.Text;
    ModelName := ComboModels.Text;
    AssignModel(DriverName, ModelName);
  End;
end;

function TForm2.CopyToClipBoard(AMediaFile: TAiMediaFile): Boolean;
var
  ClipboardSvc: IFMXClipboardService;
  LBitmap: FMX.Graphics.TBitMap;
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
        LBitmap := FMX.Graphics.TBitMap.Create;
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

procedure TForm2.EditWakeWordChange(Sender: TObject);
begin
  AIVoiceMonitor1.WakeWord := Trim(LowerCase(EditWakeWord.Text));
end;

procedure TForm2.EditChatMediaSupportsEnter(Sender: TObject);
begin
  If Sender is TEdit then
    InitModelCapabilitiesCombo(Sender as TEdit);

  ChModelAny.Visible := (Sender = EditNativeInputFiles) or (Sender = EditChatMediaSupports);
end;

procedure TForm2.EditChatMediaSupportsExit(Sender: TObject);
begin
  ModelCapabilitiesCombo.Visible := False;
end;

procedure TForm2.FormCreate(Sender: TObject);
Var
  List: TStringList;
begin

  FNoImage := 0;
  ShowArtefacts(False);

  FPrompt_tokens := 0;
  FCompletion_tokens := 0;
  FTotal_tokens := 0;
  FThinking_tokens := 0;

  ModelCapabilitiesCombo.Visible := False;

  // BtnJSonShema.Visible := False;  //Funcionalidad no implementada todavía

  Try
    List := AiConn.GetDriversNames;
    Try
      List.Sort;
      ComboDriver.Items.Text := List.Text;
      If ComboDriver.Items.Count > 0 then
        ComboDriver.ItemIndex := 0;
    Finally
      List.Free;
    End;
  Except

  End;
end;

procedure TForm2.InitModelCapabilitiesCombo(Edit: TEdit);
Var
  Opt: String;
  LocalPoint, ScreenPoint: TPointF;
begin
  FActiveModelEdit := Edit;

  ScreenPoint := Edit.LocalToScreen(Edit.Position.Point);
  LocalPoint := MainLayout.ScreenToLocal(ScreenPoint);

  // ModelCapabilitiesCombo.Parent := Edit;
  ModelCapabilitiesCombo.Position.Y := LocalPoint.Y - (Edit.Height * 1.5);
  ModelCapabilitiesCombo.Position.X := LocalPoint.X + Edit.Width;

  Opt := LowerCase(Edit.Text);
  ChModelImage.IsChecked := Opt.Contains('image');
  ChModelVideo.IsChecked := Opt.Contains('video');
  ChModelAudio.IsChecked := Opt.Contains('audio');
  ChModelPDF.IsChecked := Opt.Contains('pdf');
  ChModelAny.IsChecked := Opt.Contains('any');
  ModelCapabilitiesCombo.Visible := True;
end;

procedure TForm2.LoadCombinedChatFromStream(AContainerStream: TStream; out AChatUIStream, AHistoryStream: TStream);
var
  LHeader: array [0 .. 3] of AnsiChar;
  LVersion: Word;
  LCompressedStream: TMemoryStream;
  LSize: Int64;
begin
  AChatUIStream := nil;
  AHistoryStream := nil;
  AContainerStream.Position := 0;

  // 1. Validar cabecera y versión
  if (AContainerStream.Read(LHeader, SizeOf(LHeader)) <> SizeOf(LHeader)) or (not CompareMem(@LHeader, @MKCHAT_MAGIC_HEADER, SizeOf(LHeader))) then // *** LÍNEA CORREGIDA ***
    raise Exception.Create('Invalid .mkchat file format.');

  if (AContainerStream.Read(LVersion, SizeOf(LVersion)) <> SizeOf(LVersion)) or (LVersion <> MKCHAT_FORMAT_VERSION) then
    raise Exception.Create('Unsupported .mkchat file version.');

  // 2. Leer el bloque de la UI
  if AContainerStream.Read(LSize, SizeOf(LSize)) <> SizeOf(LSize) then
    raise Exception.Create('Corrupted .mkchat file (UI size).');

  LCompressedStream := TMemoryStream.Create;
  try
    LCompressedStream.CopyFrom(AContainerStream, LSize);
    AChatUIStream := UnpackStream(LCompressedStream);
  finally
    LCompressedStream.Free;
  end;

  // 3. Leer el bloque del Historial
  if AContainerStream.Read(LSize, SizeOf(LSize)) <> SizeOf(LSize) then
    raise Exception.Create('Corrupted .mkchat file (History size).');

  LCompressedStream := TMemoryStream.Create;
  try
    LCompressedStream.CopyFrom(AContainerStream, LSize);
    AHistoryStream := UnpackStream(LCompressedStream);
  finally
    LCompressedStream.Free;
  end;
end;

procedure TForm2.LoadMCPClientsFromJSON(AJsonString: string; AFunctions: TAiFunctions);
var
  LJson, LMcpServers, LServerConfig, LEnvObject: TJSONObject;
  LPair, LEnvPair: TJSONPair;
  LClientItem: TMCPClientItem;
  LServerName, LCommand, LUrl, LTimeoutStr: string;
  LArgsArray: TJSONArray;
  LArgsBuilder: TStringBuilder;
  I: Integer;
begin
  if not Assigned(AFunctions) then
    Exit;

  // Limpiamos la configuración existente antes de cargar la nueva
  AFunctions.MCPClients.Clear;
  AddLog('Configuración de clientes MCP anterior eliminada.');

  LJson := nil;
  try
    // 1. Parsear el JSON completo
    // (Usando tu implementación original para mantener la consistencia)
    LJson := TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
    if not Assigned(LJson) then
    begin
      AddLog('Error: El JSON proporcionado no es un objeto válido.');
      Exit;
    end;

    // 2. Navegar hasta el objeto "mcpServers"
    if not LJson.TryGetValue<TJSONObject>('mcpServers', LMcpServers) or not Assigned(LMcpServers) then
    begin
      AddLog('Error: No se encontró la clave "mcpServers" en el JSON.');
      Exit;
    end;

    // 3. Iterar sobre cada servidor definido en "mcpServers"
    AddLog('Comenzando a procesar servidores MCP...');
    for LPair in LMcpServers do
    begin
      LServerName := LPair.JsonString.Value;
      LServerConfig := LPair.JsonValue as TJSONObject;

      AddLog('------------------------------------');
      AddLog(Format('Procesando servidor: "%s"', [LServerName]));

      // 4. Crear un nuevo TMCPClientItem en la colección y configurar propiedades básicas
      LClientItem := AFunctions.MCPClients.Add;
      LClientItem.name := LServerName;
      LClientItem.Enabled := True; // Habilitado por defecto

      // 5. Determinar el tipo de transporte y construir los parámetros
      if LServerConfig.TryGetValue<string>('command', LCommand) then
      begin
        LClientItem.TransportType := tpStdIo;
        AddLog('  Tipo de transporte detectado: StdIo');

        LClientItem.Params.Add('Command=' + LCommand);
        AddLog(Format('  - Param[Command]=%s', [LCommand]));

        // --- SECCIÓN CORREGIDA PARA 'args' ---
        if LServerConfig.TryGetValue<TJSONArray>('args', LArgsArray) then
        begin
          LArgsBuilder := TStringBuilder.Create;
          try
            for I := 0 to LArgsArray.Count - 1 do
            begin
              // Simplemente toma el valor del JSON y lo añade, seguido de un espacio.
              // NO se añade ninguna comilla extra.
              LArgsBuilder.Append(LArgsArray.Items[I].Value);
              LArgsBuilder.Append(' ');
            end;

            // Añade la cadena de argumentos resultante (limpiando espacios al final)
            // a la lista de parámetros.
            LClientItem.Params.Add('Arguments=' + LArgsBuilder.ToString.Trim);
            AddLog(Format('  - Param[Arguments]=%s', [LArgsBuilder.ToString.Trim]));
          finally
            LArgsBuilder.Free;
          end;
        end;
        // --- FIN DE LA SECCIÓN CORREGIDA ---
        LClientItem.UpdateClientProperties;
        // LClientItem.Connected := True;

      end
      else if LServerConfig.TryGetValue<string>('url', LUrl) then
      begin
        LClientItem.TransportType := tpHttp;
        AddLog('  Tipo de transporte detectado: Http');

        // Guardamos la URL en la colección 'Params'
        LClientItem.Params.Add('URL=' + LUrl);
        AddLog(Format('  - Param[URL]=%s', [LUrl]));

        // Otros parámetros como 'timeout' también van a 'Params'
        if LServerConfig.TryGetValue<string>('timeout', LTimeoutStr) then
        begin
          LClientItem.Params.Add('Timeout=' + LTimeoutStr);
          AddLog(Format('  - Param[Timeout]=%s', [LTimeoutStr]));
        end;
      end
      else
      begin
        AddLog('  ADVERTENCIA: No se pudo determinar el tipo de transporte (ni "command" ni "url" encontrados).');
        LClientItem.Enabled := False;

      end;

      // 6. Procesar las variables de entorno ("env")
      if LServerConfig.TryGetValue<TJSONObject>('env', LEnvObject) then
      begin
        AddLog('  Procesando variables de entorno...');
        for LEnvPair in LEnvObject do
        begin
          LClientItem.EnvVars.Add(Format('%s=%s', [LEnvPair.JsonString.Value, LEnvPair.JsonValue.Value]));
          AddLog(Format('    - EnvVar: %s=%s', [LEnvPair.JsonString.Value, LEnvPair.JsonValue.Value]));
        end;
      end;
    end;

    AddLog('Carga de configuración MCP completada.');

  finally
    LJson.Free;
  end;
end;

function TForm2.LoadRAGFromStream(AContainerStream: TStream): TMemoryStream;
var
  LHeader: array [0 .. 3] of AnsiChar;
  LVersion: Word;
  ZStream: TZDecompressionStream;
begin
  AContainerStream.Position := 0;

  if (AContainerStream.Read(LHeader, SizeOf(LHeader)) <> SizeOf(LHeader)) or (not CompareMem(@LHeader, @MKRAG_MAGIC_HEADER, SizeOf(LHeader))) then
    raise Exception.Create('Invalid .mkrag file format.');

  if (AContainerStream.Read(LVersion, SizeOf(LVersion)) <> SizeOf(LVersion)) or (LVersion <> MKRAG_FORMAT_VERSION) then
    raise Exception.Create('Unsupported .mkrag file version.');

  // El resto del stream son los datos comprimidos. Los descomprimimos.
  Result := TMemoryStream.Create;
  // AContainerStream ya está posicionado justo después de la cabecera.
  ZStream := TZDecompressionStream.Create(AContainerStream);
  try
    Result.CopyFrom(ZStream, 0);
    Result.Position := 0;
  finally
    ZStream.Free;
  end;
end;

procedure TForm2.MnuCopyClick(Sender: TObject);
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
      ShowMessage('Este tipo de archivo no se puede copiar al portapapeles.');
    end;
  end;
end;

procedure TForm2.MnuPlayClick(Sender: TObject);
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
    ShowMessage('Error: No se ha seleccionado ningún archivo.');
    Exit;
  end;

  // 2. Verificar que el archivo tenga contenido para guardar
  if (LMediaFile.Content = nil) or (LMediaFile.Content.size = 0) then
  begin
    ShowMessage('El archivo seleccionado está vacío.');
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
        ShowMessage('No se pudo abrir el archivo. Verifique si tiene una aplicación instalada para este tipo de archivo (' + System.IOUtils.TPath.GetExtension(LMediaFile.FileName) + ').');
      end;
    end
    else
    begin
      ShowMessage('El servicio para abrir archivos no está disponible en esta plataforma.');
    end;

  except
    on E: Exception do
    begin
      // Capturar cualquier error durante la creación del archivo
      ShowMessage('Ocurrió un error al intentar abrir el archivo: ' + E.Message);
    end;
  end;
end;

procedure TForm2.MnuSaveClick(Sender: TObject);
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

procedure TForm2.TrackTemperatureTracking(Sender: TObject);
begin
  LblTemperature.Text := 'Temperature: ' + FormatFloat('0.0', TrackTemperature.Value / 10);
end;

function TForm2.UnpackStream(StreamComprimido: TStream): TMemoryStream;
var
  ZStream: TZDecompressionStream;
begin
  Result := TMemoryStream.Create;
  StreamComprimido.Position := 0;

  // Descomprimir al stream de resultado
  ZStream := TZDecompressionStream.Create(StreamComprimido);
  try
    Result.CopyFrom(ZStream, 0);
  finally
    ZStream.Free;
  end;

  // Posicionar el stream al inicio para su uso posterior
  Result.Position := 0;
end;

function TForm2.UnpackString(StreamComprimido: TStream): string;
var
  StreamDestino: TMemoryStream;
  ZStream: TZDecompressionStream;
  Datos: TBytes;
begin
  StreamComprimido.Position := 0;
  StreamDestino := TMemoryStream.Create;
  try
    // Descomprimir el stream
    ZStream := TZDecompressionStream.Create(StreamComprimido);
    try
      StreamDestino.CopyFrom(ZStream, 0);
    finally
      ZStream.Free;
    end;

    // Convertir los bytes descomprimidos a string
    StreamDestino.Position := 0;
    SetLength(Datos, StreamDestino.size);
    StreamDestino.ReadBuffer(Datos[0], StreamDestino.size);
    Result := TEncoding.UTF8.GetString(Datos);
  finally
    StreamDestino.Free;
  end;
end;

function TForm2.WebSearchToString(AWebSearch: TAiWebSearch): String;
var
  Sb: TStringBuilder;
  I: Integer;
  Item: TAiWebSearchItem;
begin
  Result := '';

  // 1. Validaciones de seguridad
  if not Assigned(AWebSearch) then
    Exit;
  if not Assigned(AWebSearch.annotations) then
    Exit;
  if AWebSearch.annotations.Count = 0 then
    Exit;

  Sb := TStringBuilder.Create;
  try
    Sb.AppendLine;
    Sb.AppendLine('--- Fuentes consultadas ---');

    // 2. Iterar sobre las anotaciones
    for I := 0 to AWebSearch.annotations.Count - 1 do
    begin
      Item := AWebSearch.annotations[I];

      // Formato: [1] Título del sitio
      // URL: https://...

      Sb.Append('[').Append(I + 1).Append('] ');

      if Item.Title <> '' then
        Sb.AppendLine(Item.Title)
      else
        Sb.AppendLine('Sin título');

      if Item.Url <> '' then
        Sb.Append('    Enlace: ').AppendLine(Item.Url);

      // Opcional: Mostrar índices si estás depurando donde encajan en el texto
      // Sb.AppendFormat('    (Index: %d - %d)', [Item.start_index, Item.end_index]);

      Sb.AppendLine;
    end;

    Result := Sb.ToString;
  finally
    Sb.Free;
  end;
end;

function TForm2.PackStream(StreamOrigen: TStream): TMemoryStream;
var
  ZStream: TZCompressionStream;
begin
  Result := TMemoryStream.Create;
  StreamOrigen.Position := 0;

  // Comprimir al stream de resultado
  ZStream := TZCompressionStream.Create(Result);
  try
    ZStream.CopyFrom(StreamOrigen, 0);
  finally
    ZStream.Free;
  end;

  // Posicionar el stream al inicio para su uso posterior
  Result.Position := 0;
end;

function TForm2.PackString(const Texto: string): TMemoryStream;
var
  StreamOrigen: TMemoryStream;
  ZStream: TZCompressionStream;
  Datos: TBytes;
begin
  Result := TMemoryStream.Create;
  StreamOrigen := TMemoryStream.Create;
  try
    // Convertir el string a bytes UTF-8
    Datos := TEncoding.UTF8.GetBytes(Texto);
    StreamOrigen.WriteBuffer(Datos[0], Length(Datos));
    StreamOrigen.Position := 0;

    // Comprimir al stream de resultado
    ZStream := TZCompressionStream.Create(Result);
    try
      ZStream.CopyFrom(StreamOrigen, 0);
    finally
      ZStream.Free;
    end;

    // Posicionar el stream al inicio para su uso posterior
    Result.Position := 0;
  finally
    StreamOrigen.Free;
  end;
end;

procedure TForm2.SaveCombinedChatToStream(AContainerStream, AChatUIStream, AHistoryStream: TStream);
var
  LCompressedUI: TMemoryStream;
  LCompressedHistory: TMemoryStream;
  LSize: Int64;
begin
  // 1. Comprimir ambos streams de entrada
  LCompressedUI := PackStream(AChatUIStream);
  try
    LCompressedHistory := PackStream(AHistoryStream);
    try
      // 2. Escribir la cabecera y versión
      AContainerStream.Write(MKCHAT_MAGIC_HEADER, SizeOf(MKCHAT_MAGIC_HEADER));
      AContainerStream.Write(MKCHAT_FORMAT_VERSION, SizeOf(MKCHAT_FORMAT_VERSION));

      // 3. Escribir el bloque de la UI
      LSize := LCompressedUI.size;
      AContainerStream.Write(LSize, SizeOf(LSize)); // Escribir tamaño
      LCompressedUI.Position := 0;
      AContainerStream.CopyFrom(LCompressedUI, 0); // Escribir datos

      // 4. Escribir el bloque del Historial
      LSize := LCompressedHistory.size;
      AContainerStream.Write(LSize, SizeOf(LSize)); // Escribir tamaño
      LCompressedHistory.Position := 0;
      AContainerStream.CopyFrom(LCompressedHistory, 0); // Escribir datos

    finally
      LCompressedHistory.Free;
    end;
  finally
    LCompressedUI.Free;
  end;
end;

procedure TForm2.SaveRAGToStream(AContainerStream: TStream; ASourceDataStream: TStream);
var
  ZStream: TZCompressionStream; // *** MODIFICADO ***
begin
  AContainerStream.WriteBuffer(MKRAG_MAGIC_HEADER, SizeOf(MKRAG_MAGIC_HEADER));
  AContainerStream.WriteBuffer(MKRAG_FORMAT_VERSION, SizeOf(MKRAG_FORMAT_VERSION));

  // Comprimimos directamente al stream final para mayor eficiencia de memoria
  ASourceDataStream.Position := 0;
  ZStream := TZCompressionStream.Create(AContainerStream);
  try
    ZStream.CopyFrom(ASourceDataStream, 0);
  finally
    ZStream.Free;
  end;
end;

procedure TForm2.ShowArtefacts(Visible: Boolean);
begin
  If Not(ChShellTool.IsChecked or ChEditTool.IsChecked) then
  Begin
    LayoutArtefacts.Visible := Visible;
    SplitterArtefacts.Visible := Visible;
  End
  Else
  Begin
    Var
    Vis := ChShellTool.IsChecked or ChEditTool.IsChecked or Visible;
    LayoutArtefacts.Visible := Vis;
    SplitterArtefacts.Visible := Vis;

  End;

  If SplitterArtefacts.Visible then
    SplitterArtefacts.Position.X := 100;

end;

procedure TForm2.BtnJSonShemaClick(Sender: TObject);
begin
  FMemoPropertiesEdit := TFMemoPropertiesEdit.Create(Self);
  FMemoPropertiesEdit.SetData('Edit JSonschema, empty for free json', FJSonShema, False, False);
  If FMemoPropertiesEdit.ShowModal = MrOk then
    FJSonShema := FMemoPropertiesEdit.GetContent;
  FMemoPropertiesEdit.Free;
end;

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

// 1. Carga de Archivo: En lugar de leer del disco, leemos lo que hay en el Memo.
procedure TForm2.AiTextEditorTool1LoadFile(Sender: TObject; const Path: string; var Content: string; var Handled: Boolean);
begin
  // Asignamos el texto actual del Memo a la variable Content que espera el componente
  Content := MemoEditTool.Lines.Text;

  // Indicamos que ya lo manejamos nosotros, así no busca en disco
  Handled := True;
end;

// 2. Guardado de Archivo: En lugar de escribir en disco, actualizamos el Memo.
procedure TForm2.AiTextEditorTool1SaveFile(Sender: TObject; const Path, Content: string; var Handled: Boolean);
begin
  // Actualizamos el Memo con el nuevo contenido procesado por la herramienta

  TThread.Queue(nil,
    procedure

    var
      SelPos: Integer;
    begin
      // 1. Actualización Atómica en FMX
      // FMX suele ser más eficiente asignando directamente a .Text
      MemoEditTool.Model.Lines.BeginUpdate; // Congela el repintado
      try
        MemoEditTool.Text := Content;
      finally
        MemoEditTool.Model.Lines.EndUpdate;
      end;

      // 2. Lógica visual: Buscar y resaltar
      if FLastNewText <> '' then
      begin
        // Buscamos la posición (Base 1)
        SelPos := Pos(FLastNewText, Content);

        if SelPos > 0 then
        begin
          MemoEditTool.SetFocus;

          // FMX: SelStart es Base 0
          MemoEditTool.SelStart := SelPos - 1;
          MemoEditTool.SelLength := Length(FLastNewText);

          // FMX SCROLL HACK:
          // FMX no tiene un "ScrollToCaret" nativo público simple en todas las versiones.
          // Sin embargo, al dar foco y poner la selección, la mayoría de plataformas (Android/iOS/Win)
          // intentan mostrar el cursor.

          // Si notas que no hace scroll en tu versión de Delphi, este código
          // fuerza al Memo a reconocer la posición del cursor:
          MemoEditTool.Repaint;
        end;
      end;

    end);

  // Opcional: Hacer scroll al final o al inicio si deseas
  // MemoEditTool.GoToTextEnd;

  Handled := True;
end;

// 3. Verificar Existencia: Simulamos si el archivo existe o no.
procedure TForm2.AiTextEditorTool1FileExists(Sender: TObject; const Path: string; var Exists, Handled: Boolean);
begin
  // LÓGICA:
  // Si vamos a ejecutar el comando 'create', el componente espera que el archivo NO exista.
  // Si vamos a ejecutar 'str_replace' o 'view', espera que SÍ exista.

  // Para este demo, asumimos que el archivo "existe" si el Memo tiene algo escrito.
  // Si el Memo está vacío, decimos que no existe (permitiendo usar el comando 'create').
  Exists := (MemoEditTool.Lines.Count > 0) or (Trim(MemoEditTool.Text) <> '');

  Handled := True;
end;

// 4. Directorios: Como es en memoria, siempre "tenemos éxito" con los directorios.
procedure TForm2.AiTextEditorTool1EnsureDirectory(Sender: TObject; const Path: string; var Handled: Boolean);
begin
  // No necesitamos crear carpetas reales.
  // Simplemente decimos "listo, carpeta creada/verificada".
  Handled := True;
end;

// 5. Antes del Comando: Útil para depuración o validación previa.
procedure TForm2.AiTextEditorTool1BeforeCommand(Sender: TObject; const Command, Path: string; Args: TJSONObject; var Result: string; var Handled: Boolean);
var
  sArgs: string;
  OldStr, NewStr, FileContent: string;
  Occurrences: Integer;

  // Función local para normalizar TODO a #10 (Estándar Unix/AI)
  function NormalizeToLF(const S: string): string;
  begin
    // Paso 1: Convertir Windows CRLF a LF
    Result := StringReplace(S, #13#10, #10, [rfReplaceAll]);
    // Paso 2: Convertir Mac antiguo CR a LF (por si acaso)
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  end;

// Conteo de ocurrencias sobre texto normalizado
  function CountOccurrencesLocal(const Text, SubText: string): Integer;
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

begin
  // --- LOGGING ---
  sArgs := '';
  FLastNewText := '';

  if Assigned(Args) then
    sArgs := Args.Format;

  if sArgs <> '' then
  begin
    TThread.Queue(nil,
      procedure
      begin
        MemoEditCommandLog.BeginUpdate;
        try
          MemoEditCommandLog.Lines.Add(Command + ' : ' + Path + sLineBreak + sArgs);
          MemoEditCommandLog.Lines.Add('');
          MemoEditCommandLog.GoToTextEnd;
        finally
          MemoEditCommandLog.EndUpdate;
        end;
      end);
  end;

  // --- INTERCEPTAR STR_REPLACE ---
  if Command = 'str_replace' then
  begin
    // 1. Obtener argumentos crudos
    OldStr := Args.GetValue<string>('old_str');
    NewStr := Args.GetValue<string>('new_str');

    // Guardamos para efectos visuales (scroll)
    FLastNewText := NewStr;

    // 2. NORMALIZACIÓN (La clave del éxito)
    // Convertimos lo que hay en el Memo a formato LF
    FileContent := NormalizeToLF(MemoEditTool.Text);

    // Convertimos lo que manda Claude a formato LF
    OldStr := NormalizeToLF(OldStr);
    NewStr := NormalizeToLF(NewStr);

    // 3. VALIDACIÓN (Usando las variables normalizadas)
    Occurrences := CountOccurrencesLocal(FileContent, OldStr);

    if Occurrences = 0 then
    begin
      // Tip: Agregamos una pista al error para debugging visual
      Result := 'Error: No match found for replacement.' + sLineBreak + 'Hint: Check whitespace and indentation exactly.';
      Handled := True;
      Exit;
    end
    else if Occurrences > 1 then
    begin
      Result := 'Error: Found ' + IntToStr(Occurrences) + ' matches. Context must be unique.';
      Handled := True;
      Exit;
    end;

    // 4. EJECUCIÓN (Sobre texto normalizado)
    // rfReplaceAll no es necesario porque validamos count=1, pero mal no hace.
    FileContent := StringReplace(FileContent, OldStr, NewStr, []);

    // 5. ACTUALIZACIÓN VISUAL
    MemoEditTool.Model.Lines.BeginUpdate;
    try
      // TMemo en FMX/VCL acepta #10 y lo renderiza bien.
      // Al asignarlo, el Memo internamente podría reconvertirlo a OS native,
      // pero el reemplazo ya fue exitoso.
      MemoEditTool.Text := FileContent;
    finally
      MemoEditTool.Model.Lines.EndUpdate;
    end;

    Result := 'Successfully replaced text at exactly one location.';
    Handled := True;
  end
  else if Command = 'str_replace' then
  begin
    // ... tu lógica existente ...
  end
  // --- NUEVO: INTERCEPTAR APPLY_DIFF (Opcional, para normalización) ---
  else if Command = 'apply_diff' then
  begin
    // GPT-5 a veces manda diffs con saltos de línea LF (#10),
    // mientras que tu Memo en Windows podría tener CRLF (#13#10).
    // Para asegurar que el parcheo no falle por culpa de los saltos de línea invisibles:

    // 1. Obtener argumentos
    var
    DiffText := Args.GetValue<string>('diff_text');

    // 2. Normalizar el contenido del Memo (Original) a LF
    FileContent := NormalizeToLF(MemoEditTool.Text);

    // 3. Normalizar el Diff a LF (solo por seguridad)
    DiffText := NormalizeToLF(DiffText);

    // 4. Instanciar el aplicador manualmente aquí para usar los textos normalizados
    var
    Applier := TDiffApplier.Create;
    var
      NewContent, ErrorMsg: string;
    try
      if Applier.Apply(FileContent, DiffText, NewContent, ErrorMsg) then
      begin
        // ÉXITO: Actualizar Memo
        TThread.Queue(nil,
          procedure
          begin
            MemoEditTool.Model.Lines.BeginUpdate;
            try
              MemoEditTool.Text := NewContent;
            finally
              MemoEditTool.Model.Lines.EndUpdate;
            end;
          end);

        Result := 'Diff aplicado exitosamente.';
      end
      else
      begin
        // FALLO
        Result := 'Error al aplicar diff: ' + ErrorMsg;
      end;
    finally
      Applier.Free;
    end;

    // Marcamos como manejado para que el componente no intente hacerlo de nuevo
    Handled := True;
  end
  else
  begin
    Handled := False;
  end;

end;

end.
