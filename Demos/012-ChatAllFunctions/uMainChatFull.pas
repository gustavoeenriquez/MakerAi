unit uMainChatFull;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, System.Rtti, System.JSON, System.Net.HttpClient, System.Threading, System.ZLib,
  System.StrUtils,

  uMakerAi.UI.ChatBubble, uMakerAi.UI.ChatInput, uMakerAi.Core, uMakerAi.Chat,
  uMakerAi.Utils.System, uMakerAi.UI.ChatList, uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi, uMakerAi.Chat.Initializations,
  uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Claude, uMakerAi.Chat.Gemini, uMakerAi.Chat.OpenAiResponses,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Layouts,
  FMX.Styles.Objects, FMX.Platform, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Menus, FMX.ListBox, uMakerAi.Chat.Mistral, uMakerAi.Chat.Ollama, uMakerAi.Chat.Groq, uMakerAi.Chat.Grok, uMakerAi.Whisper, uMakerAi.Utils.VoiceMonitor, System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, uMakerAi.RAG.Vectors, uMakerAi.ToolFunctions, uMakerAi.Embeddings.Core, uMakerAi.Embeddings, UMakerAi.MCPServer.Direct, uMakerAi.MCPServer.Core, UMakerAi.MCPServer.Http;

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
    CompletionModel: TEdit;
    ListBoxItem10: TListBoxItem;
    ChWebSearch: TCheckBox;
    ListBoxItem13: TListBoxItem;
    ChCodeInterpreter: TCheckBox;
    ListBoxItem14: TListBoxItem;
    ChExtractFiles: TCheckBox;
    ListBoxItem15: TListBoxItem;
    Label6: TLabel;
    EditReasoningEffort: TEdit;
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
    LayoutTokenUsageHistory: TLayout;
    Label18: TLabel;
    MemoTokenUsageHistory: TMemo;
    AiOpenChat1: TAiOpenChat;
    AiGeminiChat1: TAiGeminiChat;
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
  private
    FLastBubble: TChatBubble;
    FPrompt_tokens: Integer;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;
    FActiveModelEdit: TEdit;

    Function CopyToClipBoard(AMediaFile: TAiMediaFile): Boolean;
    Procedure AssignModel(Const DriverName, ModelName: String);
    Procedure InitModelCapabilitiesCombo(Edit: TEdit);
    Procedure AssignAiConnParams;
    function PackString(const Texto: string): TMemoryStream;  //para comprimir los archivos de salida
    function UnpackString(StreamComprimido: TStream): string; //para descomprimir los archivos de salida
    Function PackStream(StreamOrigen: TStream): TMemoryStream;
    Function UnpackStream(StreamComprimido: TStream): TMemoryStream;
    procedure SaveCombinedChatToStream(AContainerStream: TStream; AChatUIStream, AHistoryStream: TStream);
    procedure LoadCombinedChatFromStream(AContainerStream: TStream; out AChatUIStream: TStream; out AHistoryStream: TStream);
    procedure SaveRAGToStream(AContainerStream: TStream; ASourceDataStream: TStream);
    function LoadRAGFromStream(AContainerStream: TStream): TMemoryStream;
    Procedure AddLog(Value: String);
    procedure LoadMCPClientsFromJSON(AJsonString: string; AFunctions: TAiFunctions);

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

const
  // Opciones de versionado del formato creado para este modelo.
  MKCHAT_MAGIC_HEADER: array [0 .. 3] of AnsiChar = 'MKCH';
  MKCHAT_FORMAT_VERSION: Word = 1;

  MKRAG_MAGIC_HEADER: array [0 .. 3] of AnsiChar = 'MKRG';
  MKRAG_FORMAT_VERSION: Word = 1;

procedure TForm2.ac_aboutExecute(Sender: TObject);
begin
  //
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
        ShowMessage('Knowledge database loaded "'+OpenDialog1.FileName+'"');
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
   FunctionName : String;
begin
  FunctionName := AIToolCall.name;
  //LLamado por defecto a la función siempre se ejectua para cualquier función

end;

procedure TForm2.AiConnError(Sender: TObject; const ErrorMsg: string; Exception: Exception; const aResponse: IHTTPResponse);
begin

  TThread.Queue(nil,
    procedure
    begin
      AniIndicator1.Visible := False;
      AniIndicator1.Enabled := False;
      showMessage(ErrorMsg);
    end);
end;

procedure TForm2.AiConnReceiveData(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSONObject; aRole, aText: string);
begin

  if Not ChAsincrono.IsChecked then
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

  MemoTokenUsageHistory.Lines.Add(Format('Model: %s, Input: %d, Output %d, Total: %d',[aMsg.Model, aMsg.Prompt_tokens, aMsg.Completion_tokens, aMsg.Total_tokens]));

  FPrompt_tokens := FPrompt_tokens + aMsg.Prompt_tokens;
  FCompletion_tokens := FCompletion_tokens + aMsg.Completion_tokens;
  FTotal_tokens := FTotal_tokens + aMsg.Total_tokens;

  EditPromptTokens.Text := Format('%d', [FPrompt_tokens]);
  EditCompletionTokens.Text := Format('%d', [FCompletion_tokens]);
  EditTotalTokens.Text := Format('%d', [FTotal_tokens]);

  // aMsg.Model;

  if ChAsincrono.IsChecked then
    Exit;

  TThread.Queue(nil,
    procedure
    begin
      FLastBubble := ChatList1.AddBubble(aText, aRole, aMsg.MediaFiles);
      FLastBubble := Nil;
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
        ToolCall.Response := AiRAGVector1.SearchText(SearchText, 10, 0.3); //03 es mas permisivo para el indice que maneja RAG
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

procedure TForm2.AssignAiConnParams;
Var
  InputMediaFiles, OutputMediaFiles, ChatMediaSuports, Opt: String;
begin

  AiConn.Params.Clear;

  // Se asegura de configurar los parámetros antes de hacer la pregunta
  AiConn.Params.Values['Max_Tokens'] := StrToIntDef(EditMaxTokens.Text, 8000).ToString;
  AiConn.Params.Values['Temperature'] := (TrackTemperature.Value / 10).ToString;
  AiConn.Params.Values['Tool_Active'] := BoolToStr(ChUseTools.IsChecked, True);
  AiConn.Params.Values['Asynchronous'] := BoolToStr(ChAsincrono.IsChecked, True);
  AiConn.Params.Values['ResponseTimeOut'] := '240000';  //Aumenta el timeout


  If ChJSonFormat.IsChecked then
    AiConn.Params.Values['Response_format'] := 'tiaChatRfJson';
  //Else
  //  AiConn.Params.Values['Response_format'] := 'tiaChatRfText';

  AiConn.Params.Values['Voice'] := EditVoices.Text;
  AiConn.Params.Values['Voice_Format'] := EditVoiceFormat.Text;
  AiConn.Params.Values['ReasoningEffort'] := EditReasoningEffort.Text;

  InputMediaFiles := '';
  OutputMediaFiles := '';
  ChatMediaSuports := '';


  If ChWebSearch.IsChecked then
    ChatMediaSuports := ChatMediaSuports + ',Tcm_WebSearch';

  If ChCodeInterpreter.IsChecked then
    ChatMediaSuports := ChatMediaSuports + ',tcm_code_interpreter';

  If ChExtractFiles.IsChecked then
  Begin
    ChatMediaSuports := ChatMediaSuports + ',tcm_textFile';
    OutputMediaFiles := OutputMediaFiles + ',tfc_textFile';
  End;


  Opt := LowerCase(EditChatMediaSupports.Text);

  If Opt.Contains('image') then
    ChatMediaSuports := ChatMediaSuports + ',tcm_image';

  If Opt.Contains('audio') then
    ChatMediaSuports := ChatMediaSuports + ',tcm_audio';

  If Opt.Contains('video') then
    ChatMediaSuports := ChatMediaSuports + ',tcm_video';

  If Opt.Contains('pdf') then
    ChatMediaSuports := ChatMediaSuports + ',tcm_pdf';


    //------ NATIVE INPUT FILES ------------
  Opt := LowerCase(EditNativeInputFiles.Text);

  If Opt.Contains('image') then
    InputMediaFiles := InputMediaFiles + ',tfc_image';

  If Opt.Contains('audio') then
    InputMediaFiles := InputMediaFiles + ',tfc_audio';

  If Opt.Contains('video') then
    InputMediaFiles := InputMediaFiles + ',tfc_video';

  If Opt.Contains('pdf') then
    InputMediaFiles := InputMediaFiles + ',tfc_pdf';

    //------ NATIVE OUTPUT FILES ------------
  Opt := LowerCase(EditNativeOutputFiles.Text);

  If Opt.Contains('image') then
    OutputMediaFiles := OutputMediaFiles + ',tfc_image';

  If Opt.Contains('audio') then
    OutputMediaFiles := OutputMediaFiles + ',tfc_audio';

  If Opt.Contains('video') then
    OutputMediaFiles := OutputMediaFiles + ',tfc_video';

  If Opt.Contains('pdf') then
    OutputMediaFiles := OutputMediaFiles + ',tfc_pdf';

  if copy(InputMediaFiles,1,1) = ',' then
      InputMediaFiles := Copy(InputMediaFiles, 2, Length(InputMediaFiles));

  if copy(OutputMediaFiles,1,1) = ',' then
      OutputMediaFiles := Copy(OutputMediaFiles, 2, Length(OutputMediaFiles));

  if copy(ChatMediaSuports,1,1) = ',' then
      ChatMediaSuports := Copy(ChatMediaSuports, 2, Length(ChatMediaSuports));

  AiConn.Params.Values['NativeInputFiles'] := '[' + InputMediaFiles + ']';
  AiConn.Params.Values['NativeOutputFiles'] := '[' + OutputMediaFiles + ']';
  AiConn.Params.Values['ChatMediaSupports'] := '[' + ChatMediaSuports + ']';

  ShowMessage(AiConn.Params.Text);

end;

procedure TForm2.AssignModel(const DriverName, ModelName: String);
Var
  Max_Tokens: Integer;
  Tool_Active, Asynchronous, JSonFormat: Boolean;
  Temperature: Integer;
  MediaConf, sOpt: String;
begin

  // Clear old params
  AiConn.Params.Clear;

  AiConn.DriverName := DriverName;
  AiConn.Model := ModelName;

  // showMessage(AiConn.Params.Text);

  Max_Tokens := StrToIntDef(AiConn.Params.Values['Max_Tokens'], 8000);
  Temperature := Round(StrToIntDef(AiConn.Params.Values['Temperature'], 1) * 10);
  Tool_Active := UpperCase(AiConn.Params.Values['Tool_Active']) = 'TRUE';
  Asynchronous := UpperCase(AiConn.Params.Values['Asynchronous']) = 'TRUE';
  JSonFormat := UpperCase(AiConn.Params.Values['Response_format']) = UpperCase('tiaChatRfJson');

  ChAsincrono.IsChecked := Asynchronous;
  ChUseTools.IsChecked := Tool_Active;
  EditMaxTokens.Text := Max_Tokens.ToString;
  TrackTemperature.Value := Temperature;
  ChJSonFormat.IsChecked := JSonFormat;

  EditVoices.Text := AiConn.Params.Values['Voice'];
  EditVoiceFormat.Text := AiConn.Params.Values['Voice_Format'];
  EditReasoningEffort.Text := AiConn.Params.Values['ReasoningEffort'];

  // Native InputFiles
  MediaConf := LowerCase(AiConn.Params.Values['NativeInputFiles']);

  sOpt := '';
  If MediaConf.Contains(LowerCase('image')) then
    sOpt := sOpt + ',Image';

  If MediaConf.Contains(LowerCase('Audio')) then
    sOpt := sOpt + ',Audio';

  If MediaConf.Contains(LowerCase('Video')) then
    sOpt := sOpt + ',Video';

  If MediaConf.Contains(LowerCase('Pdf')) then
    sOpt := sOpt + ',Pdf';

  If Copy(sOpt, 1, 1) = ',' then
    sOpt := Copy(sOpt, 2, length(sOpt));

  EditNativeInputFiles.Text := sOpt;

  // NativeOutputFiles
  MediaConf := LowerCase(AiConn.Params.Values['NativeOutputFiles']);

  sOpt := '';
  If MediaConf.Contains(LowerCase('image')) then
    sOpt := sOpt + ',Image';

  If MediaConf.Contains(LowerCase('Audio')) then
    sOpt := sOpt + ',Audio';

  If MediaConf.Contains(LowerCase('Video')) then
    sOpt := sOpt + ',Video';

  If MediaConf.Contains(LowerCase('Pdf')) then
    sOpt := sOpt + ',Pdf';

  If Copy(sOpt, 1, 1) = ',' then
    sOpt := Copy(sOpt, 2, length(sOpt));

  EditNativeOutputFiles.Text := sOpt;

  // ChatMediaSuport
  MediaConf := LowerCase(AiConn.Params.Values['ChatMediaSupports']);

  //ChWebSearch.IsChecked := MediaConf.Contains(LowerCase('Tcm_WebSearch'));
  //ChCodeInterpreter.IsChecked := MediaConf.Contains(LowerCase('Tcm_code_interpreter'));
  //ChExtractFiles.IsChecked := MediaConf.Contains(LowerCase('Tcm_textFile'));


  ChWebSearch.IsChecked := AiConn.WebSearch;
  ChCodeInterpreter.IsChecked := AiConn.CodeInterpreter;
  ChExtractFiles.IsChecked := AiConn.ExtractTextFiles;


  sOpt := '';
  If MediaConf.Contains(LowerCase('image')) then
    sOpt := sOpt + ',Image';

  If MediaConf.Contains(LowerCase('Audio')) then
    sOpt := sOpt + ',Audio';

  If MediaConf.Contains(LowerCase('Video')) then
    sOpt := sOpt + ',Video';

  If MediaConf.Contains(LowerCase('Pdf')) then
    sOpt := sOpt + ',Pdf';

  If Copy(sOpt, 1, 1) = ',' then
    sOpt := Copy(sOpt, 2, length(sOpt));

  EditChatMediaSupports.Text := sOpt;

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
    LayoutTokenUsageHistory.Visible := Not LayoutTokenUsageHistory.Visible;
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
  Mf: TAiMediaFile;
begin

  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  AssignAiConnParams;

  If ChSendAudioToIA.IsChecked then
  Begin
    // Si hay un audio grabado por el control se adiciona, solo si la comunicación es solo audio ya que el texto se transcribió en APrompt
    If Assigned(aAudioStream) and (aAudioStream.size > 100) then
    Begin
      Mf := TAiMediaFile.Create;
      Mf.LoadFromStream('miaudio.wav', aAudioStream);
      aMediaFiles.Add(Mf);
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
  showMessage(AMediaFile.FileName);
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

    FActiveModelEdit.Text := List.CommaText;
  Finally
    List.Free;
  End;
end;

procedure TForm2.ComboDriverChange(Sender: TObject);
Var
  DriverName: String;
  List: TStringList;
begin
  If Assigned(ComboDriver.Selected) then
  Begin
    DriverName := Trim(ComboDriver.Text);
    AiConn.DriverName := DriverName;
    Cursor := crHourGlass;

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

    Tfc_Text, tfc_textFile, tfc_code_interpreter:
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

end;

procedure TForm2.EditChatMediaSupportsExit(Sender: TObject);
begin
  ModelCapabilitiesCombo.Visible := False;
end;

procedure TForm2.FormCreate(Sender: TObject);
Var
  List: TStringList;
begin

  FPrompt_tokens := 0;
  FCompletion_tokens := 0;
  FTotal_tokens := 0;
  ModelCapabilitiesCombo.Visible := False;
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
      LClientItem.Name := LServerName;
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
      showMessage('Este tipo de archivo no se puede copiar al portapapeles.');
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
    StreamOrigen.WriteBuffer(Datos[0], length(Datos));
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
  ZStream := TZCompressionStream.Create(AContainerStream); // *** MODIFICADO ***
  try
    ZStream.CopyFrom(ASourceDataStream, 0); // *** MODIFICADO ***
  finally
    ZStream.Free; // *** MODIFICADO ***
  end;
end;

end.
