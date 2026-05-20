program MakerAIChat;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  uFrm.Main in 'uFrm.Main.pas' {FrmMain},
  uFrm.Settings in 'uFrm.Settings.pas' {FrmSettings},
  uApp.Settings in 'uApp.Settings.pas',
  uApp.ProviderList in 'uApp.ProviderList.pas',
  uApp.Conversation in 'uApp.Conversation.pas',
  uApp.ChatBridge in 'uApp.ChatBridge.pas',
  uMakerAi.Chat.Initializations,
  AIChat.Types in '..\..\Source\ChatUI\AIChat.Types.pas',
  AIChat.TempStore in '..\..\Source\ChatUI\AIChat.TempStore.pas',
  AIChat.Bubble in '..\..\Source\ChatUI\AIChat.Bubble.pas',
  AIChat.VirtualList in '..\..\Source\ChatUI\AIChat.VirtualList.pas',
  AIChat.Control.FMX in '..\..\Source\ChatUI\AIChat.Control.FMX.pas' {*: TSkCustomControl},
  AIChat.ScreenCapture in '..\..\Source\ChatUI\AIChat.ScreenCapture.pas',
  AIChat.Input in '..\..\Source\ChatUI\AIChat.Input.pas',
  uMakerAi.MD.Types in '..\..\Source\ChatUI\uMakerAi.MD.Types.pas',
  uMakerAi.MD.Parser in '..\..\Source\ChatUI\uMakerAi.MD.Parser.pas',
  uMakerAi.MD.Highlighter in '..\..\Source\ChatUI\uMakerAi.MD.Highlighter.pas',
  uMakerAi.MD.Renderer.Skia in '..\..\Source\ChatUI\uMakerAi.MD.Renderer.Skia.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
