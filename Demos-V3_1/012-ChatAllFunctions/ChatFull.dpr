program ChatFull;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  uMainChatFull in 'uMainChatFull.pas' {FrmChatFull},
  uApp.Settings in 'uApp.Settings.pas',
  uApp.Conversation in 'uApp.Conversation.pas',
  uApp.ProviderList in 'uApp.ProviderList.pas',
  uApp.ChatBridge in 'uApp.ChatBridge.pas',
  uFrm.Settings in 'uFrm.Settings.pas' {FrmSettings},
  uAudio.Player in 'uAudio.Player.pas',
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
  Application.CreateForm(TFrmChatFull, FrmChatFull);
  Application.Run;
end.
