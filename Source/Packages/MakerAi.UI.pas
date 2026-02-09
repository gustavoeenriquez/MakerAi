{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MakerAi.UI;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMakerAi.UI.Common, uMakerAi.UI.ChatBubble, uMakerAi.UI.ChatList, 
  uMakerAi.UI.ChatInput, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uMakerAi.UI.ChatBubble', @uMakerAi.UI.ChatBubble.Register);
  RegisterUnit('uMakerAi.UI.ChatList', @uMakerAi.UI.ChatList.Register);
  RegisterUnit('uMakerAi.UI.ChatInput', @uMakerAi.UI.ChatInput.Register);
end;

initialization
  RegisterPackage('MakerAi.UI', @Register);
end.
