{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MakerAiDsg;

{$warn 5023 off : no warning about unused units}
interface

uses
  uAiEditors.ChatConnectionEditor, uMakerAi.Dsg.AboutDialog, 
  uMakerAi.VersionPropertyEditor, uMCP.ClientEditorProperties, 
  uMCPClientEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uAiEditors.ChatConnectionEditor', 
    @uAiEditors.ChatConnectionEditor.Register);
  RegisterUnit('uMakerAi.VersionPropertyEditor', 
    @uMakerAi.VersionPropertyEditor.Register);
  RegisterUnit('uMCP.ClientEditorProperties', 
    @uMCP.ClientEditorProperties.Register);
end;

initialization
  RegisterPackage('MakerAiDsg', @Register);
end.
