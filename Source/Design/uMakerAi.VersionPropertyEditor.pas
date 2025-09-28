unit uMakerAi.VersionPropertyEditor;

interface

uses
  System.SysUtils, System.Classes, DesignIntf, DesignEditors, VCL.Dialogs,
  uMakerAi.Chat.AiConnection;

type
  // Property Editor personalizado para la propiedad Version
  TVersionPropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    //function GetValue: string; override;
    //procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses
  uMakerAi.Dsg.AboutDialog;

{ TVersionPropertyEditor }

procedure Register;
begin
  // Registrar el Property Editor para la propiedad Version
  // de la clase TAiChatConnection
  RegisterPropertyEditor(TypeInfo(string), // Tipo de la propiedad (string)
    TAiChatConnection, // Clase que contiene la propiedad
    'Version', // Nombre de la propiedad
    TVersionPropertyEditor // Clase del Property Editor
    );

End;

function TVersionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  // paDialog permite mostrar el botón de tres puntos
  // paReadOnly hace que la propiedad no sea editable directamente
  Result := [paDialog, paReadOnly];
end;

procedure TVersionPropertyEditor.Edit;
var
  AboutForm: TAboutDialog;
begin
  // Crear y mostrar el diálogo About
  AboutForm := TAboutDialog.Create(nil);
  try
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

{
function TVersionPropertyEditor.GetValue: string;
begin
  // Mostrar la versión actual en el Object Inspector
   //$I uMakerAi.Version.inc
  //Result := MAKERAI_VERSION_FULL;
end;

procedure TVersionPropertyEditor.SetValue(const Value: string);
begin
  // No permitir cambio del valor (solo lectura)
  // Opcionalmente podrías mostrar un mensaje explicativo
end;
}

end.
