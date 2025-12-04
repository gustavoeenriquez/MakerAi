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
