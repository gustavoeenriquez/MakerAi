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

unit uMCP.ClientEditorProperties;

interface

uses
  System.SysUtils, DesignIntf, DesignEditors, System.UITypes,
  uMakerAi.Tools.Functions, // Para TMCPClientItem
  uMCPClientEditor; // Para TFMCPClientEditor, asumiendo que está en esta unidad

type
  TMCPClientItemEditor = class(TClassProperty) // Usamos TClassProperty porque editamos un objeto
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses
  System.Classes, Vcl.Dialogs, uMakerAi.Core,
  uMakerAi.MCPClient.Core; // Necesario para Get/SetProperties

procedure Register;
begin
  // Registramos el editor para la propiedad "Configuration" de la clase TMCPClientItem
  RegisterPropertyEditor(TypeInfo(string), // El tipo de la propiedad (string)
    TMCPClientItem, // La clase que contiene la propiedad
    'Configuration', // El nombre de la propiedad
    TMCPClientItemEditor // Nuestra clase editora
    );
end;

{ TMCPClientItemEditor }

procedure TMCPClientItemEditor.Edit;
var
  ClientItem: TMCPClientItem;
  EditorForm: TFMCPClientEditor;
  Props, VarEnv: TStringList;
begin
  // GetComponent(0) devuelve la instancia de TMCPClientItem que se está editando.
  ClientItem := GetComponent(0) as TMCPClientItem;

  // Creamos el formulario editor
  EditorForm := TFMCPClientEditor.Create(Nil);
  try
    // 1. Cargamos el estado actual del ClientItem en el formulario
    EditorForm.EditName.Text := ClientItem.Name;
    EditorForm.EditProtocol.ItemIndex := Ord(ClientItem.TransportType);

    Props := TStringList.Create;
    VarEnv := TStringList.Create;
    try
      // Puedes añadir también los TStrings de Params y DisabledFunctions si quieres
      Props.AddStrings(ClientItem.Params);
      VarEnv.AddStrings(ClientItem.EnvVars);

      EditorForm.SetProperties(Props, VarEnv);
    finally
      Props.Free;
      VarEnv.Free;
    end;

    // 2. Mostramos el formulario de forma modal
    if EditorForm.ShowModal = mrOk then
    begin
      // 3. Si el usuario presiona OK, actualizamos el ClientItem original
      ClientItem.Name := EditorForm.EditName.Text;
      ClientItem.TransportType := TToolTransportType(EditorForm.EditProtocol.ItemIndex);

      Props := TStringList.Create;
      VarEnv := TStringList.Create;
      try
        EditorForm.GetProperties(Props, VarEnv);
        ClientItem.Params.Assign(Props);
        ClientItem.EnvVars.Assign(VarEnv);
        Modified;
      finally
        Props.Free;
        VarEnv.Free;
      end;
    end;
  finally
    EditorForm.Free;
  end;
end;

function TMCPClientItemEditor.GetAttributes: TPropertyAttributes;
begin
  // paDialog: Muestra el botón [...]
  // paReadOnly: Hace que el valor no se pueda editar directamente en la rejilla.
  Result := [paDialog, paReadOnly];
end;

end.
