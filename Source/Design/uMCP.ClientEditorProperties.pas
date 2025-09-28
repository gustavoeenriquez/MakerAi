unit uMCP.ClientEditorProperties;

interface

uses
  System.SysUtils, DesignIntf, DesignEditors, System.UITypes,
  uMakerAi.ToolFunctions, // Para TMCPClientItem
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
