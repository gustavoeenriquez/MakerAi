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

unit uAiEditors.ChatConnectionEditor;

interface

uses
  System.Classes, System.SysUtils, DesignIntf, DesignEditors, uMakerAi.Chat.AiConnection;

type
  // Property Editor para la propiedad DriverName
  TAiChatDriverNamePropertyEditor = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  // Component Editor para TAiChatConnection (opcional, para futuras funciones)
  TAiChatConnectionComponentEditor = class(TDefaultEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs;

{ TAiChatDriverNamePropertyEditor }

function TAiChatDriverNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  // paValueList: Muestra lista desplegable
  // paSortList: Ordena la lista alfabéticamente
  // paMultiSelect: Permite selección múltiple (opcional)
  Result := [paValueList, paSortList];
end;

procedure TAiChatDriverNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  Connection: TAiChatConnection;
  Drivers: TArray<string>;
  Driver: string;
begin
  // Obtener la instancia del componente
  Connection := GetComponent(0) as TAiChatConnection;
  if not Assigned(Connection) then
    Exit;

  try
    // Agregar opción vacía
    Proc('');

    // Obtener drivers disponibles
    Drivers := Connection.GetAvailableDrivers;

    // Agregar cada driver a la lista
    for Driver in Drivers do
      Proc(Driver);
  except
    // Si hay error, al menos mostrar opción vacía
    Proc('');
  end;
end;

{ TAiChatConnectionComponentEditor }

function TAiChatConnectionComponentEditor.GetVerbCount: Integer;
begin
  Result := 1; // Una acción en el menú contextual
end;

function TAiChatConnectionComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Reset Parameters to Defaults';
  else
    Result := '';
  end;
end;

procedure TAiChatConnectionComponentEditor.ExecuteVerb(Index: Integer);
var
  Connection: TAiChatConnection;
begin
  Connection := Component as TAiChatConnection;
  if not Assigned(Connection) then
    Exit;

  case Index of
    0: begin
         try
           Connection.ResetParamsToDefaults;
           ShowMessage('Parameters reset to defaults for driver: ' + Connection.DriverName);
           Designer.Modified; // Marca el formulario como modificado
         except
           on E: Exception do
             ShowMessage('Error resetting parameters: ' + E.Message);
         end;
       end;
  end;
end;

procedure Register;
begin

  // Registrar el Property Editor para la propiedad DriverName
  RegisterPropertyEditor(
    TypeInfo(string),              // Tipo de la propiedad
    TAiChatConnection,             // Clase del componente
    'DriverName',                  // Nombre de la propiedad
    TAiChatDriverNamePropertyEditor // Clase del Property Editor
  );

  // Registrar el Component Editor (opcional)
  RegisterComponentEditor(
    TAiChatConnection,                    // Clase del componente
    TAiChatConnectionComponentEditor      // Clase del Component Editor
  );
end;

end.
