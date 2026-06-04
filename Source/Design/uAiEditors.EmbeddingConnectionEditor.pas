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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uAiEditors.EmbeddingConnectionEditor;

interface

uses
  System.Classes, System.SysUtils, DesignIntf, DesignEditors,
  uMakerAi.Embeddings.Connection;

type
  // Property Editor para la propiedad DriverName de TAiEmbeddingConnection
  TAiEmbeddingDriverNamePropertyEditor = class(TStringProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  // Component Editor para TAiEmbeddingConnection
  TAiEmbeddingConnectionComponentEditor = class(TDefaultEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs;

{ TAiEmbeddingDriverNamePropertyEditor }

function TAiEmbeddingDriverNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TAiEmbeddingDriverNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  Connection: TAiEmbeddingConnection;
  Drivers: TArray<string>;
  Driver: string;
begin
  Connection := GetComponent(0) as TAiEmbeddingConnection;
  if not Assigned(Connection) then
    Exit;

  try
    Proc('');
    Drivers := Connection.GetAvailableDrivers;
    for Driver in Drivers do
      Proc(Driver);
  except
    Proc('');
  end;
end;

{ TAiEmbeddingConnectionComponentEditor }

function TAiEmbeddingConnectionComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TAiEmbeddingConnectionComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Reset Parameters to Defaults';
  else
    Result := '';
  end;
end;

procedure TAiEmbeddingConnectionComponentEditor.ExecuteVerb(Index: Integer);
var
  Connection: TAiEmbeddingConnection;
begin
  Connection := Component as TAiEmbeddingConnection;
  if not Assigned(Connection) then
    Exit;

  case Index of
    0: begin
         try
           Connection.ResetParamsToDefaults;
           ShowMessage('Parameters reset to defaults for driver: ' + Connection.DriverName);
           Designer.Modified;
         except
           on E: Exception do
             ShowMessage('Error resetting parameters: ' + E.Message);
         end;
       end;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(string),
    TAiEmbeddingConnection,
    'DriverName',
    TAiEmbeddingDriverNamePropertyEditor
  );

  RegisterComponentEditor(
    TAiEmbeddingConnection,
    TAiEmbeddingConnectionComponentEditor
  );
end;

end.
