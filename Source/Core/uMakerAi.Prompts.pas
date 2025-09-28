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
// Nombre: Gustavo Enr�quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Prompts;

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TAiPromptItem = Class(TCollectionItem)
  Private
    fNombre: String;
    FString: TStrings;
    fDescripcion: String;
    function GetString: TStrings;
  Protected
    Procedure SetStrings(aValue: TStrings);
    function GetDisplayName: string; Override;
  Public
    constructor Create(Collection: TCollection); Override;
  Published
    Property Nombre: String read fNombre Write fNombre;
    Property Descripcion: String read fDescripcion Write fDescripcion;
    Property Strings: TStrings Read GetString Write SetStrings; // stored;
  End;

  TAiPrompts = class(TComponent)
  private
    FItems: TCollection;
  protected
  public
    Constructor Create(aOwner: TComponent); Override;
    Function IndexOf(Nombre: String): Integer;
    Function GetString(Nombre: String): String;
    Function AddString(Nombre, Data: String): TAiPromptItem;
    Function GetTemplate(Nombre: String; Params: Array of String): String; Overload;
    Function GetTemplate(Nombre: String; Params: TStringList): String; Overload;
    Function GetTemplate(Nombre: String; Params: TJSonObject): String; Overload;

  published
    Property Items: TCollection Read FItems Write FItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiPrompts]);
end;

constructor TAiPromptItem.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  FString := TStringList.Create;
End;

Procedure TAiPromptItem.SetStrings(aValue: TStrings);
Begin
  FString.Assign(aValue);
End;

function TAiPromptItem.GetDisplayName: string;
Begin
  Result := fNombre;
End;


function TAiPromptItem.GetString: TStrings;
begin
  Result := FString;
end;

// *************************************************************
// *************************************************************

Constructor TAiPrompts.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  FItems := TCollection.Create(TAiPromptItem);
End;

Function TAiPrompts.IndexOf(Nombre: String): Integer;
Var
  I: Integer;
  Item: TAiPromptItem;
Begin
  Result := -1;
  For I := 0 to FItems.Count - 1 do
  Begin
    Item := TAiPromptItem(FItems.Items[I]);
    If (Item <> Nil) and (AnsiUpperCase(Nombre) = AnsiUpperCase(Item.Nombre)) then
    Begin
      Result := I;
      Break;
    End;
  End;
End;

Function TAiPrompts.GetString(Nombre: String): String;
Var
  I: Integer;
  Item: TAiPromptItem;
Begin
  Result := '';
  I := IndexOf(Nombre);
  If I >= 0 then
  Begin
    Item := TAiPromptItem(FItems.Items[I]);
    Result := Item.Strings.Text;
  End;
End;

function TAiPrompts.GetTemplate(Nombre: String; Params: TStringList): String;
Var
  I: Integer;
  Res, Nom, Val: String;
begin
  Res := GetString(Nombre);

  For I := 0 to Params.Count - 1 do
  Begin
    Nom := Params.Names[I];
    Val := Params.Values[Nom];
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;
end;

function TAiPrompts.GetTemplate(Nombre: String; Params: array of String): String;
Var
  I, P: Integer;
  Res, S, Nom, Val: String;
begin
  Res := GetString(Nombre);
  For I := 0 to Length(Params) - 1 do
  Begin
    S := Params[I];
    P := Pos('=',S);
    If  p <= 0 then
      Raise Exception.Create('Los par�metros deben tener la forma Nombre=Valor');

    Nom := Copy(S, 1, P - 1);
    Val := Copy(S, P + 1, Length(S));
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;

end;

function TAiPrompts.GetTemplate(Nombre: String; Params: TJSonObject): String;
Var
  Pair : TJSonPair;
  Res, Nom, Val: String;
begin
  Res := GetString(Nombre);
  For Pair in Params do
  Begin
    Nom := Pair.JsonString.Value;
    Val := Pair.JsonValue.Value;
    Res := StringReplace(Res,'<#'+Nom+'>',Val,[rfReplaceAll,rfIgnoreCase]);
  End;

  Result := Res;
end;

Function TAiPrompts.AddString(Nombre, Data: String): TAiPromptItem;
Var
  Item: TAiPromptItem;
Begin
  Item := TAiPromptItem(FItems.Add);
  Item.Nombre := Nombre;
  Item.Strings.Text := Data;
  Result := Item;
End;

end.
