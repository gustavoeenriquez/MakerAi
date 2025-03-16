unit uMakerAi.Chat.AiConn;

// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

//Esta es una versión Beta del Driver del chat, falta implementar el manejo de un historial de mensajes
//único para permitir el cambio del LM y continué con la conversación sin aparente cambios.
//Actualmente cada LM maneja su propia memoria


interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON, uMakerAi.ToolFunctions, uMakerAi.Core, uMakerAi.Chat;

type

  TAiChatConn = class(TComponent)
  private
    FCompletion_tokens: integer;
    FTotal_tokens: integer;
    FPrompt_tokens: integer;
    FChat: TAiChat;
    FInitialInstructions: TStrings;
    FMemory: TStrings;
    FMessages: TAiChatMessages;
    FOnCallToolFunction: TOnCallToolFunction;
    FOnReceiveDataEnd: TAiOpenChatDataEvent;
    FOnReceiveData: TAiOpenChatDataEvent;
    FOnProcessMediaFile: TAiOpenChatOnMediaFile;
    FOnBeforeSendMessage: TAiOpenChatBeforeSendEvent;
    FOnAddMessage: TAiOpenChatDataEvent;
    FOnInitChat: TAiOpenChatInitChat;
    function GetLastError: String;
    procedure SetChat(const Value: TAiChat);
    procedure SetCompletion_tokens(const Value: integer);
    procedure SetInitialInstructions(const Value: TStrings);
    procedure SetMemory(const Value: TStrings);
    procedure SetOnAddMessage(const Value: TAiOpenChatDataEvent);
    procedure SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetOnInitChat(const Value: TAiOpenChatInitChat);
    procedure SetOnProcessMediaFile(const Value: TAiOpenChatOnMediaFile);
    procedure SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
    procedure SetOnReceiveData(const Value: TAiOpenChatDataEvent);
    procedure SetPrompt_tokens(const Value: integer);
    procedure SetTotal_tokens(const Value: integer);
    function GetBusy: Boolean;
  Protected
    Procedure ValideChat;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: Array of TAiMediaFile): String; Overload;

    Function AddMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje y lo adiciona a la conversación
    Function NewMessage(aPrompt, aRole: String): TAiChatMessage; // Crea un mensaje pero no lo adiciona a la conversación
    Function Run(aMsg: TAiChatMessage = Nil): String; Virtual;
    Function GetLastMessage: TAiChatMessage;
    Function RemoveMesage(Msg: TAiChatMessage): Boolean; Overload;
    Function RemoveMesage(IdMsg: integer): Boolean; Overload;
    Procedure AddToMemory(Key, Value: String);
    Procedure RemoveFromMemory(Key: String);
    Procedure NewChat;
    Procedure Abort;
    Function GetModels: TStringList; Overload; Virtual;
    Function GetMessages: TJSonArray; Virtual;

    Property Messages: TAiChatMessages read FMessages;
    Property LastError: String read GetLastError;
    Property Busy: Boolean Read GetBusy;

  Published
    Property Chat: TAiChat read FChat write SetChat;
    Property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
    Property Prompt_tokens: integer read FPrompt_tokens write SetPrompt_tokens;
    Property Completion_tokens: integer read FCompletion_tokens write SetCompletion_tokens;
    Property Total_tokens: integer read FTotal_tokens write SetTotal_tokens;
    Property Memory: TStrings read FMemory Write SetMemory;

    Property OnReceiveData: TAiOpenChatDataEvent read FOnReceiveData write SetOnReceiveData;
    Property OnReceiveDataEnd: TAiOpenChatDataEvent read FOnReceiveDataEnd write SetOnReceiveDataEnd;
    Property OnAddMessage: TAiOpenChatDataEvent read FOnAddMessage write SetOnAddMessage;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property OnBeforeSendMessage: TAiOpenChatBeforeSendEvent read FOnBeforeSendMessage write SetOnBeforeSendMessage;
    Property OnInitChat: TAiOpenChatInitChat read FOnInitChat write SetOnInitChat;
    Property OnProcessMediaFile: TAiOpenChatOnMediaFile read FOnProcessMediaFile write SetOnProcessMediaFile;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiChatConn, TAiChatConfig]);
end;

{ TAiChat }

//Const
//  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure TAiChatConn.Abort;
begin
  ValideChat;
  FChat.Abort;
end;

function TAiChatConn.AddMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  ValideChat;
  Result := FChat.AddMessage(aPrompt, aRole);
end;

function TAiChatConn.AddMessageAndRun(aPrompt, aRole: String; aMediaFiles: array of TAiMediaFile): String;
begin
  ValideChat;
  Result := FChat.AddMessageAndRun(aPrompt, aRole, aMediaFiles);
end;

procedure TAiChatConn.AddToMemory(Key, Value: String);
begin
  FMemory.AddPair(Key, Value);
end;

procedure TAiChatConn.NewChat;
begin
  ValideChat;
  FChat.NewChat;
end;

function TAiChatConn.NewMessage(aPrompt, aRole: String): TAiChatMessage;
begin
  Result := TAiChatMessage.Create(aPrompt, aRole);
end;

constructor TAiChatConn.Create(Sender: TComponent);
begin
  inherited;
  FChat := Nil;
  FInitialInstructions := TStringList.Create;
  FMemory := TStringList.Create;
  FMessages := TAiChatMessages.Create;
end;

destructor TAiChatConn.Destroy;
begin
  FChat := Nil;
  FInitialInstructions.Free;
  FMemory.Free;
  FMessages.Free;
  inherited;
end;

function TAiChatConn.GetModels: TStringList;
begin
  ValideChat;
  Result := FChat.GetModels;
end;

procedure TAiChatConn.RemoveFromMemory(Key: String);
begin
  FMemory.Values[Key] := '';
end;

function TAiChatConn.RemoveMesage(Msg: TAiChatMessage): Boolean;
begin
  result:=(FMessages.IndexOf(Msg) >= 0);
  if result then
    FMessages.Remove(Msg);
end;

function TAiChatConn.RemoveMesage(IdMsg: integer): Boolean;
Var
  I: integer;
  Msg: TAiChatMessage;
begin
  For I := 0 to FMessages.Count - 1 do
  Begin
    Msg := FMessages[I];
    If Msg.Id = IdMsg then
    Begin
      FMessages.Remove(Msg);
      Break;
    End;
  End;
  Result := True;
end;

function TAiChatConn.Run(aMsg: TAiChatMessage = Nil): String;
begin
  ValideChat;
  Result := FChat.Run(aMsg)
end;

function TAiChatConn.GetBusy: Boolean;
begin
   If Assigned(FChat) then
      Result := FChat.Busy
   Else
      Result := False;
end;

function TAiChatConn.GetLastError: String;
begin
  If Assigned(FChat) then
    Result := FChat.LastError
  Else
    Result := '';
end;

function TAiChatConn.GetLastMessage: TAiChatMessage;
begin
  Result := Nil;
  If FMessages.Count > 0 then
    Result := FMessages[FMessages.Count - 1];
end;

procedure TAiChatConn.SetChat(const Value: TAiChat);
begin
  FChat := Value;

  If Assigned(FChat) then
  Begin
    FChat.OnCallToolFunction := FOnCallToolFunction;
    FChat.OnReceiveDataEnd := FOnReceiveDataEnd;
    FChat.OnReceiveData := FOnReceiveData;
    FChat.OnProcessMediaFile := FOnProcessMediaFile;
    FChat.OnBeforeSendMessage := FOnBeforeSendMessage;
    FChat.OnAddMessage := FOnAddMessage;
    FChat.OnInitChat := FOnInitChat;
  End
  Else
  Begin
    FChat.OnCallToolFunction := Nil;
    FChat.OnReceiveDataEnd := Nil;
    FChat.OnReceiveData := Nil;
    FChat.OnProcessMediaFile := Nil;
    FChat.OnBeforeSendMessage := Nil;
    FChat.OnAddMessage := Nil;
    FChat.OnInitChat := Nil;
  End;
end;

procedure TAiChatConn.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiChatConn.SetInitialInstructions(const Value: TStrings);
begin
  FInitialInstructions.Text := Value.Text;
end;

procedure TAiChatConn.SetMemory(const Value: TStrings);
begin
  FMemory.Text := Value.Text;
end;

procedure TAiChatConn.SetOnAddMessage(const Value: TAiOpenChatDataEvent);
begin
  FOnAddMessage := Value;

  If Assigned(FChat) then
    FChat.OnAddMessage := Value;
end;

procedure TAiChatConn.SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;

  If Assigned(FChat) then
    FChat.OnBeforeSendMessage := Value;
end;

procedure TAiChatConn.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;

  If Assigned(FChat) then
    FChat.OnCallToolFunction := Value;
end;

procedure TAiChatConn.SetOnInitChat(const Value: TAiOpenChatInitChat);
begin
  FOnInitChat := Value;

  If Assigned(FChat) then
    FChat.OnInitChat := Value;
end;

procedure TAiChatConn.SetOnProcessMediaFile(const Value: TAiOpenChatOnMediaFile);
begin
  FOnProcessMediaFile := Value;

  If Assigned(FChat) then
    FChat.OnProcessMediaFile := Value;

end;

procedure TAiChatConn.SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveDataEnd := Value;

  If Assigned(FChat) then
    FChat.OnReceiveDataEnd := Value;
end;

procedure TAiChatConn.SetOnReceiveData(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveData := Value;

  If Assigned(FChat) then
    FChat.OnReceiveData := Value;
end;

procedure TAiChatConn.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiChatConn.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
end;

procedure TAiChatConn.ValideChat;
begin
  If Not Assigned(FChat) then
    Raise Exception.Create('Debe asignar un chat al componente');
end;

function TAiChatConn.GetMessages: TJSonArray;
begin
  Result := FMessages.ToJSon;
end;

end.
