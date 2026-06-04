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

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMainMinimalChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Net.HttpClient,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  uMakerAi.Chat, uMakerAi.Chat.Ollama, uMakerAi.Chat.AiConnection, uMakerAi.Chat.Initializations;

type
  TForm12 = class(TForm)
    BtnOllama: TButton;
    BtnTAiChatConnection: TButton;
    procedure BtnOllamaClick(Sender: TObject);
    procedure AiChat1Error(Sender: TObject; const ErrorMsg: string; Exception: Exception; const AResponse: IHTTPResponse);
    procedure BtnTAiChatConnectionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

procedure TForm12.AiChat1Error(Sender: TObject; const ErrorMsg: string; Exception: Exception; const AResponse: IHTTPResponse);
begin
  ShowMessage(ErrorMsg);
end;

procedure TForm12.BtnOllamaClick(Sender: TObject);
Var
  AiModel: TAiOllamaChat;
  Prompt, Res, Role: String;
begin
  AiModel := TAiOllamaChat.Create(Self);
  Try
    AiModel.Model := 'gpt-oss:20b';
    AiModel.OnError := AiChat1Error;

    //--------- Ejemplo de parámetros adicionales ---------
    // AiModel.ApiKey := 'miapikey';
    // AiModel.Url := 'http://localhost:11434/';
    AiModel.Asynchronous := False;

    Prompt := 'saluda a la audiencia de nuestro podcast de MakerAi Delphi Suite';
    Role := 'user';

    Res := AiModel.AddMessageAndRun(Prompt, Role, []);

    ShowMessage(Res);

  Finally
    AiModel.Free;
  End;

end;

procedure TForm12.BtnTAiChatConnectionClick(Sender: TObject);
Var
  AiModel: TAiChatConnection;
  Prompt, Res, Role: String;
begin
  AiModel := TAiChatConnection.Create(Self);
  Try
    AiModel.DriverName := 'Ollama';
    AiModel.Model := 'gpt-oss:20b';
    AiModel.OnError := AiChat1Error;

    //--------- Ejemplo de parámetros adicionales ---------
    //AiModel.Params.Values['ApiKey'] := 'miapikey';
    //AiModel.Params.Values['Url'] := 'http://localhost:11434/';
    AiModel.Params.Values['Asynchronous'] := 'False';

    Prompt := 'saluda a la audiencia de nuestro podcast de MakerAi Delphi Suite';
    Role := 'user';

    Res := AiModel.AddMessageAndRun(Prompt, Role, []);

    ShowMessage(Res);

  Finally
    AiModel.Free;
  End;

end;

end.
