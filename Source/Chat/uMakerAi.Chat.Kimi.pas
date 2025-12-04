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

unit uMakerAi.Chat.Kimi;


interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core;

type
  TAiKimiChat = class(TAiChat)
  private
  protected
    //function InitChatCompletions: String; override;
  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
  published
  end;

procedure Register;

implementation

const
  GlAIUrl = 'https://api.moonshot.ai/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiKimiChat]);
end;

{ TAiKimiChat }

class function TAiKimiChat.GetDriverName: string;
begin
  Result := 'Kimi';
end;

class procedure TAiKimiChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@KIMI_API_KEY');
  Params.Add('Model=kimi-k2');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=' + GlAIUrl);
end;

class function TAiKimiChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiKimiChat.Create(Sender);
end;

constructor TAiKimiChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@KIMI_API_KEY';
  Model := 'kimi-k2';
  Url := GlAIUrl;
end;

destructor TAiKimiChat.Destroy;
begin
  inherited;
end;

{function TAiKimiChat.InitChatCompletions: String;
var
  AJSONObject: TJSONObject;
  Lista: TStringList;
  JStop, JTools: TJSONArray;
  ToolChoice: TJSONObject;
  I: Integer;
  LAsincronico: Boolean;
  LModel: String;
begin
  if User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'kimi-k2';

  // Streaming permitido solo si no hay tool-calling activo
  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSONObject.Create;
  Lista := TStringList.Create;
  try
    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));
    AJSONObject.AddPair('messages', GetMessages);
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    if Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    // 🔧 Tool Calling (compatibilidad futura o experimental)
    if Tool_Active then
    begin
      JTools := GetTools; // ya definido en TAiChat base
      if (JTools <> nil) and (JTools.Count > 0) then
        AJSONObject.AddPair('tools', JTools);

      if ToolChoice <> nil then
        ToolChoice.Free;
      ToolChoice := TJSONObject.Create;
      ToolChoice.AddPair('type', Tool_Choice_Type); // e.g. "auto" / "required" / "none"
      if Tool_Choice_Name <> '' then
        ToolChoice.AddPair('function', TJSONObject.Create.AddPair('name', Tool_Choice_Name));

      AJSONObject.AddPair('tool_choice', ToolChoice);
    end;

    // Stops
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    end;

    Result := StringReplace(AJSONObject.ToJSON, '\/', '/', [rfReplaceAll]);
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;
}

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiKimiChat);

end.

