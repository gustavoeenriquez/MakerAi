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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Vision;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Core;

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

type

  TAiVision = Class(TComponent)
  Private
    FApiKey: String;
    Fcompletion_tokens: Integer;
    FModel: String;
    Ftotal_tokens: Integer;
    FFinish_reason: String;
    FRole: String;
    Fid: String;
    FContent: String;
    FPrompt_Tokenes: Integer;
    FUrl: String;
    procedure SetApiKey(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetModel(const Value: String);
  Protected
    Function ParseVision(Response: TJSonObject): String;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function GenerateByUrl(aPrompt, aUrl: String; aMax_tokens: Integer = 4000; aDetail: Boolean = False): String;
    function GenerateByBase64(aPrompt, aBase64: String; aMax_tokens: Integer = 4000; aDeetail: Boolean = False): String;
    function GenerateByStream(aPrompt: String; aStream: TMemoryStream; aMax_tokens: Integer = 4000; aDetail: Boolean = False): String;

  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property id: String read Fid;
    Property Model: String read FModel write SetModel;
    Property Finish_reason: String read FFinish_reason;
    Property role: String read FRole;
    Property Content: String read FContent;
    Property Prompt_Tokenes: Integer read FPrompt_Tokenes;
    Property completion_tokens: Integer read Fcompletion_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
    Property Url: String read FUrl write SetUrl;
  End;


implementation

{ TAiVision }

constructor TAiVision.Create(aOwner: TComponent);
begin
  Inherited;
  Url := GlOpenAIUrl;
  FModel := 'gpt-4o';
  FRole := 'user';
end;

destructor TAiVision.Destroy;
begin

  inherited;
end;

function TAiVision.GenerateByBase64(aPrompt, aBase64: String; aMax_tokens: Integer = 4000; aDeetail: Boolean = False): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJSonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  ImagePayload: TStringStream;
  St: TStringStream;
  sUrl, aBody: String;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'chat/completions';
  JObj := TJSonObject.Create;

  JMessages := TJSonArray.Create;
  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', FModel);
    JObj.AddPair('max_tokens', 4000);

    JMsg := TJSonObject.Create;
    JMsg.AddPair('role', FRole);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', aPrompt);
    JContent.Add(Obj1);

    ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:image/jpeg;base64,' + aBase64 + '"}}', TEncoding.UTF8);
    try
      JContent.Add(TJSonObject.ParseJSONValue(ImagePayload.DataString) as TJSonObject);
    finally
      ImagePayload.Free;
    end;

    JMsg.AddPair('content', JContent);
    JMessages.Add(JMsg);
    JObj.AddPair('messages', JMessages);

    aBody := UTF8Encode(JObj.ToJSON);
    aBody := StringReplace(aBody, '\/', '/', [rfReplaceAll]);
    aBody := StringReplace(aBody, '\r\n', '', [rfReplaceAll]);

    St.WriteString(aBody);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    St.SaveToFile('c:\temp\peticionvision.txt');
    St.Position := 0;


    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseVision(JObj);
      Result := Self.Content;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

function TAiVision.GenerateByStream(aPrompt: String; aStream: TMemoryStream; aMax_tokens: Integer; aDetail: Boolean): String;
Var
  Base64: String;
begin
  Base64 := StreamToBase64(aStream);
  Result := GenerateByBase64(aPrompt, Base64, aMax_tokens, aDetail);
end;

function TAiVision.GenerateByUrl(aPrompt, aUrl: String; aMax_tokens: Integer; aDetail: Boolean): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJSonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'chat/completions';
  JObj := TJSonObject.Create;

  JMessages := TJSonArray.Create;

  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', FModel);
    JObj.AddPair('max_tokens', aMax_tokens);

    JMsg := TJSonObject.Create;
    JMsg.AddPair('role', FRole);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', aPrompt);
    JContent.Add(Obj1);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'image_url');
    Obj1.AddPair('image_url', TJSonObject.Create.AddPair('url', aUrl));
    JContent.Add(Obj1);

    JMsg.AddPair('content', JContent);

    JMessages.Add(JMsg);

    JObj.AddPair('messages', JMessages);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Result := ParseVision(JObj);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

Function TAiVision.ParseVision(Response: TJSonObject): String;
Var
  id, Model, role: String;
  // Prompt_Tokenes, completion_tokens, total_tokens, index: Integer;
  JArr: TJSonArray;
  i: Integer;

begin

  id := Response.GetValue<String>('id');
  Model := Response.GetValue<String>('model');
  FPrompt_Tokenes := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('prompt_tokens');
  Fcompletion_tokens := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('completion_tokens');
  Ftotal_tokens := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('total_tokens');
  JArr := Response.GetValue<TJSonArray>('choices');
  FContent := '';

  For i := 0 to JArr.Count - 1 do
  Begin
    role := JArr.Items[i].GetValue<TJSonObject>('message').GetValue<String>('role');
    FContent := FContent + JArr.Items[i].GetValue<TJSonObject>('message').GetValue<String>('content') + sLineBreak;
  End;
  FContent := Trim(FContent);
  Result := FContent;

end;

procedure TAiVision.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiVision.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiVision.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;


end.
