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

//--- Modificaciones ----
// 30/08/2024 -- Mejora en la funci�n Edit


unit uMakerAi.Dalle;

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

  TAiDalleFile = Class(TObject)
  Private
    FImage: TMemoryStream;
    FBase64: String;
    Frevised_prompt: String;
    FUrlFile: String;
    function GetImage: TMemoryStream;
  Protected
    function Base64ToStream(Base64: String): TMemoryStream;
    Function LoadImage(UrlFile: String): TMemoryStream;
    Procedure ParseImage(JObj: TJSonObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;

  Published
    Property Revised_Prompt: String read Frevised_prompt;
    Property Base64: String read FBase64;
    Property UrlFile: String read FUrlFile;
    Property Image: TMemoryStream Read GetImage;
  End;

  TAiDalleFiles = Array of TAiDalleFile;

  TAiDalle = Class(TComponent)
  Private
    FApiKey: String;
    Frevised_prompt: String;
    FImages: TAiDalleFiles;
    FPrompt: String;
    FUrl: String;
    FResponseFormat: TAiImageResponseFormat;
    FHdQuality: Boolean;
    FStyleFormat: TAiImageAStyleFormat;
    FUseDalle3: Boolean;
    FUser: String;
    procedure SetApiKey(const Value: String);
    procedure Setrevised_prompt(const Value: String);
    procedure SetImages(const Value: TAiDalleFiles);
    procedure SetUrl(const Value: String);
    procedure SetResponseFormat(const Value: TAiImageResponseFormat);
    procedure SetHdQuality(const Value: Boolean);
    procedure SetStyleFormat(const Value: TAiImageAStyleFormat);
    procedure SetUseDalle3(const Value: Boolean);
    procedure SetUser(const Value: String);
  Protected
    Procedure ParseGenerate(JObj: TJSonObject);
    Procedure ParseVariations(JObj: TJSonObject);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function Generate(aPrompt: String; aSize: TAiImageSize; N: Integer): TAiDalleFile;
    Function Edit(aImage, aMask: TMemoryStream; aPrompt: String; aSize: TAiImageSize; N: Integer): TAiDalleFile;
    Function Variation(aImage: TMemoryStream; aSize: TAiImageSize; N: Integer): TAiDalleFile;

  Published
    Property Url: String read FUrl write SetUrl;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Revised_Prompt: String read Frevised_prompt write Setrevised_prompt;
    Property Images: TAiDalleFiles read FImages write SetImages;
    Property Prompt: String Read FPrompt;
    Property ResponseFormat: TAiImageResponseFormat read FResponseFormat write SetResponseFormat;
    Property HdQuality: Boolean read FHdQuality write SetHdQuality;
    Property StyleFormat: TAiImageAStyleFormat read FStyleFormat write SetStyleFormat;
    Property UseDalle3: Boolean read FUseDalle3 write SetUseDalle3;
    Property User: String read FUser write SetUser;
  End;


implementation


{ TAiImagesFiles }

Function TAiDalleFile.Base64ToStream(Base64: String): TMemoryStream;
begin
  Result := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(Base64));
end;

constructor TAiDalleFile.Create;
begin
  Inherited Create;
  FImage := TMemoryStream.Create;
end;

destructor TAiDalleFile.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TAiDalleFile.GetImage: TMemoryStream;
begin
  If (FImage.Size <= 0) then
  Begin
    If UrlFile <> '' then
      LoadImage(UrlFile);
  End;

  Result := FImage;
end;

Function TAiDalleFile.LoadImage(UrlFile: String): TMemoryStream;
Var
  NetHttp: TNetHTTPClient;
  Resp: IHTTPResponse;
Begin
  Result := Nil;
  FImage.Clear;
  FImage.Position := 0;
  NetHttp := TNetHTTPClient.Create(Nil);
  try
    Resp := NetHttp.Get(UrlFile, FImage);
    FImage.Position := 0;
    if Resp.StatusCode = 200 then // file was found
    Begin
      Result := FImage;
    End;
  finally
    NetHttp.Free;
  end;
End;

procedure TAiDalleFile.ParseImage(JObj: TJSonObject);
begin
  If JObj.TryGetValue<String>('url', FUrlFile) then
  Begin

  End;

  If JObj.TryGetValue<String>('b64_json', FBase64) then
  Begin
    FImage.Free;
    FImage := Base64ToStream(FBase64);
    FImage.Position := 0;
  End;

  JObj.TryGetValue<String>('revised_prompt', Frevised_prompt);

end;


{ TAiImages }

constructor TAiDalle.Create(aOwner: TComponent);
begin
  Inherited;
  SetLength(FImages, 0);
  FUrl := GlOpenAIUrl;
  FHdQuality := False;
  FResponseFormat := tiaRUrl;
  FStyleFormat := tiaStyleVivid;
  FUseDalle3 := True;
  FUser := 'user';
end;

destructor TAiDalle.Destroy;
Var
  i: Integer;
begin
  For i := 0 to Length(FImages) - 1 do
    FImages[i].Free;

  SetLength(FImages, 0);
  inherited;
end;

function TAiDalle.Edit(aImage, aMask: TMemoryStream; aPrompt: String; aSize: TAiImageSize; N: Integer): TAiDalleFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJSonObject;
begin
  sUrl := FUrl + 'images/edits';

  aImage.Position := 0;

  If Assigned(aMask) then
    aMask.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        If Assigned(aMask) then
          Body.AddStream('mask', aMask, ExtractFileName('mask.png'));

        Body.AddField('prompt', aPrompt);
        Body.AddField('user', FUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versi�n
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // A�adir el header de autorizaci�n
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post deber�a cuidar del Content-Type autom�ticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];
        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

function TAiDalle.Generate(aPrompt: String; aSize: TAiImageSize; N: Integer): TAiDalleFile;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // prompt           string Required
  // model            string Optional dall-e-2 dall-e-3
  // n                integer or null Optional Defaults to 1 The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported.
  // quality          string Optional Defaults to standard The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image. This param is only supported for dall-e-3.
  // response_format  string or null Optional Defaults to url The format in which the generated images are returned. Must be one of url or b64_json.
  // size             string or null Optional Defaults to 1024x1024 The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models.
  // style            string or null Optional Defaults to vivid The style of the generated images. Must be one of vivid or natural. Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images. This param is only supported for dall-e-3.
  // user             string Optional A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more.

  FPrompt := aPrompt;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'images/generations';

  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('prompt', aPrompt);

    If FUseDalle3 then
    Begin
      JObj.AddPair('model', 'dall-e-3');
      N := 1;

      Case aSize of
        TiaSize256:
          aSize := TiaSize1024; // peque�a
        TiaSize512:
          aSize := TiaSize1024_1792; // Mediana
        TiaSize1024:
          aSize := TiaSize1792_1024; // Grande
      End;

    End
    else
    Begin
      JObj.AddPair('model', 'dall-e-2');
      Case aSize of
        TiaSize1024:
          aSize := TiaSize256;
        TiaSize1024_1792:
          aSize := TiaSize512;
        TiaSize1792_1024:
          aSize := TiaSize1024;
      End;
    End;

    JObj.AddPair('n', N);

    If HdQuality then
      JObj.AddPair('quality', 'hd');

    Case aSize of
      TiaSize256:
        JObj.AddPair('size', '256x256');
      TiaSize512:
        JObj.AddPair('size', '512x512');
      TiaSize1024:
        JObj.AddPair('size', '1024x1024');
      TiaSize1024_1792:
        JObj.AddPair('size', '1024x1792');
      TiaSize1792_1024:
        JObj.AddPair('size', '1792x1024');
    End;

    If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
      JObj.AddPair('response_format', 'url')
    else
      JObj.AddPair('response_format', 'b64_json');

    If FStyleFormat = TAiImageAStyleFormat.tiaStyleVivid then
      JObj.AddPair('style', 'vivid')
    Else
      JObj.AddPair('style', 'natural');

    JObj.AddPair('user', FUser);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseGenerate(JObj);
      Result := FImages[0];
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

procedure TAiDalle.ParseGenerate(JObj: TJSonObject);
Var
  Data: TJSonArray;
  JObj1: TJSonObject;
  i: Integer;
  Image: TAiDalleFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJSonObject(Data.Items[i]);
    Image := TAiDalleFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiDalle.ParseVariations(JObj: TJSonObject);
Var
  Data: TJSonArray;
  JObj1: TJSonObject;
  i: Integer;
  Image: TAiDalleFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJSonObject(Data.Items[i]);
    Image := TAiDalleFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiDalle.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiDalle.SetHdQuality(const Value: Boolean);
begin
  FHdQuality := Value;
end;

procedure TAiDalle.SetImages(const Value: TAiDalleFiles);
begin
  FImages := Value;
end;

procedure TAiDalle.SetResponseFormat(const Value: TAiImageResponseFormat);
begin
  FResponseFormat := Value;
end;

procedure TAiDalle.Setrevised_prompt(const Value: String);
begin
  Frevised_prompt := Value;
end;

procedure TAiDalle.SetStyleFormat(const Value: TAiImageAStyleFormat);
begin
  FStyleFormat := Value;
end;

procedure TAiDalle.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiDalle.SetUseDalle3(const Value: Boolean);
begin
  FUseDalle3 := Value;
end;

procedure TAiDalle.SetUser(const Value: String);
begin
  FUser := Value;
end;

function TAiDalle.Variation(aImage: TMemoryStream; aSize: TAiImageSize; N: Integer): TAiDalleFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJSonObject;
begin
  sUrl := FUrl + 'images/variations';

  aImage.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        Body.AddField('user', FUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versi�n
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // A�adir el header de autorizaci�n
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post deber�a cuidar del Content-Type autom�ticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin

          JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];

        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;



end.
