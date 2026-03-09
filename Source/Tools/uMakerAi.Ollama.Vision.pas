// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// -------------------------------------------------------------------------
// TAiOllamaVisionTool: Herramienta de descripcion de imagenes usando modelos
// de vision en Ollama (ej: gemma3:4b, llava:latest, moondream:latest).
//
// Implementa IAiVisionTool -> usable como ChatTool en TAiChat.VisionTool.
// El bridge de Fase 1 lo invoca automaticamente cuando el modelo principal
// no soporta imagenes nativamente (Tfc_Image not in NativeInputFiles).
//
// Diferencias con TAiOllamaOcrTool:
//   - Orientado a descripcion / comprension general de imagenes
//   - Prompt libre configurable (no formato OCR especifico)
//   - Temperature mayor (0.7 por defecto) para respuestas mas naturales
//   - Modelo por defecto: gemma3:4b
//
// Uso:
//   AiChat.VisionTool := TAiOllamaVisionTool.Create(Self);
//   (TAiOllamaVisionTool(AiChat.VisionTool)).Model := 'llava:latest';
// -------------------------------------------------------------------------

unit uMakerAi.Ollama.Vision;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.HttpClient, System.Net.HttpClientComponent, System.Net.URLClient,
  System.Threading, System.StrUtils,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages;

type
  { TAiOllamaVisionTool
    Descripcion de imagenes con modelos multimodales de Ollama.
    Implementa IAiVisionTool a traves de TAiVisionToolBase. }
  TAiOllamaVisionTool = class(TAiVisionToolBase)
  private
    FUrl: string;
    FModel: string;
    FPrompt: string;
    FApiKey: string;
    FTemperature: Double;
    FMaxTokens: Integer;
    FKeepAlive: string;
    FTimeout: Integer;
    function GetApiKey: string;
    procedure SetTimeout(const Value: Integer);
  protected
    procedure ExecuteImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    function InternalRunVision(aMediaFile: TAiMediaFile; ResMsg: TAiChatMessage; const AOverridePrompt: string): string;
  public
    constructor Create(AOwner: TComponent); override;

    { Metodos estaticos para uso directo sin chat }
    class function Describe(AMediaFile: TAiMediaFile; const APrompt: string = ''; const AUrl: string = ''): string;
    class function DescribeFromFile(const AFilePath: string; const APrompt: string = ''; const AUrl: string = ''): string;
    class function DescribeFromStream(AStream: TStream; const AFileName: string; const APrompt: string = ''; const AUrl: string = ''): string;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Url: string read FUrl write FUrl;
    property Model: string read FModel write FModel;
    property KeepAlive: string read FKeepAlive write FKeepAlive;
    { Prompt de descripcion enviado junto a la imagen }
    property Prompt: string read FPrompt write FPrompt;
    property Temperature: Double read FTemperature write FTemperature;
    property MaxTokens: Integer read FMaxTokens write FMaxTokens default 2048;
    property Timeout: Integer read FTimeout write SetTimeout;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI Tools', [TAiOllamaVisionTool]);
end;

{ TAiOllamaVisionTool }

constructor TAiOllamaVisionTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUrl := 'http://localhost:11434/';
  FModel := 'gemma3:4b';
  FPrompt := 'Describe this image in detail.';
  FKeepAlive := '5m';
  FTemperature := 0.7;
  FMaxTokens := 2048;
  FTimeout := 120000;
end;

function TAiOllamaVisionTool.GetApiKey: string;
begin
  if csDesigning in ComponentState then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiOllamaVisionTool.SetTimeout(const Value: Integer);
begin
  if Value <= 30000 then
    FTimeout := 30000
  else
    FTimeout := Value;
end;

procedure TAiOllamaVisionTool.ExecuteImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
var
  LPrompt: string;
begin
  // Capturar el prompt antes de cualquier operacion async para evitar
  // acceder a AskMsg cuando pueda estar liberado dentro del TTask.
  LPrompt := '';
  if Assigned(AskMsg) then
    LPrompt := AskMsg.Prompt;

  // Si IsAsync=True ya estamos en el hilo background del chat: ejecutar directo.
  // Si IsAsync=False estamos en el hilo principal: lanzar task para no bloquearlo.
  if IsAsync then
    InternalRunVision(aMediaFile, ResMsg, LPrompt)
  else
    TTask.Run(procedure begin InternalRunVision(aMediaFile, ResMsg, LPrompt); end);
end;

function TAiOllamaVisionTool.InternalRunVision(aMediaFile: TAiMediaFile; ResMsg: TAiChatMessage; const AOverridePrompt: string): string;
var
  HTTP: TNetHTTPClient;
  LBody: TStringStream;
  LRequestJson, LOptions, LMessage, LMsgResponse: TJSONObject;
  LMessagesArray, LImagesArray: TJSONArray;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LFinalPrompt, LContent, LActualApiKey: string;
begin
  Result := '';
  if not Assigned(aMediaFile) or not Assigned(ResMsg) then
    Exit;

  LFinalPrompt := IfThen(AOverridePrompt.IsEmpty, FPrompt, AOverridePrompt);
  if LFinalPrompt.IsEmpty then
    LFinalPrompt := 'Describe this image in detail.';

  HTTP := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  try
    LActualApiKey := GetApiKey;

    // Payload raiz
    LRequestJson.AddPair('model', FModel);
    LRequestJson.AddPair('stream', TJSONBool.Create(False));
    LRequestJson.AddPair('keep_alive', FKeepAlive);

    // Mensaje con imagen embebida como base64
    LMessagesArray := TJSONArray.Create;
    LMessage := TJSONObject.Create;
    LMessage.AddPair('role', 'user');
    LMessage.AddPair('content', LFinalPrompt);

    LImagesArray := TJSONArray.Create;
    LImagesArray.Add(aMediaFile.Base64);
    LMessage.AddPair('images', LImagesArray);

    LMessagesArray.Add(LMessage);
    LRequestJson.AddPair('messages', LMessagesArray);

    // Opciones de generacion
    LOptions := TJSONObject.Create;
    LOptions.AddPair('temperature', TJSONNumber.Create(FTemperature));
    LOptions.AddPair('num_predict', TJSONNumber.Create(FMaxTokens));
    LRequestJson.AddPair('options', LOptions);

    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      HTTP.ResponseTimeout := FTimeout;

      if not LActualApiKey.IsEmpty then
        HTTP.CustomHeaders['Authorization'] := 'Bearer ' + LActualApiKey;

      ReportState(acsReasoning, 'Ollama analizando imagen...');
      LResponse := HTTP.Post(FUrl + 'api/chat', LBody);
    finally
      LBody.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if not Assigned(LResponseJson) then
        Exit;
      try
        // Ollama /api/chat devuelve { "message": { "content": "..." } }
        if LResponseJson.TryGetValue('message', LMsgResponse) then
          LContent := LMsgResponse.GetValue<string>('content', '')
        else
          LContent := LResponseJson.GetValue<string>('response', '');

        ResMsg.Content := LContent;
        ResMsg.Role := 'assistant';
        ResMsg.Model := FModel;

        aMediaFile.Transcription := LContent;
        aMediaFile.Procesado := True;
        Result := LContent;

        ReportDataEnd(ResMsg, 'assistant', LContent, LResponseJson);
        ReportState(acsFinished, 'Vision completada');
      finally
        LResponseJson.Free;
      end;
    end
    else
      ReportError(Format('Ollama Vision HTTP %d: %s',
        [LResponse.StatusCode, LResponse.ContentAsString]), nil);
  finally
    LRequestJson.Free;
    HTTP.Free;
  end;
end;

{ --- Metodos estaticos --- }

class function TAiOllamaVisionTool.Describe(AMediaFile: TAiMediaFile; const APrompt, AUrl: string): string;
var
  LInstance: TAiOllamaVisionTool;
  LResMsg: TAiChatMessage;
begin
  Result := '';
  if not Assigned(AMediaFile) then
    Exit;
  LInstance := TAiOllamaVisionTool.Create(nil);
  LResMsg := TAiChatMessage.Create('', 'assistant');
  try
    if AUrl <> '' then
      LInstance.Url := AUrl;
    Result := LInstance.InternalRunVision(AMediaFile, LResMsg, APrompt);
  finally
    LResMsg.Free;
    LInstance.Free;
  end;
end;

class function TAiOllamaVisionTool.DescribeFromFile(const AFilePath, APrompt, AUrl: string): string;
var
  LMedia: TAiMediaFile;
begin
  Result := '';
  if not FileExists(AFilePath) then
    Exit;
  LMedia := TAiMediaFile.Create;
  try
    LMedia.LoadFromFile(AFilePath);
    Result := Describe(LMedia, APrompt, AUrl);
  finally
    LMedia.Free;
  end;
end;

class function TAiOllamaVisionTool.DescribeFromStream(AStream: TStream; const AFileName, APrompt, AUrl: string): string;
var
  LMedia: TAiMediaFile;
  LMemStream: TMemoryStream;
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;
  LMedia := TAiMediaFile.Create;
  LMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    LMemStream.LoadFromStream(AStream);
    LMedia.LoadFromStream(AFileName, LMemStream);
    Result := Describe(LMedia, APrompt, AUrl);
  finally
    LMemStream.Free;
    LMedia.Free;
  end;
end;

end.
