unit uMakerAi.Qwen.Rerank;

// MIT License
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

// Reranker de Qwen (Alibaba Model Studio) para TAiRAGVector.Reranker.
// API nativa: POST api/v1/services/rerank/text-rerank/text-rerank
//   {model, input:{query, documents[]}, parameters:{instruct}}
//   -> output.results[] = {index, relevance_score 0..1}
// Una sola llamada puntua hasta 500 documentos (el API rechaza 501); por encima
// se parte en lotes. Instruct describe la tarea y afina el puntaje (probado: el
// pasaje correcto sube de 0.85 a 0.91 con una instruccion contable).
// Verificado en runtime (sep 2026): qwen3-rerank. gte-rerank-v2 no existe en la
// region internacional y gte-rerank responde AccessDenied.

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  uMakerAi.RAG.Vectors;

type
  TAiQwenRAGReranker = class(TAiRAGRerankerBase)
  private
    FApiKey: string;
    FUrl: string;
    FModel: string;
    FInstruct: string;
    FMaxPassageChars: Integer;
    FBatchSize: Integer;
    FLastTokens: Integer;
    function GetApiKey: string;
    procedure ScoreBatch(const AQuery: string; const ATexts: TArray<string>; AFrom, ACount: Integer;
      var AScores: TArray<Double>);
  protected
    // Transporte: recibe el cuerpo JSON y devuelve el JSON de respuesta. Virtual
    // para poder probar el parseo sin red.
    function Post(const ABody: string): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function Score(const AQuery: string; const ATexts: TArray<string>): TArray<Double>; override;
    // Tokens facturados en la ultima llamada a Score
    property LastTokens: Integer read FLastTokens;
  published
    // '@VARIABLE' lee la key de esa variable de entorno
    property ApiKey: string read FApiKey write FApiKey;
    property Url: string read FUrl write FUrl;
    property Model: string read FModel write FModel;
    // Descripcion de la tarea en ingles, p.ej. 'Given a web search query, retrieve
    // relevant passages that answer the query'. Vacio = la del modelo.
    property Instruct: string read FInstruct write FInstruct;
    // Los pasajes se recortan a este largo antes de enviarlos
    property MaxPassageChars: Integer read FMaxPassageChars write FMaxPassageChars default 4000;
    // Documentos por llamada (maximo del API: 500)
    property BatchSize: Integer read FBatchSize write FBatchSize default 500;
  end;

procedure Register;

implementation

const
  GlRerankUrl = 'https://dashscope-intl.aliyuncs.com/api/v1/services/rerank/text-rerank/text-rerank';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenRAGReranker]);
end;

{ TAiQwenRAGReranker }

constructor TAiQwenRAGReranker.Create(AOwner: TComponent);
begin
  inherited;
  FApiKey := '@DASHSCOPE_API_KEY';
  FUrl := GlRerankUrl;
  FModel := 'qwen3-rerank';
  FMaxPassageChars := 4000;
  FBatchSize := 500;
end;

function TAiQwenRAGReranker.GetApiKey: string;
begin
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

function TAiQwenRAGReranker.Post(const ABody: string): string;
var
  Client: TNetHTTPClient;
  Body: TStringStream;
  Res: IHTTPResponse;
begin
  Client := TNetHTTPClient.Create(nil);
  Body := TStringStream.Create(ABody, TEncoding.UTF8);
  try
{$IF CompilerVersion >= 34}
    Client.SynchronizeEvents := False;
{$ENDIF}
    Client.ContentType := 'application/json';
    Res := Client.Post(FUrl, Body, nil, [TNetHeader.Create('Authorization', 'Bearer ' + GetApiKey)]);
    Result := Res.ContentAsString(TEncoding.UTF8);
    if Res.StatusCode <> 200 then
      raise Exception.CreateFmt('Qwen rerank %d: %s', [Res.StatusCode, Result]);
  finally
    Body.Free;
    Client.Free;
  end;
end;

procedure TAiQwenRAGReranker.ScoreBatch(const AQuery: string; const ATexts: TArray<string>;
  AFrom, ACount: Integer; var AScores: TArray<Double>);
var
  jBody, jInput, jParams, jRes: TJSONObject;
  jDocs, jResults: TJSONArray;
  V: TJSONValue;
  I, LIdx, LTokens: Integer;
  LText: string;
begin
  jBody := TJSONObject.Create;
  jRes := nil;
  try
    jBody.AddPair('model', FModel);
    jDocs := TJSONArray.Create;
    for I := AFrom to AFrom + ACount - 1 do
    begin
      LText := ATexts[I];
      if (FMaxPassageChars > 0) and (Length(LText) > FMaxPassageChars) then
        LText := Copy(LText, 1, FMaxPassageChars);
      jDocs.Add(LText);
    end;
    jInput := TJSONObject.Create;
    jInput.AddPair('query', AQuery);
    jInput.AddPair('documents', jDocs);
    jBody.AddPair('input', jInput);
    jParams := TJSONObject.Create;
    jParams.AddPair('return_documents', TJSONBool.Create(False));
    if FInstruct <> '' then
      jParams.AddPair('instruct', FInstruct);
    jBody.AddPair('parameters', jParams);

    V := TJSONObject.ParseJSONValue(Post(jBody.ToJSON));
    if not (V is TJSONObject) then
    begin
      V.Free;
      raise Exception.Create('Qwen rerank: la respuesta no es un objeto JSON');
    end;
    jRes := TJSONObject(V);
    if not jRes.TryGetValue<TJSONArray>('output.results', jResults) then
      raise Exception.Create('Qwen rerank: respuesta sin output.results: ' + Copy(jRes.ToJSON, 1, 300));
    for V in jResults do
      if V is TJSONObject then
      begin
        LIdx := TJSONObject(V).GetValue<Integer>('index', -1);
        if (LIdx >= 0) and (LIdx < ACount) then
          AScores[AFrom + LIdx] := TJSONObject(V).GetValue<Double>('relevance_score', 0);
      end;
    if jRes.TryGetValue<Integer>('usage.total_tokens', LTokens) then
      Inc(FLastTokens, LTokens);
  finally
    jBody.Free;
    jRes.Free;
  end;
end;

function TAiQwenRAGReranker.Score(const AQuery: string; const ATexts: TArray<string>): TArray<Double>;
var
  LFrom, LBatch: Integer;
begin
  FLastTokens := 0;
  SetLength(Result, Length(ATexts));
  // Un documento que el API no devuelva queda en 0 (menos relevante que cualquiera)
  for LFrom := 0 to High(Result) do
    Result[LFrom] := 0;
  if Length(ATexts) = 0 then
    Exit;
  LBatch := FBatchSize;
  if (LBatch <= 0) or (LBatch > 500) then
    LBatch := 500;
  LFrom := 0;
  while LFrom < Length(ATexts) do
  begin
    if LFrom + LBatch > Length(ATexts) then
      LBatch := Length(ATexts) - LFrom;
    ScoreBatch(AQuery, ATexts, LFrom, LBatch, Result);
    Inc(LFrom, LBatch);
  end;
end;

end.
