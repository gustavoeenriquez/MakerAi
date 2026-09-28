unit uMakerAi.Embeddings.Qwen;

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

// Embeddings de Qwen (Alibaba Model Studio / DashScope). El endpoint
// compatible-mode/v1/embeddings acepta el mismo cuerpo que OpenAI, asi que se
// hereda TAiOpenAiEmbeddings y solo cambian URL, key y modelo.
// Verificado en runtime (sep 2026): text-embedding-v4 [default], text-embedding-v3,
// qwen3.7-text-embedding. Dimensiones de v4: 64, 128, 256, 512, 768, 1024 (default),
// 1536 y 2048. La key queda atada a la region: con otra URL el API responde 401.

interface

uses
  System.SysUtils, System.Classes,
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.Core, uMakerAi.Embeddings.OpenAi;

type
  TAiQwenEmbeddings = class(TAiOpenAiEmbeddings)
  public
    constructor Create(aOwner: TComponent); override;
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
  end;

procedure Register;

implementation

const
  GlQwenUrl = 'https://dashscope-intl.aliyuncs.com/compatible-mode/v1/';
  GlQwenModel = 'text-embedding-v4';
  GlQwenDimensions = 1024;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenEmbeddings]);
end;

{ TAiQwenEmbeddings }

constructor TAiQwenEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@DASHSCOPE_API_KEY';
  Url := GlQwenUrl;
  Model := GlQwenModel;
  Dimensions := GlQwenDimensions;
end;

class function TAiQwenEmbeddings.GetDriverName: string;
begin
  Result := 'Qwen';
end;

class function TAiQwenEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiQwenEmbeddings.Create(aOwner);
end;

class procedure TAiQwenEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey'] := '@DASHSCOPE_API_KEY';
  Params.Values['Url'] := GlQwenUrl;
  Params.Values['Model'] := GlQwenModel;
  Params.Values['Dimensions'] := IntToStr(GlQwenDimensions);
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiQwenEmbeddings);

end.
