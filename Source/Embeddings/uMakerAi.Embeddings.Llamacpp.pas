unit uMakerAi.Embeddings.Llamacpp;

// MIT License
//
// TAiLlamacppEmbeddings — local GGUF embedding component for MakerAI.
//
// Loads a GGUF model via makerai.embedder.dll and computes embeddings locally
// without any HTTP server.  Fully API-compatible with all other TAiEmbeddings
// subclasses (OpenAI, Ollama, Cohere, etc.).
//
// ── Required DLLs (copy alongside your .exe) ───────────────────────────────
//
//   makerai.embedder.dll   — MakerAI wrapper (built from LlamaKit)
//                            Source / releases: https://github.com/gustavoeenriquez/LlamaKit
//
//   llama.dll              — llama.cpp core  ┐
//   ggml.dll               — GGML base       │  Build b8660+ from
//   ggml-base.dll          — GGML helpers    │  https://github.com/ggml-org/llama.cpp
//   ggml-cpu.dll           — CPU backend     ┘  or download a pre-built release:
//                            https://github.com/ggml-org/llama.cpp/releases
//                            (choose the win-x64 asset that matches your hardware)
//
//   ggml-cuda.dll          — NVIDIA GPU backend (optional)
//                            Included in the CUDA-enabled llama.cpp release asset.
//                            Requires CUDA Toolkit 12.x runtime on the target machine:
//                            https://developer.nvidia.com/cuda-downloads
//
// ── Model files (GGUF) ─────────────────────────────────────────────────────
//
//   Any embedding model in GGUF format works, for example:
//     mxbai-embed-large-v1   1024-dim  https://huggingface.co/mixedbread-ai/mxbai-embed-large-v1
//     snowflake-arctic-embed 1024-dim  https://huggingface.co/Snowflake/snowflake-arctic-embed-l
//     nomic-embed-text-v1.5   768-dim  https://huggingface.co/nomic-ai/nomic-embed-text-v1.5-GGUF
//     all-MiniLM-L6-v2        384-dim  https://huggingface.co/sentence-transformers/all-MiniLM-L6-v2
//
//   Ollama blob files (sha256-<hex> in %LOCALAPPDATA%\Ollama\models\blobs\)
//   are also accepted — no .gguf extension required.
//
// ── Usage ──────────────────────────────────────────────────────────────────
//
//   Emb := TAiLlamacppEmbeddings.Create(nil);
//   Emb.ModelPath  := 'C:\models\mxbai-embed-large-v1-q4.gguf';
//   Emb.NGPULayers := 99;   // all layers on GPU; set 0 for CPU-only
//   Vec := Emb.CreateEmbedding('The quick brown fox', '');
//   Emb.Free;
//
// ── Design notes ───────────────────────────────────────────────────────────
//
//   The model is loaded lazily on the first call to CreateEmbedding.
//   Changing ModelPath or DLLPath at runtime automatically unloads the current
//   model so the next call reloads with the new settings.
//
//   TAiEmbeddingData = TArray<Double> (MakerAI convention).
//   makerai.embedder.dll returns TArray<Single> → converted here.
//
// ───────────────────────────────────────────────────────────────────────────

interface

uses
  System.SysUtils, System.Classes,
  uMakerAi.Embeddings.Core,
  uMakerAi.Embeddings,
  uMakerAi.Embedder.Import,
  uMakerAi.ErrorCodes,
  UMakerAi.ParamsRegistry;

type
  TAiLlamacppEmbeddings = class(TAiEmbeddings)
  private
    FEmbedder:   TEmbedder;
    FModelPath:  string;
    FNGPULayers: Integer;
    FDLLPath:    string;

    procedure SetModelPath(const Value: string);
    procedure SetDLLPath(const Value: string);
    procedure EnsureLoaded;
    procedure FreeEmbedder;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;

    { Computes the embedding locally using the loaded GGUF model.
      aModel, aUser, aDimensions and aEncodingFormat are accepted for
      interface compatibility but are ignored — the loaded model determines
      the vector dimensions. }
    function CreateEmbedding(aInput, aUser: String;
      aDimensions: Integer = -1; aModel: String = '';
      aEncodingFormat: String = 'float'): TAiEmbeddingData; override;

    { Factory support }
    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiEmbeddings; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;

    { True when the model is loaded and ready. }
    function IsLoaded: Boolean;

    { Unloads the current model; next call to CreateEmbedding will reload. }
    procedure UnloadModel;
  published
    { Full path to the GGUF model file (or Ollama blob path). }
    property ModelPath: string read FModelPath write SetModelPath;
    { Transformer layers to offload to GPU. 99 = all layers, 0 = CPU only. }
    property NGPULayers: Integer read FNGPULayers write FNGPULayers default 99;
    { Path to makerai.embedder.dll. Default: 'makerai.embedder.dll' (beside .exe). }
    property DLLPath: string read FDLLPath write SetDLLPath;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiLlamacppEmbeddings]);
end;

{ TAiLlamacppEmbeddings }

constructor TAiLlamacppEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  FNGPULayers := 99;
  FDLLPath    := 'makerai.embedder.dll';
  FModel      := '';
end;

destructor TAiLlamacppEmbeddings.Destroy;
begin
  FreeEmbedder;
  inherited;
end;

procedure TAiLlamacppEmbeddings.FreeEmbedder;
begin
  FreeAndNil(FEmbedder);
end;

procedure TAiLlamacppEmbeddings.SetModelPath(const Value: string);
begin
  if Value <> FModelPath then
  begin
    FModelPath := Value;
    FreeEmbedder;   // invalidate so next call reloads with new path
  end;
end;

procedure TAiLlamacppEmbeddings.SetDLLPath(const Value: string);
begin
  if Value <> FDLLPath then
  begin
    FDLLPath := Value;
    FreeEmbedder;   // invalidate so next call reloads from new DLL location
  end;
end;

procedure TAiLlamacppEmbeddings.EnsureLoaded;
begin
  if FEmbedder <> nil then Exit;

  if FModelPath = '' then
    raise EEmbedderError.Create(llamaErrInvalidArg,
      'TAiLlamacppEmbeddings: ModelPath is not set');

  FEmbedder := TEmbedder.Create(FDLLPath);
  FEmbedder.Load(FModelPath, FNGPULayers);

  // Sync the published Dimensions property with what the model reports
  FDimensions := FEmbedder.Dimensions;
end;

function TAiLlamacppEmbeddings.IsLoaded: Boolean;
begin
  Result := (FEmbedder <> nil) and (FEmbedder.Dimensions > 0);
end;

procedure TAiLlamacppEmbeddings.UnloadModel;
begin
  FreeEmbedder;
end;

function TAiLlamacppEmbeddings.CreateEmbedding(aInput, aUser: String;
  aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
var
  Vec: TArray<Single>;
  i:   Integer;
begin
  // If caller wired an OnGetEmbedding event, delegate to core handler
  if Assigned(OnGetEmbedding) then
  begin
    Result := inherited CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
    Exit;
  end;

  EnsureLoaded;

  Vec := FEmbedder.Embed(aInput);

  // Convert Single → Double (DLL returns Single; TAiEmbeddingData = TArray<Double>)
  SetLength(FData, Length(Vec));
  for i := 0 to High(Vec) do
    FData[i] := Vec[i];

  // Token counts are not available for local inference
  Fprompt_tokens := 0;
  Ftotal_tokens  := 0;

  Result := FData;
end;

class function TAiLlamacppEmbeddings.GetDriverName: string;
begin
  Result := 'Llamacpp';
end;

class function TAiLlamacppEmbeddings.CreateInstance(aOwner: TComponent): TAiEmbeddings;
begin
  Result := TAiLlamacppEmbeddings.Create(aOwner);
end;

class procedure TAiLlamacppEmbeddings.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ModelPath']  := '';
  Params.Values['NGPULayers'] := '99';
  Params.Values['DLLPath']    := 'makerai.embedder.dll';
  Params.Values['Model']      := '';
  Params.Values['Dimensions'] := '0';
end;

initialization
  TAiEmbeddingFactory.Instance.RegisterDriver(TAiLlamacppEmbeddings);

end.
