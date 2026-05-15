unit uMakerAi.Chat.Llamacpp;

// MIT License
//
// TAiLlamacppChat — local GGUF chat component for MakerAI.
//
// Loads a GGUF LLM via makerai.gen.dll and generates responses locally
// without any HTTP server. Fully API-compatible with all other TAiChat
// subclasses (OpenAI, Ollama, Claude, etc.).
//
// ── Required DLLs (copy alongside your .exe) ─────────────────────────────
//
//   makerai.gen.dll        — MakerAI wrapper (built from LlamaKit)
//                            Source: https://github.com/gustavoeenriquez/LlamaKit
//
//   llama.dll              — llama.cpp core  ┐
//   ggml.dll               — GGML base       │  Build b8660+ from
//   ggml-base.dll          — GGML helpers    │  https://github.com/ggml-org/llama.cpp
//   ggml-cpu.dll           — CPU backend     ┘
//
//   ggml-cuda.dll          — NVIDIA GPU backend (optional)
//
// ── Model files (GGUF) ───────────────────────────────────────────────────
//
//   Any instruction/chat model in GGUF format, for example:
//     Llama-3.2-3B-Instruct-Q4_K_M.gguf    (PromptStyle = psLlama3)
//     Qwen2.5-7B-Instruct-Q4_K_M.gguf      (PromptStyle = psChatML)
//     gemma-3-4b-it-Q4_K_M.gguf            (PromptStyle = psGemma)
//     phi-3-mini-4k-instruct.Q4_K_M.gguf   (PromptStyle = psChatML)
//
// ── Usage ─────────────────────────────────────────────────────────────────
//
//   Chat := TAiLlamacppChat.Create(nil);
//   Chat.ModelPath  := 'C:\models\Llama-3.2-3B-Instruct-Q4_K_M.gguf';
//   Chat.NGPULayers := 99;
//   Chat.PromptStyle := psLlama3;
//   Chat.SystemPrompt.Text := 'You are a helpful assistant.';
//   Response := Chat.AddMessageAndRun('Hello!', 'user', []);
//   Chat.Free;
//
// ── Design notes ──────────────────────────────────────────────────────────
//
//   Model loaded lazily on first generation call. Changing ModelPath or
//   DLLPath unloads the current model automatically.
//
//   NewChat resets the KV cache (calls LlamaGenReset) and clears the
//   message history via inherited.
//
//   Streaming: each token fires OnReceiveData as it is produced.
//   OnReceiveDataEnd fires once when generation completes.
//
//   PromptStyle selects the chat template. Use psCustom to supply your
//   own prefix/suffix strings for System, User, and Assistant turns.
//
// ─────────────────────────────────────────────────────────────────────────

interface

uses
  System.SysUtils, System.Classes,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Gen.Import,
  uMakerAi.ErrorCodes,
  uMakerAi.Core,
  UMakerAi.ParamsRegistry;

type
  { Chat template style — selects the prompt format for the loaded model. }
  TLlamacppPromptStyle = (
    psLlama3,   // Meta Llama 3/3.1/3.2/3.3 Instruct
    psChatML,   // ChatML: Qwen, Phi-3, Mistral Nemo, OpenHermes, etc.
    psGemma,    // Google Gemma 2/3 Instruct
    psAlpaca,   // Alpaca / Vicuna style (plain text, no special tokens)
    psCustom    // User-defined via Sys/User/Asst prefix+suffix properties
  );

  TAiLlamacppChat = class(TAiChat)
  private
    FGenerator:    TLlamaGenerator;
    FModelPath:    string;
    FNGPULayers:   Integer;
    FNCtx:         Integer;
    FDLLPath:      string;
    FPromptStyle:  TLlamacppPromptStyle;
    FSamplerMode:  TSamplerMode;
    FParam1:       Single;
    FParam2:       Single;
    // Template pieces — assigned by ApplyStyle or set manually (psCustom)
    FSysPrefix:    string;
    FSysSuffix:    string;
    FUserPrefix:   string;
    FUserSuffix:   string;
    FAsstPrefix:   string;
    FAsstSuffix:   string;
    FAsstOpening:  string;

    procedure SetModelPath(const Value: string);
    procedure SetDLLPath(const Value: string);
    procedure SetPromptStyle(const Value: TLlamacppPromptStyle);
    procedure ApplyStyle(Style: TLlamacppPromptStyle);
    procedure EnsureLoaded;
    procedure FreeGenerator;
    function  BuildPrompt: string;
  protected
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;

    class function GetDriverName: string; override;
    class function CreateInstance(aOwner: TComponent): TAiChat; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;

    { Clears message history and resets the KV cache. }
    procedure NewChat; override;

    { True when the model is loaded and ready. }
    function IsLoaded: Boolean;

    { Unloads the current model; next generation call reloads. }
    procedure UnloadModel;
  published
    { Full path to the GGUF model file. }
    property ModelPath: string read FModelPath write SetModelPath;

    { GPU layers to offload. 99 = all layers, 0 = CPU only. }
    property NGPULayers: Integer read FNGPULayers write FNGPULayers default 99;

    { Context window size in tokens. 0 = DLL default (4096). }
    property NCtx: Integer read FNCtx write FNCtx default 4096;

    { Path to makerai.gen.dll. Default: beside the .exe. }
    property DLLPath: string read FDLLPath write SetDLLPath;

    { Chat template format — must match the model family. }
    property PromptStyle: TLlamacppPromptStyle
      read FPromptStyle write SetPromptStyle default psLlama3;

    { Sampling algorithm. Default: smTempTopP (Temperature + Top-P). }
    property SamplerMode: TSamplerMode
      read FSamplerMode write FSamplerMode default smTempTopP;

    { Sampler extra param: min_p (smMinP), tau (sMirostat*), typical_p (smTypical). }
    property Param1: Single read FParam1 write FParam1;

    { Sampler extra param: min_keep (smMinP/smTypical), eta (sMirostat*). }
    property Param2: Single read FParam2 write FParam2;

    // ── Custom template pieces (only used when PromptStyle = psCustom) ──────
    property SysPrefix:   string read FSysPrefix   write FSysPrefix;
    property SysSuffix:   string read FSysSuffix   write FSysSuffix;
    property UserPrefix:  string read FUserPrefix  write FUserPrefix;
    property UserSuffix:  string read FUserSuffix  write FUserSuffix;
    property AsstPrefix:  string read FAsstPrefix  write FAsstPrefix;
    property AsstSuffix:  string read FAsstSuffix  write FAsstSuffix;

    { Token(s) that open the assistant turn at the end of the prompt. }
    property AsstOpening: string read FAsstOpening write FAsstOpening;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiLlamacppChat]);
end;

{ TAiLlamacppChat }

constructor TAiLlamacppChat.Create(aOwner: TComponent);
begin
  inherited;
  FNGPULayers  := 99;
  FNCtx        := 4096;
  FDLLPath     :=
    {$IFDEF MSWINDOWS}'makerai.gen.dll'
    {$ELSEIF DEFINED(ANDROID) or DEFINED(ANDROID64)}'libmakerai_gen.so'
    {$ELSE}'makerai.gen.so'{$ENDIF};
  FSamplerMode := smTempTopP;
  FParam1      := 0;
  FParam2      := 0;
  FPromptStyle := psLlama3;
  ApplyStyle(psLlama3);
  // Sensible defaults for local inference
  Temperature := 0.8;
  Top_p       := 0.95;
  Max_tokens  := 512;
end;

destructor TAiLlamacppChat.Destroy;
begin
  FreeGenerator;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TAiLlamacppChat.FreeGenerator;
begin
  FreeAndNil(FGenerator);
end;

procedure TAiLlamacppChat.SetModelPath(const Value: string);
begin
  if Value <> FModelPath then
  begin
    FModelPath := Value;
    FreeGenerator;
  end;
end;

procedure TAiLlamacppChat.SetDLLPath(const Value: string);
begin
  if Value <> FDLLPath then
  begin
    FDLLPath := Value;
    FreeGenerator;
  end;
end;

procedure TAiLlamacppChat.SetPromptStyle(const Value: TLlamacppPromptStyle);
begin
  if Value <> FPromptStyle then
  begin
    FPromptStyle := Value;
    if Value <> psCustom then
      ApplyStyle(Value);
  end;
end;

procedure TAiLlamacppChat.ApplyStyle(Style: TLlamacppPromptStyle);
begin
  // Note: <|begin_of_text|> / <s> BOS tokens are NOT included here because
  // the gen DLL tokenizes with add_special=True, which prepends BOS
  // automatically. Including it here would cause a double BOS.
  case Style of
    psLlama3:
    begin
      // Meta Llama 3.x Instruct
      FSysPrefix   := '<|start_header_id|>system<|end_header_id|>'#10#10;
      FSysSuffix   := '<|eot_id|>'#10;
      FUserPrefix  := '<|start_header_id|>user<|end_header_id|>'#10#10;
      FUserSuffix  := '<|eot_id|>'#10;
      FAsstPrefix  := '<|start_header_id|>assistant<|end_header_id|>'#10#10;
      FAsstSuffix  := '<|eot_id|>'#10;
      FAsstOpening := '<|start_header_id|>assistant<|end_header_id|>'#10#10;
    end;
    psChatML:
    begin
      // ChatML — Qwen, Phi-3, Mistral Nemo, OpenHermes, etc.
      FSysPrefix   := '<|im_start|>system'#10;
      FSysSuffix   := '<|im_end|>'#10;
      FUserPrefix  := '<|im_start|>user'#10;
      FUserSuffix  := '<|im_end|>'#10;
      FAsstPrefix  := '<|im_start|>assistant'#10;
      FAsstSuffix  := '<|im_end|>'#10;
      FAsstOpening := '<|im_start|>assistant'#10;
    end;
    psGemma:
    begin
      // Google Gemma 2/3 — system is merged into the first user turn
      FSysPrefix   := '';   // handled specially in BuildPrompt
      FSysSuffix   := '';
      FUserPrefix  := '<start_of_turn>user'#10;
      FUserSuffix  := '<end_of_turn>'#10;
      FAsstPrefix  := '<start_of_turn>model'#10;
      FAsstSuffix  := '<end_of_turn>'#10;
      FAsstOpening := '<start_of_turn>model'#10;
    end;
    psAlpaca:
    begin
      // Alpaca / Vicuna — plain text, no special tokens
      FSysPrefix   := '';
      FSysSuffix   := #10#10;
      FUserPrefix  := '### Instruction:'#10;
      FUserSuffix  := #10#10;
      FAsstPrefix  := '### Response:'#10;
      FAsstSuffix  := #10;
      FAsstOpening := '### Response:'#10;
    end;
    // psCustom: caller sets the properties directly; nothing to do here.
  end;
end;

procedure TAiLlamacppChat.EnsureLoaded;
begin
  if FGenerator <> nil then Exit;

  if FModelPath = '' then
    raise EGenError.Create(llamaErrInvalidArg,
      'TAiLlamacppChat: ModelPath is not set');

  FGenerator := TLlamaGenerator.Create(FDLLPath);
  FGenerator.Load(FModelPath, FNGPULayers, FNCtx);
end;

function TAiLlamacppChat.IsLoaded: Boolean;
begin
  Result := FGenerator <> nil;
end;

procedure TAiLlamacppChat.UnloadModel;
begin
  FreeGenerator;
end;

// -----------------------------------------------------------------------------
// BuildPrompt
//
// Assembles the full text prompt from:
//   1. System prompt (SystemPrompt.Text)
//   2. Conversation history (FMessages — includes AskMsg as the last entry)
//   3. Assistant opening token to trigger the model response
//
// Gemma is a special case: it has no separate system turn, so the system
// content is prepended to the first user message.
// -----------------------------------------------------------------------------
function TAiLlamacppChat.BuildPrompt: string;
var
  SB:        TStringBuilder;
  Msg:       TAiChatMessage;
  Sys:       string;
  FirstUser: Boolean;
begin
  SB := TStringBuilder.Create;
  try
    Sys := Trim(SystemPrompt.Text);

    if FPromptStyle = psGemma then
    begin
      // Gemma: no separate system turn — inject system into the first user turn
      FirstUser := True;
      for Msg in FMessages do
      begin
        if SameText(Msg.Role, 'user') then
        begin
          SB.Append(FUserPrefix);
          if FirstUser and (Sys <> '') then
          begin
            SB.Append(Sys).Append(#10#10);
            FirstUser := False;
          end;
          SB.Append(Msg.Prompt).Append(FUserSuffix);
        end
        else if SameText(Msg.Role, 'assistant') then
          SB.Append(FAsstPrefix).Append(Msg.Content).Append(FAsstSuffix);
      end;
    end
    else
    begin
      // All other styles: system as a dedicated first block
      if Sys <> '' then
        SB.Append(FSysPrefix).Append(Sys).Append(FSysSuffix);

      for Msg in FMessages do
      begin
        if SameText(Msg.Role, 'user') then
          SB.Append(FUserPrefix).Append(Msg.Prompt).Append(FUserSuffix)
        else if SameText(Msg.Role, 'assistant') then
          SB.Append(FAsstPrefix).Append(Msg.Content).Append(FAsstSuffix);
        // tool / function / system roles are skipped for local inference
      end;
    end;

    // End with the assistant opening to prompt the model to respond
    SB.Append(FAsstOpening);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// -----------------------------------------------------------------------------
// InternalRunCompletions — bypasses HTTP, generates locally via the DLL.
//
// Called by TAiChat.Run. FMessages already contains AskMsg as the last entry.
// Tokens are delivered to OnReceiveData as they arrive; OnReceiveDataEnd fires
// when generation is complete.
// -----------------------------------------------------------------------------
Function TAiLlamacppChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
var
  Prompt: string;
  Full:   TStringBuilder;
begin
  EnsureLoaded;
  Prompt := BuildPrompt;

  FTmpResponseText := '';
  Full := TStringBuilder.Create;
  try
    FGenerator.GenerateStream(
      Prompt, Max_tokens, Temperature, Top_p,
      FSamplerMode, FParam1, FParam2,
      procedure(const Piece: string; IsLast: Boolean; var Stop: Boolean)
      begin
        if IsLast then
        begin
          FTmpResponseText := Full.ToString;
          ResMsg.Content   := FTmpResponseText;
          DoDataEnd(ResMsg, 'assistant', FTmpResponseText);
        end
        else
        begin
          Full.Append(Piece);
          DoData(ResMsg, 'assistant', Piece);
        end;
        Stop := FAbort;
      end
    );
    Result := FTmpResponseText;
  finally
    Full.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TAiLlamacppChat.NewChat;
begin
  inherited;
  if FGenerator <> nil then
    FGenerator.Reset;
end;

class function TAiLlamacppChat.GetDriverName: string;
begin
  Result := 'Llamacpp';
end;

class function TAiLlamacppChat.CreateInstance(aOwner: TComponent): TAiChat;
begin
  Result := TAiLlamacppChat.Create(aOwner);
end;

class procedure TAiLlamacppChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ModelPath']    := '';
  Params.Values['NGPULayers']   := '99';
  Params.Values['NCtx']         := '4096';
  Params.Values['DLLPath']      :=
    {$IFDEF MSWINDOWS}'makerai.gen.dll'
    {$ELSEIF DEFINED(ANDROID) or DEFINED(ANDROID64)}'libmakerai_gen.so'
    {$ELSE}'makerai.gen.so'{$ENDIF};
  Params.Values['Temperature']  := '0.8';
  Params.Values['Top_p']        := '0.95';
  Params.Values['Max_tokens']   := '512';
  Params.Values['PromptStyle']  := '0';  // psLlama3
  Params.Values['SamplerMode']  := '1';  // smTempTopP
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiLlamacppChat);

end.
