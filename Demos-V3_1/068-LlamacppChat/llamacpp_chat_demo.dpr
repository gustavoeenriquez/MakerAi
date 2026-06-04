{ ===============================================================================
  MakerAI — TAiLlamacppChat Demo

  Demonstrates local GGUF chat using TAiLlamacppChat (uMakerAi.Chat.Llamacpp).
  Runs 3 scenarios without any HTTP server:

    1. Single-turn Q&A      — basic question / answer
    2. Multi-turn convo     — model remembers previous answers (context window)
    3. Context reset        — NewChat resets KV cache; fresh topic

  Model path configured via FALLBACK_MODEL below (or add llamakit.ini support).

  Template: Llama 3 Instruct  (PromptStyle = psLlama3)
  Requires makerai.gen.dll and llama.cpp DLLs in DLL_PATH.
=============================================================================== }
program llamacpp_chat_demo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Diagnostics,
  System.Net.HttpClient,
  Winapi.Windows,
  uMakerAi.Chat.Llamacpp,
  uMakerAi.Chat.Messages,
  uMakerAi.Core;

const
  // Path to makerai.gen.dll (and llama.cpp DLLs alongside it)
  DLL_PATH = 'E:\copilot\LlamaKit\Win64\Release\makerai.gen.dll';

  // Llama-3.2-3B-Instruct Q4_K_M from Ollama (via `ollama pull llama3.2`)
  FALLBACK_MODEL =
    'E:\IAs\Ollama\models\blobs\' +
    'sha256-dde5aa3fc5ffc17176b5e8bdc82f587b24b2678c6c66101bf7da77af9f7ccdff';

// =============================================================================
// TChat — wraps TAiLlamacppChat and streams tokens to the console.
// =============================================================================
type
  TChat = class
  private
    FChat: TAiLlamacppChat;
    FSW:   TStopwatch;
    FToks: Integer;
    procedure OnToken(const Sender: TObject; aMsg: TAiChatMessage;
                      aResponse: TJSONObject; aRole, aText: string);
    procedure OnDone (const Sender: TObject; aMsg: TAiChatMessage;
                      aResponse: TJSONObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string;
                      E: Exception; const AResponse: IHTTPResponse);
  public
    constructor Create(const AModel: string);
    destructor  Destroy; override;

    { Resets conversation + KV cache; sets system prompt and generation params. }
    procedure NewScenario(const ASystem: string;
                          MaxTok: Integer; Temp, TopP: Single);
    { Send one user turn. Pass MaxTok/Temp/TopP > 0 to override current params. }
    procedure Ask(const APrompt: string;
                  MaxTok: Integer = 0; Temp: Single = 0; TopP: Single = 0);
  end;

constructor TChat.Create(const AModel: string);
begin
  FChat                  := TAiLlamacppChat.Create(nil);
  FChat.DLLPath          := DLL_PATH;
  FChat.ModelPath        := AModel;
  FChat.NGPULayers       := 99;       // all layers on GPU; set 0 for CPU-only
  FChat.NCtx             := 4096;
  FChat.PromptStyle      := psLlama3;
  FChat.Asynchronous     := False;
  FChat.OnReceiveData    := OnToken;
  FChat.OnReceiveDataEnd := OnDone;
  FChat.OnError          := OnError;
end;

destructor TChat.Destroy;
begin
  FChat.Free;
  inherited;
end;

procedure TChat.OnToken(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
begin
  Write(aText);   // print each token piece as it arrives
  Inc(FToks);
end;

procedure TChat.OnDone(const Sender: TObject; aMsg: TAiChatMessage;
  aResponse: TJSONObject; aRole, aText: string);
var
  Ms: Int64;
begin
  Ms := FSW.ElapsedMilliseconds;
  Writeln;
  if Ms > 0 then
    Writeln(Format('    [%d tok | %d ms | %.1f tok/s]',
      [FToks, Ms, FToks * 1000.0 / Ms]))
  else
    Writeln(Format('    [%d tok]', [FToks]));
end;

procedure TChat.OnError(Sender: TObject; const ErrorMsg: string;
  E: Exception; const AResponse: IHTTPResponse);
begin
  Writeln;
  Writeln('  *** ERROR: ', ErrorMsg);
end;

procedure TChat.NewScenario(const ASystem: string;
  MaxTok: Integer; Temp, TopP: Single);
begin
  FChat.NewChat;                  // resets KV cache + message history
  FChat.SystemPrompt.Text := ASystem;
  FChat.Max_tokens        := MaxTok;
  FChat.Temperature       := Temp;
  FChat.Top_p             := TopP;
end;

procedure TChat.Ask(const APrompt: string;
  MaxTok: Integer; Temp, TopP: Single);
begin
  if MaxTok > 0 then FChat.Max_tokens  := MaxTok;
  if Temp    > 0 then FChat.Temperature := Temp;
  if TopP    > 0 then FChat.Top_p       := TopP;

  FToks := 0;
  FSW   := TStopwatch.StartNew;

  Writeln;
  Writeln('  [User]: ', APrompt);
  Write  ('  [Asst]: ');
  FChat.AddMessageAndRun(APrompt, 'user', []);
end;

// =============================================================================

procedure PrintSection(const Title: string);
var
  W: Integer;
begin
  Writeln;
  W := 58 - Length(Title);
  if W < 0 then W := 0;
  Writeln('  -- ', Title, ' ', StringOfChar('-', W));
end;

// =============================================================================
var
  Chat: TChat;
begin
  Writeln('================================================================');
  Writeln('  MakerAI — TAiLlamacppChat Demo');
  Writeln('  Driver: TAiLlamacppChat  (PromptStyle = psLlama3)');
  Writeln('  Model : Llama-3.2-3B-Instruct Q4_K_M');
  Writeln('================================================================');
  Writeln;
  Writeln('  DLL  : ', DLL_PATH);
  Writeln('  Model: ...', Copy(FALLBACK_MODEL, Length(FALLBACK_MODEL) - 11, 12));

  // Add the DLL directory to the search path so makerai.gen.dll can find
  // llama.dll and ggml.dll when loaded from a different working directory.
  SetCurrentDir(ExtractFilePath(DLL_PATH));
  SetDllDirectory(PChar(ExtractFilePath(DLL_PATH)));

  Chat := TChat.Create(FALLBACK_MODEL);
  try
    // ── Scenario 1: Single-turn Q&A ─────────────────────────────────────────
    PrintSection('Scenario 1 — Single-turn Q&A');
    Chat.NewScenario(
      'You are a concise and helpful assistant. Keep answers short (1-3 sentences).',
      96, 0.3, 0.9);
    Chat.Ask('What is the largest moon of Saturn?');

    // ── Scenario 2: Multi-turn conversation ─────────────────────────────────
    PrintSection('Scenario 2 — Multi-turn conversation');
    Chat.NewScenario('You are a helpful math tutor. Keep answers short.', 64, 0.1, 0.9);
    Chat.Ask('What is 17 multiplied by 6?');
    Chat.Ask('Now add 15 to that result.');
    Chat.Ask('Is the final result a prime number?', 80, 0.3, 0.9);

    // ── Scenario 3: Context reset, new topic ────────────────────────────────
    PrintSection('Scenario 3 — Context reset, new topic');
    Chat.NewScenario(
      'You are a creative storyteller. Write vivid, imaginative prose.',
      96, 0.9, 0.95);
    Chat.Ask('In one sentence, describe a sunset on Mars.');

  finally
    Chat.Free;
  end;

  Writeln;
  Writeln('Press Enter to exit.');
  Readln;
end.
