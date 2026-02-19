# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The Chat module contains LLM provider drivers for the MakerAI framework. Each driver implements provider-specific API communication, message formatting, streaming, and tool calling for a different LLM service.

## Module Structure

### Universal Connector
- `uMakerAi.Chat.AiConnection.pas` - `TAiChatConnection` component that abstracts all provider differences. Set `DriverName` property to switch providers without code changes. Manages tool integration (Shell, TextEditor, ComputerUse, Speech, Video, Vision, WebSearch, Image).

### Provider Drivers
Each inherits from `TAiChat` (defined in Core):

| File | Class | Provider |
|------|-------|----------|
| `uMakerAi.Chat.OpenAi.pas` | `TAiOpenChat` | OpenAI (GPT, o1, o3, Sora) |
| `uMakerAi.Chat.Claude.pas` | `TAiClaudeChat` | Anthropic Claude |
| `uMakerAi.Chat.Gemini.pas` | `TAiGeminiChat` | Google Gemini (Veo) |
| `uMakerAi.Chat.Ollama.pas` | `TAiOllamaChat` | Ollama (local models) |
| `uMakerAi.Chat.LMStudio.pas` | `TAiLMStudioChat` | LM Studio |
| `uMakerAi.Chat.Groq.pas` | `TAiGroqChat` | Groq |
| `uMakerAi.Chat.DeepSeek.pas` | `TAiDeepSeekChat` | DeepSeek |
| `uMakerAi.Chat.Mistral.pas` | `TAiMistralChat` | Mistral |
| `uMakerAi.Chat.Kimi.pas` | `TAiKimiChat` | Kimi |
| `uMakerAi.Chat.Grok.pas` | `TAiGrokChat` | xAI Grok |
| `uMakerAi.Chat.Cohere.pas` | `TCohereChat` | Cohere |
| `uMakerAi.Chat.GenericLLM.pas` | `TAiGenericLLM` | OpenAI-compatible APIs |

### Configuration
- `uMakerAi.Chat.Initializations.pas` - Driver registration and default model parameters. Uses `TAiChatFactory` to configure capabilities per model (media support, tool calling, thinking levels).

### Legacy (Deprecated)
- `uMakerAi.Chat.OpenAi_Deprecated.pas`
- `uMakerAi.Chat.Ollama_old.pas`, `uMakerAi.Chat.Ollama_old1.pas`
- `uMakerAi.Chat.Claude_beta.pas`

## Key Patterns

### Driver Implementation
All drivers implement these core methods:
- `Run()` - Execute chat completion (sync or async based on `Asynchronous` property)
- `GetMessages()` - Serialize message history to provider-specific JSON format
- `ParseChat()` - Parse provider response into `TAiChatMessage`
- `GetModels()` - Retrieve available models from API

Streaming drivers additionally implement:
- `ProcessStreamChunk()` - Handle SSE/streaming response chunks
- `OnInternalReceiveData` / `OnInternalReceiveDataEnd` - Event callbacks

### Parameter Registry
Model capabilities are configured via `TAiChatFactory.Instance.RegisterUserParam()`:
```delphi
// Global driver defaults
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens', '8000');

// Model-specific overrides
TAiChatFactory.Instance.RegisterUserParam('Ollama', 'llama3.2-vision:latest',
  'ChatMediaSupports', '[Tcm_Text, Tcm_Image]');
```

Key parameters:
- `NativeInputFiles` / `NativeOutputFiles` - Physical file types supported
- `ChatMediaSupports` - Logical capabilities (`Tcm_Text`, `Tcm_Image`, `Tcm_Audio`, `Tcm_Reasoning`, `Tcm_WebSearch`, `Tcm_CodeInterpreter`)
- `EnabledFeatures` - Features to enable (may use Bridge for unsupported features)
- `Tool_Active` - Enable function calling
- `ThinkingLevel` - Reasoning level (`tlLow`, `tlMedium`, `tlHigh`)

### Chat State Machine
Drivers follow states in `TAiChatState`:
```text
acsIdle → acsConnecting → acsReasoning → acsWriting → acsToolCalling → acsFinished/acsError
```

### Tool Calling Flow
1. Message sent with tool definitions
2. Model responds with `tool_use` block
3. `OnCallToolFunction` event fires
4. Tool result appended to messages
5. `Run()` called again for final response

## Dependencies

- `uMakerAi.Core.pas` - Base types (`TAiMediaFile`, `TAiFileCategory`, `TAiChatState`)
- `uMakerAi.Chat.pas` - Abstract `TAiChat` base class
- `uMakerAi.Chat.Messages.pas` - `TAiChatMessage`, `TAiChatMessages`
- `uMakerAi.Tools.Functions.pas` - `TAiFunctions`, `TAiToolsFunction`
- `uMakerAi.ParamsRegistry.pas` - `TAiChatFactory` for model configuration

## Adding a New Driver

1. Create `uMakerAi.Chat.NewProvider.pas`
2. Define class inheriting from `TAiChat`
3. Implement `Run()`, `GetMessages()`, `ParseChat()`, `GetModels()`
4. Register in `uMakerAi.Chat.Initializations.pas` initialization section
5. Configure default parameters via `TAiChatFactory.Instance.RegisterUserParam()`

## Provider-Specific Notes

### Claude
- Uses `x-anthropic-version` header and beta features via dynamic headers
- Supports context editing (`TClaudeContextConfig`) for long conversations
- Thinking/reasoning via `EnableThinking` and `ThinkingBudget`
- Citations support planned (see TODO in source)

### OpenAI
- Supports audio models (`gpt-4o-audio-preview`), image generation (DALL-E, gpt-image-1), video (Sora)
- Reasoning models (o1, o3, GPT-5) use `ThinkingLevel` parameter

### Ollama
- Default: text-only, no native tools
- Vision models (llava, llama3.2-vision, qwen2.5-vl) configured with `NativeInputFiles=[Tfc_Image]`
- Some models support native function calling (`Tool_Active=True`)

### Gemini
- Native web search, code interpreter capabilities
- Veo video generation support

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
