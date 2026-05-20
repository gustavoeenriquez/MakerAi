# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MinimalChat is a Delphi FireMonkey (FMX) demonstration application showcasing integration with the MakerAI 3.x Suite - an AI orchestration framework for Delphi developers. It demonstrates two patterns for connecting to AI services (Ollama, OpenAI, Gemini, Claude, etc.).

## Build & Development

**IDE Required:** Delphi 11+ (Alexandria through Florence/Delphi 13)

**Building:**
- Open `MinimalChat.dproj` in Delphi IDE
- Select target platform (Win32, Win64, Android, Android64, Linux64)
- Build with F9 or Build menu

**Output directories:** `.\$(Platform)\$(Config)` (e.g., `.\Win64\Debug\`)

**Runtime dependency:** Ollama running on `localhost:11434` for local AI models

## Architecture

```
MinimalChat.dpr           # Entry point - initializes FMX application
uMainMinimalChat.pas      # Main form with AI integration logic
uMainMinimalChat.fmx      # FireMonkey UI definition (XML)
```

**Two AI Integration Patterns Demonstrated:**

1. **Direct Provider Class** (`TAiOllamaChat`):
   ```pascal
   AiModel := TAiOllamaChat.Create(Self);
   AiModel.Model := 'gpt-oss:20b';
   AiModel.Asynchronous := False;
   Res := AiModel.AddMessageAndRun(Prompt, Role, []);
   ```

2. **Generic Connection Factory** (`TAiChatConnection`):
   ```pascal
   AiModel := TAiChatConnection.Create(Self);
   AiModel.DriverName := 'Ollama';
   AiModel.Params.Values['Url'] := 'http://localhost:11434/';
   Res := AiModel.AddMessageAndRun(Prompt, Role, []);
   ```

## Key MakerAI Units

- `uMakerAi.Chat` - Core chat functionality
- `uMakerAi.Chat.Ollama` - Ollama provider (`TAiOllamaChat`)
- `uMakerAi.Chat.AiConnection` - Generic connection abstraction
- `uMakerAi.Chat.Initializations` - Framework initialization

## Naming Conventions

- **Classes:** `T` prefix (e.g., `TForm12`, `TAiOllamaChat`)
- **Units:** `u` prefix (e.g., `uMainMinimalChat`, `uMakerAi.Chat`)
- **Event handlers:** `[ComponentName][EventType]` (e.g., `BtnOllamaClick`, `AiChat1Error`)

## Error Handling Pattern

All AI errors route to a centralized `OnError` event:
```pascal
procedure TForm12.AiChat1Error(Sender: TObject; ErrorMsg: string;
  Exception: Exception; Response: IHTTPResponse);
begin
  ShowMessage(ErrorMsg);
end;
```

## Parent Project

This demo is part of MakerAI 3.x Suite. Full documentation at:
- Website: https://makerai.cimamaker.com
- GitHub: https://github.com/gustavoeenriquez/MakerAi

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
