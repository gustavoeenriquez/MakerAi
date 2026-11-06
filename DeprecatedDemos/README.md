# Deprecated demos

Demos from earlier MakerAI versions, kept **for reference only**. They are not
maintained, are not part of `Demos/DemosVersion31.groupproj`, and may not
compile or run against the current framework (APIs, models and units have
changed since they were written). For working examples, use `Demos/`.

The folder keeps the original layout, so each project sits one level deeper
than it used to. Relative search paths such as `..\..\Source` inside the
`.dproj` files are therefore off by one; the global Library Path described in
the main README is what makes them find the framework, if they still compile.

| Folder | What it is | Replaced by |
|--------|------------|-------------|
| `Demos-V3_1/` | Demo set of MakerAI 3.1 | `Demos/` |
| `Demos-V3_4/` | Console/FMX demo set of MakerAI 3.4 | `Demos/` |
| `Demos-Book/` | Demos of the previous edition of the book *Inteligencia Artificial con Delphi* | `Demos/Libro/` (4.0 edition) |
| `Demos/010-Minimalchat` | Minimal FMX chat with Ollama | `Demos/Libro/cap03-primer-chat`, `Demos/094-ChatStreaming`, `Demos/095-ChatViewLadera` |
| `Demos/032-MCPServerDataSnap` | MCP server over DataSnap REST | `Demos/031-MCPServer` (HTTP/SSE/StdIO), `Demos/100-MCPLadera` |
| `Demos/035-MCPServerWithTAiFunctions` | `TAiFunctions` exposed as MCP tools | `Demos/036-MCPServerStdIO_AiFunction`, `Demos/100-MCPLadera` |
| `Demos/062-BidirectionalTranslator` | Call translator: STT -> LLM translation, text output | `Demos/071-VoiceBridgeTranslate` |
| `Demos/063-VoiceBridgeTranslator` | Voice bridge: STT -> translate -> TTS -> virtual cable | `Demos/071-VoiceBridgeTranslate` (`gpt-realtime-translate`, a third of the code) |
| `Demos/065-VoiceBridgeUI` | FMX front end for the 063 voice bridge | `Demos/071-VoiceBridgeTranslate` |

Moved here on 2026-11-06, while preparing MakerAI 4.0.
