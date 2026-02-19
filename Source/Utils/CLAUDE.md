# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The Utils module provides cross-platform utility classes for MakerAI. These are standalone helpers that support AI agent capabilities but are not tied to specific LLM providers.

## Units

### uMakerAi.Utils.DiffUpdater
Applies unified diff patches to text content. Used by AI code editing tools.

Key classes:
- `TDiffParser` - Parses unified diff format into `TDiffHunk` objects
- `TDiffApplier` - Applies parsed hunks to original content with fuzzy matching

Usage:
```pascal
var
  Applier: TDiffApplier;
  NewContent, ErrorMsg: string;
begin
  Applier := TDiffApplier.Create;
  try
    if Applier.Apply(OriginalContent, DiffText, NewContent, ErrorMsg) then
      // NewContent contains the patched result
    else
      // ErrorMsg contains the failure reason
  finally
    Applier.Free;
  end;
end;
```

Features:
- Fuzzy hunk positioning (+/- 20 lines search radius)
- Tolerates missing/malformed hunk headers (fallback to content-based matching)
- Ignores trailing whitespace differences

### uMakerAi.Utils.Python
Executes Python scripts from Delphi using Python4Delphi (P4D).

Key class: `TUtilsPython`

Requirements:
- Python 3.10+ installed
- Python4Delphi component
- **64-bit compilation required** if Python is 64-bit (raises `EPythonArchitectureError` otherwise)

Usage:
```pascal
// Without parameters
Result := TUtilsPython.ExecuteScript('result = 2 + 2');

// With parameters (accessed as param1, param2, etc.)
Result := TUtilsPython.ExecuteScript('result = param1 ** param2', [2, 8]);
```

Rules:
- Scripts must assign output to variable named `result`
- Parameters are injected as `param1`, `param2`, etc.
- Supports String, Integer, Float, Boolean, DateTime variant types
- Global engine instance (`GlPythonEngine`) is reused across calls

### uMakerAi.Utils.ScreenCapture
Cross-platform screen capture for FMX applications.

Key class: `TScreenCapture` (all static methods)

Platform support:
- **Windows**: Full support with multi-monitor (virtual screen coordinates)
- **macOS**: Full support via CoreGraphics
- **Android**: Not implemented (requires Media Projection API)
- **iOS**: Not supported (system restriction)

Usage:
```pascal
// Full screen capture
Bitmap := TScreenCapture.CaptureFullScreen;

// Area capture
Bitmap := TScreenCapture.CaptureArea(TRect.Create(0, 0, 800, 600));

// Interactive selection (Windows only)
if TScreenCapture.SelectArea(SelectedRect) then
  Bitmap := TScreenCapture.CaptureArea(SelectedRect);
```

### uMakerAi.Utils.VoiceMonitor
Real-time voice activity detection component for speech-to-text workflows.

Key class: `TAIVoiceMonitor` (TComponent - can be dropped on forms)

Platform support:
- **Windows**: WaveIn API
- **Android**: AudioRecord with permission handling

State machine: `msIdle` -> `msCalibrating` -> `msMonitoring` -> (back to Idle or Error)

Key features:
- Auto-calibration of ambient noise level (3 seconds by default)
- Configurable sensitivity multipliers for start/stop thresholds
- Wake word detection support via `OnWakeWordCheck` event
- Real-time transcription fragments via `OnTranscriptionFragment`
- Outputs WAV format streams

Key properties:
- `Active` - Start/stop monitoring
- `SilenceDuration` - Ms of silence before speech end (default 1000)
- `WakeWordActive` / `WakeWord` - Enable wake word detection
- `SensitivityMultiplier` / `StopSensitivityMultiplier` - Threshold tuning

Key events:
- `OnCalibrated` - Fired after noise calibration completes
- `OnChangeState` - Speaking started/stopped
- `OnSpeechEnd` - Final WAV stream ready
- `OnTranscriptionFragment` - Incremental audio chunks during speech
- `OnUpdate` - Real-time sound level updates

Thread safety:
- Uses `TCriticalSection` for buffer access
- Events are dispatched via `TThread.Queue` to main thread

## Platform Considerations

Windows-specific code uses `{$IFDEF MSWINDOWS}` conditionals. Android uses `{$IFDEF ANDROID}`. When adding new utilities, follow this pattern for cross-platform support.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
