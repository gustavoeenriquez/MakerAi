# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directory Purpose

This `Redis/` directory contains binary dependencies (audio conversion DLLs) for the MakerAI framework. It does not contain source code.

**Contents:**
- `ConvertToWave32.dll` - 32-bit audio format conversion
- `ConvertToWave64.dll` - 64-bit audio format conversion

**Deprecated (moved from the repository root to `DeprecatedDemos/` on 2026-11-06).** This file used to say the DLLs were used by `uMakerAi.Utils.VoiceMonitor.pas` and the Whisper integration; that is not true: neither unit (nor any other code in the repo) loads an external DLL. Kept for reference only.

## Main Project Documentation

For MakerAI development guidance, see the parent directory's documentation:
- `../../CLAUDE.md` - Main Claude Code instructions
- `../../README.md` - Project overview and installation guide
- `../../Docs/` - Detailed architecture and API documentation
