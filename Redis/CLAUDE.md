# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Directory Purpose

This `Redis/` directory contains binary dependencies (audio conversion DLLs) for the MakerAI framework. It does not contain source code.

**Contents:**
- `ConvertToWave32.dll` - 32-bit audio format conversion
- `ConvertToWave64.dll` - 64-bit audio format conversion

These DLLs are used by MakerAI's voice processing components (`uMakerAi.Utils.VoiceMonitor.pas`, Whisper integration).

## Main Project Documentation

For MakerAI development guidance, see the parent directory's documentation:
- `../CLAUDE.md` - Main Claude Code instructions
- `../Readme.md` - Project overview and installation guide
- `../Docs/` - Detailed architecture and API documentation
