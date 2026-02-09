# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this directory.

## Directory Purpose

`Config/` contains configuration files for the MakerAI framework. These files store API keys and other settings that are specific to each user's environment.

## Files

### .env.example (Template)
Template file for API key configuration. Users should:
1. Copy/rename to `.env`
2. Uncomment and fill in the API keys they need

This is an **alternative or complementary method** to registering API keys as system environment variables in Windows or Linux.

### .env (User-specific, not tracked by Git)
Active configuration file with real API keys. This file is excluded from version control via `.gitignore` (`*.env` rule) to prevent accidental exposure of sensitive credentials.

## How .env is Loaded in the Code

The `.env` file is loaded by `TEnvHelper` (unit `uEnvHelper.pas` in `Source/Compatibility/`):

- **Invocation**: `TEnvHelper.LoadEnv()` is called automatically from `uMakerAi.Chat.Initializations.pas` during framework initialization.
- **File search**: Starting from the executable's directory, it searches for `Config/.env` ascending up to **7 levels** in the directory tree.
- **Parsing rules**:
  - Lines starting with `#` are treated as comments (ignored)
  - Empty lines are ignored
  - Format: `KEY=VALUE` (spaces around `=` are trimmed)
  - Quoted values (`"value"` or `'value'`) are unquoted automatically
- **Effect**: Each `KEY=VALUE` pair is set as a **process-level environment variable** using `SetEnvironmentVariable()` (Windows) or `setenv()` (Linux/FPC).
- **Cross-platform**: Supports Windows (Delphi + FPC), Linux (FPC with libc bindings), and POSIX (Delphi).

### Usage example in code

```pascal
uses uEnvHelper;

// Load .env (usually done automatically by the framework)
TEnvHelper.LoadEnv();

// Read a variable
var ApiKey := TEnvHelper.GetEnv('OPENAI_API_KEY');
```

After `LoadEnv`, the keys are available via `GetEnvironmentVariable()`, which is used by `TAiChat.ApiKey` when the `@VAR_NAME` convention is used:

```pascal
// The framework resolves @OPENAI_API_KEY via GetEnvironmentVariable
AiConnection.ApiKey := '@OPENAI_API_KEY';
```

## Security Rules

- **NEVER** commit real API keys to the repository
- **NEVER** modify `.env.example` to include real keys
- When adding new providers, update `.env.example` with a commented placeholder following the pattern: `#NEW_PROVIDER_API_KEY=Tu_APIKey_De_NuevoProveedor`
