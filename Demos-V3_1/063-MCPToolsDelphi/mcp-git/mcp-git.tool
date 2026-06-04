{
  "tools": [
    {
      "name": "git_command",
      "description": "Execute any git command and return its output. Provide only the subcommand (without \"git\" prefix). Examples: \"status\", \"log --oneline -5\", \"diff\", \"branch -a\". CAUTION: Destructive commands (reset --hard, push --force) are allowed \u00e2\u20ac\u201d use with care.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "command": {
            "type": "string",
            "description": "Git subcommand and arguments. Do NOT include the \"git\" prefix. Examples: \"status\", \"log --oneline -10\", \"diff HEAD\", \"branch -a\", \"show HEAD:file.txt\""
          },
          "workingDir": {
            "type": "string",
            "description": "Working directory (repository root). Default: current directory"
          },
          "timeout": {
            "type": "integer",
            "description": "Command timeout in seconds. Default: 15"
          }
        },
        "required": [
          "command"
        ]
      }
    },
    {
      "name": "git_clone",
      "description": "Clone a git repository. Supports shallow clones and specific branch checkout.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "url": {
            "type": "string",
            "description": "Repository URL to clone"
          },
          "destination": {
            "type": "string",
            "description": "Local destination directory. Default: directory name derived from URL"
          },
          "branch": {
            "type": "string",
            "description": "Branch or tag to checkout. Default: repository default branch"
          },
          "depth": {
            "type": "integer",
            "description": "Shallow clone depth (0 = full history). Default: 0"
          }
        },
        "required": [
          "url"
        ]
      }
    }
  ]
}