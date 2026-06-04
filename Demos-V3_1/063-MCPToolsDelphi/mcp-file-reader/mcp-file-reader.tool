{
  "tools": [
    {
      "name": "file_list",
      "description": "List files and directories in a path. Supports wildcard patterns and optional recursive listing. Returns name, size, and modification date.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "Directory path to list. Use \".\" for current directory."
          },
          "pattern": {
            "type": "string",
            "description": "File name filter pattern (wildcard). Examples: \"*.pas\", \"*.txt\". Default: \"*\" (all files)"
          },
          "includeHidden": {
            "type": "boolean",
            "description": "Include hidden files (starting with dot). Default: false"
          },
          "recursive": {
            "type": "boolean",
            "description": "Recursively list subdirectories. Default: false"
          }
        },
        "required": [
          "path"
        ]
      }
    },
    {
      "name": "file_info",
      "description": "Get metadata about a file or directory: existence, size, creation time, modification time, attributes.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "Path to the file or directory to inspect"
          }
        },
        "required": [
          "path"
        ]
      }
    },
    {
      "name": "file_read",
      "description": "Read the contents of a text file. Optionally specify a line range (StartLine and EndLine, 1-based) to read only part of the file. Returns the file content as text.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "path": {
            "type": "string",
            "description": "Absolute or relative path to the file to read"
          },
          "startLine": {
            "type": "integer",
            "description": "First line to read (1-based). Default: 1 (start of file)"
          },
          "endLine": {
            "type": "integer",
            "description": "Last line to read (1-based, inclusive). Default: 0 = read all lines from StartLine"
          },
          "encoding": {
            "type": "string",
            "description": "File encoding hint: \"utf8\" (default), \"utf16\", \"ansi\""
          }
        },
        "required": [
          "path"
        ]
      }
    }
  ]
}