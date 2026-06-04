{
  "tools": [
    {
      "name": "regex_replace",
      "description": "Replace all occurrences of a pattern in text with a replacement string. Supports capture group back-references ($1, $2, ...) in the replacement.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to perform replacements in"
          },
          "pattern": {
            "type": "string",
            "description": "Regular expression pattern to search for"
          },
          "replacement": {
            "type": "string",
            "description": "Replacement string. Use $1, $2 etc. for capture group back-references"
          },
          "options": {
            "type": "string",
            "description": "Options: comma-separated list of: \"i\" (case insensitive), \"m\" (multiline), \"s\" (single-line), \"x\" (extended). Default: none"
          }
        },
        "required": [
          "text",
          "pattern",
          "replacement"
        ]
      }
    },
    {
      "name": "regex_match",
      "description": "Test a regular expression against text. Returns matched groups or all matches if MultiLine=true. Returns empty result if no match found.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to search in"
          },
          "pattern": {
            "type": "string",
            "description": "Regular expression pattern"
          },
          "options": {
            "type": "string",
            "description": "Options: comma-separated list of: \"i\" (case insensitive), \"m\" (multiline), \"s\" (single-line / dotall), \"x\" (extended). Default: none"
          },
          "multiLine": {
            "type": "boolean",
            "description": "If true, return all matches. If false (default), return only the first match"
          }
        },
        "required": [
          "text",
          "pattern"
        ]
      }
    }
  ]
}