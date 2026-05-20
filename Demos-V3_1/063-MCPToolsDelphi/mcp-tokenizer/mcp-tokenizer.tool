{
  "tools": [
    {
      "name": "tokenize",
      "description": "Estimate the number of tokens in a text for a given model. Uses heuristic approximation (not exact BPE tokenization). Accuracy: typically within 10-20% of actual token count.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to estimate token count for"
          },
          "model": {
            "type": "string",
            "description": "Model name hint for better estimation. Examples: \"gpt-4\", \"gpt-3.5\", \"claude\", \"gemini\". Default: generic English estimation"
          }
        },
        "required": [
          "text"
        ]
      }
    },
    {
      "name": "split_text",
      "description": "Split a long text into smaller chunks that fit within a token limit. Chunks are split at sentence boundaries when possible. Each chunk includes an overlap for context continuity.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to split into chunks"
          },
          "maxTokens": {
            "type": "integer",
            "description": "Maximum tokens per chunk. Default: 512"
          },
          "overlap": {
            "type": "integer",
            "description": "Token overlap between adjacent chunks (for context continuity). Default: 50"
          },
          "model": {
            "type": "string",
            "description": "Model name for token estimation. Default: generic"
          }
        },
        "required": [
          "text"
        ]
      }
    }
  ]
}