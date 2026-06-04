{
  "tools": [
    {
      "name": "url_encode",
      "description": "Percent-encode a string for use in URLs (encodes all non-unreserved characters)",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to URL-encode (percent encoding)"
          }
        },
        "required": [
          "text"
        ]
      }
    },
    {
      "name": "base64_decode",
      "description": "Decode a Base64-encoded string back to plain text",
      "inputSchema": {
        "type": "object",
        "properties": {
          "base64": {
            "type": "string",
            "description": "Base64-encoded string to decode"
          }
        },
        "required": [
          "base64"
        ]
      }
    },
    {
      "name": "base64_encode",
      "description": "Encode a string to Base64",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to encode as Base64"
          },
          "encoding": {
            "type": "string",
            "description": "Input text encoding: utf8 (default) or ascii"
          }
        },
        "required": [
          "text"
        ]
      }
    },
    {
      "name": "string_transform",
      "description": "Apply a text transformation: upper, lower, trim, reverse, title_case, camel_case, snake_case, count_words, count_chars, count_lines, sort_lines, remove_duplicates_lines, etc.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to transform"
          },
          "operation": {
            "type": "string",
            "description": "Transformation: upper, lower, trim, reverse, title_case, sentence_case, camel_case, snake_case, count_words, count_chars, count_lines, sort_lines, sort_lines_desc, remove_duplicates_lines"
          }
        },
        "required": [
          "text",
          "operation"
        ]
      }
    },
    {
      "name": "compute_hash",
      "description": "Compute a cryptographic hash of a string (MD5, SHA-1, SHA-256, SHA-512). Returns lowercase hex digest.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "Text to hash"
          },
          "algorithm": {
            "type": "string",
            "description": "Hash algorithm: md5, sha1, sha256 (default), sha512"
          }
        },
        "required": [
          "text"
        ]
      }
    },
    {
      "name": "url_decode",
      "description": "Decode a percent-encoded URL string",
      "inputSchema": {
        "type": "object",
        "properties": {
          "text": {
            "type": "string",
            "description": "URL-encoded text to decode"
          }
        },
        "required": [
          "text"
        ]
      }
    }
  ]
}