{
  "tools": [
    {
      "name": "http_request",
      "description": "Perform any HTTP request (GET, POST, PUT, DELETE, PATCH) with custom headers and body. Returns status code, response headers, and body. Headers should be provided as a JSON object string.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "method": {
            "type": "string",
            "description": "HTTP method: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS"
          },
          "url": {
            "type": "string",
            "description": "Target URL (HTTP or HTTPS)"
          },
          "headers": {
            "type": "string",
            "description": "Request headers as a JSON object string. Example: \"{\\\"Content-Type\\\":\\\"application/json\\\",\\\"Authorization\\\":\\\"Bearer TOKEN\\\"}\""
          },
          "body": {
            "type": "string",
            "description": "Request body (for POST, PUT, PATCH). Leave empty for GET/DELETE."
          },
          "timeout": {
            "type": "integer",
            "description": "Request timeout in seconds. Default: 30"
          }
        },
        "required": [
          "method",
          "url"
        ]
      }
    }
  ]
}