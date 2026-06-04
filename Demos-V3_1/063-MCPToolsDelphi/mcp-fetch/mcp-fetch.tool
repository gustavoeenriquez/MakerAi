{
  "tools": [
    {
      "name": "fetch",
      "description": "Perform an HTTP GET request and return the response body as text. Useful for retrieving web pages, APIs, or any URL-accessible content. Returns the raw response text.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "url": {
            "type": "string",
            "description": "URL to fetch (HTTP or HTTPS)"
          },
          "timeout": {
            "type": "integer",
            "description": "Request timeout in seconds. Default: 30"
          },
          "followRedirects": {
            "type": "boolean",
            "description": "Whether to follow redirects automatically. Default: true"
          }
        },
        "required": [
          "url"
        ]
      }
    }
  ]
}