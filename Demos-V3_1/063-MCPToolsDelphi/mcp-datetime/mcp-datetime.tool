{
  "tools": [
    {
      "name": "datetime_format",
      "description": "Parse a datetime string and reformat it in a different format",
      "inputSchema": {
        "type": "object",
        "properties": {
          "dateTime": {
            "type": "string",
            "description": "Source datetime in ISO 8601 format or \"now\""
          },
          "format": {
            "type": "string",
            "description": "Output format: iso, unix, rfc, date, time, datetime, or custom:FORMAT"
          }
        },
        "required": [
          "dateTime",
          "format"
        ]
      }
    },
    {
      "name": "datetime_add",
      "description": "Add or subtract a duration from a datetime value. Use negative Amount to subtract.",
      "inputSchema": {
        "type": "object",
        "properties": {
          "dateTime": {
            "type": "string",
            "description": "Source datetime in ISO 8601 format (e.g. \"2024-03-15T10:30:00\") or \"now\" for current time"
          },
          "amount": {
            "type": "integer",
            "description": "Amount to add (can be negative to subtract)"
          },
          "unit": {
            "type": "string",
            "description": "Time unit: years, months, days, hours, minutes, seconds"
          }
        },
        "required": [
          "dateTime",
          "amount",
          "unit"
        ]
      }
    },
    {
      "name": "datetime_now",
      "description": "Return the current date and time in various formats",
      "inputSchema": {
        "type": "object",
        "properties": {
          "format": {
            "type": "string",
            "description": "Output format: iso, unix, rfc, date, time, datetime, or custom:FORMAT (Delphi fmt). Default: iso"
          },
          "timezone": {
            "type": "string",
            "description": "Timezone offset in hours (e.g. -5, +2). Default: local system time"
          }
        },
        "required": []
      }
    },
    {
      "name": "datetime_diff",
      "description": "Calculate the difference between two datetimes in a given unit. Result = Date2 - Date1 (positive if Date2 is later).",
      "inputSchema": {
        "type": "object",
        "properties": {
          "date1": {
            "type": "string",
            "description": "First datetime in ISO 8601 format or \"now\""
          },
          "date2": {
            "type": "string",
            "description": "Second datetime in ISO 8601 format or \"now\""
          },
          "unit": {
            "type": "string",
            "description": "Result unit: years, months, days, hours, minutes, seconds. Default: days"
          }
        },
        "required": [
          "date1",
          "date2"
        ]
      }
    }
  ]
}