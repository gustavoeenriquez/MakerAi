{
  "tools": [
    {
      "name": "calculate",
      "description": "Evaluate a mathematical expression. Supports arithmetic operators (+,-,*,/,%), exponentiation (^), grouping (parentheses), functions (sqrt, abs, floor, ceil, round, sin, cos, tan, log10, ln, exp) and constants (pi, e).",
      "inputSchema": {
        "type": "object",
        "properties": {
          "expression": {
            "type": "string",
            "description": "Mathematical expression to evaluate. Supports: +,-,*,/,^,sqrt(),abs(),floor(),ceil(),round(),sin(),cos(),tan(),log10(),ln(),exp(),pi,e. Example: \"2+3*4\" or \"sqrt(16)+2^3\""
          }
        },
        "required": [
          "expression"
        ]
      }
    }
  ]
}