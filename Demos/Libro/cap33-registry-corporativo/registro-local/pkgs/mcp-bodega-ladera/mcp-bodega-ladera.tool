{
  "name": "mcp-bodega-ladera",
  "description": "Bodega de Cafe La Ladera. Funciones: inventario (kilos en existencia de un cafe) y precio_mayorista (precio de un pedido por kilos).",
  "version": "1.0.0",
  "category": "business",
  "inputSchema": {
    "type": "object",
    "properties": {
      "cafe": { "type": "string", "description": "Nombre del cafe, por ejemplo Huila Reserva" },
      "kilos": { "type": "integer", "description": "Kilos del pedido (solo precio_mayorista)" }
    },
    "required": ["cafe"]
  }
}
