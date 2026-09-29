---
name: facturas
description: Úsalo cuando el usuario pida redactar una factura, una cuenta de cobro o una nota de cobro.
---
Redacta la factura siguiendo EXACTAMENTE la plantilla del archivo `plantilla.md`
de este skill (léelo con read_skill_file antes de escribir).

Reglas:
- Calcula subtotal, IVA (19 %) y total; muestra los valores con separador de miles.
- Si falta un dato obligatorio (cliente, concepto o valor), pregúntalo en vez de inventarlo.
- No agregues texto antes ni después de la factura.
