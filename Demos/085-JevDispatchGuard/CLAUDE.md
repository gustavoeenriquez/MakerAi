# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 085 — **Jev en SmartDispatch y en Guardrails**. Consola. **Requiere `TYPESAFE_API_KEY`** (y ninguna otra clave).

| Bloque | Qué muestra |
|--------|-------------|
| **A. SmartDispatch** | `TAiJevDispatchClassifier` decide a qué tool va cada petición (IMAGEGEN / VIDEOGEN / TTS / WEBSEARCH / CHAT). Se llama directo por `IAiDispatchClassifier` para no exigir las claves de las tools; en una app se asigna a `ChatTools.DispatchClassifier` y lo llama el chat en `cmSmartDispatch` |
| **B. Guardrails** | `TAiGuardrails` con una lista clásica (`BlockedArgPatterns = 'rm -rf'`) + `TAiJevGuardrailClassifier` en `Classifier`, sobre 8 tool calls |
| **D. Categorías de permiso** | 7 tool calls clasificadas en read / write / delete / financial / external_comm / system en la misma llamada que el riesgo, con `financial` y `system` bloqueadas y una `ToolDescriptions` para `anular_comprobante` |
| **C. Guardrail de entrada** | 9 mensajes de usuario revisados por el sanitizador por regex que MakerAI ya trae (`TSanitizerPipeline.Check`) y por `TAiJevPromptGuard` (con `Scope` contable), lado a lado |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `JevDispatchGuardDemo.dproj`, build Win64.

```bash
JevDispatchGuardDemo.exe
```

Exit code: 0 ok, 2 si falta la API key o hay excepción.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiJevDispatchClassifier` | `Source/Tools/uMakerAi.Jev.SmartDispatch.pas` |
| `TAiJevGuardrailClassifier` | `Source/Tools/uMakerAi.Jev.Guardrails.pas` |
| `TAiJevPromptGuard` | `Source/Tools/uMakerAi.Jev.PromptGuard.pas` |
| `IAiDispatchClassifier`, `TAiDispatchClassifierBase` | `Source/Core/uMakerAi.Chat.Tools.pas` |
| `TAiGuardrails.Classifier`, `TAiGuardrailClassifierBase` | `Source/Tools/uMakerAi.Guardrails.pas` |

## Notas

- **Resultado verificado (sep 27/2026, `jev-1.13.0`):** SmartDispatch 8/8, incluida la trampa "describe cómo se vería un gato" → CHAT (0.74). Guardrails bloqueó 5 de 8, todos correctos: `rm -rf` por la lista (sin llamar a Jev) y, por Jev, `DELETE FROM clientes` (0.98), correo con una clave a un externo (0.94), transferencia a cuenta desconocida (0.96) y escalada de rol por `UPDATE` (0.91). Las tres llamadas seguras pasaron con riesgo ≤ 0.17.
- **Bloque C (sep 27/2026):** la regex atrapó 1 de 6 mensajes problemáticos ("ignore previous instructions"); Jev los 6 —pedir el prompt de sistema (injection 0.97), el jailbreak de "la abuela" (injection 0.83), una clave bancaria (sensitive_data 0.98), facturas falsas (harmful 0.99), una receta (out_of_scope 0.98)— sin bloquear el saludo, la pregunta contable ni "¿qué sanción hay si no declaro IVA a tiempo?".
- **Bloque D (sep 27/2026):** `pay_invoice` bloqueado por categoría (financial 1.00), `grant_role` invitado→admin bloqueado por riesgo de abuso (0.93); lectura, escritura, anulación y envío al auditor pasan. Antes de `ABUSE_POLICY`, la política estricta bloqueaba 4 de las 7 (también `anular_comprobante` 0.57 y el upload al auditor 0.67).
- **Listas y Jev se complementan:** lo enumerable va en las listas (gratis, determinista, se evalúa antes); Jev juzga lo que ninguna lista anticipa. El correo con la clave no tiene un patrón que una lista pueda prever.
- `Policy` (la pregunta de riesgo) es lo que define qué cuenta como daño: ajustarla al dominio. `BlockOnError` (default `True`) bloquea si Jev no responde.
- En SmartDispatch, con confianza < `MinConfidence` (0.6) el clasificador devuelve `''` y el chat hace su pase 1 por LLM como siempre.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
