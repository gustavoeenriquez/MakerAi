# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 087 — **Etiquetado masivo con Jev**. Consola. **Requiere `TYPESAFE_API_KEY`.**

Clasifica 126 movimientos contables (`casos.csv`) en una de 225 cuentas del PUC colombiano (`puc.csv`) con `TAiJevBatchLabeler`, en paralelo, y mide el acierto total, el de lo que se automatizaría (confianza ≥ `ReviewThreshold`), las filas para revisión humana con sus 3 sugerencias, tokens, costo y tiempo. Escribe el detalle por fila en `resultados.csv` (ignorado por git).

Es la validación del componente: reproduce el experimento del prototipo en Python (mismas cuentas, mismos casos, misma pregunta) para confirmar que el componente no degrada el resultado.

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `JevBatchLabelingDemo.dproj`, build Win64.

```bash
JevBatchLabelingDemo.exe
```

Los CSV se buscan junto al `.dpr` (dos niveles arriba del `.exe`) y, si no, en el directorio actual. Costo: ~1.2M tokens de entrada, ~US$0.05.

## Datos

| Archivo | Contenido |
|---------|-----------|
| `puc.csv` | `codigo;ruta` — 225 subcuentas con su ruta clase > grupo > cuenta > subcuenta (la ruta sube el acierto ~3 puntos frente al nombre solo) |
| `casos.csv` | `id;flujo;texto;gold;alt` — 126 movimientos escritos a mano (`c112`–`c123` son traducciones al inglés; `r001`–`r003` salen de la BD demo) con la cuenta esperada y alternativas aceptables |

## Key Source

| Componente | Unit |
|------------|------|
| `TAiJevBatchLabeler`, `TAiJevBatchReport`, `TAiJevBatchItem` | `Source/Tools/uMakerAi.Jev.Batch.pas` |

## Notas

- **Resultado verificado (sep 28/2026, `jev-1.13.0`):** 117/126 (92.9%); con confianza ≥ 0.8 automatiza 77 filas (61%) con **100%** de acierto; 49 para revisión; 0 errores; 1.191.860 tokens (US$0.05) en 4.2 s con 16 en paralelo. El prototipo en Python dio 119/126 con **exactamente los mismos tokens**, lo que confirma que el request es idéntico; la diferencia de 2 aciertos está dentro de la variación de confianza entre llamadas.
- Las 9 filas equivocadas tuvieron confianza < 0.8: ninguna se habría contabilizado sola. En 7 de ellas la cuenta correcta estaba entre las 3 sugerencias (`Top`).
- Los casos fueron escritos a mano y son más limpios que un extracto real: el número a mirar con datos reales es el acierto de lo automatizado, no el total.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
