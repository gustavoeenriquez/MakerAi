# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 075 — **Elicitation MCP (patrón MRTR) desde el lado del CLIENTE**. Consola, servidor y cliente in-process, **sin LLM ni API keys**.

El [demo 031](../031-MCPServer/CLAUDE.md) muestra el lado servidor (un tool que pide confirmación). Este muestra la otra mitad: qué hay que implementar cuando MakerAI **consume** un servidor MCP que usa elicitation.

| Escenario | Resultado |
|-----------|-----------|
| **A. El usuario acepta** | El handler responde `action: accept` → el cliente reintenta solo y el tool ejecuta la operación |
| **B. El usuario rechaza** | `action: decline` → el tool no ejecuta y lo reporta |
| **C. Sin handler** | La llamada devuelve el `input_required` sin resolver. Es el fallo típico al integrar un servidor con elicitation, y conviene verlo explícito |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `MCPElicitationDemo.dproj`, build Win64.

```bash
MCPElicitationDemo.exe          # los tres escenarios
MCPElicitationDemo.exe --otel   # trazas OTLP a localhost:4318
```

Levanta su propio servidor MCP HTTP en el puerto 8285 y lo apaga al terminar.

## El contrato MRTR

Dos vueltas, sin sesión en el servidor:

1. El tool devuelve `resultType='input_required'` con `inputRequests` (qué necesita) y un `requestState` opaco.
2. El cliente dispara `OnInputRequired`; el handler rellena `AInputResponses` **bajo la misma clave** que usó el servidor y pone `AHandled := True`. El cliente repite `tools/call` solo, ecoando el `requestState`. Recién ahí el tool ejecuta.

```pascal
procedure TMiHandler.InputRequired(Sender: TObject; const AToolName: string;
  AInputRequests, AInputResponses: TJSONObject; var AHandled: Boolean);
begin
  // AInputRequests['<clave>'].params.message = la pregunta al usuario
  Respuesta := TJSONObject.Create;
  Respuesta.AddPair('action', 'accept');   // o 'decline' / 'cancel'
  Respuesta.AddPair('content', ...);        // segun requestedSchema
  AInputResponses.AddPair('<clave>', Respuesta);
  AHandled := True;                         // sin esto no hay reintento
end;
```

## Key Source

| Componente | Unit |
|------------|------|
| `TMCPClientHttp` + `OnInputRequired` | `Source/MCPClient/uMakerAi.MCPClient.Core.pas` |
| `TAiMCPHttpServer` | `Source/MCPServer/UMakerAi.MCPServer.Http.pas` |
| `TAiMCPToolBase<T>`, `TAiMCPResponseBuilder` | `Source/MCPServer/uMakerAi.MCPServer.Core.pas` |
| Tool del demo | `uTool.Aprobacion.pas` |

## Notas

- La clave de `inputRequests` la elige el tool (aquí `'autorizacion'`); el cliente debe responder bajo esa misma clave. Un cliente genérico recorre todas.
- **`requestState` es entrada NO confiable**: viaja por el cliente. Aquí solo lleva el monto en Base64 con fines demostrativos. En producción, si influye en autorización o lógica de negocio, debe ir firmado (HMAC/AEAD) o el cliente puede alterarlo.
- El tool valida el eco del `requestState` y rechaza limpiamente si no cuadra, sin lanzar excepción.
- Si falta la respuesta pedida, la spec dice **volver a pedirla**, no fallar.
- Los eventos son `of object`: sin lambdas.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
> See [../../Source/MCPClient/CLAUDE.md](../../Source/MCPClient/CLAUDE.md) for the MCP client module.
