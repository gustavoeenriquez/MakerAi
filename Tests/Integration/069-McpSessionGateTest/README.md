# 069 - MCP Session Gate Test (ISSUE #110)

Test runtime self-contained del **ISSUE #110**: vetting de cliente MCP (`OnClientConnect`) + gate de sesion HTTP por `Mcp-Session-Id` (Parte A + Parte B).

## Que hace

Arranca un `TAiMCPHttpServer` real (Indy, `localhost:8765/mcp`) con una tool `echo` y lo prueba como cliente HTTP (`THTTPClient`). No requiere API keys ni servicios externos.

Valida 4 fases con asserts automaticos (18 checks):

| Fase | Escenario | Esperado |
|------|-----------|----------|
| 1 | **Gating OFF** (sin `OnClientConnect`) | `tools/call` sin sesion ejecuta el tool (backward compat) |
| 2 | **Gating ON** | `tools/call` sin sesion -> `-32001`; `initialize` emite `Mcp-Session-Id`; `tools/call` con esa sesion -> OK; sesion invalida -> `-32001` |
| 3 | `OnUnauthorizedRequest` pone `AAllow:=True` | el tool se ejecuta pese a no haber sesion |
| 4 | `OnClientConnect` pone `AAllow:=False` | `initialize` rechazado (error JSON-RPC, sin `result`, sin sesion emitida) |

## Compilar y ejecutar

```bash
msbuild McpSessionGateTest.dproj /t:Build /p:Config=Debug /p:Platform=Win64
Win64\Debug\McpSessionGateTest.exe
```

Codigo de salida `0` = todos los checks PASS. `1` = algun FAIL. `2` = excepcion fatal.

## Principio clave verificado

El gating es **100% opt-in**: solo se activa cuando hay un handler `OnClientConnect` asignado (Fase 1 demuestra que sin el, el servidor se comporta igual que antes).
