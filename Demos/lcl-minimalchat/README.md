# Demo LCL — MinimalChat

Chat mínimo con interfaz gráfica (LCL) contra un **Ollama local**. Es el
equivalente GUI de los demos de consola de `Demos/*.pas`.

Está en una subcarpeta a propósito: `build_demos.sh` / `build_demos.ps1`
compilan todos los `*.pas` de `Demos/` como programas independientes, y las
unidades de un proyecto LCL no lo son.

## Compilar

```powershell
lazbuild --add-package-link ..\..\Source\Packages\makerai_fpc.lpk   # una sola vez
lazbuild MinimalChat.lpi
```

El ejecutable queda en `bin\MinimalChat.exe`.

## Usar

1. Levanta Ollama: `ollama serve` y `ollama pull gemma3:1b`
2. Ejecuta `bin\MinimalChat.exe`
3. Ajusta URL y modelo si hace falta, escribe y pulsa **Enviar**.

El componente `TAiOllamaChat` se crea una vez en `FormCreate` y se reutiliza,
así que mantiene el historial de la conversación entre turnos.

## Notas de portabilidad

`MinimalChat.lpr` replica en versión GUI los tres arreglos de
`Demos/uDemoHelper.pas`:

- separador decimal `'.'` — sin esto `Temperature := 0.7` se serializa como
  `"0,7"` con locale español y varios proveedores rechazan el JSON;
- `InitSSLInterface` — FPC no inicializa OpenSSL solo; sin esto las conexiones
  HTTPS fallan en silencio (Ollama por HTTP local no lo necesita, pero sí
  cualquier proveedor cloud);
- el fix de code page de consola no aplica en GUI.

La llamada es **síncrona** (`Asynchronous := False`) para mantener el ejemplo
corto; para modo asíncrono mira `Demos/demo_ollama_async.pas`.
