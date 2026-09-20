# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 083 — **Computer Use en Linux (X11)**.

Ciclo agéntico real de Computer Use sobre una pantalla X: el modelo pide capturas, hace clic y teclea, y el demo le devuelve el nuevo estado. Es el mismo subsistema que el demo 066 de Windows; lo único que cambia son los dos handlers del `TAiComputerUseTool`, que aquí apuntan a **`TAiLinuxExecutor`** (`Source/Tools/uMakerAi.Tools.ComputerUse.Linux.pas`).

**El framework no necesitó ni una línea.** `uMakerAi.Tools.ComputerUse.pas` solo usa RTL, así que cruza a Linux tal cual; lo que faltaba era únicamente el ejecutor.

### Pensado para una pantalla virtual

Corre contra **Xvfb**, que es como corre un broker en un VPS headless. Eso además evita el riesgo del 066: allí la captura es el escritorio **real** del usuario y se envía entero al proveedor. Aquí solo se ve lo que haya dentro del display virtual.

> Si se apunta a un display con pantalla física (`:0`), se captura el escritorio entero y se envía al proveedor. Revisar qué hay en pantalla **antes** de lanzarlo.

## Build & Run

```bash
msbuild 083-ComputerUseLinux/ComputerUseLinuxDemo.dproj /p:Config=Release /p:Platform=Linux64
```

Compila también en Win64 a propósito (imprime un aviso y sale) para no romper el group project: todo el cuerpo va bajo `{$IFDEF LINUX}`.

### Preparar la máquina Linux

```bash
sudo apt-get install -y xvfb xdotool scrot x11-apps

Xvfb :99 -screen 0 1280x800x24 &
DISPLAY=:99 xedit /tmp/prueba.txt &          # la app "objetivo"

export OPENAI_API_KEY=...
DISPLAY=:99 ./ComputerUseLinuxDemo
```

### Opciones

```bash
./ComputerUseLinuxDemo --provider=openai|claude   # default: openai
./ComputerUseLinuxDemo --display=:99              # default: el DISPLAY del entorno
./ComputerUseLinuxDemo --width=1280 --height=800  # tamaño de la pantalla X
./ComputerUseLinuxDemo --prompt="..."
./ComputerUseLinuxDemo --help
```

| Exit code | Significado |
|-----------|-------------|
| 0 | El ciclo corrió y se ejecutó al menos una acción |
| 1 | Excepción |
| 2 | Falta `xdotool` o `scrot` |
| 3 | El modelo no usó la herramienta en ese turno |

Cada captura enviada al modelo se guarda además en `shots/` para depurar.

## Estado verificado

Probado en **WSL2 / Ubuntu 24.04** con Xvfb `:99` a 1280×800, contra la API real:

| Proveedor | Resultado |
|-----------|-----------|
| `gpt-6-astra` | 4 acciones, 4 capturas: `screenshot → click_at → type_text_at → screenshot`. Texto escrito correctamente en xedit |
| `claude-opus-4-8` | 4 acciones, 4 capturas, incluida `zoom`. Claude emite **varios tool_use por turno** (`type_text_at` + `click_at` juntos) y el ejecutor los encadena bien |

## Por qué xdotool y no libX11

El ejecutor son dos handlers, así que cambiar de motor después no toca nada más del framework. Esta versión arranca funcionando con dos paquetes de apt. Una variante sobre **libX11 + XTest por dlopen** (sin binarios externos, el patrón que ya usa `TOpenSSLTransport`) puede sustituirla sin que la aplicación se entere.

## Limitaciones del ejecutor

- **Solo X11.** Bajo Wayland nativo `xdotool` no controla las ventanas; hace falta XWayland, y aun así solo alcanza a los clientes X.
- **`catNavigate` usa `xdg-open`**, así que necesita un navegador instalado y un manejador registrado. En un Xvfb pelado no hay ninguno.
- **Sin window manager** (el caso típico de Xvfb) no hay activación de ventanas ni foco por clic en el marco: las apps reciben el foco por puntero. Si hace falta comportamiento de escritorio, levantar un WM ligero.
- **Scroll aproximado**: `ScrollAmount` viene en píxeles (default 800) y X11 solo sabe de clics de rueda; se aproxima a ~100 px por clic, con tope de 15.

## Detalles de implementación que importan

**No hay reescalado.** La pantalla X es de 1280 de ancho y se declara al modelo esa misma resolución (`ScreenWidth = AreaWidth`), así que la imagen que ve el modelo y los píxeles reales coinciden y los clics caen donde él cree. En el 066 de Windows sí hay downscale (monitor de 3440 → 1280 declarados) y por eso allí la conversión importa.

**`catType` replica la semántica de Windows**: pre-clic solo si la acción trae coordenadas (`type_text_at` de Gemini las trae; el `type` de Claude no, y clicar en (0,0) quitaría el foco). `ClearBeforeTyping` tampoco se aplica, igual que en Windows, para no divergir entre plataformas.

**`DISPLAY`**: por defecto se hereda del proceso. Si la aplicación corre como servicio y no lo tiene en el entorno, asignar `TAiLinuxExecutor.Display`.

## Archivos

| Archivo | Contenido |
|---------|-----------|
| `ComputerUseLinuxDemo.dpr` | Todo el demo: handlers, wiring y ciclo |
| `../../Source/Tools/uMakerAi.Tools.ComputerUse.Linux.pas` | El ejecutor (no está en el `.dpk`, igual que los de Windows y macOS) |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for the demos index. El subsistema está documentado en [../../Source/Tools/CLAUDE.md](../../Source/Tools/CLAUDE.md); el equivalente en Windows, con pantalla real, está en [../066-ComputerUseTest/](../066-ComputerUseTest/), y la delegación a un cliente remoto en [../082-ComputerUsePassthru/](../082-ComputerUsePassthru/).
