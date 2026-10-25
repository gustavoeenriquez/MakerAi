# Capítulo 35 — El modelo que ve la pantalla

Dos proyectos:

- `LaderaApp.dproj` (Win64, FMX): la app de Café La Ladera con el botón **Recorte** (`TScreenCapture`): el usuario
  dibuja un rectángulo sobre la pantalla y lo capturado va a Lucía como imagen. En `--prueba`, `/recorte vista|pregunta`
  captura la propia vista del chat. Requiere `BodegaLadera.exe` compilado en la misma carpeta (como desde el cap 25).
- `DespachoLadera.dproj` (**Linux64**, consola): Computer Use de verdad, sobre una pantalla X virtual. El modelo
  (`gpt-6-astra` o `claude-sonnet-5-5`) registra una guía en `pantalla/andina_despachos.py`, el programa de escritorio
  (ficticio, Tkinter) de la transportadora, que no tiene API. Compila también en Win64 (solo imprime un aviso).

```bash
msbuild DespachoLadera.dproj /p:Config=Release /p:Platform=Linux64        # desde Windows, SDK de Ubuntu
# en Linux / WSL:
sudo apt-get install -y xvfb xdotool scrot python3-tk
pantalla/iniciar.sh
export OPENAI_API_KEY=...            # o CLAUDE_API_KEY con --proveedor=claude
DISPLAY=:99 Linux64/Release/DespachoLadera
pantalla/detener.sh
```

Cada captura enviada al modelo queda en `shots/`; cada guía registrada, en `/tmp/andina_guias.txt` (para comprobar lo
que escribió el modelo). Por qué una pantalla virtual y no el escritorio: en Windows el ejecutor (`TAiWindowsFMXExecutor`)
mueve el ratón y teclea de verdad y la captura es la pantalla entera del usuario, que viaja al proveedor. Café La Ladera:
empresa, productos y precios ficticios.
