#!/bin/sh
# Pantalla virtual para el capitulo 35: Xvfb :99 a 1280x800 y encima "Andina Despachos".
# Nada de esto toca el escritorio real. Requiere: sudo apt-get install -y xvfb xdotool scrot python3-tk
AQUI=$(cd "$(dirname "$0")" && pwd)
pkill -x Xvfb 2>/dev/null; sleep 0.5
setsid Xvfb :99 -screen 0 1280x800x24 >/dev/null 2>&1 &
sleep 1
DISPLAY=:99 setsid python3 "$AQUI/andina_despachos.py" >/dev/null 2>&1 &
sleep 1.5
echo "Xvfb :99 (1280x800) y Andina Despachos listos. Ahora: DISPLAY=:99 ./DespachoLadera"
