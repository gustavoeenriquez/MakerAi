#!/usr/bin/env python3
"""
fix_license_headers.py — Repara el header MIT de los .pas/.dpr/.dpk de MakerAi.

Problema
--------
En muchos archivos, varias lineas del header de licencia MIT perdieron el
PRIMER caracter despues de "// " (corrupcion historica, inconsistente entre
archivos). Ej:
  "// MIT License"      -> "// IT License"      (falta M)
  "// to use, copy..."  -> "// o use, copy..."  (falta t)
  "// THE SOFTWARE IS"  -> "// HE SOFTWARE IS"  (falta T)

Solucion
--------
Las lineas del bloque MIT son ASCII puro e identicas en todos los archivos.
Se reparan a NIVEL DE BYTES (reemplazo de la linea corrupta completa por la
canonica). No toca el resto del archivo: preserva BOM, CRLF y el encoding de
los caracteres acentuados (eso es trabajo aparte de fix_encoding.py).
Idempotente: correr dos veces no cambia nada.

Uso:
  python fix_license_headers.py --dry-run Source Demos Apps
  python fix_license_headers.py Source Demos Apps
"""

import sys
import argparse
from pathlib import Path

# Lineas canonicas del bloque MIT (las que pueden perder su primer caracter).
# Se excluyen las lineas "//" vacias y la de Copyright (variable por archivo).
CANON = [
    "// MIT License",
    "// Permission is hereby granted, free of charge, to any person obtaining a copy",
    '// of this software and associated documentation files (the "Software"), to deal',
    "// in the Software without restriction, including without limitation the rights",
    "// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
    "// copies of the Software, and to permit persons to whom the Software is",
    "// furnished to do so, subject to the following conditions:",
    "// The above copyright notice and this permission notice shall be included in",
    "// all copies or substantial portions of the Software.",
    '// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR',
    "// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
    "// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
    "// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
    "// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
    "// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN",
    "// THE SOFTWARE.",
]

# Mapa: linea corrupta (sin el 1er caracter tras "// ") -> linea canonica, en bytes ASCII.
FIXMAP = {}
for _line in CANON:
    _body = _line[3:]                 # texto despues de "// "
    _corrupted = "// " + _body[1:]    # se come el primer caracter del cuerpo
    FIXMAP[_corrupted.encode("ascii")] = _line.encode("ascii")


def fix_bytes(data: bytes):
    """Aplica los reemplazos de linea-completa. Retorna (data, num_lineas_reparadas)."""
    fixed = 0
    for corrupted, canon in FIXMAP.items():
        if corrupted == canon:
            continue
        # Reemplazo anclado: la corrupta SOLO aparece tras un salto de linea o BOM
        # (inicio del comentario). Probamos los dos prefijos posibles.
        for prefix in (b"\n", b"\xbf"):  # \n (linea normal) o ...\xbf del BOM (linea 1)
            needle = prefix + corrupted
            repl = prefix + canon
            if needle in data:
                cnt = data.count(needle)
                data = data.replace(needle, repl)
                fixed += cnt
    return data, fixed


def main():
    ap = argparse.ArgumentParser(description="Repara el header MIT de archivos Delphi.")
    ap.add_argument("paths", nargs="*", default=["."], help="Dirs/archivos a procesar")
    ap.add_argument("--dry-run", action="store_true", help="No escribe; solo reporta")
    ap.add_argument("--ext", default=".pas,.dpr,.dpk",
                    help="Extensiones (por defecto: .pas,.dpr,.dpk)")
    args = ap.parse_args()

    exts = {e.strip().lower() for e in args.ext.split(",")}
    files = []
    for raw in args.paths:
        p = Path(raw)
        if p.is_file():
            files.append(p)
        elif p.is_dir():
            files += [q for q in p.rglob("*")
                      if q.suffix.lower() in exts and ".bak" not in q.suffixes]
    files = sorted(set(files))

    total_files = 0
    total_lines = 0
    for f in files:
        try:
            data = f.read_bytes()
        except Exception as e:
            print(f"ERROR leyendo {f}: {e}")
            continue
        new, n = fix_bytes(data)
        if n > 0:
            total_files += 1
            total_lines += n
            tag = "[DRY]" if args.dry_run else "FIX "
            print(f"{tag} {f}  ({n} linea(s))")
            if not args.dry_run:
                f.write_bytes(new)

    verb = "se repararian" if args.dry_run else "reparados"
    print()
    print(f"Resultado: {total_files} archivo(s) {verb}, {total_lines} linea(s) en total.")


if __name__ == "__main__":
    main()
