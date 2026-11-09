"""Extrae los literales de excepción de las carpetas indicadas a un JSON.

Primer paso de cada tanda de la fase 3 de la localización:
  1. python exc_extract.py tanda.json Tools RAG       -> lista de raises
  2. escribir map_tanda.py: M = {literal: (clave, inglés, español)}, más
     MANUAL / MANUAL_LITS / PATCHES para los casos partidos en varias líneas
  3. python exc_apply.py tanda.json map_tanda "Excepciones: Tools y RAG" --dry
     y luego sin --dry
  4. hardcoded_strings.py --check-keys, compilar y correr la suite

Uso: python exc_extract.py salida.json Core Chat
"""
import json, os, re, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from hardcoded_strings import RE_EXC, strip_comments

SRC = os.path.join(os.path.dirname(os.path.dirname(HERE)), 'Source')
out, folders = sys.argv[1], sys.argv[2:]
items = []
for folder in folders:
    for dp, _, fs in os.walk(os.path.join(SRC, folder)):
        for f in sorted(fs):
            if not f.endswith('.pas') or 'Deprecated' in f or f.startswith('uMakerAi.Lang.'):
                continue
            p = os.path.join(dp, f)
            text = open(p, 'rb').read().decode('utf-8-sig')
            clean = strip_comments(text.replace('\r\n', '\n')).split('\n')
            for n, line in enumerate(clean, 1):
                if 'AiStr(' in line or 'AiFmt(' in line:
                    continue
                for m in RE_EXC.finditer(line):
                    lit = m.group(1)
                    if len(lit) <= 2:
                        continue
                    # ¿Hay concatenación después del literal?
                    rest = line[m.end(1):].lstrip()
                    items.append({
                        'file': os.path.relpath(p, SRC).replace('\\', '/'),
                        'line': n,
                        'lit': lit,
                        'fmt': '.CreateFmt' in m.group(0),
                        'concat': rest.startswith('+'),
                    })
json.dump(items, open(out, 'w', encoding='utf-8'), ensure_ascii=False, indent=1)
uniq = sorted({i['lit'] for i in items})
print(len(items), 'ocurrencias,', len(uniq), 'textos distintos')
