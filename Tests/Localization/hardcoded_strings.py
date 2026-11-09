#!/usr/bin/env python3
"""Textos del framework que siguen escritos a mano (sin AiStr/AiFmt).

Recorre Source/**/*.pas y cuenta los literales que deberían pasar por
TAiLocale, por carpeta y por tipo:

  exception  primer literal de XxxException/XxxError.Create(Fmt)(...), también
             dentro de un Format(...)
  state      mensajes de DoStateChange / ReportState / DoStatusUpdate
  ui         asignaciones a Text/Caption/Hint/TextPrompt/Placeholder
  prompt     instrucciones a un LLM todavía en español (Eres/Responde/...); los
             prompts internos se escriben en inglés y no pasan por TAiLocale

Uso:
  python Tests/Localization/hardcoded_strings.py            # resumen por carpeta
  python Tests/Localization/hardcoded_strings.py --list     # además archivo:línea
  python Tests/Localization/hardcoded_strings.py --list --kind ui --folder ChatUI
  python Tests/Localization/hardcoded_strings.py --check-keys   # claves usadas vs tabla base

Ignora comentarios, las tablas uMakerAi.Lang.*.pas y las llamadas que ya
usan AiStr/AiFmt. Es una heurística para medir el avance de la localización,
no un analizador de Delphi: algún falso positivo o negativo es esperable.
"""
import argparse
import os
import re
import sys
from collections import defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SOURCE = os.path.join(ROOT, 'Source')

LIT = r"'(?:[^']|'')*'"
RE_EXC = re.compile(r"\b\w*(?:Exception|Error)\w*\.Create(?:Fmt)?\s*\(\s*(?:Format\(\s*)?(" + LIT + ")")
# Mensajes de estado que ve el usuario (OnStateChange / OnStatusUpdate)
RE_STATE = re.compile(r"\b(?:DoStateChange|ReportState|DoStatusUpdate)\([^']*?(?:Format\(\s*)?(" + LIT + ")")
RE_UI = re.compile(r"(\w*)\.(?:Text|Caption|Hint|TextPrompt|Placeholder)\s*:=\s*(" + LIT + ")")
# Una asignación a SQL.Text o un literal que empieza con SQL no es texto de pantalla
RE_SQL = re.compile(r"^'\s*(SELECT|INSERT|UPDATE|DELETE|CREATE|DROP|ALTER|WITH|MERGE|EXEC|PRAGMA)\b", re.I)
# Los prompts internos van en inglés con "responde en el idioma del usuario"
# (decisión de la v4.0): solo cuenta como pendiente un prompt en español.
RE_PROMPT = re.compile(r"('(?:Eres|Responde|Actúa|Analiza|Contesta|Tienes acceso)\b(?:[^']|'')*')")


def strip_comments(text):
    """Quita comentarios { } (* *) // respetando los literales."""
    out = []
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c == "'":
            j = i + 1
            while j < n:
                if text[j] == "'":
                    if j + 1 < n and text[j + 1] == "'":
                        j += 2
                        continue
                    break
                if text[j] == '\n':
                    break
                j += 1
            out.append(text[i:j + 1])
            i = j + 1
        elif c == '/' and text.startswith('//', i):
            j = text.find('\n', i)
            i = n if j < 0 else j
        elif c == '{':
            if text.startswith('{$', i):  # directiva: se conserva
                j = text.find('}', i)
                j = n if j < 0 else j + 1
                out.append(text[i:j])
                i = j
            else:
                j = text.find('}', i)
                # conservar los saltos de línea para no mover los números de línea
                seg = text[i:(n if j < 0 else j + 1)]
                out.append('\n' * seg.count('\n'))
                i = n if j < 0 else j + 1
        elif c == '(' and text.startswith('(*', i):
            j = text.find('*)', i)
            seg = text[i:(n if j < 0 else j + 2)]
            out.append('\n' * seg.count('\n'))
            i = n if j < 0 else j + 2
        else:
            out.append(c)
            i += 1
    return ''.join(out)


def scan_file(path):
    with open(path, 'rb') as f:
        raw = f.read()
    for enc in ('utf-8-sig', 'cp1252'):
        try:
            text = raw.decode(enc)
            break
        except UnicodeDecodeError:
            continue
    text = strip_comments(text.replace('\r\n', '\n'))
    hits = []
    for lineno, line in enumerate(text.split('\n'), 1):
        if 'AiStr(' in line or 'AiFmt(' in line:
            continue
        prompts = [m.group(1) for m in RE_PROMPT.finditer(line)]
        for lit in prompts:
            hits.append(('prompt', lineno, lit))
        for m in RE_EXC.finditer(line):
            lit = m.group(1)
            if len(lit) > 2 and lit not in prompts:  # '' no cuenta
                hits.append(('exception', lineno, lit))
        for m in RE_STATE.finditer(line):
            if re.search(r'[^\W\d_]{2}', m.group(1)):
                hits.append(('state', lineno, m.group(1)))
        for m in RE_UI.finditer(line):
            lit = m.group(2)
            # sin letras ('(', '<=', ...) son tokens de un parser, no texto
            if not re.search(r'[^\W\d_]{2}', lit) or lit in prompts:
                continue
            if m.group(1).upper().endswith('SQL') or RE_SQL.match(lit):
                continue
            # Descripciones de tools y system prompt: los lee el modelo, van en inglés
            if m.group(1).lower().endswith(('description', 'systemprompt')):
                continue
            hits.append(('ui', lineno, lit))
    return hits


RE_TABLE_KEY = re.compile(r"^\s*'([a-z0-9_]+(?:\.[a-z0-9_]+)+)'\s*,", re.M)
_KEY = r"'([a-z0-9_]+(?:\.[a-z0-9_]+)+)'"
# Dentro de AiStr/AiFmt/TextIn, o como elemento suelto de un arreglo de claves
# (una línea que solo tiene el literal, como en MENU_KEYS). Así no cuentan los
# nombres de atributos de telemetría ('rag.search', 'rag.top_k'...).
RE_KEY_LIT = re.compile(r"(?:\bAiStr|\bAiFmt|\.TextIn\s*\([^,]*,)\s*\(?\s*" + _KEY + r"|^\s*" + _KEY + r"\s*[,)]")


def check_keys():
    """Claves usadas en Source que no están en la tabla base, y claves sin uso.

    Una clave usada es un literal 'prefijo.nombre' (prefijo de la tabla:
    chatui, design, core...) pasado a AiStr/AiFmt o escrito solo en una línea,
    como los elementos de MENU_KEYS.
    """
    base = os.path.join(SOURCE, 'Core', 'uMakerAi.Lang.En.pas')
    table = set(RE_TABLE_KEY.findall(open(base, encoding='utf-8-sig').read()))
    prefixes = {k.split('.')[0] for k in table}
    used = defaultdict(list)
    for dirpath, _, files in os.walk(SOURCE):
        for name in sorted(files):
            if not name.lower().endswith('.pas') or name.startswith('uMakerAi.Lang.'):
                continue
            path = os.path.join(dirpath, name)
            raw = open(path, 'rb').read()
            try:
                text = raw.decode('utf-8-sig')
            except UnicodeDecodeError:
                text = raw.decode('cp1252')
            text = strip_comments(text.replace('\r\n', '\n'))
            for lineno, line in enumerate(text.split('\n'), 1):
                for m in RE_KEY_LIT.finditer(line):
                    key = m.group(1) or m.group(2)
                    if key.split('.')[0] in prefixes:
                        used[key].append(f'Source/{os.path.relpath(path, SOURCE).replace(os.sep, "/")}:{lineno}')
    missing = sorted(k for k in used if k not in table)
    unused = sorted(k for k in table if k not in used)
    print(f'Claves en la tabla base: {len(table)}; usadas en Source: {len(used)}')
    for k in missing:
        print(f'FALTA en uMakerAi.Lang.En.pas: {k}  ({used[k][0]})')
    for k in unused:
        print(f'sin uso: {k}')
    return 1 if missing else 0


# Exclusiones a propósito (decisiones de la v4.0):
#  - componentes y drivers obsoletos o copias viejas: no se traducen
#  - lo que lee el modelo va en inglés fijo: errores JSON-RPC del servidor MCP
#    y el "File not found:" que el TextEditor devuelve como resultado de la tool
#  - los mensajes internos de TAiLocale (no se traducen a sí mismos)
INTENTIONAL_FILES = re.compile(
    r'(_Deprecated|_beta)\.pas$|^ChatUI/uMakerAi\.(UI\.Chat\w+|Utils\.ScreenCapture)\.pas$'
    r'|^MCPServer/uMakerAi\.MCPServer\.Core\.pas$|^Core/uMakerAi\.Localization\.pas$')
INTENTIONAL_LITS = {('Tools/uMakerAi.Tools.TextEditor.pas', "'File not found: '")}


def is_intentional(rel, lit):
    if INTENTIONAL_FILES.search(rel):
        # en MCPServer.Core solo los errores; los "Cannot register" sí van por tabla
        return True
    return (rel, lit) in INTENTIONAL_LITS


def main():
    if hasattr(sys.stdout, 'reconfigure'):
        sys.stdout.reconfigure(encoding='utf-8', errors='replace')
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--list', action='store_true', help='listar cada texto con archivo:línea')
    ap.add_argument('--kind', choices=['exception', 'ui', 'prompt', 'state'], help='filtrar por tipo')
    ap.add_argument('--folder', help='filtrar por carpeta de Source (p. ej. ChatUI)')
    ap.add_argument('--check-keys', action='store_true',
                    help='verificar que cada clave usada exista en la tabla base (exit 1 si falta alguna)')
    ap.add_argument('--strict', action='store_true',
                    help='exit 1 si queda algún texto pendiente (para la checklist de release)')
    ap.add_argument('--all', action='store_true',
                    help='incluir también las exclusiones a propósito')
    args = ap.parse_args()
    if args.check_keys:
        return check_keys()

    counts = defaultdict(lambda: defaultdict(int))
    listing = []
    skipped = 0
    for dirpath, _, files in os.walk(SOURCE):
        for name in sorted(files):
            if not name.lower().endswith('.pas') or name.startswith('uMakerAi.Lang.'):
                continue
            path = os.path.join(dirpath, name)
            rel = os.path.relpath(path, SOURCE)
            folder = rel.split(os.sep)[0]
            if args.folder and folder.lower() != args.folder.lower():
                continue
            relp = rel.replace(os.sep, '/')
            for kind, lineno, lit in scan_file(path):
                if args.kind and kind != args.kind:
                    continue
                if not args.all and is_intentional(relp, lit):
                    skipped += 1
                    continue
                counts[folder][kind] += 1
                listing.append((relp, lineno, kind, lit))

    kinds = ['ui', 'state', 'prompt', 'exception']
    print(f"{'Carpeta':<14}" + ''.join(f'{k:>11}' for k in kinds) + f"{'total':>9}")
    tot = defaultdict(int)
    for folder in sorted(counts):
        row = counts[folder]
        for k in kinds:
            tot[k] += row[k]
        print(f'{folder:<14}' + ''.join(f'{row[k]:>11}' for k in kinds) + f'{sum(row.values()):>9}')
    print(f"{'TOTAL':<14}" + ''.join(f'{tot[k]:>11}' for k in kinds) + f'{sum(tot.values()):>9}')

    if skipped:
        print(f'(+{skipped} exclusiones a propósito; --all para verlas)')

    if args.list:
        print()
        for rel, lineno, kind, lit in listing:
            print(f'Source/{rel}:{lineno}  [{kind}]  {lit[:100]}')
    return 1 if args.strict and listing else 0


if __name__ == '__main__':
    sys.exit(main())
