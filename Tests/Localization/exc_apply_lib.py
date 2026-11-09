"""Partes comunes de exc_apply.py y err_apply.py: uses y tablas de idioma."""
import os, re


def add_uses(text, rel):
    """Agrega uMakerAi.Localization al uses de implementation si no está."""
    if re.search(r'\buMakerAi\.Localization\b', text):
        return text
    ls = text.split('\n')
    impl = next(i for i, l in enumerate(ls) if l.strip().lower() == 'implementation')
    j = impl + 1
    # Se saltan blancos, comentarios y directivas que no abren una rama
    # ({$R *.dfm}, {$WARN ...}); un {$IF...} se revisa abajo
    while j < len(ls) and (ls[j].strip() == '' or ls[j].strip().startswith('//') or
                           (ls[j].strip().startswith('{$') and not ls[j].strip().upper().startswith('{$IF'))):
        j += 1
    if ls[j].strip().upper().startswith('{$IF'):
        # ¿El uses está bajo la directiva? Un uses condicional no se puede
        # ampliar sin romper la otra rama: hay que reescribirlo a mano como un
        # solo uses incondicional con la parte condicional adentro. Si lo que
        # sigue a la directiva es código, se agrega un uses propio antes.
        k = j + 1
        while k < len(ls) and (ls[k].strip() == '' or ls[k].strip().startswith('{$') or ls[k].strip().startswith('//')):
            k += 1
        if k < len(ls) and re.match(r'(?i)uses\b', ls[k].strip()):
            raise SystemExit(f'{rel}: el uses de implementation está bajo una directiva; ajustarlo a mano')
        cr = '\r' if ls[impl].endswith('\r') else ''
        ls[impl + 1:impl + 1] = [cr, 'uses' + cr, '  uMakerAi.Localization;' + cr]
        return '\n'.join(ls)
    cr = '\r' if ls[impl].endswith('\r') else ''
    if ls[j].strip().lower() == 'uses':
        ls.insert(j + 1, '  uMakerAi.Localization,' + cr)
    elif ls[j].strip().lower().startswith('uses '):
        ls[j] = re.sub(r'(?i)^(\s*uses\s+)', r'\1uMakerAi.Localization, ', ls[j], count=1)
    else:
        ls[impl + 1:impl + 1] = [cr, 'uses' + cr, '  uMakerAi.Localization;' + cr]
    return '\n'.join(ls)


def pas(s):
    return "'" + s.replace("'", "''") + "'"


def write_tables(src, used_keys, defs, title, dry):
    """Agrega las claves nuevas al final de cada tabla uMakerAi.Lang.*.pas.

    defs: clave -> (inglés, español, portugués), en el orden de LANGS.
    """
    langs = [l for l in ('En', 'Es', 'Pt')
             if os.path.exists(os.path.join(src, 'Core', f'uMakerAi.Lang.{l}.pas'))]
    for idx, lang in enumerate(langs):
        path = os.path.join(src, 'Core', f'uMakerAi.Lang.{lang}.pas')
        text = open(path, 'rb').read().decode('utf-8-sig')
        existing = set(re.findall(r"^\s*'([a-z0-9_.]+)',", text, re.M))
        new_keys = sorted(k for k in used_keys if k not in existing)
        if not new_keys:
            continue
        width = max(len(k) for k in new_keys) + 3
        block = [f'\r\n    // --- {title} ---']
        for k in new_keys:
            block.append(f"    {pas(k) + ',':<{width}} {pas(defs[k][idx])},")
        block[-1] = block[-1].rstrip(',')
        pos = text.rindex('\r\n  ];')
        before = text[:pos].rstrip()
        text = before + ',\r\n' + '\r\n'.join(block) + text[pos:]
        if not dry:
            open(path, 'wb').write(b'\xef\xbb\xbf' + text.encode('utf-8'))
