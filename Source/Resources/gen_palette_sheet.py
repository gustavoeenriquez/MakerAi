# Lamina resumen de la paleta de componentes de MakerAI, agrupada por categoria.
#
# Uso:
#   python gen_palette_sheet.py [salida.png]
# Por defecto escribe Docs/Version 3/images/MakerAI-Component-Palette.png
#
# Al agregar un componente con icono, sumarlo a la categoria que corresponda;
# el script avisa si queda algun BMP de icons\ sin categoria.

import os
import sys
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ICONS = os.path.join(HERE, 'icons')
ROOT = os.path.abspath(os.path.join(HERE, '..', '..'))
DEFAULT_OUT = os.path.join(ROOT, 'Docs', 'Version 3', 'images', 'MakerAI-Component-Palette.png')
FONTS = r'C:\Windows\Fonts'

# Clases cuyo BMP no se llama igual que la clase
FILE_ALIAS = {
    'TAiOpenChat': 'TAiOpenAIChat',
    'TAiRagGraph': 'TAiRAGGraph',
    'TAiRagGraphBuilder': 'TAiRAGBuilder',
    'TAiMCPStdioServer': 'TAiMCPStdIoServer',
    'TAIVoiceMonitor': 'TAIVoiceMonitor',
    'TAiLMStudioEmbeddings': 'TAiGeminiEmbeddings',   # el .rc reutiliza ese BMP
}
IGNORED_FILES = {'TAiOpenAIResponsesChat'}   # recurso historico sin componente

CATEGORIES = [
    ('Chat connection & LLM drivers', 'MakerAI', [
        'TAiChatConnection', 'TAiOpenChat', 'TAiClaudeChat', 'TAiGeminiChat', 'TAiOllamaChat',
        'TAiGroqChat', 'TAiDeepSeekChat', 'TAiKimiChat', 'TAiGrokChat', 'TAiMistralChat',
        'TAiLMStudioChat', 'TAiGLMChat', 'TAiQwenChat', 'TAiMiniMaxChat', 'TCohereChat', 'TAiLlamacppChat',
        'TAiMakerAiChat', 'TAiGenericChat']),
    ('Tools, functions & governance', 'MakerAI', [
        'TAiFunctions', 'TAiPrompts', 'TAiSkills', 'TAiShell', 'TAiTextEditorTool',
        'TAiComputerUseTool', 'TAiGuardrails', 'TAiEvalRunner', 'TAiTelemetry']),
    ('Capability bridges', 'MakerAI', [
        'TAiChatSpeechBridge', 'TAiChatVisionBridge', 'TAiChatImageBridge', 'TAiChatVideoBridge',
        'TAiChatDocumentBridge', 'TAiChatCodeInterpreterBridge', 'TAiChatWebSearchBridge']),
    ('Media tools: image, video, speech, search, vision', 'MakerAI / MakerAI Tools', [
        'TAiDalle', 'TAiDalleImageTool', 'TAiSoraGenerator', 'TAiSoraVideoTool',
        'TAiVeoGenerator', 'TAiGeminiVideoTool', 'TAiWhisper', 'TAiOpenAiSpeechTool',
        'TAiGeminiSpeechTool', 'TAiElevenLabsSpeechTool', 'TAiQwenVoices',
        'TAiOpenAiWebSearchTool', 'TAiGeminiWebSearchTool', 'TAiOllamaVisionTool',
        'TAiOllamaOcrTool', 'TAiQwenRAGReranker']),
    ('Audio & realtime voice', 'MakerAI', [
        'TAIVoiceMonitor', 'TAiAudioCapture', 'TAiAudioPlayer', 'TAiOpenAiAudio',
        'TAiOpenAiRealtimeSTT', 'TAiOpenAiRealtimeTranslate', 'TAiOpenAiLiveChat', 'TAiGeminiRealtimeSTT',
        'TAiGrokRealtimeChat', 'TAiMakerAiRealtimeChat', 'TAiQwenRealtimeChat',
        'TAiQwenRealtimeSTT', 'TAiQwenRealtimeTTS', 'TAiQwenRealtimeTranslate']),
    ('Embeddings', 'MakerAI', [
        'TAiEmbeddingConnection', 'TAiOpenAiEmbeddings', 'TAiGeminiEmbeddings',
        'TAiMistralEmbeddings', 'TAiOllamaEmbeddings', 'TAiLMStudioEmbeddings',
        'TAiCohereEmbeddings', 'TAiQwenEmbeddings', 'TAiLlamacppEmbeddings',
        'TAiGenericEmbeddings']),
    ('RAG, storage drivers & memory', 'MakerAI / MakerAI.RAG.Drivers / MakerAI.Memory', [
        'TAiRAGVector', 'TAiRagGraph', 'TAiRagGraphBuilder', 'TAiRagDocumentManager',
        'TAiRAGVectorPostgresDriver', 'TAiRAGVectorMSSQLDriver', 'TAiRAGVectorSQLiteDriver',
        'TAiMkVecDriver', 'TAiRagGraphPostgresDriver', 'TAiMemory']),
    ('Agents & A2A', 'MakerAI', [
        'TAIAgentManager', 'TAIAgents', 'TAIAgentsNode', 'TAIAgentsLink', 'TLLMNode',
        'TAiAgentsToolSample', 'TAiA2AServer', 'TAiA2AClient', 'TAiA2AAgentTool', 'TAiA2ARemoteAgentTool']),
    ('MCP servers', 'MakerAI', [
        'TAiMCPStdioServer', 'TAiMCPHttpServer', 'TAiMCPSSEHttpServer', 'TAiMCPDirectConnection']),
    ('Jev - calibrated decisions without an LLM', 'MakerAI', [
        'TAiJev', 'TAiJevDispatchClassifier', 'TAiJevModelRouter', 'TAiJevGuardrailClassifier',
        'TAiJevPromptGuard', 'TAiJevRAGReranker', 'TAiJevEvalScorer', 'TAiJevBatchLabeler']),
    ('Chat UI (FireMonkey)', 'MakerAI UI / MakerAI Chat', [
        'TChatList', 'TChatBubble', 'TChatInput', 'TAIChatView', 'TAIChatInput']),
]

# Colores de la lamina
BG = (247, 245, 243)
CARD = (255, 255, 255)
BORDER = (226, 220, 216)
INK = (34, 30, 28)
MUTED = (120, 110, 106)
ACCENT = (122, 14, 14)

ZOOM = 2            # iconos a 48 px, pixel exacto
CELL_W, CELL_H = 132, 110
COLS = 9
PAD = 36


def font(name, size):
    return ImageFont.truetype(os.path.join(FONTS, name), size)


F_TITLE = font('segoeuib.ttf', 34)
F_SUB = font('segoeui.ttf', 17)
F_CAT = font('segoeuib.ttf', 20)
F_TAB = font('segoeui.ttf', 14)
F_NAME = font('segoeui.ttf', 13)


def icon_file(cls):
    return os.path.join(ICONS, FILE_ALIAS.get(cls, cls) + '_24.bmp')


def load_icon(cls):
    im = Image.open(icon_file(cls)).convert('RGB')
    # El IDE trata el pixel inferior izquierdo como transparente: lo imitamos
    key = im.getpixel((0, im.height - 1))
    rgba = im.convert('RGBA')
    rgba.putdata([(r, g, b, 0) if (r, g, b) == key else (r, g, b, 255) for r, g, b, _ in rgba.getdata()])
    canvas = Image.new('RGBA', (24, 24), (0, 0, 0, 0))
    canvas.paste(rgba, ((24 - im.width) // 2, (24 - im.height) // 2))
    return canvas.resize((24 * ZOOM, 24 * ZOOM), Image.NEAREST)


def check_coverage():
    used = {FILE_ALIAS.get(c, c).lower() for _, _, cls in CATEGORIES for c in cls}
    files = {f[:-7].lower() for f in os.listdir(ICONS) if f.endswith('_24.bmp')}
    missing_files = sorted(c for _, _, cls in CATEGORIES for c in cls if not os.path.exists(icon_file(c)))
    orphans = sorted(files - used - {f.lower() for f in IGNORED_FILES})
    if missing_files:
        raise SystemExit('Sin BMP: ' + ', '.join(missing_files))
    if orphans:
        print('AVISO: iconos sin categoria:', ', '.join(orphans))


def split_name(d, text, width):
    """Una linea si cabe; si no, dos cortando en la frontera CamelCase mas centrada."""
    if d.textlength(text, font=F_NAME) <= width:
        return [text]
    cuts = [i for i in range(3, len(text)) if text[i].isupper() and not text[i - 1].isupper()]
    cuts = [i for i in cuts
            if d.textlength(text[:i], font=F_NAME) <= width and d.textlength(text[i:], font=F_NAME) <= width]
    if not cuts:
        return [text]
    best = min(cuts, key=lambda i: abs(d.textlength(text[:i], font=F_NAME) - d.textlength(text[i:], font=F_NAME)))
    return [text[:best], text[best:]]


# Componentes que siguen registrados solo para que compilen apps viejas.
DEPRECATED = {'TChatList', 'TChatBubble', 'TChatInput'}


def makerai_version():
    # MAKERAI_VERSION_SHORT de uMakerAi.Version.inc (archivo ANSI), para que el
    # titulo de la lamina no quede atrasado al cambiar de version.
    inc = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'Core', 'uMakerAi.Version.inc')
    with open(inc, 'rb') as f:
        for line in f.read().decode('latin-1').splitlines():
            if 'MAKERAI_VERSION_SHORT' in line and "'" in line:
                return line.split("'")[1]
    return '?'


def main(out):
    check_coverage()
    width = PAD * 2 + COLS * CELL_W
    # altura
    h = PAD + 110
    for _, _, cls in CATEGORIES:
        rows = (len(cls) + COLS - 1) // COLS
        h += 58 + rows * CELL_H + 22
    h += 40
    img = Image.new('RGB', (width, h), BG)
    d = ImageDraw.Draw(img)

    total = sum(len(c) for _, _, c in CATEGORIES)
    d.text((PAD, PAD - 4), f'MakerAI {makerai_version()}  ·  Component Palette', font=F_TITLE, fill=INK)
    d.text((PAD, PAD + 46), f'{total} components in {len(CATEGORIES)} categories  ·  '
           'tile color = provider (maroon = MakerAI framework)', font=F_SUB, fill=MUTED)
    d.rectangle((PAD, PAD + 82, PAD + 64, PAD + 86), fill=ACCENT)

    y = PAD + 110
    for title, tabs, classes in CATEGORIES:
        rows = (len(classes) + COLS - 1) // COLS
        card_h = 58 + rows * CELL_H
        d.rounded_rectangle((PAD - 12, y, width - PAD + 12, y + card_h), radius=14,
                            fill=CARD, outline=BORDER, width=1)
        d.rectangle((PAD - 12, y + 16, PAD - 8, y + 40), fill=ACCENT)
        d.text((PAD + 4, y + 14), title, font=F_CAT, fill=INK)
        tw = d.textlength(title, font=F_CAT)
        d.text((PAD + 4 + tw + 14, y + 19), f'palette tab: {tabs}', font=F_TAB, fill=MUTED)
        for i, cls in enumerate(classes):
            cx = PAD + (i % COLS) * CELL_W
            cy = y + 54 + (i // COLS) * CELL_H
            ic = load_icon(cls)
            img.paste(ic, (cx + (CELL_W - ic.width) // 2, cy + 4), ic)
            lines = split_name(d, cls, CELL_W - 8)
            if cls in DEPRECATED:
                lines = list(lines) + ['(deprecated)']
            for k, line in enumerate(lines):
                nw = d.textlength(line, font=F_NAME)
                d.text((cx + (CELL_W - nw) / 2, cy + 58 + k * 17), line, font=F_NAME,
                       fill=MUTED if cls in DEPRECATED else INK)
        y += card_h + 22

    d.text((PAD, y + 4), 'Generated by Source/Resources/gen_palette_sheet.py  ·  '
           'icons: Source/Resources/icons  ·  https://makerai.cimamaker.com', font=F_TAB, fill=MUTED)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    img.save(out, optimize=True)
    print('lamina:', out, img.size, 'componentes:', total)


SOCIAL_DIR = os.path.join(ROOT, 'Docs', 'Version 3', 'images', 'social')


def social(width, height, cols, icon_px, out):
    # Imagen para redes: titulo con la version, contador, todos los iconos en
    # rejilla y el pie. Nombre de archivo sin version: se sobrescribe en cada una.
    classes = [c for _, _, cls in CATEGORIES for c in cls]
    img = Image.new('RGB', (width, height), BG)
    d = ImageDraw.Draw(img)
    d.rectangle((0, 0, width, 8), fill=ACCENT)
    f_big = font('segoeuib.ttf', 58)
    f_pill = font('segoeuib.ttf', 20)
    f_sub = font('segoeui.ttf', 24)
    f_feat = font('segoeuib.ttf', 21)
    f_foot = font('segoeui.ttf', 18)
    x0, y = 64, 52
    title = f'MakerAI {makerai_version()}'
    d.text((x0, y), title, font=f_big, fill=INK)
    tw = d.textlength(title, font=f_big)
    pill = f'{len(classes)} components'
    pw = d.textlength(pill, font=f_pill)
    px, py = x0 + tw + 24, y + 12
    d.rounded_rectangle((px, py, px + pw + 28, py + 40), radius=20, fill=ACCENT)
    d.text((px + 14, py + 7), pill, font=f_pill, fill=(255, 255, 255))
    y += 70
    d.text((x0, y), 'AI orchestration for Delphi, straight from the component palette', font=f_sub, fill=MUTED)
    y += 60
    gap = 10
    grid_w = cols * icon_px + (cols - 1) * gap
    gx = (width - grid_w) // 2
    rows = (len(classes) + cols - 1) // cols
    grid_h = rows * icon_px + (rows - 1) * gap
    y += max(0, ((height - 128) - y - grid_h) // 2)   # centrada entre subtitulo y pie
    for i, cls in enumerate(classes):
        ic = load_icon(cls).resize((icon_px, icon_px), Image.NEAREST)
        img.paste(ic, (gx + (i % cols) * (icon_px + gap), y + (i // cols) * (icon_px + gap)), ic)
    fy = height - 108
    d.text((x0, fy), '15+ LLM providers · Agents · RAG · MCP · A2A · Realtime voice · Jev · FMX Chat UI',
           font=f_feat, fill=INK)
    d.text((x0, fy + 38), 'Open source (MIT)  ·  makerai.cimamaker.com  ·  github.com/gustavoeenriquez/MakerAi',
           font=f_foot, fill=MUTED)
    img.save(out, optimize=True)
    print('social:', out, img.size, 'componentes:', len(classes))


if __name__ == '__main__':
    if '--social' in sys.argv:
        check_coverage()
        os.makedirs(SOCIAL_DIR, exist_ok=True)
        social(1200, 675, 19, 48, os.path.join(SOCIAL_DIR, 'MakerAI-palette-1200x675.png'))
        social(1080, 1080, 13, 64, os.path.join(SOCIAL_DIR, 'MakerAI-palette-1080x1080.png'))
    else:
        main(sys.argv[1] if len(sys.argv) > 1 else DEFAULT_OUT)
