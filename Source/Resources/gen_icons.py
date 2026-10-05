# Generador de iconos de paleta de MakerAI (24x24 BMP).
#
# Dibuja cada icono a 8x (192 px) y lo reduce a 24 px con antialiasing, sobre
# fondo blanco: el IDE toma el pixel inferior izquierdo como color transparente,
# por eso los tiles tienen esquinas redondeadas blancas.
#
# Uso:
#   python gen_icons.py            -> genera los BMP que falten en icons\
#   python gen_icons.py --force    -> regenera tambien los que ya existen
#   python gen_icons.py --preview salida.png   -> hoja de contacto ampliada
#
# Para agregar un icono: sumar una entrada a SPECS y luego la linea
# correspondiente en el .rc del paquete (ver RC_FILES) y recompilar con brcc32.

import os
import sys
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ICONS = os.path.join(HERE, 'icons')
S = 8                    # factor de sobremuestreo
N = 24 * S               # lienzo de trabajo (192)
FONTS = r'C:\Windows\Fonts'
F_NARROW = os.path.join(FONTS, 'ARIALNB.TTF')
F_BOLD = os.path.join(FONTS, 'arialbd.ttf')

WHITE = (255, 255, 255)

# Colores de familia / proveedor
MAROON = (122, 14, 14)     # MakerAI (igual a los iconos existentes)
JEV = (14, 96, 108)
OPENAI = (20, 20, 20)
GEMINI = (26, 108, 220)
GROK = (48, 48, 56)
QWEN = (97, 92, 237)
COHERE = (57, 89, 77)
GLM = (40, 72, 190)
LLAMACPP = (176, 88, 24)
OLLAMA = (70, 70, 70)
ELEVEN = (0, 0, 0)


# ---------------------------------------------------------------------------
# Primitivas
# ---------------------------------------------------------------------------

def new_canvas():
    return Image.new('RGB', (N, N), WHITE)


def tile(d, color):
    d.rounded_rectangle((0, 0, N - 1, N - 1), radius=36, fill=color)


def fit_text(d, text, box, color, font_path=None):
    """Centra el texto en box ajustando el tamano a lo que quepa."""
    if font_path is None:
        font_path = F_BOLD if len(text) <= 3 else F_NARROW
    x0, y0, x1, y1 = box
    bw, bh = x1 - x0, y1 - y0
    size = bh * 2
    while size > 8:
        f = ImageFont.truetype(font_path, size)
        l, t, r, b = d.textbbox((0, 0), text, font=f)
        if r - l <= bw and b - t <= bh:
            break
        size -= 2
    l, t, r, b = d.textbbox((0, 0), text, font=f)
    x = x0 + (bw - (r - l)) / 2 - l
    y = y0 + (bh - (b - t)) / 2 - t
    d.text((x, y), text, font=f, fill=color)


def sub_layer(draw_fn, color, box, bg):
    """Dibuja un glifo disenado en coordenadas 0..192 y lo escala a box."""
    layer = Image.new('RGBA', (N, N), (0, 0, 0, 0))
    ld = ImageDraw.Draw(layer)
    draw_fn(ld, color, bg)
    x0, y0, x1, y1 = box
    return layer.resize((x1 - x0, y1 - y0), Image.LANCZOS), (x0, y0)


# ---------------------------------------------------------------------------
# Glifos (coordenadas 0..192, trazo blanco salvo que se indique)
# ---------------------------------------------------------------------------

W = 16  # grosor de trazo base


def g_mic(d, c, bg):
    d.rounded_rectangle((72, 20, 120, 112), radius=24, fill=c)
    d.arc((44, 52, 148, 140), 0, 180, fill=c, width=W)
    d.line((96, 140, 96, 164), fill=c, width=W)
    d.line((64, 168, 128, 168), fill=c, width=W)


def g_speaker(d, c, bg):
    d.polygon([(24, 72), (60, 72), (104, 32), (104, 160), (60, 120), (24, 120)], fill=c)
    d.arc((76, 60, 148, 132), -50, 50, fill=c, width=W)
    d.arc((60, 30, 176, 162), -50, 50, fill=c, width=W)


def g_wave(d, c, bg):
    hs = [30, 70, 110, 60, 140, 90, 50, 100, 40]
    step = 19
    x = 96 - step * (len(hs) - 1) / 2
    for h in hs:
        d.line((x, 96 - h / 2, x, 96 + h / 2), fill=c, width=12)
        x += step


def g_monitor(d, c, bg):
    d.rounded_rectangle((16, 24, 176, 136), radius=10, outline=c, width=W)
    d.line((96, 136, 96, 160), fill=c, width=W)
    d.line((56, 166, 136, 166), fill=c, width=W)
    d.polygon([(76, 48), (76, 118), (94, 102), (108, 128), (120, 122), (106, 96), (130, 96)], fill=c)


def g_terminal(d, c, bg):
    d.rounded_rectangle((12, 24, 180, 168), radius=14, outline=c, width=W - 4)
    d.line([(44, 64), (84, 96), (44, 128)], fill=c, width=W + 2, joint='curve')
    d.line((96, 132, 148, 132), fill=c, width=W + 2)


def g_doc_pencil(d, c, bg):
    d.polygon([(28, 16), (104, 16), (136, 48), (136, 176), (28, 176)], outline=c, width=W - 2)
    for y in (70, 100, 130):
        d.line((52, y, 108, y), fill=c, width=10)
    # lapiz
    d.polygon([(168, 70), (186, 88), (112, 162), (88, 168), (94, 144)], fill=c)
    d.polygon([(112, 162), (88, 168), (94, 144)], fill=bg)


def g_book(d, c, bg):
    d.polygon([(96, 44), (20, 28), (20, 152), (96, 168)], fill=c)
    d.polygon([(96, 44), (172, 28), (172, 152), (96, 168)], fill=c)
    d.line((96, 44, 96, 168), fill=bg, width=8)
    for y in (64, 92, 120):
        d.line((36, y - 8, 80, y), fill=bg, width=7)
        d.line((112, y, 156, y - 8), fill=bg, width=7)


def g_shield(d, c, bg):
    d.polygon([(96, 12), (168, 40), (160, 116), (96, 180), (32, 116), (24, 40)], fill=c)
    d.line([(60, 96), (88, 124), (136, 70)], fill=bg, width=W + 4, joint='curve')


def g_checklist(d, c, bg):
    for i, y in enumerate((44, 96, 148)):
        d.line([(20, y), (34, y + 14), (58, y - 14)], fill=c, width=12, joint='curve')
        d.line((78, y, 172, y), fill=c, width=W)


def g_pulse(d, c, bg):
    d.line([(12, 104), (52, 104), (72, 60), (100, 152), (124, 36), (144, 104), (180, 104)],
           fill=c, width=W, joint='curve')


def g_chip(d, c, bg):
    d.rounded_rectangle((48, 48, 144, 144), radius=12, fill=c)
    d.ellipse((66, 66, 86, 86), fill=bg)
    for p in (72, 96, 120):
        d.line((p, 16, p, 48), fill=c, width=10)
        d.line((p, 144, p, 176), fill=c, width=10)
        d.line((16, p, 48, p), fill=c, width=10)
        d.line((144, p, 176, p), fill=c, width=10)


def g_picture(d, c, bg):
    d.rounded_rectangle((16, 32, 176, 160), radius=10, outline=c, width=W)
    d.polygon([(32, 146), (80, 84), (112, 124), (132, 104), (162, 146)], fill=c)
    d.ellipse((124, 52, 152, 80), fill=c)


def g_film(d, c, bg):
    d.rounded_rectangle((12, 36, 180, 156), radius=10, fill=c)
    for x in range(26, 180, 30):
        d.rectangle((x, 46, x + 14, 58), fill=bg)
        d.rectangle((x, 134, x + 14, 146), fill=bg)
    d.polygon([(78, 72), (78, 120), (122, 96)], fill=bg)


def g_globe(d, c, bg):
    d.ellipse((24, 24, 168, 168), outline=c, width=W)
    d.ellipse((66, 24, 126, 168), outline=c, width=W - 4)
    d.line((24, 96, 168, 96), fill=c, width=W - 4)
    d.line((38, 60, 154, 60), fill=c, width=W - 6)
    d.line((38, 132, 154, 132), fill=c, width=W - 6)


def g_eye(d, c, bg):
    d.chord((8, 48, 184, 200), 200, 340, fill=c)
    d.chord((8, -8, 184, 144), 20, 160, fill=c)
    d.ellipse((66, 66, 126, 126), fill=bg)
    d.ellipse((82, 82, 110, 110), fill=c)


def g_rank(d, c, bg):
    for i, w in enumerate((140, 108, 76, 44)):
        y = 36 + i * 38
        d.rounded_rectangle((24, y, 24 + w, y + 24), radius=6, fill=c)
    d.line((164, 36, 164, 150), fill=c, width=12)
    d.polygon([(144, 140), (184, 140), (164, 172)], fill=c)


def g_db(d, c, bg):
    d.rectangle((36, 40, 156, 150), fill=c)
    d.ellipse((36, 132, 156, 168), fill=c)
    d.ellipse((36, 22, 156, 58), fill=c)
    d.ellipse((46, 28, 146, 52), outline=bg, width=6)
    d.arc((36, 64, 156, 100), 0, 180, fill=bg, width=6)
    d.arc((36, 100, 156, 136), 0, 180, fill=bg, width=6)


def bubble_shape(d, box, c, **kw):
    x0, y0, x1, y1 = box
    d.rounded_rectangle(box, radius=int((y1 - y0) * 0.28), **({'fill': c} | kw))
    tail = [(x0 + 22, y1 - 4), (x0 + 16, y1 + 26), (x0 + 58, y1 - 4)]
    d.polygon(tail, fill=kw.get('fill', c) if 'outline' not in kw else c)


def g_graph(d, c, bg):
    pts = [(40, 44), (150, 36), (96, 104), (36, 156), (156, 150)]
    for a, b in ((0, 2), (1, 2), (2, 3), (2, 4), (0, 1), (3, 4)):
        d.line((pts[a], pts[b]), fill=c, width=10)
    for x, y in pts:
        r = 26 if (x, y) == (96, 104) else 20
        d.ellipse((x - r, y - r, x + r, y + r), fill=c)


def g_bubble(d, c, bg):
    bubble_shape(d, (12, 28, 180, 140), c)


def g_chatlist(d, c, bg):
    d.rounded_rectangle((12, 16, 128, 60), radius=14, fill=c)
    d.rounded_rectangle((64, 74, 180, 118), radius=14, fill=c)
    d.rounded_rectangle((12, 132, 128, 176), radius=14, fill=c)


def g_inputbar(d, c, bg, mic=False):
    d.rounded_rectangle((8, 56, 184, 136), radius=40, outline=c, width=W - 2)
    d.line((40, 96, 104, 96), fill=c, width=10)
    if mic:
        d.rounded_rectangle((126, 70, 150, 110), radius=12, fill=c)
        d.arc((116, 80, 160, 122), 0, 180, fill=c, width=7)
        d.line((138, 122, 138, 128), fill=c, width=7)
    else:
        d.polygon([(122, 72), (164, 96), (122, 120), (130, 96)], fill=c)


def g_inputbar_mic(d, c, bg):
    g_inputbar(d, c, bg, mic=True)


def g_chatview(d, c, bg):
    d.rounded_rectangle((8, 8, 184, 184), radius=18, outline=c, width=W - 4)
    d.rounded_rectangle((28, 30, 120, 62), radius=12, fill=c)
    d.rounded_rectangle((72, 74, 164, 106), radius=12, fill=c)
    d.rounded_rectangle((28, 132, 164, 164), radius=16, outline=c, width=8)


def g_node_llm(d, c, bg):
    d.line((8, 96, 40, 96), fill=c, width=10)
    d.line((152, 96, 184, 96), fill=c, width=10)
    d.ellipse((0, 84, 24, 108), fill=c)
    d.ellipse((168, 84, 192, 108), fill=c)
    d.rounded_rectangle((36, 52, 156, 140), radius=14, fill=c)
    fit_text(d, 'LLM', (48, 70, 144, 122), bg, F_BOLD)


GLYPHS = {
    'mic': g_mic, 'speaker': g_speaker, 'wave': g_wave, 'monitor': g_monitor,
    'terminal': g_terminal, 'docpencil': g_doc_pencil, 'book': g_book,
    'shield': g_shield, 'checklist': g_checklist, 'pulse': g_pulse,
    'chip': g_chip, 'picture': g_picture, 'film': g_film, 'globe': g_globe,
    'eye': g_eye, 'rank': g_rank, 'db': g_db, 'graph': g_graph, 'bubble': g_bubble,
    'chatlist': g_chatlist, 'inputbar': g_inputbar, 'inputbarmic': g_inputbar_mic,
    'chatview': g_chatview, 'nodellm': g_node_llm,
}


# ---------------------------------------------------------------------------
# Layouts
# ---------------------------------------------------------------------------

def paste(img, layer_pos):
    layer, pos = layer_pos
    img.paste(layer, pos, layer)


def lay_glyph(color, glyph):
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, color)
    paste(img, sub_layer(GLYPHS[glyph], WHITE, (20, 20, 172, 172), color))
    return img


def lay_glyph_tag(color, glyph, tag):
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, color)
    paste(img, sub_layer(GLYPHS[glyph], WHITE, (44, 8, 148, 112), color))
    fit_text(d, tag, (14, 124, 178, 180), WHITE)
    return img


def lay_text2(color, l1, l2):
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, color)
    fit_text(d, l1, (14, 18, 178, 86), WHITE)
    fit_text(d, l2, (14, 106, 178, 174), WHITE)
    return img


def lay_text1(color, text):
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, color)
    fit_text(d, text, (14, 48, 178, 144), WHITE, F_BOLD)
    return img


def lay_bubble(color, text):
    """Driver de chat: burbuja blanca con monograma en el color del proveedor."""
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, color)
    bubble_shape(d, (16, 26, 176, 142), WHITE)
    fit_text(d, text, (30, 46, 162, 122), color, F_BOLD)
    return img


def lay_bridge(text):
    """Bridges: burbuja blanca en tile MakerAI + capacidad que agrega."""
    img = new_canvas(); d = ImageDraw.Draw(img); tile(d, MAROON)
    bubble_shape(d, (16, 22, 176, 136), WHITE)
    fit_text(d, text, (28, 42, 164, 116), MAROON)
    d.polygon([(118, 150), (176, 150), (176, 176), (118, 176)], fill=MAROON)
    return img


def lay_emb(color, tag):
    """Embeddings: mismo documento 'EMB' de los iconos existentes + franja del proveedor."""
    img = new_canvas(); d = ImageDraw.Draw(img)
    d.polygon([(20, 4), (140, 4), (172, 36), (172, 188), (20, 188)], fill=WHITE,
              outline=(110, 110, 110), width=10)
    d.polygon([(140, 4), (172, 36), (140, 36)], fill=(60, 60, 60))
    fit_text(d, 'EMB', (32, 40, 160, 108), MAROON, F_BOLD)
    d.rectangle((26, 120, 166, 182), fill=color)
    fit_text(d, tag, (32, 128, 160, 176), WHITE)
    return img


LAYOUTS = {
    'glyph': lay_glyph, 'glyphtag': lay_glyph_tag, 'text2': lay_text2,
    'text1': lay_text1, 'bubble': lay_bubble, 'bridge': lay_bridge, 'emb': lay_emb,
}


# ---------------------------------------------------------------------------
# Catalogo: (paquete, clase, layout, args...)
#   paquete: 'core' -> uMakerAiResources.rc (MakerAI.dpk)
#            'ui'   -> uMakerAiUIIcons.rc (MakerAi.UI.dpk)
#            'rag'  -> uMakerAiRAGDriversIcons.rc (MakerAi.RAG.Drivers.dpk)
# ---------------------------------------------------------------------------

SPECS = [
    # --- A2A, audio, tools genericos ---
    ('core', 'TAiA2AClient', 'text2', MAROON, 'A2A', 'CLI'),
    ('core', 'TAiA2AServer', 'text2', MAROON, 'A2A', 'SRV'),
    ('core', 'TAiA2AAgentTool', 'text2', MAROON, 'A2A', 'TOOL'),
    ('core', 'TAiAudioCapture', 'glyph', MAROON, 'mic'),
    ('core', 'TAiAudioPlayer', 'glyph', MAROON, 'speaker'),
    ('core', 'TAiComputerUseTool', 'glyph', MAROON, 'monitor'),
    ('core', 'TAiShell', 'glyph', MAROON, 'terminal'),
    ('core', 'TAiTextEditorTool', 'glyph', MAROON, 'docpencil'),
    ('core', 'TAiSkills', 'glyph', MAROON, 'book'),
    ('core', 'TAiGuardrails', 'glyph', MAROON, 'shield'),
    ('core', 'TAiEvalRunner', 'glyph', MAROON, 'checklist'),
    ('core', 'TAiTelemetry', 'glyph', MAROON, 'pulse'),
    ('core', 'TLLMNode', 'glyph', MAROON, 'nodellm'),
    ('core', 'TAiEmbeddingConnection', 'text2', MAROON, 'EMB', 'CONN'),
    ('core', 'TAiRagDocumentManager', 'text2', MAROON, 'RAG', 'DOCS'),
    ('core', 'TAiMCPSSEHttpServer', 'text2', MAROON, 'MCP', 'SSE'),
    # --- Bridges ---
    ('core', 'TAiChatSpeechBridge', 'bridge', 'TTS'),
    ('core', 'TAiChatVisionBridge', 'bridge', 'VIS'),
    ('core', 'TAiChatImageBridge', 'bridge', 'IMG'),
    ('core', 'TAiChatVideoBridge', 'bridge', 'VID'),
    ('core', 'TAiChatDocumentBridge', 'bridge', 'DOC'),
    ('core', 'TAiChatCodeInterpreterBridge', 'bridge', '</>'),
    ('core', 'TAiChatWebSearchBridge', 'bridge', 'WEB'),
    # --- Drivers de chat sin icono de marca ---
    ('core', 'TAiGLMChat', 'bubble', GLM, 'GLM'),
    ('core', 'TAiQwenChat', 'bubble', QWEN, 'QW'),
    ('core', 'TCohereChat', 'bubble', COHERE, 'CO'),
    ('core', 'TAiLlamacppChat', 'bubble', LLAMACPP, 'CPP'),
    ('core', 'TAiMakerAiChat', 'bubble', MAROON, 'MK'),
    ('core', 'TAiGenericChat', 'bubble', MAROON, 'API'),
    # --- Embeddings ---
    ('core', 'TAiCohereEmbeddings', 'emb', COHERE, 'CO'),
    ('core', 'TAiGenericEmbeddings', 'emb', MAROON, 'API'),
    ('core', 'TAiLlamacppEmbeddings', 'emb', LLAMACPP, 'CPP'),
    ('core', 'TAiQwenEmbeddings', 'emb', QWEN, 'QWEN'),
    # --- Realtime ---
    ('core', 'TAiOpenAiRealtimeSTT', 'text2', OPENAI, 'OAI', 'STT'),
    ('core', 'TAiOpenAiRealtimeTranslate', 'text2', OPENAI, 'OAI', 'TRAN'),
    ('core', 'TAiOpenAiLiveChat', 'text2', OPENAI, 'OAI', 'LIVE'),
    ('core', 'TAiOpenAiAudio', 'glyphtag', OPENAI, 'wave', 'OAI'),
    ('core', 'TAiGeminiRealtimeSTT', 'text2', GEMINI, 'GEM', 'STT'),
    ('core', 'TAiGrokRealtimeChat', 'text2', GROK, 'GROK', 'VOX'),
    ('core', 'TAiMakerAiRealtimeChat', 'text2', MAROON, 'MK', 'VOX'),
    ('core', 'TAiQwenRealtimeChat', 'text2', QWEN, 'QWEN', 'VOX'),
    ('core', 'TAiQwenRealtimeSTT', 'text2', QWEN, 'QWEN', 'STT'),
    ('core', 'TAiQwenRealtimeTTS', 'text2', QWEN, 'QWEN', 'TTS'),
    ('core', 'TAiQwenRealtimeTranslate', 'text2', QWEN, 'QWEN', 'TRAN'),
    # --- Tools por proveedor ---
    ('core', 'TAiDalleImageTool', 'glyphtag', OPENAI, 'picture', 'OAI'),
    ('core', 'TAiOpenAiSpeechTool', 'glyphtag', OPENAI, 'speaker', 'OAI'),
    ('core', 'TAiOpenAiWebSearchTool', 'glyphtag', OPENAI, 'globe', 'OAI'),
    ('core', 'TAiSoraGenerator', 'text2', OPENAI, 'SORA', 'GEN'),
    ('core', 'TAiSoraVideoTool', 'glyphtag', OPENAI, 'film', 'SORA'),
    ('core', 'TAiGeminiSpeechTool', 'glyphtag', GEMINI, 'speaker', 'GEM'),
    ('core', 'TAiGeminiVideoTool', 'glyphtag', GEMINI, 'film', 'GEM'),
    ('core', 'TAiGeminiWebSearchTool', 'glyphtag', GEMINI, 'globe', 'GEM'),
    ('core', 'TAiVeoGenerator', 'text2', GEMINI, 'VEO', 'GEN'),
    ('core', 'TAiElevenLabsSpeechTool', 'glyphtag', ELEVEN, 'speaker', '11L'),
    ('core', 'TAiOllamaOcrTool', 'text2', OLLAMA, 'OCR', 'OLL'),
    ('core', 'TAiOllamaVisionTool', 'glyphtag', OLLAMA, 'eye', 'OLL'),
    ('core', 'TAiQwenVoices', 'glyphtag', QWEN, 'speaker', 'QWEN'),
    ('core', 'TAiQwenRAGReranker', 'glyphtag', QWEN, 'rank', 'QWEN'),
    # --- Jev ---
    ('core', 'TAiJev', 'text1', JEV, 'JEV'),
    ('core', 'TAiJevBatchLabeler', 'text2', JEV, 'JEV', 'LBL'),
    ('core', 'TAiJevDispatchClassifier', 'text2', JEV, 'JEV', 'DISP'),
    ('core', 'TAiJevEvalScorer', 'text2', JEV, 'JEV', 'EVAL'),
    ('core', 'TAiJevGuardrailClassifier', 'text2', JEV, 'JEV', 'GRD'),
    ('core', 'TAiJevModelRouter', 'text2', JEV, 'JEV', 'RTR'),
    ('core', 'TAiJevPromptGuard', 'text2', JEV, 'JEV', 'INJ'),
    ('core', 'TAiJevRAGReranker', 'text2', JEV, 'JEV', 'RANK'),
    # --- UI (MakerAi.UI.dpk) ---
    ('ui', 'TChatList', 'glyph', MAROON, 'chatlist'),
    ('ui', 'TChatBubble', 'glyph', MAROON, 'bubble'),
    ('ui', 'TChatInput', 'glyph', MAROON, 'inputbar'),
    ('ui', 'TAIChatView', 'glyph', MAROON, 'chatview'),
    ('ui', 'TAIChatInput', 'glyph', MAROON, 'inputbarmic'),
    # --- RAG drivers + memoria (MakerAi.RAG.Drivers.dpk) ---
    ('rag', 'TAiMemory', 'glyph', MAROON, 'chip'),
    ('rag', 'TAiRAGVectorPostgresDriver', 'glyphtag', MAROON, 'db', 'PG'),
    ('rag', 'TAiRAGVectorMSSQLDriver', 'glyphtag', MAROON, 'db', 'MS'),
    ('rag', 'TAiRAGVectorSQLiteDriver', 'glyphtag', MAROON, 'db', 'LITE'),
    ('rag', 'TAiMkVecDriver', 'glyphtag', MAROON, 'db', 'MKV'),
    ('rag', 'TAiRagGraphPostgresDriver', 'glyphtag', MAROON, 'graph', 'PG'),
]

RC_FILES = {
    'core': 'uMakerAiResources.rc',
    'ui': 'uMakerAiUIIcons.rc',
    'rag': 'uMakerAiRAGDriversIcons.rc',
}


def render(spec):
    _, _, layout, *args = spec
    big = LAYOUTS[layout](*args)
    small = big.resize((24, 24), Image.LANCZOS)
    small.putpixel((0, 23), WHITE)   # pixel de transparencia del IDE
    return small


def icon_path(cls):
    return os.path.join(ICONS, cls + '_24.bmp')


def main(argv):
    force = '--force' in argv
    preview = argv[argv.index('--preview') + 1] if '--preview' in argv else None
    made = []
    for spec in SPECS:
        cls = spec[1]
        p = icon_path(cls)
        if force or not os.path.exists(p):
            render(spec).save(p, 'BMP')
            made.append(cls)
    print('generados:', len(made))
    if preview:
        z, cols = 5, 10
        cell = 24 * z + 16
        rows = (len(SPECS) + cols - 1) // cols
        sheet = Image.new('RGB', (cols * cell, rows * (cell + 30)), (236, 236, 236))
        d = ImageDraw.Draw(sheet)
        f = ImageFont.truetype(F_NARROW, 13)
        for i, spec in enumerate(SPECS):
            im = Image.open(icon_path(spec[1])).convert('RGB')
            x, y = (i % cols) * cell, (i // cols) * (cell + 30)
            sheet.paste(im.resize((24 * z, 24 * z), Image.NEAREST), (x + 8, y + 8))
            sheet.paste(im, (x + 8, y + 24 * z + 12))          # tamano real
            d.text((x + 38, y + 24 * z + 14), spec[1][1:][:18], font=f, fill=(0, 0, 0))
        sheet.save(preview)
        print('preview:', preview)


if __name__ == '__main__':
    main(sys.argv[1:])
