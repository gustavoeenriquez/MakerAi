// MIT License - Copyright (c) 2024 Gustavo Enríquez - CimaMaker
// (texto completo de la licencia en uMakerAi.Localization.pas y LICENSE.txt)
//
// Textos del framework en español.
//
// Mismas claves que uMakerAi.Lang.En.pas (la tabla base). Una clave que falte
// aquí cae al inglés en ejecución, pero la suite de regresión la reporta
// ('locale.tables-complete'). Conservar los marcadores de Format del inglés.

unit uMakerAi.Lang.Es;

interface

const
  AiLangEs: TArray<string> = [
    // --- ChatUI: menú contextual de TAIChatView ---
    'chatui.copy_message',      'Copiar mensaje',
    'chatui.copy_conversation', 'Copiar conversación',
    'chatui.copy_selection',    'Copiar selección',
    'chatui.open_attachment',   'Abrir adjunto',
    'chatui.open',              'Abrir',
    'chatui.play',              'Reproducir',
    'chatui.save_as',           'Guardar como…',
    'chatui.speak',             'Escuchar en voz alta',
    // --- ChatUI: menú de adjuntos de TAIChatInput y filtro del diálogo ---
    'chatui.attach.paste',      'Pegar',
    'chatui.attach.upload',     'Subir archivo...',
    'chatui.attach.screenshot', 'Captura de pantalla...',
    'chatui.attach.clear',      'Quitar adjuntos',
    'chatui.filter.supported',  'Todos los compatibles',
    'chatui.filter.images',     'Imágenes',
    'chatui.filter.documents',  'Documentos',
    'chatui.filter.all_files',  'Todos los archivos',
    // --- ChatUI: "Copiar conversación" (prefijo de cada mensaje) ---
    'chatui.role.user',         'Usuario',
    'chatui.role.assistant',    'Asistente',
    'chatui.role.system',       'Sistema',
    // --- ChatUI: botones dibujados (bloque de código, tarjeta de medio) y pasos ---
    'chatui.copy',              'Copiar',
    'chatui.copied',            'Copiado',
    'chatui.save',              'Guardar',
    'chatui.steps_count',       '%d pasos',

    // --- Design: editor del cliente MCP (uMCPClientEditor) ---
    'design.mcp.caption',        'Editor del cliente MCP',
    'design.mcp.transport',      'Tipo de transporte',
    'design.mcp.name',           'Nombre:',
    'design.mcp.test',           'Probar',
    'design.mcp.set_defaults',   'Parámetros por defecto',
    'design.mcp.tab_params',     'Parámetros',
    'design.mcp.tab_log',        'Registro',
    'design.mcp.col_property',   'Propiedad',
    'design.mcp.col_value',      'Valor',
    'design.mcp.status',         'Estado: %s',
    'design.mcp.test_start',     '--- Iniciando la prueba de conexión ---',
    'design.mcp.sse_url',        'Error: con SSE el parámetro "URL" es obligatorio (ej.: http://localhost:3000/sse)',
    'design.mcp.protocol',       'Protocolo: %s',
    'design.mcp.init_ok',        '>> Inicialización correcta. Solicitando herramientas...',
    'design.mcp.tools_received', '>> Herramientas recibidas:',
    'design.mcp.test_ok',        '[OK] Conexión correcta.',
    'design.mcp.tools_found',    'Se encontraron %d herramientas.',
    'design.mcp.tools_empty',    '[!] Conectado, pero ListTools no devolvió nada.',
    'design.mcp.init_error',     '[ERR] Error al inicializar el cliente.',
    'design.mcp.see_log',        'Revisa la pestaña Registro para más detalles.',
    'design.mcp.exception',      'Excepción: %s',
    // --- Design: editores de TAiChatConnection / TAiEmbeddingConnection ---
    'design.conn.reset_verb',    'Restablecer parámetros por defecto',
    'design.conn.reset_done',    'Parámetros restablecidos a los del driver: %s',
    'design.conn.reset_error',   'Error al restablecer los parámetros: %s',
    // --- Design: Acerca de ---
    'design.about.caption',      'Acerca de MakerAI',
    'design.about.version',      'Versión:',
    'design.about.copyright',    'Copyright:',
    'design.about.description',  'Descripción:',
    'design.about.version_info', 'Compilación:'
  ];

implementation

end.
