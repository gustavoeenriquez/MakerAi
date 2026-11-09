// MIT License - Copyright (c) 2024 Gustavo Enríquez - CimaMaker
// (texto completo de la licencia en uMakerAi.Localization.pas y LICENSE.txt)
//
// Tabla BASE de textos del framework: inglés.
//
// Debe tener TODAS las claves: es el respaldo de cualquier otro idioma y la
// referencia contra la que se validan las demás tablas (TAiLocale.Validate y
// el caso 'locale.tables-complete' de la suite de regresión).
//
// Formato: pares clave, texto. Convención de claves '<módulo>.<nombre>'.
// Los textos con marcadores de Format (%s, %d, %0:s ...) deben conservar los
// mismos marcadores, en el mismo orden, en todas las traducciones.
//
// Al agregar una clave aquí, agregarla también en uMakerAi.Lang.Es.pas.

unit uMakerAi.Lang.En;

interface

const
  AiLangEn: TArray<string> = [
    // --- ChatUI: menú contextual de TAIChatView ---
    'chatui.copy_message',      'Copy Message',
    'chatui.copy_conversation', 'Copy Conversation',
    'chatui.copy_selection',    'Copy Selection',
    'chatui.open_attachment',   'Open Attachment',
    'chatui.open',              'Open',
    'chatui.play',              'Play',
    'chatui.save_as',           'Save as…',
    'chatui.speak',             'Read aloud',
    // --- ChatUI: menú de adjuntos de TAIChatInput y filtro del diálogo ---
    'chatui.attach.paste',      'Paste',
    'chatui.attach.upload',     'Upload file...',
    'chatui.attach.screenshot', 'Screenshot...',
    'chatui.attach.clear',      'Clear attachments',
    'chatui.filter.supported',  'All supported',
    'chatui.filter.images',     'Images',
    'chatui.filter.documents',  'Documents',
    'chatui.filter.all_files',  'All files',
    // --- ChatUI: "Copiar conversación" (prefijo de cada mensaje) ---
    'chatui.role.user',         'User',
    'chatui.role.assistant',    'Assistant',
    'chatui.role.system',       'System',
    // --- ChatUI: botones dibujados (bloque de código, tarjeta de medio) y pasos ---
    'chatui.copy',              'Copy',
    'chatui.copied',            'Copied',
    'chatui.save',              'Save',
    'chatui.steps_count',       '%d steps',

    // --- Design: editor del cliente MCP (uMCPClientEditor) ---
    'design.mcp.caption',        'MCP Client Editor',
    'design.mcp.transport',      'Transport Type',
    'design.mcp.name',           'Name:',
    'design.mcp.test',           'Test',
    'design.mcp.set_defaults',   'Set Default Params',
    'design.mcp.tab_params',     'Params',
    'design.mcp.tab_log',        'Log',
    'design.mcp.col_property',   'Property',
    'design.mcp.col_value',      'Value',
    'design.mcp.status',         'Status: %s',
    'design.mcp.test_start',     '--- Starting connection test ---',
    'design.mcp.sse_url',        'Error: SSE requires the "URL" parameter (e.g. http://localhost:3000/sse)',
    'design.mcp.protocol',       'Protocol: %s',
    'design.mcp.init_ok',        '>> Initialized. Requesting tools...',
    'design.mcp.tools_received', '>> Tools received:',
    'design.mcp.test_ok',        '[OK] Connection succeeded.',
    'design.mcp.tools_found',    '%d tools found.',
    'design.mcp.tools_empty',    '[!] Connected, but ListTools returned nothing.',
    'design.mcp.init_error',     '[ERR] The client failed to initialize.',
    'design.mcp.see_log',        'See the Log tab for details.',
    'design.mcp.exception',      'Exception: %s',
    // --- Design: editores de TAiChatConnection / TAiEmbeddingConnection ---
    'design.conn.reset_verb',    'Reset Parameters to Defaults',
    'design.conn.reset_done',    'Parameters reset to defaults for driver: %s',
    'design.conn.reset_error',   'Error resetting parameters: %s',
    // --- Design: Acerca de ---
    'design.about.caption',      'About MakerAI',
    'design.about.version',      'Version:',
    'design.about.copyright',    'Copyright:',
    'design.about.description',  'Description:',
    'design.about.version_info', 'Build:'
  ];

implementation

end.
