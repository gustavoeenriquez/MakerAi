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
    'chatui.speak',             'Read aloud'
  ];

implementation

end.
