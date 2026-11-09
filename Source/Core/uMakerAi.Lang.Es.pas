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
    'chatui.speak',             'Escuchar en voz alta'
  ];

implementation

end.
