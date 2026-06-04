unit uMainChatWebList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.WebBrowser, FMX.Layouts,
  uMakerAi.UI.ChatWebList;

type
  TForm25 = class(TForm)
    Layout1: TLayout;
    WebBrowser1: TWebBrowser;
    Layout3: TLayout;
    BtnSendDataToChatList: TButton;
    BtnReload: TButton;
    ChatWebList1: TChatWebList;
    procedure FormShow(Sender: TObject);
    procedure BtnSendDataToChatListClick(Sender: TObject);
    procedure BtnReloadClick(Sender: TObject);
  private
    FMsgCounter: Integer;
    procedure ActualizarTextoBoton;
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.fmx}

const
  TOTAL_MENSAJES = 8;

procedure TForm25.FormShow(Sender: TObject);
begin
  // Conectar el TWebBrowser al TChatWebList una vez que Edge está listo
  ChatWebList1.Browser := WebBrowser1;
  ActualizarTextoBoton;
end;

procedure TForm25.ActualizarTextoBoton;
begin
  if FMsgCounter >= TOTAL_MENSAJES then
    BtnSendDataToChatList.Text := 'Limpiar chat'
  else
    BtnSendDataToChatList.Text := Format('Enviar mensaje (%d/%d)',
      [FMsgCounter + 1, TOTAL_MENSAJES]);
end;

procedure TForm25.BtnSendDataToChatListClick(Sender: TObject);
begin
  // Cuando ya se enviaron todos los mensajes, limpiar y reiniciar
  if FMsgCounter >= TOTAL_MENSAJES then
  begin
    ChatWebList1.Clear;
    FMsgCounter := 0;
    ActualizarTextoBoton;
    Exit;
  end;

  case FMsgCounter of
    // --- Mensaje 1: usuario, texto simple con énfasis ---
    0: ChatWebList1.AddMessage(
         'Hola! Puedes mostrarme cómo usar `TChatWebList` con **Delphi**?',
         'Usuario', False);

    // --- Mensaje 2: asistente, bloque de código Pascal ---
    1: ChatWebList1.AddMessage(
         '¡Claro! Aquí tienes el uso básico:' + #10#10 +
         '```pascal' + #10 +
         'procedure TForm1.FormShow(Sender: TObject);' + #10 +
         'begin' + #10 +
         '  // Conectar el browser al componente' + #10 +
         '  ChatWebList1.Browser := WebBrowser1;' + #10 +
         '  // Agregar mensajes con Markdown' + #10 +
         '  ChatWebList1.AddMessage(''Hola **mundo**'', ''Bot'', True);' + #10 +
         'end;' + #10 +
         '```' + #10#10 +
         'El segundo parámetro es el **nombre de usuario** y el tercero indica' + #10 +
         'si es *entrante* (burbuja izquierda) o *saliente* (burbuja derecha).',
         'Asistente', True);

    // --- Mensaje 3: usuario, pregunta corta ---
    2: ChatWebList1.AddMessage(
         'Qué formatos de Markdown soporta?',
         'Usuario', False);

    // --- Mensaje 4: asistente, tabla + lista + blockquote ---
    3: ChatWebList1.AddMessage(
         '## Formatos soportados' + #10#10 +
         'El componente incluye un parser Markdown **embebido en JavaScript**:' + #10#10 +
         '| Sintaxis | Resultado |' + #10 +
         '|----------|-----------|' + #10 +
         '| `**texto**` | **negrita** |' + #10 +
         '| `*texto*` | *cursiva* |' + #10 +
         '| `~~texto~~` | ~~tachado~~ |' + #10 +
         '| `# Título` | heading H1 |' + #10 +
         '| `` `code` `` | `código inline` |' + #10#10 +
         '> **Nota:** Los bloques de código tienen syntax highlighting' + #10 +
         '> para Pascal, JavaScript, Python, SQL, JSON, Bash y HTML.' + #10#10 +
         'Listas también funcionan:' + #10 +
         '- Elemento uno' + #10 +
         '- Elemento dos' + #10 +
         '  - Subelemento' + #10 +
         '- Elemento tres',
         'Asistente', True);

    // --- Mensaje 5: usuario ---
    4: ChatWebList1.AddMessage(
         'Puedes mostrar syntax highlighting en varios lenguajes?',
         'Usuario', False);

    // --- Mensaje 6: asistente, múltiples bloques de código ---
    5: ChatWebList1.AddMessage(
         'Sí! Aquí ejemplos en tres lenguajes:' + #10#10 +
         '```javascript' + #10 +
         'const fetchData = async (url) => {' + #10 +
         '  const response = await fetch(url);' + #10 +
         '  if (!response.ok) throw new Error(''HTTP error'');' + #10 +
         '  return await response.json();' + #10 +
         '};' + #10 +
         '```' + #10#10 +
         '```python' + #10 +
         'def es_primo(n: int) -> bool:' + #10 +
         '    if n < 2: return False' + #10 +
         '    return all(n % i != 0 for i in range(2, int(n**0.5) + 1))' + #10 +
         '```' + #10#10 +
         '```sql' + #10 +
         'SELECT u.nombre, COUNT(m.id) AS total_mensajes' + #10 +
         'FROM usuarios u' + #10 +
         'LEFT JOIN mensajes m ON m.usuario_id = u.id' + #10 +
         'GROUP BY u.nombre' + #10 +
         'ORDER BY total_mensajes DESC' + #10 +
         'LIMIT 10;' + #10 +
         '```',
         'Asistente', True);

    // --- Mensaje 7: usuario ---
    6: ChatWebList1.AddMessage(
         'Cómo funciona el streaming?',
         'Usuario', False);

    // --- Mensaje 8: asistente, lista ordenada ---
    7: ChatWebList1.AddMessage(
         '### Streaming progresivo' + #10#10 +
         'El flujo de streaming es muy sencillo:' + #10#10 +
         '1. Llama a `BeginStreaming(''Asistente'', True)` — retorna un **ID**' + #10 +
         '2. Por cada fragmento del LLM llama `AppendFragment(id, chunk)`' + #10 +
         '3. Al terminar llama `EndStreaming(id)`' + #10#10 +
         'Durante el streaming el componente muestra un **cursor animado** ▋' + #10 +
         'y al finalizar re-renderiza el texto acumulado como Markdown completo.' + #10#10 +
         '---' + #10#10 +
         '*Prueba el botón __Reload__ para verificar que los mensajes persisten' + #10 +
         'tras reconstruir la página HTML.*',
         'Asistente', True);
  end;

  Inc(FMsgCounter);
  ActualizarTextoBoton;
end;

procedure TForm25.BtnReloadClick(Sender: TObject);
begin
  ChatWebList1.Reload;
end;

end.
