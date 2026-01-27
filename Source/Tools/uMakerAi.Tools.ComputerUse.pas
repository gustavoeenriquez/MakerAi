unit uMakerAi.tools.ComputerUse;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Math, System.StrUtils, System.Types,
  uMakerAi.Core, uMakerAi.Tools.Functions, uMakerAi.Chat.Messages;

type
  // Tipos de acciones soportadas por Gemini 2.5
  TAiComputerActionType = (catUnknown, catClick, // click_at, left_click
    catRightClick, // right_click
    catMiddleClick, // middle_click
    catDoubleClick, // double_click
    catType, // type_text_at
    catKeyCombination, // key_combination
    catScroll, // scroll_at, scroll_document
    catDrag, // drag_and_drop
    catHover, // hover_at
    catNavigate, // navigate, search, open_web_browser
    catScreenshot, // screenshot (solicitud explícita del modelo)
    catWait, // wait_5_seconds
    catTerminate, // Para detener el bucle
    catImageEdit, catDrawBox);

  // Estructura con los datos ya procesados (Coordenadas reales, no normalizadas)
  TAiActionData = record
    ActionType: TAiComputerActionType;
    FunctionName: string;

    // Coordenadas calculadas a píxeles reales de pantalla
    X, Y: Integer;
    DestX, DestY: Integer; // Para Drag & Drop

    // Datos de texto y teclado
    TextToType: string;
    KeyCombo: string; // Ej: 'Control+S'
    PressEnter: Boolean; // Para type_text_at

    // Datos de Scroll
    ScrollDirection: string; // 'up', 'down', 'left', 'right'
    ScrollAmount: Integer; // Default 800 (según docs)

    // Datos de Edición de Imagen
    Width, Height: Integer;
    EditType: string; // 'black_out', 'highlight', etc.
    ColorName: string;

    // Datos de navegación
    Url: string;
  end;

  // Resultado devuelto por tu aplicación
  TAiActionResult = record
    Success: Boolean;
    ErrorMessage: string;
    CustomOutput: string; // Opcional, para devolver info extra
  end;

  // Eventos
  TOnExecuteAction = procedure(Sender: TObject; const ActionData: TAiActionData; var Result: TAiActionResult) of object;
  TOnRequestScreenshot = procedure(Sender: TObject; var MediaFile: TAiMediaFile) of object;
  TOnSafetyConfirmation = procedure(Sender: TObject; const Explanation: string; var Allow: Boolean) of object;

  TAiComputerUseTool = class(TComponent)
  private
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FCurrentUrl: string;

    FOnRequestScreenshot: TOnRequestScreenshot;
    FOnSafetyConfirmation: TOnSafetyConfirmation;
    FOnExecuteAction: TOnExecuteAction;
    FAreaWidth: Integer;
    FAreaTop: Integer;
    FAreaHeight: Integer;
    FAreaLeft: Integer;

    // Helpers
    function DenormalizeCoordinate(Coord, MaxPixels, Offset: Integer): Integer;

    function ParseAction(ToolCall: TAiToolsFunction; out SafetyReason: string): TAiActionData;
    function ActionTypeToString(AType: TAiComputerActionType): string;

  public
    constructor Create(AOwner: TComponent); override;

    // Método principal llamado desde TAiGeminiChat
    // Retorna el JSON string para la respuesta y el MediaFile (Screenshot) por referencia
    function ProcessToolCall(ToolCall: TAiToolsFunction; out ResponseMedia: TAiMediaFile): string;

    // Convierte un punto X,Y de Gemini (0-1000) a píxeles reales de pantalla
    function GetRealPoint(GeminiX, GeminiY: Integer): TPoint;

    // Convierte dos puntos (TopLeft, BottomRight) de Gemini a un TRect de pantalla
    function GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;

  published
    // Configuración de tu pantalla física
    property ScreenWidth: Integer read FScreenWidth write FScreenWidth default 1920;
    property ScreenHeight: Integer read FScreenHeight write FScreenHeight default 1080;

    // URL simulada para devolver a la API (Requisito de Gemini)
    // Default: 'app://desktop'
    property CurrentUrl: string read FCurrentUrl write FCurrentUrl;

    // Eventos
    property OnExecuteAction: TOnExecuteAction read FOnExecuteAction write FOnExecuteAction;
    property OnRequestScreenshot: TOnRequestScreenshot read FOnRequestScreenshot write FOnRequestScreenshot;
    property OnSafetyConfirmation: TOnSafetyConfirmation read FOnSafetyConfirmation write FOnSafetyConfirmation;
    // Si AreaWidth es 0, se asumirá pantalla completa en tiempo de ejecución
    property AreaLeft: Integer read FAreaLeft write FAreaLeft default 0;
    property AreaTop: Integer read FAreaTop write FAreaTop default 0;
    property AreaWidth: Integer read FAreaWidth write FAreaWidth default 1920;
    property AreaHeight: Integer read FAreaHeight write FAreaHeight default 1080;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiComputerUseTool]);
end;

{ TAiComputerUseTool }

constructor TAiComputerUseTool.Create(AOwner: TComponent);
begin
  inherited;
  FScreenWidth := 1920;
  FScreenHeight := 1080;
  FCurrentUrl := 'app://desktop';
end;

function TAiComputerUseTool.DenormalizeCoordinate(Coord, MaxPixels, Offset: Integer): Integer;
begin
  // Gemini devuelve 0-999. Convertimos a píxeles reales.
  if Coord < 0 then
    Coord := 0;
  if Coord > 999 then
    Coord := 999;

  // Fórmula: (Normalizado % * Tamaño Imagen) + Desplazamiento Monitor
  Result := Round((Coord / 1000) * MaxPixels) + Offset;
end;

function TAiComputerUseTool.GetRealPoint(GeminiX, GeminiY: Integer): TPoint;
begin
  // Reutilizamos la lógica interna para asegurar consistencia
  // DenormalizeCoordinate aplica: (Coord / 1000 * Tamaño) + Offset
  Result.X := DenormalizeCoordinate(GeminiX, FAreaWidth, FAreaLeft);
  Result.Y := DenormalizeCoordinate(GeminiY, FAreaHeight, FAreaTop);
end;

function TAiComputerUseTool.GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;
begin
  // Calculamos las dos esquinas
  Result.TopLeft := GetRealPoint(GemX1, GemY1);
  Result.BottomRight := GetRealPoint(GemX2, GemY2);
end;

function TAiComputerUseTool.ActionTypeToString(AType: TAiComputerActionType): string;
begin
  // Solo para debug o logs internos
  case AType of
    catClick:
      Result := 'Click';
    catType:
      Result := 'Type';
    catScroll:
      Result := 'Scroll';
  else
    Result := 'Unknown';
  end;
end;

function TAiComputerUseTool.ParseAction(ToolCall: TAiToolsFunction; out SafetyReason: string): TAiActionData;
var
  JArgs, JSafety: TJSONObject;
  NormX, NormY: Integer;
begin
  // Inicializar record
  Result.ActionType := catUnknown;
  Result.FunctionName := ToolCall.Name;
  Result.X := 0;
  Result.Y := 0;
  Result.DestX := 0;
  Result.DestY := 0;
  Result.TextToType := '';
  Result.KeyCombo := '';
  Result.PressEnter := False;
  Result.ScrollDirection := '';
  Result.ScrollAmount := 0;
  Result.Url := '';
  SafetyReason := '';

  // Parsear Argumentos JSON
  JArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  try
    if not Assigned(JArgs) then
      Exit;

    // 1. Detección de Safety Decision (Human-in-the-loop)
    // "safety_decision": { "decision": "require_confirmation", "explanation": "..." }
    if JArgs.TryGetValue<TJSONObject>('safety_decision', JSafety) then
    begin
      var
        Decision: string;
      if JSafety.TryGetValue<string>('decision', Decision) and SameText(Decision, 'require_confirmation') then
      begin
        JSafety.TryGetValue<string>('explanation', SafetyReason);
      end;
    end;

    // 2. Mapeo de Función a Tipo de Acción
    var
    FName := LowerCase(Trim(ToolCall.Name));

    if (FName = 'click_at') or (FName = 'left_click') then
      Result.ActionType := catClick
    else if (FName = 'right_click') then
      Result.ActionType := catRightClick
    else if (FName = 'middle_click') then
      Result.ActionType := catMiddleClick
    else if (FName = 'double_click') then
      Result.ActionType := catDoubleClick
    else if (FName = 'type_text_at') or (FName = 'type') then
      Result.ActionType := catType
    else if (FName = 'key_combination') then
      Result.ActionType := catKeyCombination
    else if (FName = 'scroll_at') or (FName = 'scroll_document') then
      Result.ActionType := catScroll
    else if (FName = 'drag_and_drop') then
      Result.ActionType := catDrag
    else if (FName = 'hover_at') or (FName = 'mouse_move') then
      Result.ActionType := catHover
    else if (FName = 'navigate') or (FName = 'search') or (FName = 'open_web_browser') then
      Result.ActionType := catNavigate
    else if (FName = 'screenshot') then
      Result.ActionType := catScreenshot
    else if (FName = 'wait_5_seconds') then
      Result.ActionType := catWait
    else if (FName = 'image_edit_at') then
      Result.ActionType := catImageEdit
    else if (FName = 'draw_box_at') then
      Result.ActionType := catDrawBox;

    // 3. Extracción y Normalización de Parámetros

    // Extraer Width y Height (y desnormalizarlos también)
    // Nota: width/height en Gemini también suelen ser relativos a 1000x1000
    // Si Gemini envía 288, significa 28.8% del ancho total.
    if JArgs.TryGetValue<Integer>('width', NormX) then
      Result.Width := DenormalizeCoordinate(NormX, FAreaWidth, 0); // Offset 0 porque es una magnitud, no una posición

    if JArgs.TryGetValue<Integer>('height', NormY) then
      Result.Height := DenormalizeCoordinate(NormY, FAreaHeight, 0);

    JArgs.TryGetValue<string>('color', Result.ColorName);

    // Extraer tipo de edición
    JArgs.TryGetValue<string>('edit_type', Result.EditType);

    // Coordenadas X, Y
    if JArgs.TryGetValue<Integer>('x', NormX) then
      Result.X := DenormalizeCoordinate(NormX, FAreaWidth, FAreaLeft); // <--- Usamos Width y Left

    if JArgs.TryGetValue<Integer>('y', NormY) then
      Result.Y := DenormalizeCoordinate(NormY, FAreaHeight, FAreaTop); // <--- Usamos Height y Top

    // Coordenadas Destino (Drag)
    if JArgs.TryGetValue<Integer>('destination_x', NormX) then
      Result.DestX := DenormalizeCoordinate(NormX, FAreaWidth, FAreaLeft);

    if JArgs.TryGetValue<Integer>('destination_y', NormY) then
      Result.DestY := DenormalizeCoordinate(NormY, FAreaHeight, FAreaTop);

    // Texto y Teclado
    JArgs.TryGetValue<string>('text', Result.TextToType);
    JArgs.TryGetValue<string>('keys', Result.KeyCombo);

    // Flags booleanos (type_text_at)
    if JArgs.GetValue('press_enter') is TJSONBool then
      Result.PressEnter := JArgs.GetValue<Boolean>('press_enter')
    else
      Result.PressEnter := True; // Default según docs

    // Scroll
    JArgs.TryGetValue<string>('direction', Result.ScrollDirection);
    // Magnitude default 800 si no viene
    if not JArgs.TryGetValue<Integer>('magnitude', Result.ScrollAmount) then
      Result.ScrollAmount := 800;

    // Navegación
    JArgs.TryGetValue<string>('url', Result.Url);

  finally
    JArgs.Free;
  end;
end;

function TAiComputerUseTool.ProcessToolCall(ToolCall: TAiToolsFunction; out ResponseMedia: TAiMediaFile): string;
var
  ActionData: TAiActionData;
  ActionResult: TAiActionResult;
  SafetyReason: string;
  UserAllowed: Boolean;
  JResponse, JContent: TJSONObject;
begin
  ResponseMedia := nil;
  ActionResult.Success := False;
  ActionResult.ErrorMessage := 'Unknown error';
  ActionResult.CustomOutput := '';

  // 1. Parsear datos y detectar seguridad
  ActionData := ParseAction(ToolCall, SafetyReason);

  // 2. Verificación de Seguridad (Human-in-the-loop)
  if SafetyReason <> '' then
  begin
    UserAllowed := False;
    if Assigned(FOnSafetyConfirmation) then
      FOnSafetyConfirmation(Self, SafetyReason, UserAllowed)
    else
      UserAllowed := False; // Por seguridad, si no hay evento, denegar.

    if not UserAllowed then
    begin
      // Retornar rechazo al modelo (sin ejecutar acción)
      // Gemini necesita saber que hubo un safety check
      JResponse := TJSONObject.Create;
      try
        JResponse.AddPair('output', 'action_denied_by_user');
        JResponse.AddPair('url', FCurrentUrl);
        JResponse.AddPair('safety_acknowledgement', TJSONBool.Create(False));
        Result := JResponse.ToJSON;
      finally
        JResponse.Free;
      end;
      Exit;
    end;
  end;

  // 3. Ejecutar Acción (Eventos Externos)
  if Assigned(FOnExecuteAction) then
  begin
    try
      FOnExecuteAction(Self, ActionData, ActionResult);
    except
      on E: Exception do
      begin
        ActionResult.Success := False;
        ActionResult.ErrorMessage := E.Message;
      end;
    end;
  end
  else
  begin
    ActionResult.Success := False;
    ActionResult.ErrorMessage := 'OnExecuteAction event not assigned.';
  end;

  // --- TIEMPO DE ASENTAMIENTO ---
  // Esperar a que la UI de Windows reaccione antes de tomar la foto.
  // 500ms a 1000ms es un buen balance.
  if ActionResult.Success then
    Sleep(1000);

  // 4. Capturar Nuevo Estado (Si fue exitoso)
  if ActionResult.Success then
  begin
    if Assigned(FOnRequestScreenshot) then
      FOnRequestScreenshot(Self, ResponseMedia);

    // Si la acción fue de navegación, actualizar CurrentUrl simulado si el usuario lo devolvió en CustomOutput
    // O mantener el estático si no cambia.
    if (ActionData.ActionType = catNavigate) and (ActionData.Url <> '') then
      FCurrentUrl := ActionData.Url;
  end;

  // 5. Construir JSON de Respuesta
  JResponse := TJSONObject.Create;
  try
    if ActionResult.Success then
      JResponse.AddPair('output', 'action_executed_successfully')
    else
      JResponse.AddPair('output', 'error: ' + ActionResult.ErrorMessage);

    // Requisito: Siempre devolver URL
    JResponse.AddPair('url', FCurrentUrl);

    // Si pasamos el safety check, confirmarlo
    if SafetyReason <> '' then
      JResponse.AddPair('safety_acknowledgement', TJSONBool.Create(True));

    // Si el usuario generó data custom (ej: texto leído), agregarla
    if ActionResult.CustomOutput <> '' then
      JResponse.AddPair('data', ActionResult.CustomOutput);

    Result := JResponse.ToJSON;
  finally
    JResponse.Free;
  end;
end;

end.



'''

# Guía de Arquitectura: Adaptador Universal "Computer Use" (Delphi)

Esta guía define la lógica necesaria para implementar una unidad unificada (`uMakerAi.Tools.ComputerUse`) que abstrae las diferencias entre **Gemini**, **Claude** y **OpenAI**. El sistema utiliza el patrón **Adapter** para traducir cualquier dialecto de IA a instrucciones físicas estandarizadas.

## 1. Detección de Proveedor (Auto-Discovery)
El sistema no debe requerir configuración manual. Debe detectar el proveedor analizando la firma del JSON de entrada (`RawArgs`) y el nombre de la herramienta (`ToolName`).

| Proveedor | Firma Heurística | Ejemplo JSON |
| :--- | :--- | :--- |
| **Claude** | `ToolName` contiene "computer" **Y** existe clave `"action"`. | `{"action": "left_click", "coordinate": [x, y]}` |
| **OpenAI** | Existe clave `"type"` (**Y NO** existe `"action"`). | `{"type": "click", "x": 100}` |
| **Gemini** | `ToolName` explícito (ej: `click_at`) **O** argumentos planos `x,y`. | `{"x": 500, "y": 500}` (Tool: `click_at`) |

## 2. Unificación de Coordenadas
Cada modelo opera en un espacio de coordenadas distinto. El adaptador debe traducir todo a **Píxeles Físicos Reales** (`FPhysicalWidth/Height`) antes de ejecutar.

### Gemini: Coordenadas Normalizadas
*   **Espacio:** 0 a 1000 (independiente de la resolución).
*   **Fórmula:** `X_Real = Round((Input / 1000) * PhysicalWidth)`

### Claude: Coordenadas Escaladas
*   **Espacio:** Relativo a la imagen redimensionada enviada a la API (max ~1.5MP).
*   **Requisito:** Se debe almacenar el ancho/alto de la última imagen enviada (`FLastAiViewWidth`).
*   **Fórmula:** `X_Real = Round(Input * (PhysicalWidth / FLastAiViewWidth))`

### OpenAI: Coordenadas Virtuales Absolutas
*   **Espacio:** Relativo a la resolución virtual declarada en el prompt (ej: 1024x768).
*   **Requisito:** Se debe configurar `FVirtualWidth`.
*   **Fórmula:** `X_Real = Round(Input * (PhysicalWidth / FVirtualWidth))`

## 3. Mapeo de Acciones (Parsing)
El JSON de entrada debe convertirse a un registro interno unificado `TAiActionData`.

*   **Clicks:** Unificar `click_at`, `left_click` y `type: "click"` en un enum `catLeftClick`.
*   **Teclado:**
    *   *Gemini/Claude:* Reciben strings combinados (ej: `"Ctrl+C"`). Se debe hacer `Split` por `+`.
    *   *OpenAI:* Recibe un Array JSON (ej: `["Ctrl", "C"]`). Asignar directo.
*   **Scroll:** Unificar todo a Deltas (`DeltaX`, `DeltaY`).
    *   Convertir direcciones textuales ("up", "down") a valores positivos/negativos.

## 4. Protocolo de Seguridad (Safety Loop)
El flujo de ejecución cambia según el modelo cuando se detecta riesgo.

1.  **Gemini:** Envía flag `require_confirmation` dentro de los argumentos.
    *   *Acción:* Pausar, pedir confirmación. Si Usuario=No, devolver error JSON.
2.  **OpenAI (Handshake):**
    *   *Paso 1:* Envía `pending_safety_checks` (Lista de IDs).
    *   *Acción:* **DETENER EJECUCIÓN FÍSICA**.
    *   *Paso 2:* Responder inmediatamente a la API con `acknowledged_safety_checks` (copiando los IDs).
    *   *Paso 3:* La IA reenvía la acción en el siguiente turno.

## 5. Formato de Respuesta (Output)
La unidad debe generar el string de resultado en el formato nativo del proveedor detectado.

**Gemini:**


{ "output": "success", "url": "app://desktop" }



Claude
                       {
  "content": [
    { "type": "image", "source": { "type": "base64", "data": "..." } },
    { "type": "text", "text": "Action executed" }
  ]
}


OpenAI
{
  "type": "computer_call_output",
  "output": {
    "type": "computer_screenshot",
    "image_url": "data:image/png;base64,..."
  },
  "acknowledged_safety_checks": [...] // Solo si hubo handshake
}

6. Extensibilidad (Custom Actions)
Si el parser no reconoce la herramienta (ej: una herramienta custom definida por el usuario como open_calculator):
Asignar ActionType := catUnknown.
Guardar RawFunctionName y RawJsonArgs.
Disparar evento OnCustomAction para que el código del usuario intente manejarla.




