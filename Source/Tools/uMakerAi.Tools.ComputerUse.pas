unit uMakerAi.tools.ComputerUse;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Math, System.StrUtils, System.Types,
  uMakerAi.Core, uMakerAi.Tools.Functions, uMakerAi.Chat.Messages;

type
  // Tipos de acciones soportadas por Gemini 2.5
  TAiComputerActionType = (catUnknown, catClick, // click_at, left_click
    catRightClick,    // right_click
    catMiddleClick,   // middle_click
    catDoubleClick,   // double_click
    catTripleClick,   // triple_click (Claude computer_2025xxxx)
    catType,          // type_text_at
    catKeyCombination, // key_combination
    catHoldKey,       // hold_key (mantener tecla N segundos)
    catScroll,        // scroll_at, scroll_document
    catDrag,          // drag_and_drop
    catHover,         // hover_at
    catCursorPosition, // cursor_position (consultar posición actual)
    catZoom,          // zoom (ampliar región del screenshot, computer_20251124)
    catNavigate,      // navigate, search, open_web_browser
    catScreenshot,    // screenshot (solicitud explícita del modelo)
    catWait,          // wait_5_seconds
    catTerminate,     // Para detener el bucle
    catGoBack, catGoForward); // go_back, go_forward

  // Estructura con los datos ya procesados (Coordenadas reales, no normalizadas)
  TAiActionData = record
    ActionType: TAiComputerActionType;
    FunctionName: string;

    // Coordenadas calculadas a píxeles reales de pantalla
    X, Y: Integer;
    DestX, DestY: Integer; // Para Drag & Drop

    // Datos de texto y teclado
    TextToType: string;
    KeyCombo: string; // Ej: 'Control+S' (key_combination / hold_key)
    PressEnter: Boolean; // Para type_text_at
    ClearBeforeTyping: Boolean; // Para type_text_at (default true)
    Modifiers: string; // Teclas modificadoras en click/scroll: 'shift', 'ctrl+alt'...
    HoldDuration: Double; // Segundos a mantener la tecla en hold_key (default 1.0)

    // Datos de Scroll
    ScrollDirection: string; // 'up', 'down', 'left', 'right'
    ScrollAmount: Integer; // Default 800 (según docs)

    // Región a ampliar en zoom (píxeles reales de pantalla)
    ZoomRect: TRect;

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
    FEnableZoom: Boolean;
    FCurrentAction: TAiActionData;

    // Helpers
    function DenormalizeCoordinate(Coord, MaxPixels, Offset: Integer): Integer;

    function ParseAction(ToolCall: TAiToolsFunction; out SafetyReason: string): TAiActionData;

  public
    constructor Create(AOwner: TComponent); override;

    // Método principal llamado desde TAiGeminiChat
    // Retorna el JSON string para la respuesta y el MediaFile (Screenshot) por referencia
    function ProcessToolCall(ToolCall: TAiToolsFunction; out ResponseMedia: TAiMediaFile): string;

    // Convierte un punto X,Y de Gemini (0-1000) a píxeles reales de pantalla
    function GetRealPoint(GeminiX, GeminiY: Integer): TPoint;

    // Convierte dos puntos (TopLeft, BottomRight) de Gemini a un TRect de pantalla
    function GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;

    // Retorna un JSON array string (formato OpenAI) con las definiciones de todas las funciones
    // de Computer Use. Útil para modelos sin soporte nativo: pasar el resultado a TAiFunctions.
    function GetFunctionDefinitions: string;

    // Translates a native Claude Computer Use tool_call (action + coordinate[px], etc.)
    // into the x,y-normalized 0-999 format that ParseAction/ProcessToolCall expect, and
    // maps the action name. Mutates ToolCall.Name and ToolCall.Arguments in place, using
    // ScreenWidth/ScreenHeight as the pixel reference. This is the single source of truth
    // for the Claude->tool translation: the Claude driver uses it server-side, and a
    // client that receives tool_calls from an OpenAI-compatible broker (without going
    // through the Claude driver) can call it before ProcessToolCall to run Computer Use
    // locally with identical behavior.
    procedure TranslateClaudeToolCall(ToolCall: TAiToolsFunction);

    // Última acción procesada (se actualiza antes de disparar OnExecuteAction).
    // Útil en OnRequestScreenshot para conocer el contexto, p.ej. CurrentAction.ZoomRect
    // cuando CurrentAction.ActionType = catZoom.
    property CurrentAction: TAiActionData read FCurrentAction;

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

    // Habilita la acción 'zoom' de Claude (solo computer_20251124). Permite al
    // modelo ampliar una región del screenshot para leer texto pequeño.
    property EnableZoom: Boolean read FEnableZoom write FEnableZoom default False;
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
  FEnableZoom := False;
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
  Result.X := DenormalizeCoordinate(GeminiX, FAreaWidth, FAreaLeft);
  Result.Y := DenormalizeCoordinate(GeminiY, FAreaHeight, FAreaTop);
end;

function TAiComputerUseTool.GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;
begin
  Result.TopLeft := GetRealPoint(GemX1, GemY1);
  Result.BottomRight := GetRealPoint(GemX2, GemY2);
end;

procedure TAiComputerUseTool.TranslateClaudeToolCall(ToolCall: TAiToolsFunction);
// Converts the native Claude Computer Use format to the TAiComputerUseTool format.
// Claude sends: {"action":"left_click","coordinate":[x_px, y_px], ...}
// ParseAction expects: {"x":norm, "y":norm, "text":"...", ...} + ToolCall.Name = mapped action.
var
  JArgs, JNew: TJSONObject;
  JCoord, JStartCoord, JRegion: TJSONArray;
  Action, MappedName, SText, SDir: string;
  ScrW, ScrH, PxX, PxY, NormX, NormY, Amount: Integer;
  DDur: Double;
begin
  JArgs := TJSONObject.ParseJSONValue(ToolCall.Arguments) as TJSONObject;
  if not Assigned(JArgs) then
    Exit;
  try
    if not JArgs.TryGetValue<string>('action', Action) then
      Exit;

    ScrW := FScreenWidth;
    ScrH := FScreenHeight;
    if ScrW <= 0 then ScrW := 1920;
    if ScrH <= 0 then ScrH := 1080;

    // Map Claude action names to TAiComputerUseTool action names
    if      Action = 'left_click'       then MappedName := 'click_at'
    else if Action = 'right_click'      then MappedName := 'right_click'
    else if Action = 'middle_click'     then MappedName := 'middle_click'
    else if Action = 'double_click'     then MappedName := 'double_click'
    else if Action = 'left_click_drag'  then MappedName := 'drag_and_drop'
    else if Action = 'mouse_move'       then MappedName := 'hover_at'
    else if Action = 'type'             then MappedName := 'type_text_at'
    else if Action = 'key'              then MappedName := 'key_combination'
    else if Action = 'scroll'           then MappedName := 'scroll_at'
    else if Action = 'wait'             then MappedName := 'wait_5_seconds'
    else MappedName := Action; // screenshot, go_back, go_forward pass through

    ToolCall.Name := MappedName;

    JNew := TJSONObject.Create;
    try
      // Drag: start_coordinate = origin (-> x,y); coordinate = destination (-> destination_x,y)
      if (Action = 'left_click_drag') and
         JArgs.TryGetValue<TJSONArray>('start_coordinate', JStartCoord) and
         (JStartCoord.Count >= 2) then
      begin
        PxX  := (JStartCoord.Items[0] as TJSONNumber).AsInt;
        PxY  := (JStartCoord.Items[1] as TJSONNumber).AsInt;
        NormX := Round(PxX / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round(PxY / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));

        if JArgs.TryGetValue<TJSONArray>('coordinate', JCoord) and (JCoord.Count >= 2) then
        begin
          NormX := Round((JCoord.Items[0] as TJSONNumber).AsInt / ScrW * 1000);
          NormY := Round((JCoord.Items[1] as TJSONNumber).AsInt / ScrH * 1000);
          if NormX > 999 then NormX := 999;
          if NormY > 999 then NormY := 999;
          JNew.AddPair('destination_x', TJSONNumber.Create(NormX));
          JNew.AddPair('destination_y', TJSONNumber.Create(NormY));
        end;
      end
      else if JArgs.TryGetValue<TJSONArray>('coordinate', JCoord) and (JCoord.Count >= 2) then
      begin
        PxX  := (JCoord.Items[0] as TJSONNumber).AsInt;
        PxY  := (JCoord.Items[1] as TJSONNumber).AsInt;
        NormX := Round(PxX / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round(PxY / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));
      end;

      // Text / keys / modifiers: Claude's 'text' field changes meaning by action.
      if JArgs.TryGetValue<string>('text', SText) then
      begin
        if (Action = 'key') or (Action = 'hold_key') then
          JNew.AddPair('keys', SText)
        else if Action = 'type' then
        begin
          JNew.AddPair('text', SText);
          // Claude 'type' implies neither Enter nor a position: it types into the
          // focused control. Suppress the automatic Enter (ParseAction defaults to True).
          JNew.AddPair('press_enter', TJSONBool.Create(False));
        end
        else
          // In click/scroll/triple_click the 'text' holds the modifiers
          JNew.AddPair('modifiers', SText);
      end;

      // hold_key duration (seconds)
      if (Action = 'hold_key') and JArgs.TryGetValue<Double>('duration', DDur) then
        JNew.AddPair('duration', TJSONNumber.Create(DDur));

      // Zoom: region [x1,y1,x2,y2] (px) -> x,y + destination_x,destination_y (norm 0-999)
      if (Action = 'zoom') and JArgs.TryGetValue<TJSONArray>('region', JRegion) and (JRegion.Count >= 4) then
      begin
        NormX := Round((JRegion.Items[0] as TJSONNumber).AsInt / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round((JRegion.Items[1] as TJSONNumber).AsInt / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('x', TJSONNumber.Create(NormX));
        JNew.AddPair('y', TJSONNumber.Create(NormY));
        NormX := Round((JRegion.Items[2] as TJSONNumber).AsInt / ScrW * 1000); if NormX > 999 then NormX := 999;
        NormY := Round((JRegion.Items[3] as TJSONNumber).AsInt / ScrH * 1000); if NormY > 999 then NormY := 999;
        JNew.AddPair('destination_x', TJSONNumber.Create(NormX));
        JNew.AddPair('destination_y', TJSONNumber.Create(NormY));
      end;

      // Scroll: Claude uses 'scroll_direction'/'scroll_amount'. Accept 'direction'/'amount' too.
      if JArgs.TryGetValue<string>('scroll_direction', SDir) or
         JArgs.TryGetValue<string>('direction', SDir) then
        JNew.AddPair('direction', SDir);
      if JArgs.TryGetValue<Integer>('scroll_amount', Amount) or
         JArgs.TryGetValue<Integer>('amount', Amount) then
        JNew.AddPair('magnitude', TJSONNumber.Create(Amount * 120))
      else if Action = 'scroll' then
        JNew.AddPair('magnitude', TJSONNumber.Create(800));

      ToolCall.Arguments := JNew.ToJSON;
    finally
      JNew.Free;
    end;
  finally
    JArgs.Free;
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
  Result.ClearBeforeTyping := True;
  Result.Modifiers := '';
  Result.HoldDuration := 0;
  Result.ScrollDirection := '';
  Result.ScrollAmount := 0;
  Result.ZoomRect := TRect.Empty;
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
    else if (FName = 'triple_click') then
      Result.ActionType := catTripleClick
    else if (FName = 'type_text_at') or (FName = 'type') then
      Result.ActionType := catType
    else if (FName = 'key_combination') then
      Result.ActionType := catKeyCombination
    else if (FName = 'hold_key') then
      Result.ActionType := catHoldKey
    else if (FName = 'scroll_at') or (FName = 'scroll_document') then
      Result.ActionType := catScroll
    else if (FName = 'drag_and_drop') then
      Result.ActionType := catDrag
    else if (FName = 'hover_at') or (FName = 'mouse_move') then
      Result.ActionType := catHover
    else if (FName = 'cursor_position') or (FName = 'get_cursor_position') then
      Result.ActionType := catCursorPosition
    else if (FName = 'zoom') then
      Result.ActionType := catZoom
    else if (FName = 'navigate') or (FName = 'search') or (FName = 'open_web_browser') then
      Result.ActionType := catNavigate
    else if (FName = 'screenshot') then
      Result.ActionType := catScreenshot
    else if (FName = 'wait_5_seconds') then
      Result.ActionType := catWait
    else if (FName = 'go_back') then
      Result.ActionType := catGoBack
    else if (FName = 'go_forward') then
      Result.ActionType := catGoForward;

    // 3. Extracción y Normalización de Parámetros

    // Coordenadas X, Y
    if JArgs.TryGetValue<Integer>('x', NormX) then
      Result.X := DenormalizeCoordinate(NormX, FAreaWidth, FAreaLeft);

    if JArgs.TryGetValue<Integer>('y', NormY) then
      Result.Y := DenormalizeCoordinate(NormY, FAreaHeight, FAreaTop);

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
      Result.PressEnter := True;

    if JArgs.GetValue('clear_before_typing') is TJSONBool then
      Result.ClearBeforeTyping := JArgs.GetValue<Boolean>('clear_before_typing')
    else
      Result.ClearBeforeTyping := True;

    // Scroll
    JArgs.TryGetValue<string>('direction', Result.ScrollDirection);
    if not JArgs.TryGetValue<Integer>('magnitude', Result.ScrollAmount) then
      Result.ScrollAmount := 800;

    // Navegación
    JArgs.TryGetValue<string>('url', Result.Url);

    // Modificadores en click/scroll (Claude los envía en 'text'; aquí ya normalizados a 'modifiers')
    JArgs.TryGetValue<string>('modifiers', Result.Modifiers);

    // Duración de hold_key (segundos). Default 1.0 si no se especifica.
    if not JArgs.TryGetValue<Double>('duration', Result.HoldDuration) then
      Result.HoldDuration := 1.0;

    // Región de zoom: se construye a partir de (x,y) y (destination_x, destination_y)
    // ya denormalizados a píxeles reales.
    if Result.ActionType = catZoom then
      Result.ZoomRect := TRect.Create(Result.X, Result.Y, Result.DestX, Result.DestY);

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
  JResponse: TJSONObject;
begin
  ResponseMedia := nil;
  ActionResult.Success := False;
  ActionResult.ErrorMessage := 'Unknown error';
  ActionResult.CustomOutput := '';

  // 1. Parsear datos y detectar seguridad
  ActionData := ParseAction(ToolCall, SafetyReason);
  FCurrentAction := ActionData; // Expuesto vía CurrentAction (p.ej. ZoomRect en OnRequestScreenshot)

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
      JResponse := TJSONObject.Create;
      try
        JResponse.AddPair('output', 'action_denied_by_user');
        JResponse.AddPair('url', FCurrentUrl);
        JResponse.AddPair('safety_acknowledgement', 'false');
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

  // Esperar a que la UI reaccione antes de tomar screenshot (500-1000ms)
  if ActionResult.Success then
    Sleep(1000);

  // 4. Capturar Nuevo Estado (Si fue exitoso)
  if ActionResult.Success then
  begin
    if Assigned(FOnRequestScreenshot) then
      FOnRequestScreenshot(Self, ResponseMedia);

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

    JResponse.AddPair('url', FCurrentUrl);

    if SafetyReason <> '' then
      JResponse.AddPair('safety_acknowledgement', 'true');

    if ActionResult.CustomOutput <> '' then
      JResponse.AddPair('data', ActionResult.CustomOutput);

    Result := JResponse.ToJSON;
  finally
    JResponse.Free;
  end;
end;

function TAiComputerUseTool.GetFunctionDefinitions: string;

  function MakeFn(const AName, ADesc: string; const AProps: TJSONObject; const ARequired: TJSONArray): TJSONObject;
  var
    JFn, JParams: TJSONObject;
    JTool: TJSONObject;
  begin
    JParams := TJSONObject.Create;
    JParams.AddPair('type', 'object');
    JParams.AddPair('properties', AProps);
    JParams.AddPair('required', ARequired);
    JFn := TJSONObject.Create;
    JFn.AddPair('name', AName);
    JFn.AddPair('description', ADesc);
    JFn.AddPair('parameters', JParams);
    JTool := TJSONObject.Create;
    JTool.AddPair('type', 'function');
    JTool.AddPair('function', JFn);
    Result := JTool;
  end;

  function CoordProps: TJSONObject;
  var JP: TJSONObject;
  begin
    Result := TJSONObject.Create;
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Coordenada X normalizada 0-999 (horizontal)'); Result.AddPair('x', JP);
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Coordenada Y normalizada 0-999 (vertical)');   Result.AddPair('y', JP);
  end;

  function ReqXY: TJSONArray;
  begin
    Result := TJSONArray.Create;
    Result.Add('x');
    Result.Add('y');
  end;

var
  JArray: TJSONArray;
  JProps, JP: TJSONObject;
  JReq: TJSONArray;
begin
  JArray := TJSONArray.Create;
  try
    // click_at (con modificadores opcionales: shift, ctrl, alt, super)
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Teclas modificadoras opcionales mientras se hace click: shift, ctrl, alt, super (combinables con +)'); JProps.AddPair('modifiers', JP);
    JArray.AddElement(MakeFn('click_at', 'Click izquierdo en las coordenadas dadas.', JProps, ReqXY));

    // right_click
    JArray.AddElement(MakeFn('right_click', 'Click derecho en las coordenadas dadas.', CoordProps, ReqXY));

    // middle_click
    JArray.AddElement(MakeFn('middle_click', 'Click con botón central en las coordenadas dadas.', CoordProps, ReqXY));

    // double_click
    JArray.AddElement(MakeFn('double_click', 'Doble click en las coordenadas dadas.', CoordProps, ReqXY));

    // triple_click
    JArray.AddElement(MakeFn('triple_click', 'Triple click en las coordenadas dadas.', CoordProps, ReqXY));

    // drag_and_drop
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','X destino del arrastre (normalizado 0-999)'); JProps.AddPair('destination_x', JP);
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Y destino del arrastre (normalizado 0-999)'); JProps.AddPair('destination_y', JP);
    JReq := ReqXY; JReq.Add('destination_x'); JReq.Add('destination_y');
    JArray.AddElement(MakeFn('drag_and_drop', 'Arrastra desde (x,y) hasta (destination_x, destination_y).', JProps, JReq));

    // hover_at
    JArray.AddElement(MakeFn('hover_at', 'Mueve el cursor a las coordenadas sin hacer click.', CoordProps, ReqXY));

    // type_text_at
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Texto a escribir'); JProps.AddPair('text', JP);
    JP := TJSONObject.Create; JP.AddPair('type','boolean'); JP.AddPair('description','Presionar Enter al final (default true)'); JProps.AddPair('press_enter', JP);
    JReq := ReqXY; JReq.Add('text');
    JArray.AddElement(MakeFn('type_text_at', 'Hace click en (x,y) y escribe el texto indicado.', JProps, JReq));

    // key_combination
    JProps := TJSONObject.Create;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Combinación de teclas, ej: "Control+S", "Alt+F4"'); JProps.AddPair('keys', JP);
    JReq := TJSONArray.Create; JReq.Add('keys');
    JArray.AddElement(MakeFn('key_combination', 'Ejecuta una combinación de teclas del sistema.', JProps, JReq));

    // hold_key
    JProps := TJSONObject.Create;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Tecla(s) a mantener presionada(s), ej: "shift", "ctrl+alt"'); JProps.AddPair('keys', JP);
    JP := TJSONObject.Create; JP.AddPair('type','number'); JP.AddPair('description','Duración en segundos (default 1.0)'); JProps.AddPair('duration', JP);
    JReq := TJSONArray.Create; JReq.Add('keys');
    JArray.AddElement(MakeFn('hold_key', 'Mantiene una tecla presionada durante los segundos indicados.', JProps, JReq));

    // scroll_at (con modificadores opcionales)
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Dirección: up, down, left, right'); JProps.AddPair('direction', JP);
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Cantidad de scroll en píxeles (default 800)'); JProps.AddPair('magnitude', JP);
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Teclas modificadoras opcionales durante el scroll: shift, ctrl, alt, super'); JProps.AddPair('modifiers', JP);
    JReq := ReqXY; JReq.Add('direction');
    JArray.AddElement(MakeFn('scroll_at', 'Realiza scroll en la posición indicada.', JProps, JReq));

    // cursor_position
    JProps := TJSONObject.Create;
    JReq := TJSONArray.Create;
    JArray.AddElement(MakeFn('cursor_position', 'Devuelve la posición actual del cursor (en data como {"x":..,"y":..}).', JProps, JReq));

    // zoom (ampliar región: (x,y) esquina superior-izquierda, (destination_x,destination_y) inferior-derecha)
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','X esquina inferior-derecha de la región (normalizado 0-999)'); JProps.AddPair('destination_x', JP);
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Y esquina inferior-derecha de la región (normalizado 0-999)'); JProps.AddPair('destination_y', JP);
    JReq := ReqXY; JReq.Add('destination_x'); JReq.Add('destination_y');
    JArray.AddElement(MakeFn('zoom', 'Amplía una región de la pantalla para inspeccionarla en detalle.', JProps, JReq));

    // navigate
    JProps := TJSONObject.Create;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','URL a la que navegar'); JProps.AddPair('url', JP);
    JReq := TJSONArray.Create; JReq.Add('url');
    JArray.AddElement(MakeFn('navigate', 'Navega a la URL especificada.', JProps, JReq));

    // screenshot
    JProps := TJSONObject.Create;
    JReq := TJSONArray.Create;
    JArray.AddElement(MakeFn('screenshot', 'Captura la pantalla actual y la retorna como imagen.', JProps, JReq));

    // wait_5_seconds
    JProps := TJSONObject.Create;
    JReq := TJSONArray.Create;
    JArray.AddElement(MakeFn('wait_5_seconds', 'Espera 5 segundos para que la UI reaccione.', JProps, JReq));

    // go_back
    JProps := TJSONObject.Create;
    JReq := TJSONArray.Create;
    JArray.AddElement(MakeFn('go_back', 'Navega hacia atrás en el historial.', JProps, JReq));

    // go_forward
    JProps := TJSONObject.Create;
    JReq := TJSONArray.Create;
    JArray.AddElement(MakeFn('go_forward', 'Navega hacia adelante en el historial.', JProps, JReq));

    Result := JArray.ToJSON;
  finally
    JArray.Free;
  end;
end;

end.
