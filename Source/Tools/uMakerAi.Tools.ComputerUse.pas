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
    catType,          // type_text_at
    catKeyCombination, // key_combination
    catScroll,        // scroll_at, scroll_document
    catDrag,          // drag_and_drop
    catHover,         // hover_at
    catNavigate,      // navigate, search, open_web_browser
    catScreenshot,    // screenshot (solicitud explícita del modelo)
    catWait,          // wait_5_seconds
    catTerminate,     // Para detener el bucle
    catImageEdit, catDrawBox,
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
    KeyCombo: string; // Ej: 'Control+S'
    PressEnter: Boolean; // Para type_text_at
    ClearBeforeTyping: Boolean; // Para type_text_at (default true)

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
  Result.X := DenormalizeCoordinate(GeminiX, FAreaWidth, FAreaLeft);
  Result.Y := DenormalizeCoordinate(GeminiY, FAreaHeight, FAreaTop);
end;

function TAiComputerUseTool.GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;
begin
  Result.TopLeft := GetRealPoint(GemX1, GemY1);
  Result.BottomRight := GetRealPoint(GemX2, GemY2);
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
    else if (FName = 'go_back') then
      Result.ActionType := catGoBack
    else if (FName = 'go_forward') then
      Result.ActionType := catGoForward
    else if (FName = 'image_edit_at') then
      Result.ActionType := catImageEdit
    else if (FName = 'draw_box_at') then
      Result.ActionType := catDrawBox;

    // 3. Extracción y Normalización de Parámetros

    if JArgs.TryGetValue<Integer>('width', NormX) then
      Result.Width := DenormalizeCoordinate(NormX, FAreaWidth, 0);

    if JArgs.TryGetValue<Integer>('height', NormY) then
      Result.Height := DenormalizeCoordinate(NormY, FAreaHeight, 0);

    JArgs.TryGetValue<string>('color', Result.ColorName);
    JArgs.TryGetValue<string>('edit_type', Result.EditType);

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
    // click_at
    JArray.AddElement(MakeFn('click_at', 'Click izquierdo en las coordenadas dadas.', CoordProps, ReqXY));

    // right_click
    JArray.AddElement(MakeFn('right_click', 'Click derecho en las coordenadas dadas.', CoordProps, ReqXY));

    // middle_click
    JArray.AddElement(MakeFn('middle_click', 'Click con botón central en las coordenadas dadas.', CoordProps, ReqXY));

    // double_click
    JArray.AddElement(MakeFn('double_click', 'Doble click en las coordenadas dadas.', CoordProps, ReqXY));

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

    // scroll_at
    JProps := CoordProps;
    JP := TJSONObject.Create; JP.AddPair('type','string'); JP.AddPair('description','Dirección: up, down, left, right'); JProps.AddPair('direction', JP);
    JP := TJSONObject.Create; JP.AddPair('type','integer'); JP.AddPair('description','Cantidad de scroll en píxeles (default 800)'); JProps.AddPair('magnitude', JP);
    JReq := ReqXY; JReq.Add('direction');
    JArray.AddElement(MakeFn('scroll_at', 'Realiza scroll en la posición indicada.', JProps, JReq));

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
