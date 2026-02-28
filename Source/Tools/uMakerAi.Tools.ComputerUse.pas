// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.Tools.ComputerUse
// Adaptador universal para acciones de Computer Use (Gemini, Claude, OpenAI).
unit uMakerAi.Tools.ComputerUse;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Types,
  fpjson, jsonparser,
  uMakerAi.Core,
  uMakerAi.Chat.Messages;

type

  // Tipos de acciones soportadas (principalmente Gemini 2.5)
  TAiComputerActionType = (
    catUnknown,
    catClick,         // click_at, left_click
    catRightClick,    // right_click
    catMiddleClick,   // middle_click
    catDoubleClick,   // double_click
    catType,          // type_text_at, type
    catKeyCombination,// key_combination
    catScroll,        // scroll_at, scroll_document
    catDrag,          // drag_and_drop
    catHover,         // hover_at, mouse_move
    catNavigate,      // navigate, search, open_web_browser
    catScreenshot,    // screenshot
    catWait,          // wait_5_seconds
    catTerminate,
    catImageEdit,     // image_edit_at
    catDrawBox        // draw_box_at
  );

  // Datos procesados de la acción (coordenadas ya en píxeles reales)
  TAiActionData = record
    ActionType   : TAiComputerActionType;
    FunctionName : string;

    // Coordenadas en píxeles reales de pantalla
    X, Y         : Integer;
    DestX, DestY : Integer;   // Drag & Drop

    // Texto y teclado
    TextToType   : string;
    KeyCombo     : string;
    PressEnter   : Boolean;

    // Scroll
    ScrollDirection: string;
    ScrollAmount   : Integer;

    // Edición de imagen
    Width, Height: Integer;
    EditType     : string;
    ColorName    : string;

    // Navegación
    Url          : string;
  end;

  // Resultado devuelto por la aplicación al ejecutar una acción
  TAiActionResult = record
    Success      : Boolean;
    ErrorMessage : string;
    CustomOutput : string;
  end;

  // Eventos
  TOnExecuteAction     = procedure(Sender: TObject; const ActionData: TAiActionData;
      var Result: TAiActionResult) of object;
  TOnRequestScreenshot = procedure(Sender: TObject;
      var MediaFile: TAiMediaFile) of object;
  TOnSafetyConfirmation = procedure(Sender: TObject; const Explanation: string;
      var Allow: Boolean) of object;

  TAiComputerUseTool = class(TComponent)
  private
    FScreenWidth : Integer;
    FScreenHeight: Integer;
    FCurrentUrl  : string;
    FAreaLeft    : Integer;
    FAreaTop     : Integer;
    FAreaWidth   : Integer;
    FAreaHeight  : Integer;

    FOnRequestScreenshot  : TOnRequestScreenshot;
    FOnSafetyConfirmation : TOnSafetyConfirmation;
    FOnExecuteAction      : TOnExecuteAction;

    function DenormalizeCoordinate(Coord, MaxPixels, Offset: Integer): Integer;
    function ParseAction(ToolCall: TAiToolsFunction;
        out SafetyReason: string): TAiActionData;
    function ActionTypeToString(AType: TAiComputerActionType): string;

  public
    constructor Create(AOwner: TComponent); override;

    // Método principal — llamado desde el driver de chat
    function ProcessToolCall(ToolCall: TAiToolsFunction;
        out ResponseMedia: TAiMediaFile): string;

    // Conversión de coordenadas Gemini (0-1000) a píxeles reales
    function GetRealPoint(GeminiX, GeminiY: Integer): TPoint;
    function GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;

  published
    property ScreenWidth  : Integer read FScreenWidth  write FScreenWidth  default 1920;
    property ScreenHeight : Integer read FScreenHeight write FScreenHeight default 1080;
    property CurrentUrl   : string  read FCurrentUrl  write FCurrentUrl;
    property AreaLeft     : Integer read FAreaLeft     write FAreaLeft     default 0;
    property AreaTop      : Integer read FAreaTop      write FAreaTop      default 0;
    property AreaWidth    : Integer read FAreaWidth    write FAreaWidth    default 1920;
    property AreaHeight   : Integer read FAreaHeight   write FAreaHeight  default 1080;

    property OnExecuteAction     : TOnExecuteAction     read FOnExecuteAction     write FOnExecuteAction;
    property OnRequestScreenshot : TOnRequestScreenshot read FOnRequestScreenshot write FOnRequestScreenshot;
    property OnSafetyConfirmation: TOnSafetyConfirmation read FOnSafetyConfirmation write FOnSafetyConfirmation;
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
  inherited Create(AOwner);
  FScreenWidth  := 1920;
  FScreenHeight := 1080;
  FAreaLeft     := 0;
  FAreaTop      := 0;
  FAreaWidth    := 1920;
  FAreaHeight   := 1080;
  FCurrentUrl   := 'app://desktop';
end;

function TAiComputerUseTool.DenormalizeCoordinate(Coord, MaxPixels,
    Offset: Integer): Integer;
begin
  // Gemini devuelve 0-999. Convertimos a píxeles reales.
  if Coord < 0   then Coord := 0;
  if Coord > 999 then Coord := 999;
  Result := Round((Coord / 1000) * MaxPixels) + Offset;
end;

function TAiComputerUseTool.GetRealPoint(GeminiX, GeminiY: Integer): TPoint;
begin
  Result.X := DenormalizeCoordinate(GeminiX, FAreaWidth,  FAreaLeft);
  Result.Y := DenormalizeCoordinate(GeminiY, FAreaHeight, FAreaTop);
end;

function TAiComputerUseTool.GetRealRect(GemX1, GemY1, GemX2, GemY2: Integer): TRect;
begin
  Result.TopLeft     := GetRealPoint(GemX1, GemY1);
  Result.BottomRight := GetRealPoint(GemX2, GemY2);
end;

function TAiComputerUseTool.ActionTypeToString(AType: TAiComputerActionType): string;
begin
  case AType of
    catClick : Result := 'Click';
    catType  : Result := 'Type';
    catScroll: Result := 'Scroll';
  else
    Result := 'Unknown';
  end;
end;

function TAiComputerUseTool.ParseAction(ToolCall: TAiToolsFunction;
    out SafetyReason: string): TAiActionData;
var
  JData    : TJSONData;
  JArgs    : TJSONObject;
  JSafety  : TJSONObject;
  D        : TJSONData;
  Decision : string;
  FuncName : string;
begin
  // Inicializar record
  Result.ActionType      := catUnknown;
  Result.FunctionName    := ToolCall.Name;
  Result.X               := 0;
  Result.Y               := 0;
  Result.DestX           := 0;
  Result.DestY           := 0;
  Result.TextToType      := '';
  Result.KeyCombo        := '';
  Result.PressEnter      := False;
  Result.ScrollDirection := '';
  Result.ScrollAmount    := 0;
  Result.Width           := 0;
  Result.Height          := 0;
  Result.EditType        := '';
  Result.ColorName       := '';
  Result.Url             := '';
  SafetyReason           := '';

  // Parsear argumentos JSON
  JData := GetJSON(ToolCall.Arguments);
  if not (JData is TJSONObject) then
  begin
    if JData <> nil then JData.Free;
    Exit;
  end;
  JArgs := TJSONObject(JData);
  try
    // 1. Detección de Safety Decision (Human-in-the-loop)
    D := JArgs.Find('safety_decision');
    if (D <> nil) and (D is TJSONObject) then
    begin
      JSafety  := TJSONObject(D);
      Decision := '';
      D := JSafety.Find('decision');
      if D <> nil then
        Decision := D.AsString;
      if SameText(Decision, 'require_confirmation') then
      begin
        D := JSafety.Find('explanation');
        if D <> nil then
          SafetyReason := D.AsString;
      end;
    end;

    // 2. Mapeo de función a tipo de acción
    FuncName := LowerCase(Trim(ToolCall.Name));

    if (FuncName = 'click_at') or (FuncName = 'left_click') then
      Result.ActionType := catClick
    else if FuncName = 'right_click' then
      Result.ActionType := catRightClick
    else if FuncName = 'middle_click' then
      Result.ActionType := catMiddleClick
    else if FuncName = 'double_click' then
      Result.ActionType := catDoubleClick
    else if (FuncName = 'type_text_at') or (FuncName = 'type') then
      Result.ActionType := catType
    else if FuncName = 'key_combination' then
      Result.ActionType := catKeyCombination
    else if (FuncName = 'scroll_at') or (FuncName = 'scroll_document') then
      Result.ActionType := catScroll
    else if FuncName = 'drag_and_drop' then
      Result.ActionType := catDrag
    else if (FuncName = 'hover_at') or (FuncName = 'mouse_move') then
      Result.ActionType := catHover
    else if (FuncName = 'navigate') or (FuncName = 'search') or
            (FuncName = 'open_web_browser') then
      Result.ActionType := catNavigate
    else if FuncName = 'screenshot' then
      Result.ActionType := catScreenshot
    else if FuncName = 'wait_5_seconds' then
      Result.ActionType := catWait
    else if FuncName = 'image_edit_at' then
      Result.ActionType := catImageEdit
    else if FuncName = 'draw_box_at' then
      Result.ActionType := catDrawBox;

    // 3. Extracción de parámetros

    // Width/Height (también desnormalizados)
    D := JArgs.Find('width');
    if D <> nil then
      Result.Width := DenormalizeCoordinate(D.AsInteger, FAreaWidth, 0);

    D := JArgs.Find('height');
    if D <> nil then
      Result.Height := DenormalizeCoordinate(D.AsInteger, FAreaHeight, 0);

    D := JArgs.Find('color');
    if D <> nil then
      Result.ColorName := D.AsString;

    D := JArgs.Find('edit_type');
    if D <> nil then
      Result.EditType := D.AsString;

    // Coordenadas X, Y
    D := JArgs.Find('x');
    if D <> nil then
      Result.X := DenormalizeCoordinate(D.AsInteger, FAreaWidth, FAreaLeft);

    D := JArgs.Find('y');
    if D <> nil then
      Result.Y := DenormalizeCoordinate(D.AsInteger, FAreaHeight, FAreaTop);

    // Coordenadas destino (Drag)
    D := JArgs.Find('destination_x');
    if D <> nil then
      Result.DestX := DenormalizeCoordinate(D.AsInteger, FAreaWidth, FAreaLeft);

    D := JArgs.Find('destination_y');
    if D <> nil then
      Result.DestY := DenormalizeCoordinate(D.AsInteger, FAreaHeight, FAreaTop);

    // Texto y teclado
    D := JArgs.Find('text');
    if D <> nil then
      Result.TextToType := D.AsString;

    D := JArgs.Find('keys');
    if D <> nil then
      Result.KeyCombo := D.AsString;

    // PressEnter (boolean, default True según docs Gemini)
    D := JArgs.Find('press_enter');
    if D is TJSONBoolean then
      Result.PressEnter := D.AsBoolean
    else
      Result.PressEnter := True;

    // Scroll
    D := JArgs.Find('direction');
    if D <> nil then
      Result.ScrollDirection := D.AsString;

    D := JArgs.Find('magnitude');
    if D <> nil then
      Result.ScrollAmount := D.AsInteger
    else
      Result.ScrollAmount := 800;

    // Navegación
    D := JArgs.Find('url');
    if D <> nil then
      Result.Url := D.AsString;

  finally
    JArgs.Free;
  end;
end;

function TAiComputerUseTool.ProcessToolCall(ToolCall: TAiToolsFunction;
    out ResponseMedia: TAiMediaFile): string;
var
  ActionData  : TAiActionData;
  ActionResult: TAiActionResult;
  SafetyReason: string;
  UserAllowed : Boolean;
  JResponse   : TJSONObject;
begin
  ResponseMedia := nil;
  ActionResult.Success      := False;
  ActionResult.ErrorMessage := 'Unknown error';
  ActionResult.CustomOutput := '';

  // 1. Parsear datos y detectar safety
  ActionData := ParseAction(ToolCall, SafetyReason);

  // 2. Verificación de seguridad (Human-in-the-loop)
  if SafetyReason <> '' then
  begin
    UserAllowed := False;
    if Assigned(FOnSafetyConfirmation) then
      FOnSafetyConfirmation(Self, SafetyReason, UserAllowed)
    else
      UserAllowed := False;

    if not UserAllowed then
    begin
      JResponse := TJSONObject.Create;
      try
        JResponse.Add('output', 'action_denied_by_user');
        JResponse.Add('url', FCurrentUrl);
        JResponse.Add('safety_acknowledgement', False);
        Result := JResponse.AsJSON;
      finally
        JResponse.Free;
      end;
      Exit;
    end;
  end;

  // 3. Ejecutar acción (eventos externos)
  if Assigned(FOnExecuteAction) then
  begin
    try
      FOnExecuteAction(Self, ActionData, ActionResult);
    except
      on E: Exception do
      begin
        ActionResult.Success      := False;
        ActionResult.ErrorMessage := E.Message;
      end;
    end;
  end
  else
  begin
    ActionResult.Success      := False;
    ActionResult.ErrorMessage := 'OnExecuteAction event not assigned.';
  end;

  // Tiempo de asentamiento para que la UI reaccione
  if ActionResult.Success then
    Sleep(1000);

  // 4. Capturar nuevo estado (si fue exitoso)
  if ActionResult.Success then
  begin
    if Assigned(FOnRequestScreenshot) then
      FOnRequestScreenshot(Self, ResponseMedia);

    if (ActionData.ActionType = catNavigate) and (ActionData.Url <> '') then
      FCurrentUrl := ActionData.Url;
  end;

  // 5. Construir JSON de respuesta
  JResponse := TJSONObject.Create;
  try
    if ActionResult.Success then
      JResponse.Add('output', 'action_executed_successfully')
    else
      JResponse.Add('output', 'error: ' + ActionResult.ErrorMessage);

    JResponse.Add('url', FCurrentUrl);

    if SafetyReason <> '' then
      JResponse.Add('safety_acknowledgement', True);

    if ActionResult.CustomOutput <> '' then
      JResponse.Add('data', ActionResult.CustomOutput);

    Result := JResponse.AsJSON;
  finally
    JResponse.Free;
  end;
end;

end.
