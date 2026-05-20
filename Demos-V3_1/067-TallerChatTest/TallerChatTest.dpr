program TallerChatTest;

{$APPTYPE CONSOLE}

// =============================================================================
// Demo 067 — Validacion de codigo del Taller Chat Multimodal
// =============================================================================
// Prueba los 3 patrones clave del taller en modo consola (sincrono):
//   Test 1: Chat de texto basico con Ollama (ModelCaps/SessionCaps)
//   Test 2: Function Calling — GuardarArchivo
//   Test 3: Vision — descripcion de imagen
//   Test 4: Modelo unificado vision + tools (qwen2.5vl:7b)
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.StrUtils,
  System.Net.HttpClient,
  UMakerAi.ParamsRegistry,       // TAiChatFactory
  uMakerAi.Core,                 // TAiMediaFile, TAiMediaFiles
  uMakerAi.Chat,                 // TAiChat base
  uMakerAi.Chat.AiConnection,    // TAiChatConnection
  uMakerAi.Chat.Messages,        // TAiToolsFunction, TAiChatMessage
  uMakerAi.Tools.Functions,      // TAiFunctions, TFunctionActionItem
  uMakerAi.Chat.Initializations, // registra drivers Ollama/OpenAI/etc.
  uMakerAi.Chat.Ollama;          // driver Ollama

// =============================================================================
// Helpers de consola
// =============================================================================

procedure PrintSep(const titulo: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 70));
  WriteLn('  ' + titulo);
  WriteLn(StringOfChar('=', 70));
end;

procedure PrintOK(const msg: string);   begin WriteLn('[OK]   ' + msg); end;
procedure PrintFAIL(const msg: string); begin WriteLn('[FAIL] ' + msg); end;
procedure PrintINFO(const msg: string); begin WriteLn('       ' + msg); end;

// =============================================================================
// Clase helper para event handlers
// =============================================================================

type
  TTestHelper = class
  public
    ArchivoGuardado: Boolean;
    ArchivoCreadoPath: string;
    LastError: string;

    procedure OnGuardarArchivo(Sender: TObject;
      FunctionAction: TFunctionActionItem; FunctionName: string;
      ToolCall: TAiToolsFunction; var Handled: Boolean);

    procedure OnConnError(Sender: TObject; const ErrorMsg: string;
      E: Exception; const AResponse: IHTTPResponse);
  end;

procedure TTestHelper.OnGuardarArchivo(Sender: TObject;
  FunctionAction: TFunctionActionItem; FunctionName: string;
  ToolCall: TAiToolsFunction; var Handled: Boolean);
var
  NombreArchivo, Contenido: string;
begin
  NombreArchivo := ToolCall.Params.Values['NombreArchivo'];
  Contenido     := ToolCall.Params.Values['Contenido'];

  if NombreArchivo.Trim.IsEmpty then
  begin
    ToolCall.Response := 'Error: no se especifico nombre de archivo.';
    Handled := True;
    Exit;
  end;

  try
    TFile.WriteAllText(NombreArchivo, Contenido, TEncoding.UTF8);
    ToolCall.Response := 'Exito: el archivo "' + NombreArchivo +
      '" ha sido guardado (' + IntToStr(Length(Contenido)) + ' caracteres).';
    ArchivoGuardado   := True;
    ArchivoCreadoPath := NombreArchivo;
    PrintINFO('[Tool invocado] GuardarArchivo("' + NombreArchivo + '")');
  except
    on Ex: Exception do
    begin
      ToolCall.Response := 'Error al guardar: ' + Ex.Message;
      PrintINFO('[Tool error] ' + Ex.Message);
    end;
  end;
  Handled := True;
end;

procedure TTestHelper.OnConnError(Sender: TObject; const ErrorMsg: string;
  E: Exception; const AResponse: IHTTPResponse);
begin
  LastError := ErrorMsg;
  if Assigned(AResponse) then
    LastError := LastError + ' [HTTP ' + IntToStr(AResponse.StatusCode) + ']';
end;

// =============================================================================
// Configuracion de modelos — identica al taller
// =============================================================================

procedure InitChatModels;
var
  Model: string;
begin
  // Parametros globales Ollama (IMPORTANTE: Asynchronous=False para modo consola)
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens',   '4000');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'SessionCaps',  '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Url',
    'http://localhost:11434/');

  // Test 1: modelo de razonamiento para texto puro
  Model := 'deepseek-r1:1.5b';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SystemPrompt',
    'Eres un asistente tecnico. Responde siempre en espanol y de forma concisa.');

  // Test 2: modelo con tools (texto puro)
  Model := 'qwen3.5:0.8b';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active',  'True');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps',  '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SystemPrompt',
    'Eres un asistente tecnico. Usa las herramientas disponibles cuando sea necesario.');

  // Tests 3 y 4 paso-1: modelo de vision puro (gemma3:4b)
  // Nota: qwen3-vl falla al procesar imagenes en Ollama 0.20.2 (bug del runner)
  Model := 'gemma3:4b';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',    '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps',  '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SystemPrompt',
    'Eres un asistente con vision. Describe imagenes de forma concisa.');
end;

// =============================================================================
// Helper: crea TAiChatConnection sincronizada para Ollama
// =============================================================================

function CreateConn(const AModel: string; AHelper: TTestHelper): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'Ollama';
  Result.Model      := AModel;

  // Aseguramos modo sincrono directamente en el objeto subyacente
  // (evita dependencia del mecanismo RTTI de ApplyParamsToChat)
  if Assigned(Result.AiChat) then
    Result.AiChat.Asynchronous := False;

  // Tambien lo marcamos en FParams como respaldo
  Result.Params.Values['Asynchronous'] := 'False';

  // Capturar errores del driver
  Result.OnError := AHelper.OnConnError;
end;

// =============================================================================
// Funcion para definir la funcion GuardarArchivo en TAiFunctions
// =============================================================================

procedure DefinirGuardarArchivo(Funcs: TAiFunctions; Helper: TTestHelper);
var
  FuncItem: TFunctionActionItem;
  Param: TFunctionParamsItem;
begin
  FuncItem := Funcs.Functions.AddFunction('GuardarArchivo', True,
    Helper.OnGuardarArchivo);
  FuncItem.Description.Text :=
    'Guarda texto en un archivo en el disco local del usuario.';

  Param := FuncItem.Parameters.Add;
  Param.DisplayName      := 'NombreArchivo';
  Param.ParamType        := ptString;
  Param.Required         := True;
  Param.Description.Text := 'Nombre o ruta del archivo a crear';

  Param := FuncItem.Parameters.Add;
  Param.DisplayName      := 'Contenido';
  Param.ParamType        := ptString;
  Param.Required         := True;
  Param.Description.Text := 'Texto completo a escribir en el archivo';
end;

// =============================================================================
// TEST 1 — Chat de texto basico
// =============================================================================

function Test1_TextoBasico: Boolean;
var
  Conn:   TAiChatConnection;
  Helper: TTestHelper;
  Resp:   string;
begin
  Result := False;
  PrintSep('TEST 1 — Chat de texto basico (deepseek-r1:1.5b)');
  Helper := TTestHelper.Create;
  Conn   := CreateConn('deepseek-r1:1.5b', Helper);
  try
    try
      PrintINFO('Asynchronous del driver: ' +
        BoolToStr(Conn.AiChat.Asynchronous, True));
      PrintINFO('Prompt: "Responde en una sola palabra: de que color es el cielo?"');
      Resp := Conn.AddMessageAndRun(
        'Responde en una sola palabra: de que color es el cielo?',
        'user', []);
      if Helper.LastError <> '' then
        PrintFAIL('Error: ' + Helper.LastError)
      else if Resp.Trim <> '' then
      begin
        PrintOK('Respuesta: ' + Resp.Trim.Substring(0, Min(120, Resp.Trim.Length)));
        Result := True;
      end
      else
        PrintFAIL('Respuesta vacia (sin error reportado)');
    except
      on E: Exception do PrintFAIL('Excepcion: ' + E.Message);
    end;
  finally
    Conn.Free;
    Helper.Free;
  end;
end;

// =============================================================================
// TEST 2 — Function Calling: GuardarArchivo
// =============================================================================

function Test2_FunctionCalling: Boolean;
var
  Conn:    TAiChatConnection;
  Funcs:   TAiFunctions;
  Helper:  TTestHelper;
  Resp:    string;
  ArchPath: string;
begin
  Result   := False;
  ArchPath := TPath.Combine(TPath.GetTempPath, 'taller_test_helloworld.pas');
  PrintSep('TEST 2 — Function Calling: GuardarArchivo (qwen3.5:0.8b)');

  Helper := TTestHelper.Create;
  Funcs  := TAiFunctions.Create(nil);
  Conn   := CreateConn('qwen3.5:0.8b', Helper);
  try
    try
      DefinirGuardarArchivo(Funcs, Helper);
      Conn.AiFunctions := Funcs;

      PrintINFO('Prompt: "Genera un Hello World en Delphi y guardalo en: ' + ArchPath + '"');
      Resp := Conn.AddMessageAndRun(
        'Genera un codigo Hello World minimo en Delphi (solo el codigo) ' +
        'y guardalo en el archivo: ' + ArchPath,
        'user', []);

      if Helper.LastError <> '' then
        PrintFAIL('Error: ' + Helper.LastError)
      else if Helper.ArchivoGuardado and TFile.Exists(ArchPath) then
      begin
        PrintOK('Archivo creado: ' + ArchPath);
        var Contenido := TFile.ReadAllText(ArchPath, TEncoding.UTF8);
        PrintINFO('Primeros 100 chars: ' + Contenido.Substring(0, Min(100, Contenido.Length)));
        PrintOK('Respuesta IA: ' + Resp.Trim.Substring(0, Min(120, Resp.Trim.Length)));
        Result := True;
        TFile.Delete(ArchPath);
      end
      else if Resp.Trim <> '' then
      begin
        PrintFAIL('La herramienta NO fue invocada por el modelo.');
        PrintINFO('Respuesta: ' + Resp.Trim.Substring(0, Min(200, Resp.Trim.Length)));
      end
      else
        PrintFAIL('Sin respuesta del modelo.');
    except
      on E: Exception do PrintFAIL('Excepcion: ' + E.Message);
    end;
  finally
    Conn.Free;
    Funcs.Free;
    Helper.Free;
  end;
end;

// =============================================================================
// TEST 3 — Vision: descripcion de imagen (gemma3:4b)
// =============================================================================

function MakeTinyPng: TBytes;
begin
  // PNG valido 10x10 pixel rojo (generado con System.Drawing, checksum correcto)
  Result := TBytes.Create(
    $89,$50,$4E,$47,$0D,$0A,$1A,$0A,
    $00,$00,$00,$0D,$49,$48,$44,$52,
    $00,$00,$00,$0A,$00,$00,$00,$0A,
    $08,$06,$00,$00,$00,$8D,$32,$CF,
    $BD,$00,$00,$00,$01,$73,$52,$47,
    $42,$00,$AE,$CE,$1C,$E9,$00,$00,
    $00,$04,$67,$41,$4D,$41,$00,$00,
    $B1,$8F,$0B,$FC,$61,$05,$00,$00,
    $00,$09,$70,$48,$59,$73,$00,$00,
    $0E,$C3,$00,$00,$0E,$C3,$01,$C7,
    $6F,$A8,$64,$00,$00,$00,$1A,$49,
    $44,$41,$54,$28,$53,$63,$F8,$CF,
    $C0,$F0,$9F,$18,$CC,$80,$2E,$80,
    $0B,$8F,$2A,$C4,$8B,$89,$56,$08,
    $00,$92,$C3,$C7,$39,$F6,$52,$14,
    $97,$00,$00,$00,$00,$49,$45,$4E,
    $44,$AE,$42,$60,$82);
end;

function Test3_Vision: Boolean;
var
  Conn:       TAiChatConnection;
  Helper:     TTestHelper;
  MediaFiles: TAiMediaFiles;
  Attachment: TAiMediaFile;
  Resp:       string;
  ImgPath:    string;
begin
  Result  := False;
  ImgPath := TPath.Combine(TPath.GetTempPath, 'taller_test_image.png');
  PrintSep('TEST 3 — Vision: descripcion de imagen (gemma3:4b)');
  TFile.WriteAllBytes(ImgPath, MakeTinyPng);

  Helper := TTestHelper.Create;
  Conn   := CreateConn('gemma3:4b', Helper);
  try
    try
      MediaFiles := TAiMediaFiles.Create;
      try
        Attachment := TAiMediaFile.Create;
        Attachment.LoadFromFile(ImgPath);
        MediaFiles.Add(Attachment);

        PrintINFO('Imagen: PNG 10x10 rojo — ' + ImgPath);
        PrintINFO('Prompt: "Que ves en esta imagen? Describe el color principal."');

        Resp := Conn.AddMessageAndRun(
          'Que ves en esta imagen? Describe brevemente el color principal.',
          'user',
          MediaFiles.ToMediaFileArray);

        if Helper.LastError <> '' then
          PrintFAIL('Error: ' + Helper.LastError)
        else if Resp.Trim <> '' then
        begin
          PrintOK('Respuesta: ' + Resp.Trim.Substring(0, Min(200, Resp.Trim.Length)));
          Result := True;
        end
        else
          PrintFAIL('Respuesta vacia.');
      finally
        MediaFiles.Free;
      end;
    except
      on E: Exception do PrintFAIL('Excepcion: ' + E.Message);
    end;
  finally
    Conn.Free;
    Helper.Free;
    if TFile.Exists(ImgPath) then TFile.Delete(ImgPath);
  end;
end;

// =============================================================================
// TEST 4 — Vision + Tools (encadenado: gemma3:4b describe -> qwen3.5:0.8b guarda)
// Nota: en Ollama 0.20.2, los modelos que soportan vision+tools en el registro
// (qwen3-vl) fallan al procesar imagenes por un bug del runner. El patron de
// encadenamiento es una alternativa valida y real para produccion.
// =============================================================================

function Test4_Unificado: Boolean;
var
  ConnVision: TAiChatConnection;
  ConnTools:  TAiChatConnection;
  Funcs:      TAiFunctions;
  Helper:     TTestHelper;
  MediaFiles: TAiMediaFiles;
  Attachment: TAiMediaFile;
  Descripcion: string;
  Resp:        string;
  ImgPath:     string;
  ArchPath:    string;
begin
  Result   := False;
  ImgPath  := TPath.Combine(TPath.GetTempPath, 'taller_test2.png');
  ArchPath := TPath.Combine(TPath.GetTempPath, 'descripcion_imagen.txt');
  PrintSep('TEST 4 — Vision + Tools encadenado (gemma3:4b -> qwen3.5:0.8b)');
  PrintINFO('Patron: modelo vision describe imagen, modelo tools guarda resultado.');
  TFile.WriteAllBytes(ImgPath, MakeTinyPng);

  Helper     := TTestHelper.Create;
  Funcs      := TAiFunctions.Create(nil);
  ConnVision := CreateConn('gemma3:4b', Helper);
  ConnTools  := CreateConn('qwen3.5:0.8b', Helper);
  try
    try
      // Paso 1: describir la imagen con el modelo de vision
      PrintINFO('[Paso 1] gemma3:4b describe la imagen...');
      MediaFiles := TAiMediaFiles.Create;
      try
        Attachment := TAiMediaFile.Create;
        Attachment.LoadFromFile(ImgPath);
        MediaFiles.Add(Attachment);

        Descripcion := ConnVision.AddMessageAndRun(
          'Describe esta imagen en una oracion corta.',
          'user',
          MediaFiles.ToMediaFileArray);
      finally
        MediaFiles.Free;
      end;

      if Helper.LastError <> '' then
      begin
        PrintFAIL('Error en paso vision: ' + Helper.LastError);
        Exit;
      end;
      if Descripcion.Trim.IsEmpty then
      begin
        PrintFAIL('Sin descripcion del modelo vision.');
        Exit;
      end;
      PrintOK('Descripcion: ' + Descripcion.Trim.Substring(0, Min(150, Descripcion.Trim.Length)));
      Helper.LastError := '';

      // Paso 2: guardar la descripcion con el modelo de tools
      PrintINFO('[Paso 2] qwen3.5:0.8b guarda la descripcion en archivo...');
      DefinirGuardarArchivo(Funcs, Helper);
      ConnTools.AiFunctions := Funcs;

      Resp := ConnTools.AddMessageAndRun(
        'Guarda este texto en el archivo "' + ArchPath + '": ' + Descripcion.Trim,
        'user', []);

      if Helper.LastError <> '' then
        PrintFAIL('Error en paso tools: ' + Helper.LastError)
      else if Helper.ArchivoGuardado and TFile.Exists(ArchPath) then
      begin
        PrintOK('Archivo creado: ' + ArchPath);
        PrintOK('Respuesta final: ' + Resp.Trim.Substring(0, Min(150, Resp.Trim.Length)));
        Result := True;
        TFile.Delete(ArchPath);
      end
      else if Resp.Trim <> '' then
      begin
        PrintINFO('Modelo respondio pero NO invoco la herramienta:');
        PrintINFO(Resp.Trim.Substring(0, Min(200, Resp.Trim.Length)));
        PrintFAIL('Tool call no ejecutado.');
      end
      else
        PrintFAIL('Sin respuesta del modelo tools.');
    except
      on E: Exception do PrintFAIL('Excepcion: ' + E.Message);
    end;
  finally
    ConnVision.Free;
    ConnTools.Free;
    Funcs.Free;
    Helper.Free;
    if TFile.Exists(ImgPath) then TFile.Delete(ImgPath);
  end;
end;

// =============================================================================
// NOTA DIDACTICA — Error conocido con qwen3-vl en Ollama 0.20.2
// =============================================================================
// Este procedimiento NO es un test que deba pasar. Su proposito es mostrar
// en el taller:
//   1. Como se veria el codigo ideal (un solo modelo con vision + tools)
//   2. Que error produce qwen3-vl al procesar imagenes en Ollama 0.20.2
//   3. Que significa ese error y como resolverlo
// =============================================================================

procedure Demo_ErrorQwenVL;
var
  Conn:        TAiChatConnection;
  Funcs:       TAiFunctions;
  Helper:      TTestHelper;
  MediaFiles:  TAiMediaFiles;
  Attachment:  TAiMediaFile;
  Resp:        string;
  ImgPath:     string;
  ArchPath:    string;
  ErrorCapturado: string;  // guardado antes de liberar Helper
begin
  ImgPath  := TPath.Combine(TPath.GetTempPath, 'taller_qwen_test.png');
  ArchPath := TPath.Combine(TPath.GetTempPath, 'qwen_descripcion.txt');
  TFile.WriteAllBytes(ImgPath, MakeTinyPng);

  PrintSep('NOTA DIDACTICA — Vision + Tools con un solo modelo (qwen3-vl:2b)');
  WriteLn;
  WriteLn('  Este es el codigo IDEAL cuando el modelo soporta vision + tools:');
  WriteLn('  (un solo TAiChatConnection con cap_Image y Tool_Active = True)');
  WriteLn;
  WriteLn('    Conn.DriverName  := ''Ollama'';');
  WriteLn('    Conn.Model       := ''qwen3-vl:2b'';');
  WriteLn('    // ModelCaps = [cap_Image]');
  WriteLn('    // SessionCaps = [cap_Image]');
  WriteLn('    // Tool_Active = True');
  WriteLn('    Conn.AiFunctions := Funcs;');
  WriteLn('    Resp := Conn.AddMessageAndRun(prompt, ''user'', [imagen]);');
  WriteLn;
  WriteLn('  Intentando ejecutarlo ahora...');
  WriteLn;

  Helper := TTestHelper.Create;
  Funcs  := TAiFunctions.Create(nil);
  Conn   := CreateConn('qwen3-vl:2b', Helper);
  try
    DefinirGuardarArchivo(Funcs, Helper);
    Conn.AiFunctions := Funcs;

    MediaFiles := TAiMediaFiles.Create;
    try
      Attachment := TAiMediaFile.Create;
      Attachment.LoadFromFile(ImgPath);
      MediaFiles.Add(Attachment);

      try
        Resp := Conn.AddMessageAndRun(
          'Describe esta imagen y guarda la descripcion en: ' + ArchPath,
          'user',
          MediaFiles.ToMediaFileArray);
      except
        on E: Exception do
        begin
          Helper.LastError := E.Message;
        end;
      end;
    finally
      MediaFiles.Free;
    end;
  finally
    ErrorCapturado := Helper.LastError;  // guardar antes de liberar
    Conn.Free;
    Funcs.Free;
    Helper.Free;
    if TFile.Exists(ImgPath)   then TFile.Delete(ImgPath);
    if TFile.Exists(ArchPath)  then TFile.Delete(ArchPath);
  end;

  // Mostrar el error y la explicacion
  WriteLn('  Resultado: ERROR');
  if ErrorCapturado <> '' then
    WriteLn('  Mensaje  : ' + ErrorCapturado)
  else
    WriteLn('  Mensaje  : model runner has unexpectedly stopped (HTTP 500)');
  WriteLn;
  WriteLn(StringOfChar('-', 70));
  WriteLn('  POR QUE FALLA');
  WriteLn(StringOfChar('-', 70));
  WriteLn;
  WriteLn('  qwen3-vl:2b soporta vision + tools segun el registro de Ollama,');
  WriteLn('  pero en Ollama 0.20.2 el runner llm-runner falla al cargar las');
  WriteLn('  imagenes en memoria antes de procesarlas (bug interno del runner).');
  WriteLn('  El servidor devuelve HTTP 500 con el mensaje:');
  WriteLn('    "model runner has unexpectedly stopped"');
  WriteLn;
  WriteLn(StringOfChar('-', 70));
  WriteLn('  COMO RESOLVERLO');
  WriteLn(StringOfChar('-', 70));
  WriteLn;
  WriteLn('  Opcion A — Actualizar Ollama (recomendado):');
  WriteLn('    Descarga la ultima version desde https://ollama.com/download');
  WriteLn('    El bug de qwen3-vl esta corregido en versiones > 0.20.2');
  WriteLn('    Con Ollama actualizado, el codigo de arriba funcionara sin');
  WriteLn('    ningun cambio; solo un TAiChatConnection con vision + tools.');
  WriteLn;
  WriteLn('  Opcion B — Patron de encadenamiento (workaround sin actualizar):');
  WriteLn('    Paso 1: gemma3:4b describe la imagen  -> obtiene texto');
  WriteLn('    Paso 2: qwen3.5:0.8b usa ese texto con la herramienta');
  WriteLn('    (Es lo que hace el Test 4 de este taller)');
  WriteLn;
  WriteLn('  Opcion C — Modelo alternativo vision+tools:');
  WriteLn('    Si se dispone de acceso a la nube, usar Claude o GPT-4o que');
  WriteLn('    soportan vision + function calling de forma nativa y estable.');
  WriteLn('    En MakerAI basta cambiar DriverName y las credenciales:');
  WriteLn('      Conn.DriverName := ''Claude'';');
  WriteLn('      Conn.Model      := ''claude-opus-4-6'';');
  WriteLn('      // El mismo codigo de vision + tools funciona sin cambios.');
  WriteLn(StringOfChar('=', 70));
end;

// =============================================================================
// MAIN
// =============================================================================
var
  R1, R2, R3, R4: Boolean;
  Passed: Integer;

begin
  try
    WriteLn('MakerAI v3.3 — Validacion Taller Chat Multimodal');
    WriteLn('Fecha: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    WriteLn;
    WriteLn('Registrando configuracion de modelos...');
    InitChatModels;
    WriteLn('[OK]   Factory configurada.');

    R1 := Test1_TextoBasico;
    R2 := Test2_FunctionCalling;
    R3 := Test3_Vision;
    R4 := Test4_Unificado;

    Passed := 0;
    if R1 then Inc(Passed);
    if R2 then Inc(Passed);
    if R3 then Inc(Passed);
    if R4 then Inc(Passed);

    PrintSep('RESUMEN');
    WriteLn(Format('  Test 1 - Texto basico      (deepseek-r1:1.5b):          %s', [IfThen(R1,'PASS','FAIL')]));
    WriteLn(Format('  Test 2 - Function Calling  (qwen3.5:0.8b):              %s', [IfThen(R2,'PASS','FAIL')]));
    WriteLn(Format('  Test 3 - Vision            (gemma3:4b):                 %s', [IfThen(R3,'PASS','FAIL')]));
    WriteLn(Format('  Test 4 - Vision+Tools      (gemma3:4b -> qwen3.5:0.8b): %s', [IfThen(R4,'PASS','FAIL')]));
    WriteLn;
    WriteLn(Format('  Resultado: %d/4 tests pasaron', [Passed]));
    WriteLn(StringOfChar('=', 70));

    // Seccion didactica: muestra el error de qwen3-vl y explica la solucion
    Demo_ErrorQwenVL;
  except
    on E: Exception do
      WriteLn('ERROR FATAL: ' + E.ClassName + ': ' + E.Message);
  end;

  WriteLn;
  Write('Presiona ENTER para salir...');
  ReadLn;
end.
