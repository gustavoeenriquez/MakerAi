unit uMakerAi.MCPServer.Bridge;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  uMakerAi.Core, uMakerAi.Tools.Functions, uMakerAi.MCPServer.Core,
  uMakerAi.Chat.Messages;

type
  { TTAiFunctionToolProxy:
    Implementa IAiMCPTool para exponer cualquier funci?n de TAiFunctions
    (local o remota de otro MCP) a trav?s de nuestro servidor. }

  TTAiFunctionToolProxy = class(TInterfacedObject, IAiMCPTool)
  private
    FFunctionItem: TFunctionActionItem;
    FAiFunctions: TAiFunctions;
    function GetName: string;
    function GetDescription: string;
    function GetInputSchema: TJSONObject;
  public
    constructor Create(AAiFunctions: TAiFunctions; AItem: TFunctionActionItem);
    destructor Destroy; override;

    function Execute(const Arguments: TJSONObject; const AuthContext: TAiAuthContext): TJSONObject;

    property Name: string read GetName;
    property Description: string read GetDescription;
    property InputSchema: TJSONObject read GetInputSchema; // El interface pide TJSONObject, el getter lo maneja
  end;

implementation

{ TTAiFunctionToolProxy }

constructor TTAiFunctionToolProxy.Create(AAiFunctions: TAiFunctions; AItem: TFunctionActionItem);
begin
  inherited Create;
  FAiFunctions := AAiFunctions;
  FFunctionItem := AItem;
end;

destructor TTAiFunctionToolProxy.Destroy;
begin
  // No liberamos FFunctionItem ni FAiFunctions porque no somos sus due?os
  inherited;
end;

function TTAiFunctionToolProxy.GetName: string;
begin
  // Devolvemos el nombre completo (incluyendo el prefijo _99_ si es una funci?n externa)
  Result := FFunctionItem.FunctionName;
end;

function TTAiFunctionToolProxy.GetDescription: string;
begin
  Result := FFunctionItem.Description.Text.Trim;
end;

function TTAiFunctionToolProxy.GetInputSchema: TJSONObject;
var
  LFullOpenAiJson, LFuncObj, LParams: TJSONObject;
begin
  Result := nil;

  // 1. Obtenemos la definici?n completa que ya genera tu componente local.
  // El formato es: {"type": "function", "function": {"name": "...", "parameters": {...}}}
  LFullOpenAiJson := FFunctionItem.ToJSon(False);

  if not Assigned(LFullOpenAiJson) then
    Exit(TJSONObject.Create);

  try
    // 2. El servidor MCP solo necesita el esquema de par?metros (InputSchema),
    // no toda la envoltura de OpenAI.
    if LFullOpenAiJson.TryGetValue<TJSONObject>('function', LFuncObj) then
    begin
      if LFuncObj.TryGetValue<TJSONObject>('parameters', LParams) then
      begin
        // 3. Clonamos el objeto de par?metros.
        // El servidor MCP se encargar? de liberar este objeto despu?s de usarlo.
        Result := LParams.Clone as TJSONObject;
      end;
    end;

    // 4. Si el esquema no existe o est? vac?o, devolvemos un objeto de esquema v?lido pero vac?o.
    if not Assigned(Result) then
    begin
      Result := TJSONObject.Create;
      Result.AddPair('type', 'object');
      Result.AddPair('properties', TJSONObject.Create);
    end;

  finally
    LFullOpenAiJson.Free; // Liberamos el JSON temporal de OpenAI
  end;
end;


function TTAiFunctionToolProxy.Execute(const Arguments: TJSONObject; const AuthContext: TAiAuthContext): TJSONObject;
var
  LToolCall: TAiToolsFunction;
  LResponseBuilder: TAiMCPResponseBuilder;
  LResMsg: TAIChatMessage;
  I: Integer;
begin
  Result := nil;
  LResponseBuilder := TAiMCPResponseBuilder.New;
  LToolCall := TAiToolsFunction.Create;

  // Creamos un mensaje temporal para capturar posibles archivos multimedia
  LResMsg := TAIChatMessage.Create('','');
  try
    // 1. Preparar la llamada
    LToolCall.Name := FFunctionItem.FunctionName;
    if Assigned(Arguments) then
      LToolCall.Arguments := Arguments.ToJSON;

    // Asignamos el mensaje para que DoCallFunction pueda depositar mediafiles ah?
    LToolCall.ResMsg := LResMsg;

    // 2. Ejecutar el motor central de TAiFunctions
    // Esto disparar? OnAction si es local, o llamar? a otro servidor si es remoto.
    if FAiFunctions.DoCallFunction(LToolCall) then
    begin
      // 3. Procesar la respuesta de texto
      if not LToolCall.Response.IsEmpty then
      begin
        // Si la respuesta ya es un JSON de contenido MCP (un objeto con 'content')
        if LToolCall.Response.Trim.StartsWith('{"content":') then
        begin
          var LParsed := TJSONObject.ParseJSONValue(LToolCall.Response);
          if LParsed is TJSONObject then
            Result := TJSONObject(LParsed)
          else
          begin
            LParsed.Free;
            LResponseBuilder.AddText(LToolCall.Response);
          end;
        end
        else
        begin
          // Si es texto plano (lo m?s com?n), lo a?adimos al builder
          LResponseBuilder.AddText(LToolCall.Response);
        end;
      end;

      // 4. PROCESAR MEDIAFILES (Im?genes, Audio, etc.)
      // Si la funci?n gener? archivos (ej. un gr?fico o un PDF), los incluimos en la respuesta MCP
      if Assigned(LResMsg.MediaFiles) and (LResMsg.MediaFiles.Count > 0) then
      begin
        for I := 0 to LResMsg.MediaFiles.Count - 1 do
        begin
          LResponseBuilder.AddFileFromStream(
            LResMsg.MediaFiles[I].Content,
            LResMsg.MediaFiles[I].filename,
            LResMsg.MediaFiles[I].MimeType
          );
        end;
      end;

      // Si a?n no tenemos resultado (porque era texto plano + media), lo construimos
      if not Assigned(Result) then
        Result := LResponseBuilder.Build;
    end
    else
    begin
      // Manejo de errores
      Result := TAiMCPResponseBuilder.New
        .AddText('Error: La ejecuci?n de la herramienta no devolvi? un resultado exitoso.')
        .Build;
    end;

  finally
    LToolCall.Free;
    LResMsg.Free;
    LResponseBuilder.Free;
  end;
end;

end.
