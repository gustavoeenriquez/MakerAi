unit uMakerAi.Chat.OpenAiResponses;
// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// Esta es una versión Alpha,  todavía está incompleta
// Falta implementar el llamado de funciones,  por alguna razón siguiendo la documentación no responde correctamente
// Falta implementar el modo ascincrónico

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  System.NetEncoding, System.Net.URLClient, System.Net.HttpClient,
  System.Net.Mime,
  System.Net.HttpClientComponent, System.JSON, Rest.JSON,
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core,
  uMakerAi.ToolFunctions, uMakerAi.Utils.CodeExtractor;

type

  TAiOpenAiResponses = class(TAiChat)
  private
    FStore: Boolean;
    FTruncation: String;
    FParallel_ToolCalls: Boolean;
    FReasoningEffort: String; // Nuevo campo para reasoning effort

    // Nuevos campos para representar la Response
    FResponseId: String;
    FResponseStatus: String; // completed, failed, in_progress, incomplete
    FLastApiResponse: TJSonObject; // Guarda la última respuesta completa de la API

    procedure SetStore(const Value: Boolean);
    procedure SetTruncation(const Value: String);
    procedure SetParallel_ToolCalls(const Value: Boolean);
    procedure SetReasoningEffort(const Value: String); // Setter para reasoning effort

  protected
    // function CallResponsesApi(ABody: string): TJSonObject;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    procedure ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);

    procedure UpdateResponseStatus(aStatus: String);

    // function GetMessages: TJSonArray; override;  //Ya no es necesario sobreescribir GetMessage

    function InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;

    Function InitChatCompletions: String; Override;
    Function GetTools(Funcion: TAiFunctions): TJSonArray;
    Function GetToolsItems(Functions: TFunctionActionItems): TJSonArray;

    procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Procedure DoCallFunction(ToolCall: TAiToolsFunction); Override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // function Run(aMsg: TAiChatMessage = Nil): String; override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

    property ResponseId: String read FResponseId;
    property ResponseStatus: String read FResponseStatus;

  published
    property Store: Boolean read FStore write SetStore default True;
    property Truncation: String read FTruncation write SetTruncation;
    property Parallel_ToolCalls: Boolean read FParallel_ToolCalls write SetParallel_ToolCalls default True;
    property ReasoningEffort: String read FReasoningEffort write SetReasoningEffort; // Nueva propiedad
  end;

procedure Register;

implementation

uses
  System.Generics.Defaults, System.Generics.Collections;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenAiResponses]);
end;

{ TAiOpenAiResponses }

class function TAiOpenAiResponses.GetDriverName: string;
Begin
  Result := 'OpenAiResponses';
End;

class procedure TAiOpenAiResponses.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@OPENAI_API_KEY');
  Params.Add('Model=gpt-4o');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.openai.com/v1/');
End;

class function TAiOpenAiResponses.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiOpenAiResponses.Create(Sender);
End;

constructor TAiOpenAiResponses.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Valores por defecto
  ApiKey := '@OPENAI_API_KEY';
  FStore := True;
  FTruncation := 'disabled';
  FParallel_ToolCalls := True;
  FReasoningEffort := ''; // Valor por defecto para reasoning
  FResponseId := '';
  FResponseStatus := '';
  FLastApiResponse := Nil;
end;

destructor TAiOpenAiResponses.Destroy;
begin
  if Assigned(FLastApiResponse) then
    FLastApiResponse.Free;
  inherited;
end;

procedure TAiOpenAiResponses.SetStore(const Value: Boolean);
begin
  FStore := Value;
end;

procedure TAiOpenAiResponses.SetTruncation(const Value: String);
begin
  FTruncation := Value;
end;

procedure TAiOpenAiResponses.SetParallel_ToolCalls(const Value: Boolean);
begin
  FParallel_ToolCalls := Value;
end;

procedure TAiOpenAiResponses.SetReasoningEffort(const Value: String);
begin
  FReasoningEffort := Value;
end;

procedure TAiOpenAiResponses.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  S: String;
  JOutput: TJSonArray;
  JOutputItem: TJSonObject;
  JContent: TJSonArray;
  JText: TJSonObject;
  JAnnotations: TJSonArray;
  JAnnotationItem: TJSonObject;
  newMsg, AskMsg: TAiChatMessage;
  WebSearch: TAiWebSearch;
  WebSearchItem: TAiWebSearchItem;
  i, j: Integer;
  FunctionCallId, FunctionName, FunctionArguments: String;
  ToolCall: TAiToolsFunction;
  ToolCalls: TObjectList<TAiToolsFunction>; // Lista para almacenar ToolCalls
  TaskList: array of ITask;
  NumTasks: Integer;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

begin
  AskMsg := GetLastMessage; // Obtiene el mensaje de la solicitud

  // Extraer los datos del JObj
  if Assigned(jObj) then
  Begin
    try
      Try
        // Actualizar el ResponseId
        if jObj.TryGetValue<String>('id', S) then
          FResponseId := S;

        // Actualizar el status
        if jObj.TryGetValue<String>('status', S) then
          UpdateResponseStatus(S);

        // Inicializar variables
        FLastContent := '';
        WebSearch := nil;
        ToolCalls := TObjectList<TAiToolsFunction>.Create; // Inicializar la lista

        // Procesar el array 'output'
        if jObj.TryGetValue<TJSonArray>('output', JOutput) then
        begin
          for i := 0 to JOutput.Count - 1 do
          begin
            JOutputItem := TJSonObject(JOutput.Items[i]);

            // Determinar el tipo de output
            if JOutputItem.TryGetValue<String>('type', S) then
            begin
              if S = 'message' then
              begin
                // Procesar el mensaje de texto (como antes)
                if JOutputItem.TryGetValue<TJSonArray>('content', JContent) then
                begin
                  if JContent.Count > 0 then
                  Begin
                    JText := TJSonObject(JContent.Items[0]);
                    if JText.TryGetValue<string>('text', S) then
                      FLastContent := S;

                    // Procesar 'annotations' (resultados de la búsqueda web)
                    if JText.TryGetValue<TJSonArray>('annotations', JAnnotations) then
                    begin
                      WebSearch := TAiWebSearch.Create;
                      WebSearch.&type := 'web_search_results'; // Puedes cambiarlo si necesitas
                      WebSearch.annotations := TAiWebSearchArray.Create;
                      WebSearch.text := FLastContent; // O usar otra fuente si lo prefieres

                      for j := 0 to JAnnotations.Count - 1 do
                      begin
                        JAnnotationItem := TJSonObject(JAnnotations.Items[j]);
                        WebSearchItem := TAiWebSearchItem.Create;
                        if JAnnotationItem.TryGetValue<string>('type', S) then
                          WebSearchItem.&type := S; // e.g., url_citation
                        JAnnotationItem.TryGetValue<Integer>('start_index', WebSearchItem.start_index);
                        JAnnotationItem.TryGetValue<Integer>('end_index', WebSearchItem.end_index);
                        JAnnotationItem.TryGetValue<string>('url', WebSearchItem.Url);
                        JAnnotationItem.TryGetValue<string>('title', WebSearchItem.title);

                        WebSearch.annotations.Add(WebSearchItem);
                      end;
                    end;
                  End;
                End;
              end
              else if S = 'function_call' then
              begin
                // Procesar la llamada a la función
                if JOutputItem.TryGetValue<String>('id', FunctionCallId) and JOutputItem.TryGetValue<String>('name', FunctionName) and
                  JOutputItem.TryGetValue<String>('arguments', FunctionArguments) then
                begin
                  // Aquí tienes la información para ejecutar la función
                  // FunctionCallId: Identificador único de la llamada a la función
                  // FunctionName: Nombre de la función a ejecutar
                  // FunctionArguments: Argumentos para la función (en formato JSON)

                  // Crear un objeto TAiToolsFunction para encapsular la información
                  ToolCall := TAiToolsFunction.Create;
                  ToolCall.Id := FunctionCallId; // Aquí debes asignar el 'id' del function_call
                  ToolCall.Name := FunctionName;
                  ToolCall.Arguments := FunctionArguments; // Asigna los argumentos como JSON string
                  ToolCalls.Add(ToolCall); // Agregar a la lista de ToolCalls
                  // Ejecutar la función
                  // DoCallFunction(ToolCall); // Se comentarea ya que se realiza multi hilo
                end;
              end
              else if S = 'image_generation_call' then
              begin

                Var
                ImageBase64 := '';
                Var
                ImageTranscript := '';
                Var
                ImageId := '';
                Var
                ImageStatus := '';
                Var
                ImageQuality := '';
                Var
                ImageOutputFormat := '';
                Var
                ImageSize := '';

                JOutputItem.TryGetValue<string>('id', ImageId);
                JOutputItem.TryGetValue<string>('status', ImageStatus);
                JOutputItem.TryGetValue<string>('output_format', ImageOutputFormat);
                JOutputItem.TryGetValue<string>('quality', ImageQuality);
                JOutputItem.TryGetValue<string>('result', ImageBase64);
                JOutputItem.TryGetValue<string>('revised_prompt', ImageTranscript);
                JOutputItem.TryGetValue<String>('size', ImageSize);

                // -----------------
                // Si hay datos de audio, creamos un MediaFile y lo adjuntamos.
                if not ImageBase64.IsEmpty then
                begin
                  Var
                  MediaFile := TAiMediaFile.Create;
                  MediaFile.LoadFromBase64('response_' + ImageId + '.' + ImageOutputFormat, ImageBase64);
                  MediaFile.Transcription := ImageTranscript;
                  // MediaFile.idAudio := idAudio;
                  ResMsg.AddMediaFile(MediaFile);
                end;
              end
              else if S = 'web_search_call' then
              begin
                // Podrías extraer información adicional sobre la llamada a la búsqueda web si es necesario
                // por ejemplo, el 'id' o 'status' de la llamada a la búsqueda.
              end;
            end;
          end;
        end;

        // Ejecución de funciones en paralelo
        if ToolCalls.Count > 0 then
        begin
          NumTasks := ToolCalls.Count;
          SetLength(TaskList, NumTasks);

          // Crear y ejecutar tareas para cada ToolCall
          for i := 0 to ToolCalls.Count - 1 do
          begin
            ToolCall := ToolCalls[i];

            ToolCall.ResMsg := ResMsg;
            ToolCall.AskMsg := AskMsg;

            TaskList[i] := TTask.Create(
              procedure
              begin
                DoCallFunction(ToolCall);
              end);
            TaskList[i].Start;
          end;

          // Esperar a que todas las tareas terminen
          TTask.WaitForAll(TaskList);

          // Después de que todas las funciones se hayan ejecutado, crear mensajes para cada ToolCall
          for i := 0 to ToolCalls.Count - 1 do
          begin
            ToolCall := ToolCalls[i];
            newMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
            newMsg.PreviousResponseId := FResponseId; // Asignar el FResponseId a la propiedad PreviousResponseId
            newMsg.Id := Self.Messages.Count + 1;
            Self.Messages.Add(newMsg);
          end;

          // Enviar todas las respuestas al LLM
          Self.Run(Nil, ResMsg);
        end
        else
        begin
          // Crear un nuevo TAiChatMessage para guardar la respuesta localmente
          // newMsg := TAiChatMessage.Create(FLastContent, 'assistant'); // El rol es 'assistant' según el JSON
          // newMsg.Id := Self.Messages.Count + 1;
          // newMsg.PreviousResponseId := FResponseId; // Asignar el FResponseId a la propiedad PreviousResponseId

          // Asignar la búsqueda web al mensaje (si hubo resultados)
          ResMsg.WebSearchResponse := WebSearch;
          ResMsg.Prompt := FLastContent;

          // Self.Messages.Add(newMsg);

          If tfc_textFile in NativeOutputFiles then
          Begin
            Code := TMarkdownCodeExtractor.Create;
            Try

              CodeFiles := Code.ExtractCodeFiles(ResMsg.Prompt);
              For CodeFile in CodeFiles do
              Begin
                St := TStringStream.Create(CodeFile.Code);
                Try
                  St.Position := 0;

                  MF := TAiMediaFile.Create;
                  MF.LoadFromStream('file.' + CodeFile.FileType, St);
                  ResMsg.MediaFiles.Add(MF);
                Finally
                  St.Free;
                End;

              End;
            Finally
              Code.Free;
            End;
          End;

          DoProcessResponse(AskMsg, ResMsg, FLastContent);

          ResMsg.Prompt := FLastContent;

          // Si no hay llamadas a funciones, ejecutar el código normal
          If Assigned(FOnReceiveDataEnd) then
            FOnReceiveDataEnd(Self, ResMsg, jObj, 'assistant', FLastContent); // Respuesta);
        end;

      except
        ON E: Exception do
        Begin

          // Liberar WebSearch si hubo un error en el parseo de las annotations
          if (Assigned(WebSearch) and Assigned(WebSearch.annotations)) then
          Begin
            for i := 0 to WebSearch.annotations.Count - 1 do
              WebSearch.annotations[i].Free;

            WebSearch.annotations.Free;
          end;

          WebSearch.Free;

          Raise Exception.Create('Error al parsear la información del Json: ' + E.Message);
        End;
      end;
    finally
      // Liberar la lista de ToolCalls
      if Assigned(ToolCalls) then
      begin
        for i := 0 to ToolCalls.Count - 1 do
          ToolCalls[i].Free; // Liberar cada ToolCall en la lista
        ToolCalls.Free;
      end;
    end;
  end;
end;

procedure TAiOpenAiResponses.ParseJsonTranscript(jObj: TJSonObject; ResMsg: TAiChatMessage; aMediaFile: TAiMediaFile);
var
  // JSON parsing
  jUsage, jInputTokenDetails: TJSonObject;
  jArrWords, jArrSegments: TJSonArray;

  // Datos extraídos
  sTextoTranscrito, sTextoWords, sTextoSegments: String;
  aTotal_tokens, aInput_tokens, aOutput_tokens: Integer;
  aText_tokens, aAudio_tokens: Integer; // Tokens detallados del input

begin
  sTextoTranscrito := '';
  aTotal_tokens := 0;
  aInput_tokens := 0;
  aOutput_tokens := 0;
  aText_tokens := 0;
  aAudio_tokens := 0;

  if not jObj.TryGetValue<string>('text', sTextoTranscrito) then
  begin
    FLastError := 'La respuesta de la API no contiene el campo "text" con la transcripción.';
    DoError(FLastError, nil);
    FBusy := False; // Asumiendo que usas FBusy como en ParseChat
    Exit;
  end;

  If jObj.TryGetValue<TJSonArray>('words', jArrWords) then
    sTextoWords := jArrWords.Format;

  If jObj.TryGetValue<TJSonArray>('segments', jArrSegments) then
    sTextoSegments := jArrSegments.Format;

  // --- 3. EXTRACCIÓN DE DATOS DE USO (TOKENS) ---
  // El objeto 'usage' podría no venir, así que lo manejamos de forma segura.
  if jObj.TryGetValue<TJSonObject>('usage', jUsage) then
  begin
    // Extraemos los tokens principales
    jUsage.TryGetValue<Integer>('total_tokens', aTotal_tokens);
    jUsage.TryGetValue<Integer>('input_tokens', aInput_tokens); // Costo del audio (y prompt si hubo)
    jUsage.TryGetValue<Integer>('output_tokens', aOutput_tokens); // Costo del texto generado

    // Extraemos los detalles de los tokens de entrada (sub-objeto)
    // Esto nos dice cuántos tokens correspondieron al audio y cuántos a un posible prompt de texto.
    if jUsage.TryGetValue<TJSonObject>('input_token_details', jInputTokenDetails) then
    begin
      jInputTokenDetails.TryGetValue<Integer>('text_tokens', aText_tokens);
      jInputTokenDetails.TryGetValue<Integer>('audio_tokens', aAudio_tokens);
    end;
  end;

  // --- 4. ACTUALIZACIÓN DEL ESTADO DEL COMPONENTE ---
  // Actualizamos los contadores de tokens globales, sumando los de esta llamada.
  Self.Total_tokens := Self.Total_tokens + aTotal_tokens;
  Self.Prompt_tokens := Self.Prompt_tokens + aInput_tokens; // 'input' equivale a 'prompt'
  Self.Completion_tokens := Self.Completion_tokens + aOutput_tokens; // 'output' equivale a 'completion'

  // Guardamos el resultado principal
  // Self.FLastContent := sTextoTranscrito;

  If Trim(sTextoWords + sLineBreak + sTextoSegments) <> '' then
  Begin
    aMediaFile.Transcription := Trim(sTextoWords + sLineBreak + sTextoSegments);
    aMediaFile.Detail := Trim(sTextoTranscrito);
    ResMsg.Prompt := Trim(ResMsg.Prompt + aMediaFile.Transcription);
    ResMsg.Content := ResMsg.Content + sLineBreak + aMediaFile.Detail;
  End
  Else
  Begin
    aMediaFile.Transcription := sTextoTranscrito;
    ResMsg.Prompt := Trim(ResMsg.Prompt + sLineBreak + sTextoTranscrito);
    ResMsg.Content := Trim(ResMsg.Content + sLineBreak + sTextoTranscrito);
  End;

  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aInput_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aOutput_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;

end;

procedure TAiOpenAiResponses.DoCallFunction(ToolCall: TAiToolsFunction);
Var
  LFuncion: TFunctionActionItem;
  Handle: Boolean;
begin
  If Not Assigned(AiFunctions) then
    Exit;

  LFuncion := AiFunctions.Functions.GetFunction(ToolCall.Name);

  If Assigned(LFuncion) then
  Begin
    LFuncion.OnAction(Self, LFuncion, ToolCall.Name, ToolCall, Handle);
    If Handle = False then
    Begin
      // Si el evento OnAction no manejó la llamada, llama al evento FOnCallToolFunction
      If Assigned(FOnCallToolFunction) then
        FOnCallToolFunction(Self, ToolCall);
    End;
  End
  Else
  Begin
    // Si no se encuentra la función, llama al evento FOnCallToolFunction
    If Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall);
  End;
end;

// Actualiza el Status de la respuesta
procedure TAiOpenAiResponses.UpdateResponseStatus(aStatus: String);
begin
  FResponseStatus := aStatus;
end;

function TAiOpenAiResponses.GetToolsItems(Functions: TFunctionActionItems): TJSonArray;
var
  i: Integer;
  FunctionAction: TFunctionActionItem;
  JToolObject, JParamsObject, JPropertiesObject: TJSonObject;
  JRequiredArray: TJSonArray;
  ParamItem: TFunctionParamsItem;
begin
  Result := TJSonArray.Create;
  if not Assigned(Functions) then
    Exit;

  for i := 0 to Functions.Count - 1 do
  begin
    FunctionAction := Functions.Items[i];

    // Crear el objeto principal de la herramienta
    JToolObject := TJSonObject.Create;
    JToolObject.AddPair('type', 'function');
    JToolObject.AddPair('name', FunctionAction.FunctionName);
    JToolObject.AddPair('description', FunctionAction.Description.text);

    // Crear el objeto parameters
    JParamsObject := TJSonObject.Create;
    JParamsObject.AddPair('type', 'object');

    // Crear el objeto properties
    JPropertiesObject := TJSonObject.Create;

    // Crear el array required
    JRequiredArray := TJSonArray.Create;

    // Iterar sobre los parámetros de la función
    for var j := 0 to FunctionAction.Parameters.Count - 1 do
    begin
      ParamItem := TFunctionParamsItem(FunctionAction.Parameters.Items[j]);
      var
      JPropertyObject := TJSonObject.Create;

      case ParamItem.ParamType of
        ptString:
          JPropertyObject.AddPair('type', 'string');
        ptInteger:
          JPropertyObject.AddPair('type', 'integer');
        ptBoolean:
          JPropertyObject.AddPair('type', 'boolean');
        ptFloat:
          JPropertyObject.AddPair('type', 'number'); // JSON doesn't distinguish between float and double
        ptDate, ptTime, ptDateTime:
          JPropertyObject.AddPair('type', 'string'); // Represent dates and times as strings in JSON
        ptBase64:
          JPropertyObject.AddPair('type', 'string'); // Base64 is a string
      end;

      if ParamItem.Description.text <> '' then
        JPropertyObject.AddPair('description', ParamItem.Description.text);

      if ParamItem.Enum <> '' then
      begin
        var
        JEnumArray := TJSonArray.Create;
        var
          EnumValues: TArray<string>;
        EnumValues := ParamItem.Enum.Split([','], 0); // Split the string into an array of strings

        for var k := Low(EnumValues) to High(EnumValues) do
          JEnumArray.Add(Trim(EnumValues[k])); // Add the trimmed enum value to the array

        JPropertyObject.AddPair('enum', JEnumArray);
      end;

      JPropertiesObject.AddPair(ParamItem.Name, JPropertyObject); // Add the property to the properties object

      if ParamItem.Required then
        JRequiredArray.Add(ParamItem.Name);
    end;

    JParamsObject.AddPair('properties', JPropertiesObject);
    JParamsObject.AddPair('required', JRequiredArray);

    JToolObject.AddPair('parameters', JParamsObject);

    // Agregar el objeto de la herramienta al array de resultados
    Result.Add(JToolObject);
  end;
end;

function TAiOpenAiResponses.GetTools(Funcion: TAiFunctions): TJSonArray;
begin
  if Assigned(Funcion) then
    Result := GetToolsItems(Funcion.Functions)
  else
    Result := TJSonArray.Create;
end;

function TAiOpenAiResponses.InitChatCompletions: String;
var
  JResult, JReasoning: TJSonObject;
  JInputArray, JContentArray: TJSonArray;
  JUserObject, JTextObject, JImageObject, JDocumentObject: TJSonObject;
  MediaFile: TAiMediaFile;
  LastMessage: TAiChatMessage;
  PenultimateMessage: TAiChatMessage;

  JToolsArray, jToolCalls: TJSonArray;
  JToolObject, jToolCall: TJSonObject;
  i: Integer; // Contador para iterar sobre MediaFiles
  MediaArr: TAiMediaFilesArray;
  LModel: String;

begin
  JResult := TJSonObject.Create;

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  try
    // Model - requerido
    JResult.AddPair('model', LModel);

    // Determinar si enviar input o previous_response_id
    if FMessages.Count > 0 then
      LastMessage := FMessages[FMessages.Count - 1]
    else
      LastMessage := Nil;

    if FMessages.Count > 1 then
      PenultimateMessage := FMessages[FMessages.Count - 2]
    else
      PenultimateMessage := Nil;

    // Verificar si hay que usar previous_response_id
    if Assigned(PenultimateMessage) and (Trim(PenultimateMessage.PreviousResponseId) <> '') then
      JResult.AddPair('previous_response_id', PenultimateMessage.PreviousResponseId);

    // Preparar el mensaje actual (input)
    if Assigned(LastMessage) then
    begin

      If LastMessage.TollCallId <> '' then
      Begin

        // JResult.AddPair('previous_response_id', LastMessage.PreviousResponseId);

        jToolCalls := TJSonArray.Create;


        // ------------------ Type Function_Call ------------------
        { jToolCall := TJSonObject.Create;
          jToolCall.AddPair('type', 'function_call');
          jToolCall.AddPair('id', LastMessage.TollCallId); //es el que comienza con rc_  toolcallid
          jToolCall.AddPair('call_id', 'call_001'); //LastMessage.TollCallId);
          jToolCall.AddPair('name', LastMessage.FunctionName);
          jToolCall.AddPair('arguments', 'NA');
          jToolCalls.Add(jToolCall);
        }

        // ------------------ Type Function_Call_Output ------------------
        jToolCall := TJSonObject.Create;
        jToolCall.AddPair('type', 'function_call_output');
        jToolCall.AddPair('id', LastMessage.TollCallId);
        jToolCall.AddPair('call_id', LastMessage.TollCallId);
        jToolCall.AddPair('output', 'hoy es 5 de abril del 2025'); // LastMessage.Prompt);
        jToolCalls.Add(jToolCall);

        JResult.AddPair('input', jToolCalls);
      End
      Else
      Begin

        // Filtra solo los archivos de medios que puede manejar y están en NativeInputFiles
        MediaArr := LastMessage.MediaFiles.GetMediaList(NativeInputFiles, False);

        // Para mensajes con documentos, el input debe ser un array (si hay al menos un archivo multimedia)
        // Aquí se procesan los MediaFile de Entrada más abajo se procesan los medios de salida (Generación)
        if Length(MediaArr) > 0 then
        begin
          // Para mensajes con imágenes, el input debe ser un array
          JInputArray := TJSonArray.Create;

          // Crear el objeto user
          JUserObject := TJSonObject.Create;
          JUserObject.AddPair('role', 'user');

          // Crear el array content
          JContentArray := TJSonArray.Create;

          // Añadir el texto si existe
          if Trim(LastMessage.Prompt) <> '' then
          begin
            JTextObject := TJSonObject.Create;
            JTextObject.AddPair('type', 'input_text');
            JTextObject.AddPair('text', LastMessage.Prompt);
            JContentArray.Add(JTextObject);
          end;

          // Añadir los archivos multimedia
          for i := 0 to Length(MediaArr) - 1 do
          begin
            MediaFile := MediaArr[i];

            case MediaFile.FileCategory of
              Tfc_Image:
                begin
                  JImageObject := TJSonObject.Create;
                  JImageObject.AddPair('type', 'input_image');

                  // Asignar directamente la URL o el base64 como cadena, no como objeto
                  if MediaFile.UrlMedia <> '' then
                    JImageObject.AddPair('image_url', MediaFile.UrlMedia)
                  else
                    JImageObject.AddPair('image_url', 'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);

                  // Si tiene la propiedad Detail, la agregamos
                  if MediaFile.Detail <> '' then
                    JImageObject.AddPair('detail', MediaFile.Detail);

                  JContentArray.Add(JImageObject);

                end;
              Tfc_Audio:
                begin

                  // Responses todavía no soporta Audio de forma nativa en el chat, hay que utilizar las transcriptions
                  {
                    JAudioObject := TJSonObject.Create;
                    JInputAudioObject := TJSonObject.Create;

                    if MediaFile.IdAudio <> '' then // Si es una respuesta del modelo va esto
                    begin
                    JAudioObject.AddPair('id', MediaFile.IdAudio);
                    JInputAudioObject.AddPair('audio', JAudioObject);
                    JContentArray.Add(JInputAudioObject); // ojo esto puede estar malo
                    end
                    else // Si es un audio del usuario va esto
                    begin
                    JAudioObject.AddPair('data', MediaFile.Base64);
                    JAudioObject.AddPair('format', StringReplace(MediaFile.MimeType, 'audio/', '', [rfReplaceAll]));

                    JInputAudioObject.AddPair('input_audio', JAudioObject);
                    JInputAudioObject.AddPair('type', 'input_audio');
                    JContentArray.Add(JInputAudioObject);
                    end;
                  }
                end;
              Tfc_Video:
                begin
                  // TODO: Implementar soporte para video
                end;
              Tfc_Pdf:
                begin
                  JDocumentObject := TJSonObject.Create;
                  JDocumentObject.AddPair('type', 'input_file');
                  JDocumentObject.AddPair('filename', MediaFile.FileName);

                  // Por facilidad no manejamos el ID del documento solo subimos el Base64
                  // Podría adicionarse también el manejo de archivos por los vectores de openAi
                  // Asignar directamente el base64 como cadena, no como objeto
                  if MediaFile.UrlMedia <> '' then
                    Raise Exception.Create('En documentos no se maneja la url') // JDocumentObject.AddPair('image_url', MediaFile.UrlMedia)
                  else
                    JDocumentObject.AddPair('file_data', 'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);

                  // Si tiene la propiedad Detail, la agregamos
                  if MediaFile.Detail <> '' then
                    JDocumentObject.AddPair('detail', MediaFile.Detail);

                  JContentArray.Add(JDocumentObject);
                end;
              Tfc_Document:
                Begin

                End;

              TAiFileCategory.Tfc_Text:
                Begin
                end;
              TAiFileCategory.Tfc_CalcSheet:
                Begin
                end;
              TAiFileCategory.Tfc_Presentation:
                Begin
                end;
              TAiFileCategory.Tfc_CompressFile:
                Begin
                end;
              TAiFileCategory.Tfc_Web:
                Begin
                end;
              TAiFileCategory.Tfc_GraphicDesign:
                Begin
                end;
              TAiFileCategory.Tfc_Unknow:
                Begin
                end;

            else
              begin
                // Otros tipos de archivo no soportados por ahora, simplemente los ignoramos
              end;
            end;
          end;

          // Añadir content al objeto user
          JUserObject.AddPair('content', JContentArray);

          // Añadir el objeto user al array input
          JInputArray.Add(JUserObject);

          // Añadir el array input al objeto resultado
          JResult.AddPair('input', JInputArray);
        end
        else
        begin
          // Para mensajes de texto simple, el input puede ser string
          JResult.AddPair('input', LastMessage.Prompt);
        end;
      End;
    end
    else
    begin
      JResult.AddPair('input', ''); // o algún valor por defecto
    end;

    // Store, parallel_tool_calls, truncation
    JResult.AddPair('store', FStore);
    JResult.AddPair('parallel_tool_calls', FParallel_ToolCalls);
    JResult.AddPair('truncation', FTruncation);

    // Reasoning
    if Self.FReasoningEffort <> '' then
    begin
      JReasoning := TJSonObject.Create;
      JReasoning.AddPair('effort', Self.FReasoningEffort);
      JResult.AddPair('reasoning', JReasoning);
    end;


    // Funciones para la generación de medios de salida, audio (Todavía no está disponible para Responses pero lo estará

    if (Tfc_Audio in NativeOutputFiles) then
    begin
      // Escenario: Chat con respuesta de AUDIO CONTEXTUAL
      var
      jModalities := TJSonArray.Create;
      jModalities.Add('text');
      jModalities.Add('audio');
      // JResult.AddPair('modalities', jModalities);

      Var
      jAudio := TJSonObject.Create;
      jAudio.AddPair('voice', Voice);
      jAudio.AddPair('format', voice_format);
      JResult.AddPair('audio', jAudio);
    end;



    // Agregar tools si hay

    if Tool_active then
    Begin
      if Assigned(AiFunctions) then
      begin
        var
        JTools := GetTools(AiFunctions);
        if Assigned(JTools) then
        begin
          JResult.AddPair('tools', JTools);

          if (Trim(Tool_choice) <> '') then
          begin
            var
            JToolChoice := TJSonObject(TJSonObject.ParseJSONValue(Tool_choice));
            if Assigned(JToolChoice) then
            begin
              JResult.AddPair('tool_choice', JToolChoice);
            end;
          end
          else
          begin
            JResult.AddPair('tool_choice', 'auto'); // Por defecto
          end;
        end;
      end;
    End
    Else if (Tcm_Image in ChatMediaSupports) and (Tfc_Image in NativeOutputFiles) then // Si el modelo maneja la búsqueda  web
    begin
      JToolsArray := TJSonArray.Create;
      JToolObject := TJSonObject.Create;
      JToolObject.AddPair('type', 'image_generation');
      JToolsArray.Add(JToolObject);
      JResult.AddPair('tools', JToolsArray);
    end
    Else if tcm_WebSearch in ChatMediaSupports then // Si el modelo maneja la búsqueda  web
    begin
      JToolsArray := TJSonArray.Create;
      JToolObject := TJSonObject.Create;
      JToolObject.AddPair('type', 'web_search_preview');
      JToolsArray.Add(JToolObject);

      If tcm_code_Interpreter in ChatMediaSupports then
      Begin
        JToolObject := TJSonObject.Create;
        var
        jContainer := TJSonObject.Create;
        jContainer.AddPair('type', 'auto');;
        JToolObject.AddPair('container', jContainer);
        JToolObject.AddPair('type', 'code_interpreter');
        JToolsArray.Add(JToolObject);
      End;

      JResult.AddPair('tools', JToolsArray);
    end
    Else if (tcm_code_interpreter in ChatMediaSupports) then // Si el modelo maneja la búsqueda  web
    begin
      JToolsArray := TJSonArray.Create;

      JToolObject := TJSonObject.Create;
      var
      jContainer := TJSonObject.Create;
      jContainer.AddPair('type', 'auto');;
      JToolObject.AddPair('container', jContainer);
      JToolObject.AddPair('type', 'code_interpreter');
      JToolsArray.Add(JToolObject);
      JResult.AddPair('tools', JToolsArray);
    end;

    Result := JResult.ToString;
  finally
    JResult.Free;
  end;
end;

function TAiOpenAiResponses.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FResponseId := '';
  FResponseStatus := '';

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'responses'; // Endpoint de la nueva API

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    { If Assigned(aMsg) then  //Esto se hace en el run
      Begin
      // Añadimos el objeto a la lista antes de enviarlo a OPENAI
      Self.Messages.Add(aMsg);
      End;
    }

    // Construir el request body (usando el método específico de la API responses)
    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;

//$IFDEF APIDEBUG
    St.SaveToFile('c:\temp\peticion_responses.txt'); // Para Debug
    St.Position := 0;
//$ENDIF
    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FResponse.Position := 0;
{$IFDEF APIDEBUG}
    FResponse.SaveToFile('c:\temp\respuesta_responses.txt'); // Para Debug
    FResponse.Position := 0;
{$ENDIF}
    FLastContent := '';

    If FClient.Asynchronous = False then // If Self.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(jObj, ResMsg); // Usa el método específico para la API responses
          Result := FLastContent;

        Finally
          FreeAndNil(jObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free; // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

function TAiOpenAiResponses.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModel, LVoice, LResponseFormat: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders; // <--- Variable ya declarada, ahora la usaremos
  LResponse: IHTTPResponse;
  LJsonObject: TJSonObject;
  LErrorResponse: string;
  LNewAudioFile: TAiMediaFile;
begin
  Result := ''; // La función Run devuelve el texto, que en este caso es vacío.
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Añadir el mensaje del usuario al historial para mantener la consistencia
  if FMessages.IndexOf(AskMsg) < 0 then // Solo lo añadimos si no está ya en la lista
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  // 2. Preparar parámetros para la API de TTS
  LUrl := Url + 'audio/speech';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LVoice := Self.Voice; // Usamos la propiedad del componente
  LResponseFormat := Self.voice_format; // Usamos la propiedad del componente

  // 3. Construir y ejecutar la petición
  LJsonObject := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := TMemoryStream.Create;
  try
    LJsonObject.AddPair('model', LModel);
    LJsonObject.AddPair('input', AskMsg.Prompt);
    LJsonObject.AddPair('voice', LVoice);
    LJsonObject.AddPair('response_format', LResponseFormat);

    LBodyStream := TStringStream.Create(UTF8ToString(LJsonObject.Format), TEncoding.UTF8);
    // LBodyStream := TStringStream.Create(LJsonObject.ToString, TEncoding.UTF8);

    // S := LJsonObject.ToString;

    // LBodyStream := TStringStream.Create;
    // LBodyStream.WriteString(S);

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\peticionAudio.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    // 4. Procesar la respuesta
    if LResponse.StatusCode = 200 then
    begin
      // ... (el resto del código de procesamiento de la respuesta es correcto)
      LNewAudioFile := TAiMediaFile.Create;
      try
        LResponseStream.Position := 0;
        LNewAudioFile.LoadFromStream('generated_audio.' + LResponseFormat, LResponseStream);
        // LResponseMsg := TAiChatMessage.Create('', 'model');

        ResMsg.MediaFiles.Add(LNewAudioFile);
        // ResMsg.Id := FMessages.Count + 1; //Lo adiciona en el run al finalizar el proceso
        // FMessages.Add(ResMsg);

        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');
      except
        LNewAudioFile.Free;
        raise;
      end;
    end
    else
    begin
      // ... (el resto del código de manejo de errores es correcto)
      LResponseStream.Position := 0;
      LErrorResponse := TStreamReader.Create(LResponseStream).ReadToEnd;
      FLastError := Format('Error generando audio: %d, %s', [LResponse.StatusCode, LErrorResponse]);
      DoError(FLastError, nil);
    end;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;

  FBusy := False;
end;

function TAiOpenAiResponses.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList; // Para procesar las granularidades
  i: Integer;
  LModel: String;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripción.');

  sUrl := Url + 'audio/transcriptions';

  Client := THTTPClient.Create;
  LResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  Granularities := TStringList.Create;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Crear un stream temporal para pasarlo al formulario multipart
    LTempStream := TMemoryStream.Create;
    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

    // --- 1. CONSTRUCCIÓN DEL BODY MULTIPART CON PARÁMETROS GENÉRICOS ---
    Body.AddStream('file', LTempStream, aMediaFile.FileName, aMediaFile.MimeType);

    Body.AddField('model', LModel); // Default seguro

    if not AskMsg.Prompt.IsEmpty then
      Body.AddField('prompt', AskMsg.Prompt);

    // Formato de respuesta (genérico, como string)
    if not Self.Transcription_ResponseFormat.IsEmpty then
      Body.AddField('response_format', Self.Transcription_ResponseFormat)
    else
      Body.AddField('response_format', 'json'); // Default a JSON si no se especifica

    // Parámetros opcionales
    if not Self.Language.IsEmpty then
      Body.AddField('language', Self.Language);

    // Usamos la propiedad de Temperatura existente en el componente
    if Self.Temperature > 0 then
      Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

    // Timestamps Granularities (procesamos la cadena)
    if not Self.Transcription_TimestampGranularities.IsEmpty then
    begin
      // Dividimos la cadena por comas
      Granularities.CommaText := Self.Transcription_TimestampGranularities;
      for i := 0 to Granularities.Count - 1 do
      begin
        // Añadimos cada granularidad como un campo separado con '[]'
        Body.AddField('timestamp_granularities[]', Trim(Granularities[i]));
      end;
    end;

    { TODO : TODAVÍA NO ESTÁ LISTO PARA UTILIZAR LA TRANSCRIPCIÓN EN MODO ASCINCRÓNICO, Falta implementar a futuro }
    // Streaming
    // if Self.Asynchronous then
    // Body.AddField('stream', 'true');

    // --- 2. EJECUCIÓN DE LA PETICIÓN POST ---

    // (La lógica de streaming/síncrono se mantiene igual)
    { if Self.Asynchronous then
      begin
      FClient.Asynchronous := True;
      // La lógica de OnReceiveData se encargará del resto
      FClient.Post(sUrl, Body, FResponse, Headers);
      Result := '';
      end
      else
    }
    begin
      Res := Client.Post(sUrl, Body, LResponseStream, Headers);

      if Res.StatusCode = 200 then
      begin

        LResponseObj := TJSonObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;

        If Not Assigned(LResponseObj) then
        Begin
          LResponseObj := TJSonObject.Create(TJSonPair.Create('text', Res.ContentAsString));
        End;

        try
          // Aquí llamas al procedimiento de parseo de transcripciones
          ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
        finally
          LResponseObj.Free;
        end;

        Result := ResMsg.Prompt;
      end
      else
      begin
        Raise Exception.CreateFmt('Error en la transcripción: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    end;

  finally
    Body.Free;
    Client.Free;
    LResponseStream.Free;
    // Granularities.Free;
    // LTempStream es propiedad de Body
  end;
end;

procedure TAiOpenAiResponses.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  inherited;
  // TODO: AQUI Implementar el soporte para el stream, no estoy muy seguro de como se haria
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOpenAiResponses);

end.
