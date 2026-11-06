program ChatbotDocumentacion;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.RAG.Vectors,
  uMakerAi.Embeddings;

const
  INDEX_FILE  = 'kb\index.bin';
  DOCS_FOLDER = 'documentos\';

var
  AiConn  : TAiChatConnection;
  RAG     : TAiRAGVector;
  Emb     : TAiEmbeddings;
  Pregunta: string;

procedure IndexarDocumentos(const Carpeta: string);
var
  Archivos: TArray<string>;
  Archivo : string;
  Count   : Integer;
begin
  Archivos := TDirectory.GetFiles(Carpeta, '*.txt');
  WriteLn('Indexando ', Length(Archivos), ' documentos...');
  for Archivo in Archivos do
  begin
    Count := RAG.AddItemsFromPlainText(
      TFile.ReadAllText(Archivo),
      nil,   // metadata
      500,   // chunk size (caracteres)
      10     // overlap %
    );
    WriteLn('  + ', ExtractFileName(Archivo), ' -> ', Count, ' chunks');
  end;
  RAG.BuildIndex;
  RAG.SaveToFile(INDEX_FILE);
  WriteLn('Indexacion completa: ', RAG.Count, ' fragmentos totales.');
end;

function ObtenerContexto(const Pregunta: string): string;
begin
  // SearchText devuelve los fragmentos mas similares como string formateado
  Result := RAG.SearchText(Pregunta, 4, 0.6, nil, False, False);
end;

procedure CrearDocumentosEjemplo;
begin
  ForceDirectories(DOCS_FOLDER);
  TFile.WriteAllText(DOCS_FOLDER + 'politicas.txt',
    'POLITICA DE DEVOLUCIONES' + sLineBreak +
    'Los clientes pueden devolver productos dentro de los 30 dias desde la compra.' + sLineBreak +
    'Se requiere el ticket de compra original.' + sLineBreak +
    'Los productos electronicos tienen 15 dias de garantia.' + sLineBreak +
    sLineBreak +
    'POLITICA DE ENVIO' + sLineBreak +
    'Envio gratuito en compras mayores a $50.' + sLineBreak +
    'Tiempo estimado de entrega: 3 a 5 dias habiles.' + sLineBreak +
    'Las devoluciones tienen costo de envio a cargo del cliente.'
  );
  TFile.WriteAllText(DOCS_FOLDER + 'productos.txt',
    'CATALOGO DE PRODUCTOS 2025' + sLineBreak +
    'Laptop Pro X: $1200 — procesador i9, 32GB RAM, SSD 1TB.' + sLineBreak +
    'Laptop Basica: $450 — procesador i5, 16GB RAM, SSD 512GB.' + sLineBreak +
    'Monitor 4K 27": $350 — panel IPS, 144Hz, HDR.' + sLineBreak +
    'Teclado Mecanico: $80 — switches Cherry MX Blue.' + sLineBreak +
    sLineBreak +
    'Todos los productos incluyen garantia de 1 año.'
  );
  WriteLn('Documentos de ejemplo creados en: ', DOCS_FOLDER);
end;

begin
  WriteLn('=================================================');
  WriteLn('  Cap. 22 - RAG Vectorial: Chatbot Demo');
  WriteLn('=================================================');
  WriteLn;

  Emb    := TAiEmbeddings.Create(nil);
  RAG    := TAiRAGVector.Create(nil);
  AiConn := TAiChatConnection.Create(nil);
  try
    // Configurar embedder
    Emb.ApiKey     := '@OPENAI_API_KEY';
    RAG.Embeddings := Emb;

    // Crear o cargar indice
    if TFile.Exists(INDEX_FILE) then
    begin
      RAG.LoadFromFile(INDEX_FILE);
      WriteLn('Indice cargado: ', RAG.Count, ' fragmentos.');
    end
    else
    begin
      // Crear documentos de ejemplo si no existen
      if not TDirectory.Exists(DOCS_FOLDER) then
        CrearDocumentosEjemplo;

      ForceDirectories('kb');
      IndexarDocumentos(DOCS_FOLDER);
    end;

    // Configurar LLM (Groq llama-3.3-70b — rapido y gratuito en tier)
    AiConn.DriverName                    := 'Groq';
    AiConn.Params.Values['ApiKey']       := '@GROQ_API_KEY';
    AiConn.Model                          := 'llama-3.3-70b-versatile';
    AiConn.Params.Values['Asynchronous'] := 'False';

    WriteLn;
    WriteLn('Chatbot RAG listo. Preguntas de ejemplo:');
    WriteLn('  "?Cual es el plazo para devolver un producto?"');
    WriteLn('  "?Cuanto cuesta la Laptop Pro X?"');
    WriteLn('  "?Hay envio gratis?"');
    WriteLn('Escribe "salir" para terminar.');
    WriteLn;

    repeat
      Write('Tu > ');
      ReadLn(Pregunta);
      if SameText(Trim(Pregunta), 'salir') then Break;
      if Trim(Pregunta) = '' then Continue;

      // 1. Recuperar fragmentos relevantes del indice
      var Contexto := ObtenerContexto(Pregunta);

      if Contexto = '' then
      begin
        WriteLn('IA  > No encontre informacion relevante en la base de conocimiento.');
        WriteLn;
        Continue;
      end;

      // 2. Configurar system prompt con el contexto recuperado
      AiConn.NewChat;
      AiConn.SystemPrompt.Text :=
        'Eres un asistente que responde preguntas basandote SOLO en la ' +
        'documentacion de la empresa proporcionada. ' +
        'Si la respuesta no esta en el contexto, di: "No tengo informacion sobre eso."' +
        sLineBreak + sLineBreak +
        'DOCUMENTACION:' + sLineBreak + Contexto;

      // 3. Obtener respuesta del LLM
      var Respuesta := AiConn.AddMessageAndRun(Pregunta, 'user', []);
      WriteLn('IA  > ', Respuesta);
      WriteLn;

    until False;

  finally
    AiConn.Free;
    RAG.Free;
    Emb.Free;
  end;
end.
