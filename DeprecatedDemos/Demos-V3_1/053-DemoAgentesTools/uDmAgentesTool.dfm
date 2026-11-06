object FDataModule: TFDataModule
  Height = 415
  Width = 672
  object AiGeminiChat1: TAiGeminiChat
    ApiKey = '@GEMINI_API_KEY'
    Model = 'gemini-3-pro'
    Max_tokens = 3000
    Response_format = tiaChatRfText
    Asynchronous = False
    K = 0
    Tool_choice = 'auto'
    Tool_Active = False
    User = 'user'
    SystemPrompt.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Completion_tokens = 0
    Url = 'https://generativelanguage.googleapis.com/v1beta/'
    ResponseTimeOut = 60000
    Stream_Usage = False
    ThinkingLevel = tlLow
    MediaResolution = mrDefault
    Left = 496
    Top = 88
  end
  object AiOpenChat1: TAiOpenChat
    ApiKey = '@OPENAI_API_KEY'
    Model = 'gpt-5'
    Max_tokens = 3000
    Response_format = tiaChatRfText
    Asynchronous = False
    K = 0
    Tool_choice = 'auto'
    Tool_Active = False
    User = 'user'
    SystemPrompt.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Completion_tokens = 0
    Url = 'https://api.openai.com/v1/'
    ResponseTimeOut = 60000
    Stream_Usage = False
    ThinkingLevel = tlDefault
    MediaResolution = mrDefault
    Truncation = 'disabled'
    Left = 552
    Top = 152
  end
  object AiRAGVector1: TAiRAGVector
    Embeddings = AiOllamaEmbeddings1
    LexicalLanguage = alSpanish
    Driver = VectorRAGDriver
    OwnsObjects = True
    SearchOptions.UseBM25 = False
    SearchOptions.BM25Weight = 0.700000000000000000
    SearchOptions.EmbeddingWeight = 0.300000000000000000
    Left = 104
    Top = 152
  end
  object AiOllamaEmbeddings1: TAiOllamaEmbeddings
    Model = 'snowflake-arctic-embed'
    Dimensions = 1024
    ApiKey = '@OLLAMA_API_KEY'
    Url = 'http://localhost:11434/'
    Left = 352
    Top = 168
  end
  object VectorRAGDriver: TAiRAGVectorPostgresDriver
    Connection = FDConnection1
    TableName = 'taller2vrag'
    CurrentEntidad = 'DEFAULT'
    Left = 240
    Top = 104
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=lunaidb'
      'User_Name=postgres'
      'Password=masterkey'
      'Server=localhost'
      'DriverID=PG')
    Connected = True
    LoginPrompt = False
    Left = 360
    Top = 64
  end
  object AiOllamaPdfTool1: TAiOllamaOcrTool
    Url = 'http://localhost:11434/'
    Model = 'deepseek-ocr'
    Prompt = 'extract all'
    GhostscriptPath = 'gswin64c.exe'
    TimeOut = 0
    Left = 480
    Top = 208
  end
  object NanoBanana: TAiGeminiChat
    ApiKey = '@GEMINI_API_KEY'
    Model = 'gemini-3-pro-image-preview'
    Max_tokens = 8000
    Response_format = tiaChatRfText
    Asynchronous = False
    K = 0
    Tool_choice = 'auto'
    Tool_Active = False
    User = 'user'
    SystemPrompt.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Completion_tokens = 0
    Url = 'https://generativelanguage.googleapis.com/v1beta/'
    ResponseTimeOut = 120000
    Stream_Usage = False
    ThinkingLevel = tlDefault
    MediaResolution = mrDefault
    Left = 360
    Top = 240
  end
  object OllamaHtml: TAiOllamaChat
    ApiKey = '@OLLAMA_API_KEY'
    Model = 'gpt-oss:20b'
    Max_tokens = 8000
    Response_format = tiaChatRfText
    Asynchronous = False
    K = 0
    Tool_choice = 'auto'
    Tool_Active = False
    User = 'user'
    SystemPrompt.Strings = (
      
        'Eres un asistente para la creaci'#243'n de Dashboards en html con css' +
        ' utilizando un framework de bootstrap'
      'Siempre generas html + CSS + JS en el mismo documento'
      'Presentaciones gerenciales.')
    Completion_tokens = 0
    Url = 'http://localhost:11434/'
    ResponseTimeOut = 60000
    Stream_Usage = False
    ThinkingLevel = tlDefault
    MediaResolution = mrDefault
    keep_alive = '1m'
    Left = 208
    Top = 288
  end
end
