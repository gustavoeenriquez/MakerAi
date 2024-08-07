object AiModule: TAiModule
  Height = 480
  Width = 640
  object RagChat: TAiRagChat
    Chat = OpenChat
    DataVec = DataVec1
    Left = 240
    Top = 152
  end
  object DataVec1: TAiDataVec
    Embeddings = AiEmbeddings1
    Left = 272
    Top = 288
  end
  object AiEmbeddings1: TAiEmbeddings
    ApiKey = 'aquí va el apikey'
    Model = 'text-embedding-3-small'
    Url = 'https://api.openai.com/v1/'
    Dimensions = 1536
    Left = 400
    Top = 328
  end
  object OpenChat: TAiOpenChat
    ApiKey = 'aquí va el apikey'
    Model = 'gpt-4o'
    Logprobs = False
    Max_tokens = 300
    N = 1
    Response_format = tiaChatRfText
    Seed = 0
    Asynchronous = False
    Temperature = 1.000000000000000000
    Top_p = 1.000000000000000000
    Tool_Active = False
    User = 'user'
    InitialInstructions.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Prompt_tokens = 0
    Completion_tokens = 0
    Total_tokens = 0
    Url = 'https://api.openai.com/v1/'
    ResponseTimeOut = 60000
    Functions = <
      item
        FunctionName = 'get_fecha'
        Description.Strings = (
          'Retorna la fecha y la hora actrual')
        Parameters = <
          item
            Name = 'Localizacion'
            ParamType = ptString
            Description.Strings = (
              
                'la ciudad o regi'#243'n donde se solicita la fecha, ej, Colombia, Rus' +
                'ia, EEUU, etc.')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    Left = 384
    Top = 208
  end
end
