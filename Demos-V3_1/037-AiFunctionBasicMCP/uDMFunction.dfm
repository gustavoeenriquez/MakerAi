object DmFunction: TDmFunction
  Height = 480
  Width = 640
  object AiMCPStdioServer1: TAiMCPStdioServer
    ServerName = 'SystemFileUtils'
    AiFunctions = AiFunctions1
    Left = 112
    Top = 144
  end
  object AiMCPHttpServer1: TAiMCPHttpServer
    ServerName = 'SystemFileUtils'
    AiFunctions = AiFunctions1
    OnValidateRequest = AiMCPHttpServer1ValidateRequest
    Port = 3000
    CorsEnabled = False
    Left = 280
    Top = 88
  end
  object AiFunctions1: TAiFunctions
    Functions = <
      item
        FunctionName = 'ReadDirectory'
        OnAction = AiFunctions1Functions0ReadDirectoryAction
        Description.Strings = (
          'Read a Directory in Windows File System')
        Parameters = <
          item
            Name = 'DirectoryPath'
            ParamType = ptString
            Description.Strings = (
              'DirectoryPath is the path to get the file list')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end
      item
        FunctionName = 'CreateDirectory'
        OnAction = AiFunctions1Functions1CreateDirectoryAction
        Description.Strings = (
          'Create a new directory in the specifig path')
        Parameters = <
          item
            Name = 'DirectoryPath'
            ParamType = ptString
            Description.Strings = (
              'DirectoryPath is the path to directory to create')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    MCPClients = <>
    AutoMCPConfig.RegistryUrl = 'https://registry.pascalai.org'
    Left = 296
    Top = 208
  end
  object AiMCPSSEHttpServer1: TAiMCPSSEHttpServer
    SseEndpoint = '/sse'
    MessagesEndpoint = '/messages'
    Left = 440
    Top = 136
  end
end
