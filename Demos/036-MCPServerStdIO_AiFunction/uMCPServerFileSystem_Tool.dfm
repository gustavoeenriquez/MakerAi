object FMCPServerFileSystem_Tool: TFMCPServerFileSystem_Tool
  Height = 480
  Width = 640
  object AiMCPStdioServer1: TAiMCPStdioServer
    ServerName = 'FileSystem'
    AiFunctions = AiFunctions1
    Left = 160
    Top = 104
  end
  object AiFunctions1: TAiFunctions
    Functions = <
      item
        FunctionName = 'fs_listar'
        OnAction = AiFunctions1Functions0fs_listarAction
        Description.Strings = (
          
            'Lista el directorio al cual tiene acceso, es un directorio '#250'nico' +
            ' sandbox.')
        Parameters = <
          item
            Name = 'filtro'
            ParamType = ptString
            Description.Strings = (
              'Filtro de b'#250'squeda para los archivos')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    MCPClients = <>
    Left = 304
    Top = 168
  end
end
