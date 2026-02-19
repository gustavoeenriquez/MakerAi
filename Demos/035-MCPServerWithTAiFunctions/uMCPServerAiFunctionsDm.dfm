object MCPServerAiFunctionsDm: TMCPServerAiFunctionsDm
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object AiFunctions1: TAiFunctions
    Functions = <
      item
        FunctionName = 'GetFechaHora'
        OnAction = AiFunctions1Functions0GetFechaHoraAction
        Description.Strings = (
          'Obtiene la fecha y hora de una ciudad  PAIS,CIUDAD determinada')
        Parameters = <
          item
            Name = 'Ciudad'
            ParamType = ptString
            Description.Strings = (
              'La Ciudad a la cual se desea conocer la fecha y hora')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    MCPClients = <
      item
        Connected = False
        Enabled = True
        Name = 'MCPClient'
        TransportType = tpStdIo
        Params.Strings = (
          'Command=C:\mcp\servers\McpFileServerStIO.exe'
          'Timeout=15000')
        Configuration = '(Properties, Click [...] to edit)'
      end>
    Left = 352
    Top = 240
  end
  object AiMCPServer1: TAiMCPStdioServer
    ServerName = 'Delphi-DataModule-Server StdIO'
    AiFunctions = AiFunctions1
    Left = 208
    Top = 104
  end
  object AiMCPHttpServer1: TAiMCPHttpServer
    ServerName = 'Delphi-DataModule-Server HTTP'
    AiFunctions = AiFunctions1
    Port = 3000
    CorsEnabled = True
    CorsAllowedOrigins = '*'
    Left = 392
    Top = 104
  end
end
