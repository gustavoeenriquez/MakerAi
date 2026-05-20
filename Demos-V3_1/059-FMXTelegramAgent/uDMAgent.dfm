object DmAgent: TDmAgent
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 480
  Width = 640
  object AiConn: TAiChatConnection
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    Params.Strings = (
      'ApiKey=@CLAUDE_API_KEY'
      'Asynchronous=False'
      'Max_tokens=2048'
      'Tool_Active=True'
      'Model=claude-haiku-4-5-20251001')
    AiFunctions = AgentFuncs
    Completion_tokens = 0
    Left = 136
    Top = 136
  end
  object AgentFuncs: TAiFunctions
    Functions = <
      item
        FunctionName = 'ppm_install'
        OnAction = AgentFuncsPPMInstall
        Description.Strings = (
          'Search and install a new capability from the PPM registry '
          '(158+ MCP tools: weather, GitHub, databases, email, Telegram, '
          'Discord, finance, translation, Kubernetes, S3, and more). '
          'Call this when you need a tool not currently available. '
          'After installation the new tools are immediately callable.')
        Parameters = <
          item
            Name = 'capability'
            ParamType = ptString
            Description.Strings = (
              'Keyword or package name describing the needed capability'
              '(e.g. "weather", "github", "postgres", "email", "translate")')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    MCPClients = <>
    PPMRegistryUrl = 'https://registry.pascalai.org'
    Left = 296
    Top = 136
  end
  object AgentPrompts: TAiPrompts
    Items = <
      item
        Nombre = 'system'
        Strings.Strings = (
          'You are a capable AI assistant.'
          'You have access to ppm_install to download new tools from the PPM'
          'registry whenever you need a capability that is not yet available.'
          'After installing a package its tools are immediately ready to use.'
          'Be concise. Use plain text (no markdown).'
          'Use tools proactively when the user asks to do something actionable.'
          'Ask for confirmation before destructive operations.')
      end>
    PPMRegistryUrl = 'https://registry.pascalai.org'
    Left = 456
    Top = 136
  end
end
