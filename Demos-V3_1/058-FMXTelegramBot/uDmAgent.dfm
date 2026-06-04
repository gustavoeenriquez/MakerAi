object DmAgent: TDmAgent
  Height = 480
  Width = 640
  object AiPrompts1: TAiPrompts
    Items = <
      item
        Nombre = 'SYS_PROMPT'
        Strings.Strings = (
          'Eres un potente asistente de IA accesible v'#237'a Telegram.'
          'Herramientas disponibles:'
          'SISTEMA : shell_exec, file_read, file_write, file_list'
          
            'MEMORIA : memory_save, memory_get, memory_delete, memory_list, m' +
            'emory_search'
          'AGENTES : agent_run (tipo: "analyze" o "code_review")'
          
            'VOZ     : tg_voice (s'#237'ntesis de voz '#8212' env'#237'a un audio al chat de ' +
            'Telegram)'
          
            'PPM     : ppm_install (instala cualquier capacidad externa desde' +
            ' el registro PPM)'
          
            'Regla clave: si necesitas una herramienta que no est'#225' en la list' +
            'a (clima, base de datos,'
          
            'GitHub, correo, traducci'#243'n, etc.), llama primero a ppm_install{"' +
            'capability":"keyword"}.'
          
            'Tras la instalaci'#243'n, las nuevas herramientas quedan disponibles ' +
            'de inmediato.'
          'Otras reglas: s'#233' conciso, usa texto plano (sin markdown),'
          
            'usa las herramientas de forma proactiva cuando el usuario pida h' +
            'acer algo en el sistema'
          'o quiera recordar informaci'#243'n.'
          'Pide confirmaci'#243'n antes de operaciones destructivas.'
          'Responde siempre en espa'#241'ol.')
      end>
    PPMRegistryUrl = 'https://registry.pascalai.org'
    Left = 456
    Top = 224
  end
  object AiConn: TAiChatConnection
    AiFunctions = AiFunctions1
    Completion_tokens = 0
    Left = 136
    Top = 232
  end
  object AiFunctions1: TAiFunctions
    Functions = <
      item
        FunctionName = 'ppm_install'
        OnAction = AiFunctions1Functions0ppm_installAction
        Description.Strings = (
          
            '    Search and auto-install an external capability from the PPM ' +
            'registry'
          
            '    (118+ MCP tools: weather, GitHub, databases, email, translat' +
            'ion, finance,'
          
            '    Kubernetes, Kafka, S3, Telegram, Discord, Shopify, and more)' +
            '.'
          
            '    Call this when you need a tool that is not currently availab' +
            'le.'
          '    After installation the new tools are immediately callable.')
        Parameters = <
          item
            Name = 'capability'
            ParamType = ptString
            Description.Strings = (
              'Keyword or package name describing the needed capability'
              
                '     (e.g. "weather", "github", "postgres", "email", "translate"' +
                ', "coingecko")')
            Required = True
          end>
        Default = False
        Tag = 0
        ToolType = tt_function
      end>
    MCPClients = <>
    PPMRegistryUrl = 'https://registry.pascalai.org'
    Left = 296
    Top = 184
  end
end
