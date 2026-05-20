object DmDebate: TDmDebate
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 0
  Top = 0
  Height = 480
  Width = 680
  object AgentManager: TAIAgents
    StartNode = Moderador
    EndNode = Juez
    TimeoutMs = 300000
    OnPrint = AgentManagerPrint
    OnFinish = AgentManagerFinish
    OnEnterNode = AgentManagerEnterNode
    OnExitNode = AgentManagerExitNode
    Left = 312
    Top = 24
  end
  object Moderador: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link1
    Graph = AgentManager
    Left = 80
    Top = 96
  end
  object Link1: TAIAgentsLink
    NextA = Propositor
    Graph = AgentManager
    Left = 200
    Top = 96
  end
  object Propositor: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link2
    Graph = AgentManager
    Left = 80
    Top = 176
  end
  object Link2: TAIAgentsLink
    NextA = Critico
    Graph = AgentManager
    Left = 200
    Top = 176
  end
  object Critico: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link3
    Graph = AgentManager
    Left = 80
    Top = 256
  end
  object Link3: TAIAgentsLink
    NextA = Rebatidor
    Graph = AgentManager
    Left = 200
    Top = 256
  end
  object Rebatidor: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link4
    Graph = AgentManager
    Left = 80
    Top = 336
  end
  object Link4: TAIAgentsLink
    NextA = Juez
    Graph = AgentManager
    Left = 200
    Top = 336
  end
  object Juez: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Graph = AgentManager
    Left = 80
    Top = 416
  end
end
