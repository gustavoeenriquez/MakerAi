object DmAnalysis: TDmAnalysis
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 0
  Top = 0
  Height = 400
  Width = 600
  object AgentManager: TAIAgents
    StartNode = Analyzer
    EndNode = Summarizer
    TimeoutMs = 300000
    OnFinish = AgentManagerFinish
    OnEnterNode = AgentManagerEnterNode
    OnExitNode = AgentManagerExitNode
    Left = 320
    Top = 24
  end
  object Analyzer: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link1
    Graph = AgentManager
    Left = 80
    Top = 80
  end
  object Link1: TAIAgentsLink
    NextA = Extractor
    Graph = AgentManager
    Left = 200
    Top = 80
  end
  object Extractor: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Next = Link2
    Graph = AgentManager
    Left = 80
    Top = 160
  end
  object Link2: TAIAgentsLink
    NextA = Summarizer
    Graph = AgentManager
    Left = 200
    Top = 160
  end
  object Summarizer: TLLMNode
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    UseAllTools = False
    MaxTokens = 0
    Graph = AgentManager
    Left = 80
    Top = 240
  end
end
