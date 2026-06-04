object DmCodeReview: TDmCodeReview
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 0
  Top = 0
  Height = 400
  Width = 600
  object AgentManager: TAIAgents
    StartNode = Generator
    EndNode = Refactorer
    TimeoutMs = 300000
    OnFinish = AgentManagerFinish
    OnEnterNode = AgentManagerEnterNode
    OnExitNode = AgentManagerExitNode
    Left = 320
    Top = 24
  end
  object Generator: TLLMNode
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
    NextA = Reviewer
    Graph = AgentManager
    Left = 200
    Top = 80
  end
  object Reviewer: TLLMNode
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
    NextA = Refactorer
    Graph = AgentManager
    Left = 200
    Top = 160
  end
  object Refactorer: TLLMNode
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
