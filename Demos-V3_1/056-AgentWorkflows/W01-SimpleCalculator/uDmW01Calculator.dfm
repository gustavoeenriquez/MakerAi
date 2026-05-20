object DmW01: TDmW01
  OnCreate = DataModuleCreate
  Height = 304
  Width = 971
  object Manager: TAIAgentManager
    TimeoutMs = 90000
    Asynchronous = False
    Left = 48
    Top = 32
  end
  object Parser: TLLMNode
    Next = LinkPC
    Graph = Manager
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    SystemPrompt = 
      'You are a math expression extractor. Given a math word problem, ' +
      'extract ONLY the arithmetic expression and output it in clean st' +
      'andard format using numbers and operators +  -  *  /  ()  sqrt()' +
      '.'#13#10'Rules:'#13#10'- Output ONLY the expression, nothing else. No explan' +
      'ations.'#13#10'- Use decimal numbers when needed (e.g. 0.30 for 30%).'#13 +
      #10'- Percentages: write as multiplication (15% of 240 -> 240 * 0.1' +
      '5).'#13#10'- Examples:'#13#10'    15 percent of 240 -> 240 * 0.15'#13#10'    squar' +
      'e root of 144 plus 7 times 3 -> sqrt(144) + 7 * 3'#13#10'    45 minus ' +
      '30 percent of 45 -> 45 - 45 * 0.30'#13#10'    4 pizzas at 12.50 with 2' +
      '0% discount -> 4 * 12.50 * 0.80'
    UseAllTools = False
    Left = 160
    Top = 32
  end
  object LinkPC: TAIAgentsLink
    Graph = Manager
    ConditionalKey = 'next_route'
    ManualTargetsKey = 'next_targets'
    Left = 320
    Top = 32
  end
  object Calculator: TAIAgentsNode
    Next = LinkCF
    Graph = Manager
    OnExecute = DoCalculate
    Left = 440
    Top = 32
  end
  object LinkCF: TAIAgentsLink
    Graph = Manager
    ConditionalKey = 'next_route'
    ManualTargetsKey = 'next_targets'
    Left = 560
    Top = 32
  end
  object Formatter: TLLMNode
    Graph = Manager
    DriverName = 'Claude'
    Model = 'claude-haiku-4-5-20251001'
    ApiKey = '@CLAUDE_API_KEY'
    SystemPrompt = 
      'You receive a math result in the format EXPR = VALUE.'#13#10'Write a s' +
      'ingle friendly sentence answering the original math question wit' +
      'h the result. Be concise. Do not repeat the expression.'#13#10'Example' +
      ': 240 CLAUDE.md Demos Docs LICENSE.txt README.md README_3.1.md R' +
      'edis Resources Source build_058.bat build_058.log build_064.bat ' +
      'build_064.log build_agents2.log build_all.log build_all_demos.ps' +
      '1 build_demo.bat build_package.bat build_package.log cohere_test' +
      '.log encoding_fixes.json fix_encoding.py makerai_tools.log nul o' +
      'penai_diff.txt ppm ppm_test.log ppm_test_out.txt run_ppm_test.ba' +
      't 0.15 = 36 -> 15% of 240 is 36.'
    UseAllTools = False
    Left = 680
    Top = 32
  end
end
