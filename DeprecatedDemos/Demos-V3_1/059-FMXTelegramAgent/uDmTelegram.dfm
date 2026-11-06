object DmTelegram: TDmTelegram
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 480
  Width = 640
  object BotPrompts: TAiPrompts
    Items = <
      item
        Nombre = 'welcome'
        Strings.Strings = (
          'MakerAI Agent ready!'
          'I have ppm_install built in and can get more tools on demand.'
          'Commands: /help  /clear')
      end
      item
        Nombre = 'help'
        Strings.Strings = (
          'Commands: /start /clear /help'
          'ppm_install downloads any of 158+ tools from the PPM registry.')
      end>
    Left = 280
    Top = 200
  end
end
