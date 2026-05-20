unit uApp.Settings;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.IOUtils, System.JSON, System.Classes;

type
  TAppSettings = class
  private
    FFilePath   : string;
    FProviderName: string;
    FModelName  : string;
    FThemeDark  : Boolean;
    FSystemPrompt: string;
    FApiKeys    : TStringList;
    function  GetApiKey(const AProvider: string): string;
    procedure SetApiKey(const AProvider, AKey: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Load;
    procedure Save;
    property ProviderName : string  read FProviderName  write FProviderName;
    property ModelName    : string  read FModelName     write FModelName;
    property ThemeDark    : Boolean read FThemeDark     write FThemeDark;
    property SystemPrompt : string  read FSystemPrompt  write FSystemPrompt;
    property ApiKey[const AProvider: string]: string read GetApiKey write SetApiKey;
  end;

var
  AppSettings: TAppSettings;

implementation

function DataDir: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'MakerAIChat');
end;

constructor TAppSettings.Create;
begin
  FApiKeys      := TStringList.Create;
  FProviderName := 'Claude';
  FModelName    := 'claude-haiku-4-5-20251001';
  FThemeDark    := False;
  FSystemPrompt := 'You are a helpful AI assistant. Answer clearly and concisely.';
  FFilePath     := TPath.Combine(DataDir, 'settings.json');
  Load;
end;

destructor TAppSettings.Destroy;
begin
  FApiKeys.Free;
  inherited;
end;

function TAppSettings.GetApiKey(const AProvider: string): string;
begin
  Result := FApiKeys.Values[AProvider];
end;

procedure TAppSettings.SetApiKey(const AProvider, AKey: string);
begin
  FApiKeys.Values[AProvider] := AKey;
end;

procedure TAppSettings.Load;
begin
  if not TFile.Exists(FFilePath) then Exit;
  try
    var Text := TFile.ReadAllText(FFilePath);
    var JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if JSON = nil then Exit;
    try
      FProviderName  := JSON.GetValue<string>('provider',     FProviderName);
      FModelName     := JSON.GetValue<string>('model',        FModelName);
      FThemeDark     := JSON.GetValue<Boolean>('dark',        FThemeDark);
      FSystemPrompt  := JSON.GetValue<string>('systemPrompt', FSystemPrompt);
      var Keys := JSON.GetValue('apiKeys') as TJSONObject;
      if Assigned(Keys) then
        for var Pair in Keys do
          FApiKeys.Values[Pair.JsonString.Value] := Pair.JsonValue.Value;
    finally
      JSON.Free;
    end;
  except
  end;
end;

procedure TAppSettings.Save;
begin
  if not TDirectory.Exists(DataDir) then
    TDirectory.CreateDirectory(DataDir);

  var JSON := TJSONObject.Create;
  try
    JSON.AddPair('provider',     FProviderName);
    JSON.AddPair('model',        FModelName);
    JSON.AddPair('dark',         TJSONBool.Create(FThemeDark));
    JSON.AddPair('systemPrompt', FSystemPrompt);
    var Keys := TJSONObject.Create;
    for var I := 0 to FApiKeys.Count - 1 do
      Keys.AddPair(FApiKeys.Names[I], FApiKeys.ValueFromIndex[I]);
    JSON.AddPair('apiKeys', Keys);
    TFile.WriteAllText(FFilePath, JSON.Format);
  finally
    JSON.Free;
  end;
end;

initialization
  AppSettings := TAppSettings.Create;
finalization
  FreeAndNil(AppSettings);
end.
