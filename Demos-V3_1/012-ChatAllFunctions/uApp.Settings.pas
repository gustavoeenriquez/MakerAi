unit uApp.Settings;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils;

type
  TAppSettings = class
  private
    FProviderName : string;
    FModelName    : string;
    FThemeDark    : Boolean;
    FSystemPrompt : string;
    FApiKeys      : TStringList;
    function  GetApiKey(const AProvider: string): string;
    procedure SetApiKey(const AProvider, AValue: string);
    function  GetSettingsDir: string;
    function  GetFilePath: string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Load;
    procedure   Save;
    property ProviderName : string  read FProviderName  write FProviderName;
    property ModelName    : string  read FModelName     write FModelName;
    property ThemeDark    : Boolean read FThemeDark     write FThemeDark;
    property SystemPrompt : string  read FSystemPrompt  write FSystemPrompt;
    property ApiKey[const AProvider: string]: string read GetApiKey write SetApiKey;
  end;

var
  AppSettings: TAppSettings;

implementation

const
  DefaultProvider = 'Claude';
  DefaultModel    = 'claude-haiku-4-5-20251001';

constructor TAppSettings.Create;
begin
  inherited;
  FApiKeys      := TStringList.Create;
  FProviderName := DefaultProvider;
  FModelName    := DefaultModel;
  FThemeDark    := False;
  FSystemPrompt := '';
  Load;
end;

destructor TAppSettings.Destroy;
begin
  FApiKeys.Free;
  inherited;
end;

function TAppSettings.GetSettingsDir: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'MakerAIChat');
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

function TAppSettings.GetFilePath: string;
begin
  Result := TPath.Combine(GetSettingsDir, 'settings.json');
end;

function TAppSettings.GetApiKey(const AProvider: string): string;
begin
  Result := FApiKeys.Values[AProvider];
end;

procedure TAppSettings.SetApiKey(const AProvider, AValue: string);
begin
  FApiKeys.Values[AProvider] := AValue;
end;

procedure TAppSettings.Load;
var
  LPath : string;
  LJson : TJSONObject;
  LKeys : TJSONObject;
  I     : Integer;
  LPair : TJSONPair;
begin
  LPath := GetFilePath;
  if not TFile.Exists(LPath) then Exit;
  try
    LJson := TJSONObject.ParseJSONValue(TFile.ReadAllText(LPath)) as TJSONObject;
    if not Assigned(LJson) then Exit;
    try
      LJson.TryGetValue<string>('provider', FProviderName);
      LJson.TryGetValue<string>('model',    FModelName);
      LJson.TryGetValue<Boolean>('dark',    FThemeDark);
      LJson.TryGetValue<string>('system',   FSystemPrompt);
      LKeys := LJson.GetValue<TJSONObject>('keys');
      if Assigned(LKeys) then
        for I := 0 to LKeys.Count - 1 do
        begin
          LPair := LKeys.Pairs[I];
          FApiKeys.Values[LPair.JsonString.Value] := LPair.JsonValue.Value;
        end;
    finally
      LJson.Free;
    end;
  except
  end;
end;

procedure TAppSettings.Save;
var
  LPath : string;
  LJson : TJSONObject;
  LKeys : TJSONObject;
  I     : Integer;
begin
  LPath := GetFilePath;
  LJson := TJSONObject.Create;
  try
    LJson.AddPair('provider', FProviderName);
    LJson.AddPair('model',    FModelName);
    LJson.AddPair('dark',     TJSONBool.Create(FThemeDark));
    LJson.AddPair('system',   FSystemPrompt);
    LKeys := TJSONObject.Create;
    for I := 0 to FApiKeys.Count - 1 do
      LKeys.AddPair(FApiKeys.Names[I], FApiKeys.ValueFromIndex[I]);
    LJson.AddPair('keys', LKeys);
    TFile.WriteAllText(LPath, LJson.ToJSON);
  finally
    LJson.Free;
  end;
end;

initialization
  AppSettings := TAppSettings.Create;

finalization
  FreeAndNil(AppSettings);

end.
