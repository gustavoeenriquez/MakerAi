unit uApp.ProviderList;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Generics.Defaults,
  uMakerAi.Chat.AiConnection;

type
  TProviderItem = record
    Name       : string;
    DisplayName: string;
    NeedsKey   : Boolean;
  end;

  TProviderList = class
  private
    FItems: TList<TProviderItem>;
    function GetCount: Integer;
    class function DisplayNameFor(const AName: string): string; static;
    class function NeedsApiKey(const AName: string): Boolean; static;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BuildFromConnection;
    function    Item(AIndex: Integer): TProviderItem;
    function    IndexOf(const AProviderName: string): Integer;
    function    FetchModels(const ADriverName, AApiKey: string): TStringList;
    property Count: Integer read GetCount;
  end;

var
  ProviderList: TProviderList;

implementation

const
  NoKeyProviders: array[0..3] of string = ('Ollama', 'LMStudio', 'LlamaCpp', 'GenericLLM');

class function TProviderList.NeedsApiKey(const AName: string): Boolean;
var
  S: string;
begin
  for S in NoKeyProviders do
    if SameText(S, AName) then Exit(False);
  Result := True;
end;

class function TProviderList.DisplayNameFor(const AName: string): string;
begin
  if      SameText(AName, 'Claude')     then Result := 'Claude (Anthropic)'
  else if SameText(AName, 'OpenAI')     then Result := 'OpenAI (GPT)'
  else if SameText(AName, 'Gemini')     then Result := 'Google Gemini'
  else if SameText(AName, 'Groq')       then Result := 'Groq (Fast)'
  else if SameText(AName, 'Grok')       then Result := 'Grok (xAI)'
  else if SameText(AName, 'DeepSeek')   then Result := 'DeepSeek'
  else if SameText(AName, 'Mistral')    then Result := 'Mistral AI'
  else if SameText(AName, 'Cohere')     then Result := 'Cohere'
  else if SameText(AName, 'Kimi')       then Result := 'Kimi (Moonshot)'
  else if SameText(AName, 'Ollama')     then Result := 'Ollama (Local)'
  else if SameText(AName, 'LMStudio')   then Result := 'LM Studio (Local)'
  else if SameText(AName, 'LlamaCpp')   then Result := 'LlamaCpp (Local)'
  else if SameText(AName, 'GenericLLM') then Result := 'Generic LLM'
  else Result := AName;
end;

constructor TProviderList.Create;
begin
  inherited;
  FItems := TList<TProviderItem>.Create;
  BuildFromConnection;
end;

destructor TProviderList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TProviderList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TProviderList.BuildFromConnection;
var
  LDrivers: TArray<string>;
  LName   : string;
  LItem   : TProviderItem;
begin
  FItems.Clear;
  LDrivers := TAiChatConnection.AvailableDrivers;
  for LName in LDrivers do
  begin
    LItem.Name        := LName;
    LItem.DisplayName := DisplayNameFor(LName);
    LItem.NeedsKey    := NeedsApiKey(LName);
    FItems.Add(LItem);
  end;
  // Sort by display name
  FItems.Sort(TComparer<TProviderItem>.Construct(
    function(const A, B: TProviderItem): Integer
    begin
      Result := CompareText(A.DisplayName, B.DisplayName);
    end));
end;

function TProviderList.Item(AIndex: Integer): TProviderItem;
begin
  Result := FItems[AIndex];
end;

function TProviderList.IndexOf(const AProviderName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].Name, AProviderName) then Exit(I);
end;

function TProviderList.FetchModels(const ADriverName, AApiKey: string): TStringList;
var
  LConn  : TAiChatConnection;
  LModels: TStringList;
begin
  Result := TStringList.Create;
  LConn  := TAiChatConnection.Create(nil);
  try
    if AApiKey <> '' then
      LConn.Params.Values['ApiKey'] := AApiKey;
    LConn.DriverName := ADriverName;
    try
      LModels := LConn.GetModels;
      try
        Result.Assign(LModels);
      finally
        LModels.Free;
      end;
    except
      Result.Clear;
    end;
  finally
    LConn.Free;
  end;
end;

initialization
  ProviderList := TProviderList.Create;

finalization
  FreeAndNil(ProviderList);

end.
