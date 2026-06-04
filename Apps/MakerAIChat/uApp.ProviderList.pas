unit uApp.ProviderList;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  TProviderInfo = record
    Name       : string;
    DisplayName: string;
    NeedsApiKey: Boolean;
  end;

  TProviderList = class
  private
    FItems: TArray<TProviderInfo>;
    procedure BuildFromConnection;
  public
    constructor Create;
    function Count: Integer;
    function Item(AIndex: Integer): TProviderInfo;
    function IndexOf(const AName: string): Integer;
    function DisplayNameOf(const AName: string): string;
    function NeedsApiKey(const AName: string): Boolean;
    // HTTP call to provider API — caller must free the result
    function FetchModels(const ADriverName, AApiKey: string): TStringList;
  end;

var
  ProviderList: TProviderList;

implementation

uses
  uMakerAi.Chat.AiConnection;

{ display name and key requirement for known drivers }
function DriverDisplayName(const AName: string): string;
begin
  if      SameText(AName, 'Claude')    then Result := 'Claude (Anthropic)'
  else if SameText(AName, 'OpenAI')    then Result := 'OpenAI (GPT)'
  else if SameText(AName, 'Gemini')    then Result := 'Google Gemini'
  else if SameText(AName, 'Groq')      then Result := 'Groq'
  else if SameText(AName, 'Grok')      then Result := 'Grok (xAI)'
  else if SameText(AName, 'DeepSeek')  then Result := 'DeepSeek'
  else if SameText(AName, 'Cohere')    then Result := 'Cohere'
  else if SameText(AName, 'Ollama')    then Result := 'Ollama (local)'
  else if SameText(AName, 'LMStudio')  then Result := 'LM Studio (local)'
  else if SameText(AName, 'LlamaCpp')  then Result := 'LlamaCpp (local)'
  else if SameText(AName, 'GenericLLM')then Result := 'Generic LLM'
  else                                      Result := AName;
end;

function DriverNeedsKey(const AName: string): Boolean;
begin
  Result := not (
    SameText(AName, 'Ollama')    or
    SameText(AName, 'LMStudio')  or
    SameText(AName, 'LlamaCpp')  or
    SameText(AName, 'GenericLLM')
  );
end;

{ TProviderList }

procedure TProviderList.BuildFromConnection;
var
  P: TProviderInfo;
begin
  SetLength(FItems, 0);
  for var D in TAiChatConnection.AvailableDrivers do
  begin
    P.Name        := D;
    P.DisplayName := DriverDisplayName(D);
    P.NeedsApiKey := DriverNeedsKey(D);
    FItems := FItems + [P];
  end;
  TArray.Sort<TProviderInfo>(FItems, TComparer<TProviderInfo>.Construct(
    function(const A, B: TProviderInfo): Integer
    begin
      Result := CompareText(A.DisplayName, B.DisplayName);
    end));
end;

constructor TProviderList.Create;
begin
  BuildFromConnection;
end;

function TProviderList.Count: Integer;
begin
  Result := Length(FItems);
end;

function TProviderList.Item(AIndex: Integer): TProviderInfo;
begin
  Result := FItems[AIndex];
end;

function TProviderList.IndexOf(const AName: string): Integer;
begin
  Result := -1;
  for var I := 0 to High(FItems) do
    if SameText(FItems[I].Name, AName) then Exit(I);
end;

function TProviderList.DisplayNameOf(const AName: string): string;
begin
  var I := IndexOf(AName);
  if I >= 0 then Result := FItems[I].DisplayName
  else Result := AName;
end;

function TProviderList.NeedsApiKey(const AName: string): Boolean;
begin
  var I := IndexOf(AName);
  if I >= 0 then Result := FItems[I].NeedsApiKey
  else Result := True;
end;

function TProviderList.FetchModels(const ADriverName, AApiKey: string): TStringList;
var
  LConn  : TAiChatConnection;
  LModels: TStringList;
begin
  Result := TStringList.Create;
  LConn  := TAiChatConnection.Create(nil);
  try
    LConn.DriverName          := ADriverName;
    LConn.Params.Values['ApiKey'] := AApiKey;
    try
      LModels := LConn.GetModels;
      try
        Result.Assign(LModels);
      finally
        LModels.Free;
      end;
    except
      // network / auth error → return empty list
    end;
  finally
    LConn.Free;
  end;
end;

end.
