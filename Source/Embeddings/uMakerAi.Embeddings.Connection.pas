// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Embeddings.Connection;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.TypInfo, System.StrUtils,
  uMakerAi.ParamsRegistry, uMakerAi.Embeddings, uMakerAi.Embeddings.core,
  uMakerAi.Chat.Initializations;

type
  TAiEmbeddingConnection = class(TComponent)
  private
    FEmbeddings: TAiEmbeddings;
    FDriverName: String;
    FModel: String;
    FParams: TStrings;
    FOnGetEmbedding: TOnGetEmbedding;
    FVersion: String;

    procedure SetDriverName(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetParams(const Value: TStrings);
    procedure ParamsChanged(Sender: TObject);

    function GetApiKey: String;
    procedure SetApiKey(const Value: String);
    function GetUrl: String;
    procedure SetUrl(const Value: String);
    function GetDimensions: Integer;
    procedure SetDimensions(const Value: Integer);
    function GetPrompt_tokens: Integer;
    function GetTotal_tokens: Integer;
    function GetData: TAiEmbeddingData;

    procedure SetOnGetEmbedding(const Value: TOnGetEmbedding);

  protected
    procedure ValidateEmbeddings;
    procedure UpdateAndApplyParams;
    procedure SetupEmbeddingsFromDriver;
    procedure ApplyParamsToEmbeddings(AEmb: TAiEmbeddings; AParams: TStrings);
    function MergeParams(Origin, Destination: TStrings): TStrings;
    procedure Loaded; override;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1;
      aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData;
    function GetAvailableDrivers: TArray<string>;
    function IsDriverAvailable(const aDriverName: string): Boolean;
    procedure ResetParamsToDefaults;

    property AiEmbeddings: TAiEmbeddings read FEmbeddings;
    property prompt_tokens: Integer read GetPrompt_tokens;
    property total_tokens: Integer read GetTotal_tokens;
    property Data: TAiEmbeddingData read GetData;

  published
    property DriverName: String read FDriverName write SetDriverName;
    property Model: String read FModel write SetModel;
    property ApiKey: String read GetApiKey write SetApiKey;
    property Url: String read GetUrl write SetUrl;
    property Dimensions: Integer read GetDimensions write SetDimensions;
    property Params: TStrings read FParams write SetParams;
    property OnGetEmbedding: TOnGetEmbedding read FOnGetEmbedding write SetOnGetEmbedding;
    property Version: String read FVersion;
  end;

procedure Register;

implementation

{$I uMakerAi.Version.inc}

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiEmbeddingConnection]);
end;

{ TAiEmbeddingConnection }

constructor TAiEmbeddingConnection.Create(aOwner: TComponent);
begin
  inherited;
  FEmbeddings := nil;
  FParams := TStringList.Create;
  TStringList(FParams).OnChange := ParamsChanged;
  FVersion := MAKERAI_VERSION_FULL;
end;

destructor TAiEmbeddingConnection.Destroy;
begin
  if Assigned(FEmbeddings) then
    FEmbeddings.Free;
  FParams.Free;
  inherited;
end;

procedure TAiEmbeddingConnection.SetDriverName(const Value: String);
begin
  if FDriverName <> Value then
  begin
    FDriverName := Value;
    FModel := '';

    if not (csDesigning in ComponentState) then
    begin
      UpdateAndApplyParams;
      SetupEmbeddingsFromDriver;
    end
    else
    begin
      UpdateAndApplyParams;
    end;
  end;
end;

procedure TAiEmbeddingConnection.SetModel(const Value: String);
begin
  if FModel <> Value then
  begin
    FModel := Value;
    TAiEmbeddingFactory.Instance.RegisterUserParam(FDriverName, FModel, 'Model', FModel);
    UpdateAndApplyParams;
  end;
end;

procedure TAiEmbeddingConnection.SetParams(const Value: TStrings);
begin
  if Assigned(Value) then
    FParams.Assign(Value);
end;

procedure TAiEmbeddingConnection.ParamsChanged(Sender: TObject);
begin
  if Assigned(FEmbeddings) then
    ApplyParamsToEmbeddings(FEmbeddings, FParams);
end;

procedure TAiEmbeddingConnection.SetupEmbeddingsFromDriver;
var
  OldEmb, NewEmb: TAiEmbeddings;
begin
  if csLoading in ComponentState then
    Exit;

  if FDriverName = '' then
  begin
    if Assigned(FEmbeddings) then
      FreeAndNil(FEmbeddings);
    Exit;
  end;

  OldEmb := FEmbeddings;
  FEmbeddings := nil;

  NewEmb := TAiEmbeddingFactory.Instance.CreateDriver(FDriverName);
  if not Assigned(NewEmb) then
    raise Exception.CreateFmt('Failed to create embedding driver instance for "%s"', [FDriverName]);

  ApplyParamsToEmbeddings(NewEmb, FParams);

  // Delegar evento
  if Assigned(FOnGetEmbedding) then
    NewEmb.OnGetEmbedding := FOnGetEmbedding;

  FEmbeddings := NewEmb;

  if Assigned(OldEmb) then
    OldEmb.Free;
end;

procedure TAiEmbeddingConnection.UpdateAndApplyParams;
var
  LRegistryParams: TStringList;
  ShouldExpand: Boolean;
begin
  if csLoading in ComponentState then
    Exit;

  if FDriverName = '' then
  begin
    FParams.Clear;
    Exit;
  end;

  if TAiEmbeddingFactory.Instance.HasDriver(FDriverName) then
  begin
    ShouldExpand := not (csDesigning in ComponentState);
    LRegistryParams := TStringList.Create;
    try
      TAiEmbeddingFactory.Instance.GetDriverParams(FDriverName, FModel, LRegistryParams, ShouldExpand);

      FParams.BeginUpdate;
      try
        MergeParams(LRegistryParams, FParams);
      finally
        FParams.EndUpdate;
      end;
    finally
      LRegistryParams.Free;
    end;
  end
  else
    FParams.Clear;

  if Assigned(FEmbeddings) then
    ApplyParamsToEmbeddings(FEmbeddings, FParams);
end;

procedure TAiEmbeddingConnection.ApplyParamsToEmbeddings(AEmb: TAiEmbeddings; AParams: TStrings);
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LProp: TRttiProperty;
  LValue: TValue;
  I: Integer;
  ParamName, ParamValue: string;
  LIntVal: Int64;
  LFloatVal: Double;
begin
  if not Assigned(AEmb) then
    Exit;

  if not Assigned(AParams) or (AParams.Count <= 0) then
    Exit;

  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(AEmb.ClassType);

    for I := 0 to AParams.Count - 1 do
    begin
      ParamName := AParams.Names[I];
      ParamValue := AParams.Values[ParamName].Trim;

      if ParamName.IsEmpty then
        Continue;

      LProp := LRttiType.GetProperty(ParamName);

      if Assigned(LProp) and LProp.IsWritable then
      begin
        try
          case LProp.PropertyType.TypeKind of
            tkInteger, tkInt64:
              if TryStrToInt64(ParamValue, LIntVal) then
                LProp.SetValue(AEmb, LIntVal);

            tkFloat:
              if TryStrToFloat(ParamValue, LFloatVal) then
                LProp.SetValue(AEmb, LFloatVal);

            tkString, tkUString, tkWideString:
              LProp.SetValue(AEmb, ParamValue);

            tkEnumeration:
              begin
                if LProp.PropertyType.Handle = TypeInfo(Boolean) then
                  LValue := MatchStr(LowerCase(ParamValue), ['true', '1', 'yes', 't'])
                else
                  LValue := TValue.FromOrdinal(LProp.PropertyType.Handle,
                    GetEnumValue(LProp.PropertyType.Handle, ParamValue));
                LProp.SetValue(AEmb, LValue);
              end;
          end;
        except
          // Fallo silencioso por propiedad individual
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TAiEmbeddingConnection.MergeParams(Origin, Destination: TStrings): TStrings;
var
  I: Integer;
begin
  Result := Destination;
  for I := 0 to Origin.Count - 1 do
    Destination.Values[Origin.Names[I]] := Origin.ValueFromIndex[I];
end;

procedure TAiEmbeddingConnection.ValidateEmbeddings;
begin
  if not Assigned(FEmbeddings) and (FDriverName <> '') then
    SetupEmbeddingsFromDriver;

  if not Assigned(FEmbeddings) then
    raise Exception.Create('A valid DriverName must be specified to create an Embeddings instance.');
end;

procedure TAiEmbeddingConnection.Loaded;
begin
  inherited;
  SetupEmbeddingsFromDriver;
end;

function TAiEmbeddingConnection.CreateEmbedding(aInput, aUser: String;
  aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
begin
  ValidateEmbeddings;
  Result := FEmbeddings.CreateEmbedding(aInput, aUser, aDimensions, aModel, aEncodingFormat);
end;

function TAiEmbeddingConnection.GetAvailableDrivers: TArray<string>;
begin
  Result := TAiEmbeddingFactory.Instance.GetRegisteredDrivers;
end;

function TAiEmbeddingConnection.IsDriverAvailable(const aDriverName: string): Boolean;
begin
  Result := TAiEmbeddingFactory.Instance.HasDriver(aDriverName);
end;

procedure TAiEmbeddingConnection.ResetParamsToDefaults;
begin
  UpdateAndApplyParams;
end;

// --- Getters y Setters delegados al driver ---

function TAiEmbeddingConnection.GetApiKey: String;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.ApiKey
  else
    Result := FParams.Values['ApiKey'];
end;

procedure TAiEmbeddingConnection.SetApiKey(const Value: String);
begin
  FParams.Values['ApiKey'] := Value;
  if Assigned(FEmbeddings) then
    FEmbeddings.ApiKey := Value;
end;

function TAiEmbeddingConnection.GetUrl: String;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.Url
  else
    Result := FParams.Values['Url'];
end;

procedure TAiEmbeddingConnection.SetUrl(const Value: String);
begin
  FParams.Values['Url'] := Value;
  if Assigned(FEmbeddings) then
    FEmbeddings.Url := Value;
end;

function TAiEmbeddingConnection.GetDimensions: Integer;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.Dimensions
  else
    Result := StrToIntDef(FParams.Values['Dimensions'], 1536);
end;

procedure TAiEmbeddingConnection.SetDimensions(const Value: Integer);
begin
  FParams.Values['Dimensions'] := IntToStr(Value);
  if Assigned(FEmbeddings) then
    FEmbeddings.Dimensions := Value;
end;

function TAiEmbeddingConnection.GetPrompt_tokens: Integer;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.prompt_tokens
  else
    Result := 0;
end;

function TAiEmbeddingConnection.GetTotal_tokens: Integer;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.total_tokens
  else
    Result := 0;
end;

function TAiEmbeddingConnection.GetData: TAiEmbeddingData;
begin
  if Assigned(FEmbeddings) then
    Result := FEmbeddings.Data
  else
    Result := nil;
end;

procedure TAiEmbeddingConnection.SetOnGetEmbedding(const Value: TOnGetEmbedding);
begin
  FOnGetEmbedding := Value;
  if Assigned(FEmbeddings) then
    FEmbeddings.OnGetEmbedding := Value;
end;

end.
