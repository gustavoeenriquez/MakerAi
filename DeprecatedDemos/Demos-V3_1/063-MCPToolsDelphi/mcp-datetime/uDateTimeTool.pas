unit uDateTimeTool;

interface

uses
  System.SysUtils, System.DateUtils, System.JSON,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: datetime_now ---
  TDateTimeNowParams = class
  private
    FFormat: string;
    FTimezone: string;
  public
    [AiMCPSchemaDescription('Output format: iso, unix, rfc, date, time, datetime, or custom:FORMAT (Delphi fmt). Default: iso')]
    [AiMCPOptional]
    property Format: string read FFormat write FFormat;
    [AiMCPSchemaDescription('Timezone offset in hours (e.g. -5, +2). Default: local system time')]
    [AiMCPOptional]
    property Timezone: string read FTimezone write FTimezone;
  end;

  TDateTimeNowTool = class(TAiMCPToolBase<TDateTimeNowParams>)
  protected
    function ExecuteWithParams(const AParams: TDateTimeNowParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: datetime_add ---
  TDateTimeAddParams = class
  private
    FDateTime: string;
    FAmount: Integer;
    FUnit: string;
  public
    [AiMCPSchemaDescription('Source datetime in ISO 8601 format (e.g. "2024-03-15T10:30:00") or "now" for current time')]
    property DateTime: string read FDateTime write FDateTime;
    [AiMCPSchemaDescription('Amount to add (can be negative to subtract)')]
    property Amount: Integer read FAmount write FAmount;
    [AiMCPSchemaDescription('Time unit: years, months, days, hours, minutes, seconds')]
    property &Unit: string read FUnit write FUnit;
  end;

  TDateTimeAddTool = class(TAiMCPToolBase<TDateTimeAddParams>)
  protected
    function ExecuteWithParams(const AParams: TDateTimeAddParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 3: datetime_diff ---
  TDateTimeDiffParams = class
  private
    FDate1: string;
    FDate2: string;
    FUnit: string;
  public
    [AiMCPSchemaDescription('First datetime in ISO 8601 format or "now"')]
    property Date1: string read FDate1 write FDate1;
    [AiMCPSchemaDescription('Second datetime in ISO 8601 format or "now"')]
    property Date2: string read FDate2 write FDate2;
    [AiMCPSchemaDescription('Result unit: years, months, days, hours, minutes, seconds. Default: days')]
    [AiMCPOptional]
    property &Unit: string read FUnit write FUnit;
  end;

  TDateTimeDiffTool = class(TAiMCPToolBase<TDateTimeDiffParams>)
  protected
    function ExecuteWithParams(const AParams: TDateTimeDiffParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 4: datetime_format ---
  TDateTimeFormatParams = class
  private
    FDateTime: string;
    FFormat: string;
  public
    [AiMCPSchemaDescription('Source datetime in ISO 8601 format or "now"')]
    property DateTime: string read FDateTime write FDateTime;
    [AiMCPSchemaDescription('Output format: iso, unix, rfc, date, time, datetime, or custom:FORMAT')]
    property Format: string read FFormat write FFormat;
  end;

  TDateTimeFormatTool = class(TAiMCPToolBase<TDateTimeFormatParams>)
  protected
    function ExecuteWithParams(const AParams: TDateTimeFormatParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

function ParseDateTimeInput(const S: string): TDateTime;
function FormatDateTimeOutput(const DT: TDateTime; const Fmt: string): string;

implementation

function ParseDateTimeInput(const S: string): TDateTime;
var
  Trimmed: string;
begin
  Trimmed := Trim(S);
  if SameText(Trimmed, 'now') or (Trimmed = '') then
  begin
    Result := Now;
    Exit;
  end;
  // Try ISO 8601
  if not TryISO8601ToDate(Trimmed, Result, True) then
  begin
    // Try local formats
    if not TryStrToDateTime(Trimmed, Result) then
    begin
      // Try date-only
      if not TryStrToDate(Trimmed, Result) then
        raise Exception.CreateFmt('Cannot parse datetime: "%s". Use ISO 8601 format.', [S]);
    end;
  end;
end;

function FormatDateTimeOutput(const DT: TDateTime; const Fmt: string): string;
var
  F, CustomFmt: string;
  Epoch: Int64;
begin
  F := LowerCase(Trim(Fmt));
  if (F = '') or (F = 'iso') then
    Result := DateToISO8601(DT, True)
  else if F = 'unix' then
  begin
    Epoch := DateTimeToUnix(DT, True);
    Result := IntToStr(Epoch);
  end
  else if F = 'rfc' then
    Result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', DT) + ' +0000'
  else if F = 'date' then
    Result := FormatDateTime('yyyy-mm-dd', DT)
  else if F = 'time' then
    Result := FormatDateTime('hh:nn:ss', DT)
  else if F = 'datetime' then
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', DT)
  else if Copy(F, 1, 7) = 'custom:' then
  begin
    CustomFmt := Copy(Fmt, 8, MaxInt);
    Result := FormatDateTime(CustomFmt, DT);
  end
  else
    raise Exception.CreateFmt('Unknown format: "%s"', [Fmt]);
end;

// -------------------------------------------------------------------------
// TDateTimeNowTool
// -------------------------------------------------------------------------

constructor TDateTimeNowTool.Create;
begin
  inherited;
  FName        := 'datetime_now';
  FDescription := 'Return the current date and time in various formats';
end;

function TDateTimeNowTool.ExecuteWithParams(const AParams: TDateTimeNowParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  DT: TDateTime;
  TZOffset: Double;
  Fmt: string;
begin
  DT := Now;

  // Apply timezone offset if specified
  if Trim(AParams.Timezone) <> '' then
  begin
    TZOffset := StrToFloatDef(Trim(AParams.Timezone), 0);
    DT := DT + TZOffset / 24.0;
  end;

  Fmt := AParams.Format;
  if Fmt = '' then Fmt := 'iso';

  Result := TAiMCPResponseBuilder.New
    .AddText(FormatDateTimeOutput(DT, Fmt))
    .Build;
end;

// -------------------------------------------------------------------------
// TDateTimeAddTool
// -------------------------------------------------------------------------

constructor TDateTimeAddTool.Create;
begin
  inherited;
  FName        := 'datetime_add';
  FDescription := 'Add or subtract a duration from a datetime value. ' +
                  'Use negative Amount to subtract.';
end;

function TDateTimeAddTool.ExecuteWithParams(const AParams: TDateTimeAddParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  DT: TDateTime;
  U:  string;
begin
  DT := ParseDateTimeInput(AParams.DateTime);
  U  := LowerCase(Trim(AParams.&Unit));

  if      U = 'years'   then DT := IncYear(DT, AParams.Amount)
  else if U = 'months'  then DT := IncMonth(DT, AParams.Amount)
  else if U = 'days'    then DT := IncDay(DT, AParams.Amount)
  else if U = 'hours'   then DT := IncHour(DT, AParams.Amount)
  else if U = 'minutes' then DT := IncMinute(DT, AParams.Amount)
  else if U = 'seconds' then DT := IncSecond(DT, AParams.Amount)
  else
    raise Exception.CreateFmt('Unknown unit: "%s". Use: years, months, days, hours, minutes, seconds', [AParams.&Unit]);

  Result := TAiMCPResponseBuilder.New
    .AddText(DateToISO8601(DT, True))
    .Build;
end;

// -------------------------------------------------------------------------
// TDateTimeDiffTool
// -------------------------------------------------------------------------

constructor TDateTimeDiffTool.Create;
begin
  inherited;
  FName        := 'datetime_diff';
  FDescription := 'Calculate the difference between two datetimes in a given unit. ' +
                  'Result = Date2 - Date1 (positive if Date2 is later).';
end;

function TDateTimeDiffTool.ExecuteWithParams(const AParams: TDateTimeDiffParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  DT1, DT2: TDateTime;
  U: string;
  Diff: Int64;
begin
  DT1 := ParseDateTimeInput(AParams.Date1);
  DT2 := ParseDateTimeInput(AParams.Date2);
  U   := LowerCase(Trim(AParams.&Unit));
  if U = '' then U := 'days';

  if      U = 'years'   then Diff := YearsBetween(DT1, DT2)
  else if U = 'months'  then Diff := MonthsBetween(DT1, DT2)
  else if U = 'days'    then Diff := DaysBetween(DT1, DT2)
  else if U = 'hours'   then Diff := HoursBetween(DT1, DT2)
  else if U = 'minutes' then Diff := MinutesBetween(DT1, DT2)
  else if U = 'seconds' then Diff := SecondsBetween(DT1, DT2)
  else
    raise Exception.CreateFmt('Unknown unit: "%s"', [U]);

  // Make signed: positive if DT2 > DT1
  if DT2 < DT1 then Diff := -Diff;

  Result := TAiMCPResponseBuilder.New
    .AddText(Format('%d %s', [Diff, U]))
    .Build;
end;

// -------------------------------------------------------------------------
// TDateTimeFormatTool
// -------------------------------------------------------------------------

constructor TDateTimeFormatTool.Create;
begin
  inherited;
  FName        := 'datetime_format';
  FDescription := 'Parse a datetime string and reformat it in a different format';
end;

function TDateTimeFormatTool.ExecuteWithParams(const AParams: TDateTimeFormatParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  DT: TDateTime;
begin
  DT := ParseDateTimeInput(AParams.DateTime);
  Result := TAiMCPResponseBuilder.New
    .AddText(FormatDateTimeOutput(DT, AParams.Format))
    .Build;
end;

end.
