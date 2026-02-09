unit uRegularExpressionsHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, RegExpr;
  {$ELSE}
  System.SysUtils, System.RegularExpressions;
  {$ENDIF}

type
{$IFNDEF FPC}
  TRegEx = System.RegularExpressions.TRegEx;
{$ENDIF}

{$IFDEF FPC}
  TGroup = record
  private
    FValue: string;
    FSuccess: Boolean;
    FIndex: Integer;
    FLength: Integer;
  public
    property Value: string read FValue;
    property Success: Boolean read FSuccess;
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
  end;

  TGroupCollection = class
  private
    FItems: array of TGroup;
    function GetItem(Index: Integer): TGroup;
    function GetCount: Integer;
  public
    constructor Create(ARegExpr: TRegExpr);
    property Item[Index: Integer]: TGroup read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TMatch = record
  private
    FGroups: TGroupCollection;
    FValue: string;
    FSuccess: Boolean;
    FIndex: Integer;
    FLength: Integer;
  public
    property Groups: TGroupCollection read FGroups;
    property Value: string read FValue;
    property Success: Boolean read FSuccess;
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
  end;

  TRegEx = record
  private
    class function InternalReplace(const Input, Pattern, Replacement: string; Flags: TRegExprModifiers): string; static;
  public
    class function Replace(const Input, Pattern, Replacement: string): string; overload; static;
    class function Replace(const Input, Pattern, Replacement: string; Options: TRegExprModifiers): string; overload; static;
    class function IsMatch(const Input, Pattern: string): Boolean; overload; static;
    class function IsMatch(const Input, Pattern: string; Options: TRegExprModifiers): Boolean; overload; static;
    class function Match(const Input, Pattern: string): TMatch; overload; static;
    class function Match(const Input, Pattern: string; Options: TRegExprModifiers): TMatch; overload; static;
    
    // Instance method shim if needed, or stick to static
    // Delphi TRegEx has both.
    // For now static Match is enough for CodeExtractor usage.
  end;

  // Mapa de opciones para compatibilidad (Delphi usa TRegExOptions enum, FPC usa params o RegExprModifiers)
  // Definimos TRegExOptions para FPC como set de flags simulados si fuera necesario.
  // Por ahora DmGenerator solo usa Replace simple.
{$ENDIF}

implementation

{$IFDEF FPC}
class function TRegEx.InternalReplace(const Input, Pattern, Replacement: string; Flags: TRegExprModifiers): string;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.ModifierStr := ''; // Clear defaults
    // Set modifiers based on Flags?
    // FPC TRegExpr by default is CaseSensitive.
    // If we wanted case insensitive: RE.ModifierI := True;
    
    // To mimic Delphi's default behavior?
    // Delphi's TRegEx.Replace(Input, Pattern, Replacement) is standard.
    
    RE.Expression := Pattern;
    // TRegEx.Replace in Delphi replaces ALL occurrences by default?
    // Yes, TRegEx.Replace is global by default in usage, or implicitly creates a regex and calls Replace.
    // FPC Replace function:
    Result := RE.Replace(Input, Replacement, True); // Exec(Input) then Replace?
    
    // RegExpr.Replace behavior:
    // It replaces occurrences in the input string.
    // Actually, TRegExpr.Replace(const InputStr: RegExprString; const ReplaceStr: RegExprString; UseSubstitution: boolean): RegExprString;
    
  finally
    RE.Free;
  end;
end;

class function TRegEx.Replace(const Input, Pattern, Replacement: string): string;
begin
  // Default behavior
  Result := InternalReplace(Input, Pattern, Replacement, Default(TRegExprModifiers));
end;

class function TRegEx.Replace(const Input, Pattern, Replacement: string; Options: TRegExprModifiers): string;
begin
  Result := InternalReplace(Input, Pattern, Replacement, Options);
end;

class function TRegEx.IsMatch(const Input, Pattern: string): Boolean;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.Expression := Pattern;
    Result := RE.Exec(Input);
  finally
    RE.Free;
  end;
end;

class function TRegEx.IsMatch(const Input, Pattern: string; Options: TRegExprModifiers): Boolean;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.Expression := Pattern;
    // Apply options if needed
    Result := RE.Exec(Input);
  finally
    RE.Free;
  end;
end;

class function TRegEx.Match(const Input, Pattern: string): TMatch;
begin
  Result := Match(Input, Pattern, Default(TRegExprModifiers));
end;

class function TRegEx.Match(const Input, Pattern: string; Options: TRegExprModifiers): TMatch;
var
  RE: TRegExpr;
begin
  RE := TRegExpr.Create;
  try
    RE.Expression := Pattern;
    // Options...
    if RE.Exec(Input) then
    begin
      Result.FSuccess := True;
      Result.FValue := RE.Match[0];
      Result.FIndex := RE.MatchPos[1]; // FPC 1-based usually for pos? Checking docs...
      // TRegExpr MatchPos[0] is the whole match.
      Result.FIndex := RE.MatchPos[0];
      Result.FLength := RE.MatchLen[0];
      Result.FGroups := TGroupCollection.Create(RE);
    end
    else
    begin
      Result.FSuccess := False;
      Result.FValue := '';
      Result.FIndex := 0;
      Result.FLength := 0;
      Result.FGroups := TGroupCollection.Create(nil); // Empty
    end;
  except
    RE.Free;
    raise;
  end;
end;

{ TGroupCollection }

constructor TGroupCollection.Create(ARegExpr: TRegExpr);
var
  i: Integer;
begin
  if ARegExpr = nil then
  begin
    SetLength(FItems, 0);
  end
  else
  begin
    // SubExprMatchCount returns count of subexpressions (groups).
    // Match[0] is the whole match. Match[1] is first group.
    SetLength(FItems, ARegExpr.SubExprMatchCount + 1);
    for i := 0 to ARegExpr.SubExprMatchCount do
    begin
      FItems[i].FValue := ARegExpr.Match[i];
      FItems[i].FIndex := ARegExpr.MatchPos[i];
      FItems[i].FLength := ARegExpr.MatchLen[i];
      FItems[i].FSuccess := (ARegExpr.MatchPos[i] > 0);
    end;
    ARegExpr.Free; 
  end;
end;

function TGroupCollection.GetItem(Index: Integer): TGroup;
begin
  if (Index >= 0) and (Index < Length(FItems)) then
    Result := FItems[Index]
  else
  begin
    Result.FSuccess := False;
    Result.FValue := '';
  end;
end;

function TGroupCollection.GetCount: Integer;
begin
  Result := Length(FItems);
end;
{$ENDIF}
end.

