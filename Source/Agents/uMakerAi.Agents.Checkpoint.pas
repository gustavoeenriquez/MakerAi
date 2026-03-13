// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Checkpoint durable para TAIAgentManager.
// Permite suspender la ejecucion (human-in-the-loop) y reanudar tras reinicio.
//
// Adaptaciones FPC:
//   - System.JSON → fpjson + jsonparser
//   - TObjectList<T> → specialize TObjectList<T> (generics.collections)
//   - TFile.*/TPath.*/TDirectory.* → SysUtils + FileStream
//   - TryISO8601ToDate → TryParseISO8601 local
//   - TEncoding.UTF8 → no necesario (FPC strings son UTF-8 nativo)

unit uMakerAi.Agents.Checkpoint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  generics.collections,
  fpjson, jsonparser;

type
  // -------------------------------------------------------------------------
  // TAiPendingStep -- paso suspendido esperando aprobacion humana
  // -------------------------------------------------------------------------
  TAiPendingStep = class
  private
    FNodeName:       string;
    FSourceNodeName: string;
    FLinkName:       string;
    FInput:          string;
    FStatus:         string;
    FSuspendReason:  string;
    FSuspendContext: string;
    FCreatedAt:      TDateTime;
  public
    constructor Create(const ANodeName, ASourceNodeName, ALinkName,
                       AInput, AStatus, AReason, AContext: string);
    function ToJSON: TJSONObject;
    class function FromJSON(AJson: TJSONObject): TAiPendingStep;

    property NodeName:       string    read FNodeName;
    property SourceNodeName: string    read FSourceNodeName;
    property LinkName:       string    read FLinkName;
    property Input:          string    read FInput;
    property Status:         string    read FStatus;
    property SuspendReason:  string    read FSuspendReason;
    property SuspendContext: string    read FSuspendContext;
    property CreatedAt:      TDateTime read FCreatedAt;
  end;

  // -------------------------------------------------------------------------
  // TAiCheckpointSnapshot -- estado completo de una ejecucion
  // -------------------------------------------------------------------------
  TAiPendingStepList = specialize TObjectList<TAiPendingStep>;

  TAiCheckpointSnapshot = class
  private
    FThreadID:     string;
    FCheckpointID: Integer;
    FGraphID:      string;
    FCreatedAt:    TDateTime;
    FBlackboard:   TJSONObject;  // owned
    FNodeStates:   TJSONObject;  // owned
    FLinkStates:   TJSONObject;  // owned
    FPendingSteps: TAiPendingStepList; // owned
  public
    constructor Create;
    destructor Destroy; override;
    function ToJSON: TJSONObject;
    class function FromJSON(AJson: TJSONObject): TAiCheckpointSnapshot;

    property ThreadID:     string    read FThreadID     write FThreadID;
    property CheckpointID: Integer   read FCheckpointID write FCheckpointID;
    property GraphID:      string    read FGraphID      write FGraphID;
    property CreatedAt:    TDateTime read FCreatedAt    write FCreatedAt;
    property Blackboard:   TJSONObject read FBlackboard  write FBlackboard;
    property NodeStates:   TJSONObject read FNodeStates  write FNodeStates;
    property LinkStates:   TJSONObject read FLinkStates  write FLinkStates;
    property PendingSteps: TAiPendingStepList read FPendingSteps;
  end;

  // -------------------------------------------------------------------------
  // IAiCheckpointer -- contrato de persistencia; implementaciones thread-safe
  // -------------------------------------------------------------------------
  IAiCheckpointer = interface
  ['{A3F2C1D4-8B5E-4F7A-9C3D-2E1B0F6A8D4C}']
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot; // caller frees
    function  GetActiveThreadIDs: TStringDynArray;
    procedure DeleteCheckpoint(const AThreadID: string);
  end;

  // -------------------------------------------------------------------------
  // TAiNullCheckpointer -- no-op: comportamiento sin persistencia
  // -------------------------------------------------------------------------
  TAiNullCheckpointer = class(TInterfacedObject, IAiCheckpointer)
  public
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
    function  GetActiveThreadIDs: TStringDynArray;
    procedure DeleteCheckpoint(const AThreadID: string);
  end;

  // -------------------------------------------------------------------------
  // TAiFileCheckpointer -- JSON en disco: <dir>/<GUID>.checkpoint.json
  // -------------------------------------------------------------------------
  TAiFileCheckpointer = class(TInterfacedObject, IAiCheckpointer)
  private
    FDirectory: string;
    FLock:      TCriticalSection;
    function BuildFilePath(const AThreadID: string): string;
  public
    constructor Create(const ADirectory: string);
    destructor  Destroy; override;
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
    function  GetActiveThreadIDs: TStringDynArray;
    procedure DeleteCheckpoint(const AThreadID: string);
    property Directory: string read FDirectory;
  end;

implementation

// ---------------------------------------------------------------------------
// Helpers de archivo
// ---------------------------------------------------------------------------

function ReadTextFile(const APath: string): string;
var
  SS: TStringStream;
  FS: TFileStream;
begin
  SS := TStringStream.Create('');
  try
    FS := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
    try
      SS.CopyFrom(FS, 0);
    finally
      FS.Free;
    end;
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure WriteTextFile(const APath, AContent: string);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(AContent);
  try
    SS.SaveToFile(APath);
  finally
    SS.Free;
  end;
end;

// Parsea 'yyyy-mm-ddThh:nn:ss' en TDateTime
function TryParseISO8601(const S: string; out ADate: TDateTime): Boolean;
var
  Y, M, D, H, Mi, Se: Integer;
begin
  Result := False;
  if Length(S) < 19 then Exit;
  try
    Y  := StrToInt(Copy(S,  1, 4));
    M  := StrToInt(Copy(S,  6, 2));
    D  := StrToInt(Copy(S,  9, 2));
    H  := StrToInt(Copy(S, 12, 2));
    Mi := StrToInt(Copy(S, 15, 2));
    Se := StrToInt(Copy(S, 18, 2));
    ADate  := EncodeDate(Y, M, D) + EncodeTime(H, Mi, Se, 0);
    Result := True;
  except
    Result := False;
  end;
end;

// ---------------------------------------------------------------------------
// TAiPendingStep
// ---------------------------------------------------------------------------

constructor TAiPendingStep.Create(const ANodeName, ASourceNodeName, ALinkName,
  AInput, AStatus, AReason, AContext: string);
begin
  inherited Create;
  FNodeName       := ANodeName;
  FSourceNodeName := ASourceNodeName;
  FLinkName       := ALinkName;
  FInput          := AInput;
  FStatus         := AStatus;
  FSuspendReason  := AReason;
  FSuspendContext := AContext;
  FCreatedAt      := Now;
end;

function TAiPendingStep.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('nodeName',       FNodeName);
  Result.Add('sourceNodeName', FSourceNodeName);
  Result.Add('linkName',       FLinkName);
  Result.Add('input',          FInput);
  Result.Add('status',         FStatus);
  Result.Add('suspendReason',  FSuspendReason);
  Result.Add('suspendContext', FSuspendContext);
  Result.Add('createdAt',      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));
end;

class function TAiPendingStep.FromJSON(AJson: TJSONObject): TAiPendingStep;
var
  LDateStr: string;
  LDate:    TDateTime;
begin
  Result := TAiPendingStep.Create(
    AJson.Get('nodeName',       ''),
    AJson.Get('sourceNodeName', ''),
    AJson.Get('linkName',       ''),
    AJson.Get('input',          ''),
    AJson.Get('status',         ''),
    AJson.Get('suspendReason',  ''),
    AJson.Get('suspendContext', ''));

  LDateStr := AJson.Get('createdAt', '');
  if TryParseISO8601(LDateStr, LDate) then
    Result.FCreatedAt := LDate;
end;

// ---------------------------------------------------------------------------
// TAiCheckpointSnapshot
// ---------------------------------------------------------------------------

constructor TAiCheckpointSnapshot.Create;
begin
  inherited;
  FPendingSteps := TAiPendingStepList.Create(True);
  FCreatedAt    := Now;
end;

destructor TAiCheckpointSnapshot.Destroy;
begin
  FPendingSteps.Free;
  FBlackboard.Free;
  FNodeStates.Free;
  FLinkStates.Free;
  inherited;
end;

function TAiCheckpointSnapshot.ToJSON: TJSONObject;
var
  LSteps: TJSONArray;
  LStep:  TAiPendingStep;
  I:      Integer;
begin
  Result := TJSONObject.Create;
  Result.Add('threadID',     FThreadID);
  Result.Add('checkpointID', FCheckpointID);
  Result.Add('graphID',      FGraphID);
  Result.Add('createdAt',    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));

  if Assigned(FBlackboard) then
    Result.Add('blackboard', FBlackboard.Clone as TJSONObject)
  else
    Result.Add('blackboard', TJSONObject.Create);

  if Assigned(FNodeStates) then
    Result.Add('nodeStates', FNodeStates.Clone as TJSONObject)
  else
    Result.Add('nodeStates', TJSONObject.Create);

  if Assigned(FLinkStates) then
    Result.Add('linkStates', FLinkStates.Clone as TJSONObject)
  else
    Result.Add('linkStates', TJSONObject.Create);

  LSteps := TJSONArray.Create;
  for I := 0 to FPendingSteps.Count - 1 do
  begin
    LStep := FPendingSteps[I];
    LSteps.Add(LStep.ToJSON);
  end;
  Result.Add('pendingSteps', LSteps);
end;

class function TAiCheckpointSnapshot.FromJSON(AJson: TJSONObject): TAiCheckpointSnapshot;
var
  LStepsArr: TJSONArray;
  LBB, LNS, LLS: TJSONObject;
  LDateStr: string;
  LDate:    TDateTime;
  I:        Integer;
begin
  Result := TAiCheckpointSnapshot.Create;
  try
    Result.FThreadID     := AJson.Get('threadID',     '');
    Result.FCheckpointID := AJson.Get('checkpointID', 0);
    Result.FGraphID      := AJson.Get('graphID',      '');

    LDateStr := AJson.Get('createdAt', '');
    if TryParseISO8601(LDateStr, LDate) then
      Result.FCreatedAt := LDate;

    LBB := AJson.Find('blackboard') as TJSONObject;
    if Assigned(LBB) then
      Result.FBlackboard := LBB.Clone as TJSONObject;

    LNS := AJson.Find('nodeStates') as TJSONObject;
    if Assigned(LNS) then
      Result.FNodeStates := LNS.Clone as TJSONObject;

    LLS := AJson.Find('linkStates') as TJSONObject;
    if Assigned(LLS) then
      Result.FLinkStates := LLS.Clone as TJSONObject;

    LStepsArr := AJson.Find('pendingSteps') as TJSONArray;
    if Assigned(LStepsArr) then
      for I := 0 to LStepsArr.Count - 1 do
        if LStepsArr.Items[I] is TJSONObject then
          Result.FPendingSteps.Add(
            TAiPendingStep.FromJSON(TJSONObject(LStepsArr.Items[I])));
  except
    Result.Free;
    raise;
  end;
end;

// ---------------------------------------------------------------------------
// TAiNullCheckpointer
// ---------------------------------------------------------------------------

procedure TAiNullCheckpointer.SaveCheckpoint(const AThreadID: string;
  ASnapshot: TAiCheckpointSnapshot);
begin
  // no-op
end;

function TAiNullCheckpointer.LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
begin
  Result := nil;
end;

function TAiNullCheckpointer.GetActiveThreadIDs: TStringDynArray;
begin
  SetLength(Result, 0);
end;

procedure TAiNullCheckpointer.DeleteCheckpoint(const AThreadID: string);
begin
  // no-op
end;

// ---------------------------------------------------------------------------
// TAiFileCheckpointer
// ---------------------------------------------------------------------------

constructor TAiFileCheckpointer.Create(const ADirectory: string);
begin
  inherited Create;
  FDirectory := ADirectory;
  FLock      := TCriticalSection.Create;
  if not DirectoryExists(FDirectory) then
    ForceDirectories(FDirectory);
end;

destructor TAiFileCheckpointer.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TAiFileCheckpointer.BuildFilePath(const AThreadID: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FDirectory) + AThreadID + '.checkpoint.json';
end;

procedure TAiFileCheckpointer.SaveCheckpoint(const AThreadID: string;
  ASnapshot: TAiCheckpointSnapshot);
var
  LJson:    TJSONObject;
  LContent: string;
  LPath:    string;
begin
  LJson := ASnapshot.ToJSON;
  try
    LContent := LJson.AsJSON;
  finally
    LJson.Free;
  end;

  LPath := BuildFilePath(AThreadID);
  FLock.Enter;
  try
    WriteTextFile(LPath, LContent);
  finally
    FLock.Leave;
  end;
end;

function TAiFileCheckpointer.LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
var
  LPath:    string;
  LContent: string;
  LJson:    TJSONData;
begin
  Result   := nil;
  LPath    := BuildFilePath(AThreadID);

  FLock.Enter;
  try
    if not FileExists(LPath) then
      Exit;
    LContent := ReadTextFile(LPath);
  finally
    FLock.Leave;
  end;

  LJson := GetJSON(LContent);
  if Assigned(LJson) then
  try
    if LJson is TJSONObject then
      Result := TAiCheckpointSnapshot.FromJSON(TJSONObject(LJson));
  finally
    LJson.Free;
  end;
end;

function TAiFileCheckpointer.GetActiveThreadIDs: TStringDynArray;
var
  SR:   TSearchRec;
  List: TStringList;
  Base: string;
  I:    Integer;
begin
  List := TStringList.Create;
  try
    FLock.Enter;
    try
      if FindFirst(IncludeTrailingPathDelimiter(FDirectory) + '*.checkpoint.json',
                   faAnyFile and not faDirectory, SR) = 0 then
      try
        repeat
          // SR.Name = 'guid.checkpoint.json' → strip extensions twice
          Base := SR.Name;
          Base := ChangeFileExt(Base, '');  // → 'guid.checkpoint'
          Base := ChangeFileExt(Base, '');  // → 'guid'
          if Base <> '' then
            List.Add(Base);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    finally
      FLock.Leave;
    end;

    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := List[I];
  finally
    List.Free;
  end;
end;

procedure TAiFileCheckpointer.DeleteCheckpoint(const AThreadID: string);
var
  LPath: string;
begin
  LPath := BuildFilePath(AThreadID);
  FLock.Enter;
  try
    if FileExists(LPath) then
      DeleteFile(LPath);
  finally
    FLock.Leave;
  end;
end;

end.
