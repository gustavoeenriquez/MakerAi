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

unit uMakerAi.Agents.Checkpoint;

// Durable execution checkpoint for TAIAgentManager.
// Allows suspending agent execution (human-in-the-loop) and resuming after
// a restart, following the LangGraph checkpoint pattern.
//
// IAiCheckpointer contract:
//   - All implementations MUST be thread-safe.
//   - LoadCheckpoint returns a new object; CALLER must free it.
//   - SaveCheckpoint does NOT take ownership of ASnapshot.

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Generics.Collections, System.SyncObjs, System.IOUtils,
  System.DateUtils;

type
  // -------------------------------------------------------------------------
  // TAiPendingStep  -- un paso suspendido esperando aprobaci?n humana
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
  // TAiCheckpointSnapshot  -- estado completo de una ejecuci?n en un instante
  // -------------------------------------------------------------------------
  TAiCheckpointSnapshot = class
  private
    FThreadID:     string;
    FCheckpointID: Integer;
    FGraphID:      string;
    FCreatedAt:    TDateTime;
    FBlackboard:   TJSONObject;  // owned
    FNodeStates:   TJSONObject;  // owned
    FLinkStates:   TJSONObject;  // owned
    FPendingSteps: TObjectList<TAiPendingStep>; // owned
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
    property PendingSteps: TObjectList<TAiPendingStep> read FPendingSteps;
  end;

  // -------------------------------------------------------------------------
  // IAiCheckpointer  -- contrato de persistencia; implementaciones thread-safe
  // -------------------------------------------------------------------------
  IAiCheckpointer = interface
  ['{A3F2C1D4-8B5E-4F7A-9C3D-2E1B0F6A8D4C}']
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot; // caller frees
    function  GetActiveThreadIDs: TArray<string>;
    procedure DeleteCheckpoint(const AThreadID: string);
  end;

  // -------------------------------------------------------------------------
  // TAiNullCheckpointer  -- no-op: mantiene comportamiento sin persistencia
  // -------------------------------------------------------------------------
  TAiNullCheckpointer = class(TInterfacedObject, IAiCheckpointer)
  public
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
    function  GetActiveThreadIDs: TArray<string>;
    procedure DeleteCheckpoint(const AThreadID: string);
  end;

  // -------------------------------------------------------------------------
  // TAiFileCheckpointer  -- JSON en disco: <dir>/<GUID>.checkpoint.json
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
    function  GetActiveThreadIDs: TArray<string>;
    procedure DeleteCheckpoint(const AThreadID: string);
    property Directory: string read FDirectory;
  end;

implementation

{ TAiPendingStep }

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
  Result.AddPair('nodeName',       FNodeName);
  Result.AddPair('sourceNodeName', FSourceNodeName);
  Result.AddPair('linkName',       FLinkName);
  Result.AddPair('input',          FInput);
  Result.AddPair('status',         FStatus);
  Result.AddPair('suspendReason',  FSuspendReason);
  Result.AddPair('suspendContext', FSuspendContext);
  Result.AddPair('createdAt',      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));
end;

class function TAiPendingStep.FromJSON(AJson: TJSONObject): TAiPendingStep;
var
  LDateStr: string;
  LDate:    TDateTime;
begin
  Result := TAiPendingStep.Create(
    AJson.GetValue<string>('nodeName',       ''),
    AJson.GetValue<string>('sourceNodeName', ''),
    AJson.GetValue<string>('linkName',       ''),
    AJson.GetValue<string>('input',          ''),
    AJson.GetValue<string>('status',         ''),
    AJson.GetValue<string>('suspendReason',  ''),
    AJson.GetValue<string>('suspendContext', ''));

  LDateStr := AJson.GetValue<string>('createdAt', '');
  if (LDateStr <> '') and TryISO8601ToDate(LDateStr, LDate, False) then
    Result.FCreatedAt := LDate;
end;

{ TAiCheckpointSnapshot }

constructor TAiCheckpointSnapshot.Create;
begin
  inherited;
  FPendingSteps := TObjectList<TAiPendingStep>.Create(True);
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
begin
  Result := TJSONObject.Create;
  Result.AddPair('threadID',     FThreadID);
  Result.AddPair('checkpointID', TJSONNumber.Create(FCheckpointID));
  Result.AddPair('graphID',      FGraphID);
  Result.AddPair('createdAt',    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));

  if Assigned(FBlackboard) then
    Result.AddPair('blackboard', FBlackboard.Clone as TJSONObject)
  else
    Result.AddPair('blackboard', TJSONObject.Create);

  if Assigned(FNodeStates) then
    Result.AddPair('nodeStates', FNodeStates.Clone as TJSONObject)
  else
    Result.AddPair('nodeStates', TJSONObject.Create);

  if Assigned(FLinkStates) then
    Result.AddPair('linkStates', FLinkStates.Clone as TJSONObject)
  else
    Result.AddPair('linkStates', TJSONObject.Create);

  LSteps := TJSONArray.Create;
  for LStep in FPendingSteps do
    LSteps.Add(LStep.ToJSON);
  Result.AddPair('pendingSteps', LSteps);
end;

class function TAiCheckpointSnapshot.FromJSON(AJson: TJSONObject): TAiCheckpointSnapshot;
var
  LStepsArr: TJSONArray;
  LStepVal:  TJSONValue;
  LBB, LNS, LLS: TJSONObject;
  LDateStr: string;
  LDate:    TDateTime;
begin
  Result := TAiCheckpointSnapshot.Create;
  try
    Result.FThreadID     := AJson.GetValue<string>('threadID',     '');
    Result.FCheckpointID := AJson.GetValue<Integer>('checkpointID', 0);
    Result.FGraphID      := AJson.GetValue<string>('graphID',      '');

    LDateStr := AJson.GetValue<string>('createdAt', '');
    if (LDateStr <> '') and TryISO8601ToDate(LDateStr, LDate, False) then
      Result.FCreatedAt := LDate;

    LBB := AJson.GetValue('blackboard') as TJSONObject;
    if Assigned(LBB) then
      Result.FBlackboard := LBB.Clone as TJSONObject;

    LNS := AJson.GetValue('nodeStates') as TJSONObject;
    if Assigned(LNS) then
      Result.FNodeStates := LNS.Clone as TJSONObject;

    LLS := AJson.GetValue('linkStates') as TJSONObject;
    if Assigned(LLS) then
      Result.FLinkStates := LLS.Clone as TJSONObject;

    LStepsArr := AJson.GetValue('pendingSteps') as TJSONArray;
    if Assigned(LStepsArr) then
      for LStepVal in LStepsArr do
        if LStepVal is TJSONObject then
          Result.FPendingSteps.Add(TAiPendingStep.FromJSON(LStepVal as TJSONObject));
  except
    Result.Free;
    raise;
  end;
end;

{ TAiNullCheckpointer }

procedure TAiNullCheckpointer.SaveCheckpoint(const AThreadID: string;
  ASnapshot: TAiCheckpointSnapshot);
begin
  // No-op: no persistence
end;

function TAiNullCheckpointer.LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
begin
  Result := nil;
end;

function TAiNullCheckpointer.GetActiveThreadIDs: TArray<string>;
begin
  SetLength(Result, 0);
end;

procedure TAiNullCheckpointer.DeleteCheckpoint(const AThreadID: string);
begin
  // No-op
end;

{ TAiFileCheckpointer }

constructor TAiFileCheckpointer.Create(const ADirectory: string);
begin
  inherited Create;
  FDirectory := ADirectory;
  FLock      := TCriticalSection.Create;
  if not TDirectory.Exists(FDirectory) then
    TDirectory.CreateDirectory(FDirectory);
end;

destructor TAiFileCheckpointer.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TAiFileCheckpointer.BuildFilePath(const AThreadID: string): string;
begin
  Result := TPath.Combine(FDirectory, AThreadID + '.checkpoint.json');
end;

procedure TAiFileCheckpointer.SaveCheckpoint(const AThreadID: string;
  ASnapshot: TAiCheckpointSnapshot);
var
  LJson:    TJSONObject;
  LContent: string;
  LPath:    string;
begin
  // Build JSON outside the lock (expensive serialization)
  LJson := ASnapshot.ToJSON;
  try
    LContent := LJson.ToString;
  finally
    LJson.Free;
  end;

  LPath := BuildFilePath(AThreadID);
  FLock.Enter;
  try
    TFile.WriteAllText(LPath, LContent, TEncoding.UTF8);
  finally
    FLock.Leave;
  end;
end;

function TAiFileCheckpointer.LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
var
  LPath:    string;
  LContent: string;
  LJson:    TJSONValue;
begin
  Result   := nil;
  LPath    := BuildFilePath(AThreadID);

  // Short critical section: only the file read
  FLock.Enter;
  try
    if not TFile.Exists(LPath) then
      Exit;
    LContent := TFile.ReadAllText(LPath, TEncoding.UTF8);
  finally
    FLock.Leave;
  end;

  // Parse outside the lock
  LJson := TJSONObject.ParseJSONValue(LContent);
  if Assigned(LJson) then
  try
    if LJson is TJSONObject then
      Result := TAiCheckpointSnapshot.FromJSON(LJson as TJSONObject);
  finally
    LJson.Free;
  end;
end;

function TAiFileCheckpointer.GetActiveThreadIDs: TArray<string>;
var
  LFiles: TArray<string>;
  LList:  TList<string>;
  LFile:  string;
  LBase:  string;
begin
  LList := TList<string>.Create;
  try
    FLock.Enter;
    try
      LFiles := TDirectory.GetFiles(FDirectory, '*.checkpoint.json');
    finally
      FLock.Leave;
    end;

    // Extract GUID from "<GUID>.checkpoint.json"
    // GetFileNameWithoutExtension twice: .json -> .checkpoint -> GUID
    for LFile in LFiles do
    begin
      LBase := TPath.GetFileNameWithoutExtension(LFile);    // "guid.checkpoint"
      LBase := TPath.GetFileNameWithoutExtension(LBase);    // "guid"
      if LBase <> '' then
        LList.Add(LBase);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TAiFileCheckpointer.DeleteCheckpoint(const AThreadID: string);
var
  LPath: string;
begin
  LPath := BuildFilePath(AThreadID);
  FLock.Enter;
  try
    if TFile.Exists(LPath) then
      TFile.Delete(LPath);
  finally
    FLock.Leave;
  end;
end;

end.
