// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Github: https://github.com/gustavoeenriquez/

unit uMakerAi.Agents.Checkpoint.DB;

// TAiDatabaseCheckpointer: implementaci?n de IAiCheckpointer sobre FireDAC.
//
// Compatible con cualquier base de datos que soporte FireDAC:
// PostgreSQL, SQLite, MySQL, Firebird, SQL Server, etc.
//
// Tabla requerida (crear con CreateSchema o ejecutar el DDL manualmente):
//
//   agent_checkpoints (
//     thread_id     VARCHAR(255) PRIMARY KEY,
//     snapshot      TEXT NOT NULL,     -- JSON de TAiCheckpointSnapshot
//     checkpoint_id INTEGER NOT NULL,
//     created_at    TIMESTAMP NOT NULL,
//     updated_at    TIMESTAMP NOT NULL
//   )
//
// Uso t?pico:
//
//   var FCP := TAiDatabaseCheckpointer.Create(FDConnection1);
//   FCP.CreateSchema;                         // solo la primera vez
//   AgentManager.Checkpointer := FCP;
//
//   // Al arrancar la app: recuperar threads suspendidos
//   for ThreadID in AgentManager.GetActiveThreads do
//     ShowMessage('Pendiente: ' + ThreadID);
//
//   // Reanudar cuando el humano aprueba
//   AgentManager.ResumeThread(ThreadID, 'NombreNodo', 'Aprobado');

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Generics.Collections, System.SyncObjs,
  Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DApt,
  uMakerAi.Agents.Checkpoint;

type
  // -------------------------------------------------------------------------
  // TAiDatabaseCheckpointer
  //   - TInterfacedObject (misma base que TAiFileCheckpointer)
  //   - Thread-safe: TCriticalSection protege accesos concurrentes
  //   - El caller gestiona el ciclo de vida de FConnection
  // -------------------------------------------------------------------------
  TAiDatabaseCheckpointer = class(TInterfacedObject, IAiCheckpointer)
  private
    FConnection: TFDConnection;
    FTableName:  string;
    FLock:       TCriticalSection;
    function NewQuery: TFDQuery;
  public
    constructor Create(AConnection: TFDConnection;
                       const ATableName: string = 'agent_checkpoints');
    destructor  Destroy; override;

    // Crea la tabla si no existe. Llamar una vez al iniciar la aplicaci?n.
    procedure CreateSchema;

    // IAiCheckpointer
    procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
    function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot; // caller frees
    function  GetActiveThreadIDs: TArray<string>;
    procedure DeleteCheckpoint(const AThreadID: string);

    property Connection: TFDConnection read FConnection;
    property TableName:  string        read FTableName;
  end;

implementation

{ TAiDatabaseCheckpointer }

constructor TAiDatabaseCheckpointer.Create(AConnection: TFDConnection;
  const ATableName: string);
begin
  inherited Create;
  if not Assigned(AConnection) then
    raise Exception.Create('TAiDatabaseCheckpointer: Connection no puede ser nil.');
  FConnection := AConnection;
  FTableName  := ATableName;
  FLock       := TCriticalSection.Create;
end;

destructor TAiDatabaseCheckpointer.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TAiDatabaseCheckpointer.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('TAiDatabaseCheckpointer: sin conexi?n asignada.');
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

procedure TAiDatabaseCheckpointer.CreateSchema;
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS ' + FTableName + ' (' +
      '  thread_id     VARCHAR(255) NOT NULL,' +
      '  snapshot      TEXT         NOT NULL,' +
      '  checkpoint_id INTEGER      NOT NULL DEFAULT 0,' +
      '  created_at    TIMESTAMP    NOT NULL,' +
      '  updated_at    TIMESTAMP    NOT NULL,' +
      '  CONSTRAINT pk_' + FTableName + ' PRIMARY KEY (thread_id)' +
      ')';
    FLock.Enter;
    try
      Q.ExecSQL;
    finally
      FLock.Leave;
    end;
  finally
    Q.Free;
  end;
end;

procedure TAiDatabaseCheckpointer.SaveCheckpoint(const AThreadID: string;
  ASnapshot: TAiCheckpointSnapshot);
var
  Q:        TFDQuery;
  LJson:    TJSONObject;
  LContent: string;
  LNow:     TDateTime;
begin
  // Serializar fuera del lock (operaci?n costosa)
  LJson := ASnapshot.ToJSON;
  try
    LContent := LJson.ToString;
  finally
    LJson.Free;
  end;

  LNow := Now;
  Q    := NewQuery;
  try
    FLock.Enter;
    try
      // Borrar si existe (UPSERT gen?rico compatible con todos los drivers)
      Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE thread_id = :tid';
      Q.ParamByName('tid').AsString := AThreadID;
      Q.ExecSQL;

      // Insertar nueva versi?n
      Q.SQL.Text :=
        'INSERT INTO ' + FTableName +
        ' (thread_id, snapshot, checkpoint_id, created_at, updated_at)' +
        ' VALUES (:tid, :snap, :cid, :cat, :uat)';
      Q.ParamByName('tid').AsString   := AThreadID;
      Q.ParamByName('snap').AsString  := LContent;
      Q.ParamByName('cid').AsInteger  := ASnapshot.CheckpointID;
      Q.ParamByName('cat').AsDateTime := ASnapshot.CreatedAt;
      Q.ParamByName('uat').AsDateTime := LNow;
      Q.ExecSQL;
    finally
      FLock.Leave;
    end;
  finally
    Q.Free;
  end;
end;

function TAiDatabaseCheckpointer.LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
var
  Q:        TFDQuery;
  LContent: string;
  LJson:    TJSONValue;
begin
  Result := nil;
  Q := NewQuery;
  try
    Q.SQL.Text :=
      'SELECT snapshot FROM ' + FTableName + ' WHERE thread_id = :tid';
    Q.ParamByName('tid').AsString := AThreadID;

    FLock.Enter;
    try
      Q.Open;
      if Q.IsEmpty then
        Exit;
      LContent := Q.FieldByName('snapshot').AsString;
    finally
      FLock.Leave;
    end;
  finally
    Q.Free;
  end;

  // Parsear fuera del lock
  LJson := TJSONObject.ParseJSONValue(LContent);
  if Assigned(LJson) then
  try
    if LJson is TJSONObject then
      Result := TAiCheckpointSnapshot.FromJSON(LJson as TJSONObject);
  finally
    LJson.Free;
  end;
end;

function TAiDatabaseCheckpointer.GetActiveThreadIDs: TArray<string>;
var
  Q:     TFDQuery;
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    Q := NewQuery;
    try
      Q.SQL.Text := 'SELECT thread_id FROM ' + FTableName +
                    ' ORDER BY updated_at DESC';
      FLock.Enter;
      try
        Q.Open;
        while not Q.Eof do
        begin
          LList.Add(Q.FieldByName('thread_id').AsString);
          Q.Next;
        end;
      finally
        FLock.Leave;
      end;
    finally
      Q.Free;
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TAiDatabaseCheckpointer.DeleteCheckpoint(const AThreadID: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE thread_id = :tid';
    Q.ParamByName('tid').AsString := AThreadID;
    FLock.Enter;
    try
      Q.ExecSQL;
    finally
      FLock.Leave;
    end;
  finally
    Q.Free;
  end;
end;

end.
