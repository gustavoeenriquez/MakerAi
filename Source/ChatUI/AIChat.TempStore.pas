unit AIChat.TempStore;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// Per-session temp folder for attachment files and generated thumbnails.
// Folder is created lazily; cleared on Release or component destroy.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils;

type
  TAIChatTempStore = class
  private
    FRoot     : string;   // e.g.  <TempPath>/AIChat/<GUID>/
    FThumbDir : string;   // FRoot + 'thumbs/'
    FCreated  : Boolean;
    procedure EnsureCreated;
    function UniqueBaseName(const AExt: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Copy ASourcePath into the store; returns the new local path.
    // If ASourcePath is already inside the store, returns it unchanged.
    function AddFile(const ASourcePath: string): string;

    // Returns a writable path for a thumbnail derived from ALocalPath.
    // Creates the thumbs/ subdirectory if needed.
    function ThumbnailPath(const ALocalPath: string): string;

    // Delete one file and its associated thumbnail (if any).
    procedure RemoveFile(const ALocalPath: string);

    // Delete the entire temp folder and reset.
    procedure Clear;

    property Root     : string read FRoot;
    property ThumbDir : string read FThumbDir;
  end;

implementation

uses
  System.Hash;


// ── TAIChatTempStore ──────────────────────────────────────────────────────────

constructor TAIChatTempStore.Create;
var
  GUID: TGUID;
begin
  inherited Create;
  CreateGUID(GUID);
  FRoot     := TPath.Combine(TPath.GetTempPath,
                 TPath.Combine('AIChat', GUIDToString(GUID).Replace('{','').Replace('}','').Replace('-','').ToLower));
  FThumbDir := TPath.Combine(FRoot, 'thumbs');
  FCreated  := False;
end;

destructor TAIChatTempStore.Destroy;
begin
  Clear;
  inherited;
end;

procedure TAIChatTempStore.EnsureCreated;
begin
  if FCreated then Exit;
  TDirectory.CreateDirectory(FRoot);
  TDirectory.CreateDirectory(FThumbDir);
  FCreated := True;
end;

function TAIChatTempStore.UniqueBaseName(const AExt: string): string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID).Replace('{','').Replace('}','').Replace('-','').ToLower + AExt;
end;

function TAIChatTempStore.AddFile(const ASourcePath: string): string;
var
  Ext    : string;
  DestPath: string;
begin
  if ASourcePath = '' then Exit('');

  // Already inside the store — no copy needed
  if ASourcePath.StartsWith(FRoot, True) then
    Exit(ASourcePath);

  EnsureCreated;
  Ext      := TPath.GetExtension(ASourcePath).ToLower;
  DestPath := TPath.Combine(FRoot, UniqueBaseName(Ext));
  TFile.Copy(ASourcePath, DestPath, False);
  Result := DestPath;
end;

function TAIChatTempStore.ThumbnailPath(const ALocalPath: string): string;
var
  BaseName: string;
begin
  EnsureCreated;
  BaseName := THashMD5.GetHashString(ALocalPath);
  Result   := TPath.Combine(FThumbDir, BaseName + '.png');
end;

procedure TAIChatTempStore.RemoveFile(const ALocalPath: string);
var
  ThumbPath: string;
begin
  if TFile.Exists(ALocalPath) then
    TFile.Delete(ALocalPath);

  ThumbPath := ThumbnailPath(ALocalPath);
  if TFile.Exists(ThumbPath) then
    TFile.Delete(ThumbPath);
end;

procedure TAIChatTempStore.Clear;
begin
  if FCreated and TDirectory.Exists(FRoot) then
  try
    TDirectory.Delete(FRoot, True);
  except
    // Best-effort: ignore if OS refuses (file locked etc.)
  end;
  FCreated := False;
end;

end.
