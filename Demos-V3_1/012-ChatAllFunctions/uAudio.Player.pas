unit uAudio.Player;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils;

type
  TAudioPlayer = class
  private
    FCurrentFile: string;
    FIsPlaying  : Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Play(const AFilePath, AMimeType: string): Boolean;
    procedure Stop;
    function  IsPlaying: Boolean;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.ShellAPI;
  {$ELSE}
  FMX.Dialogs;
  {$ENDIF}

constructor TAudioPlayer.Create;
begin
  inherited;
  FIsPlaying := False;
end;

destructor TAudioPlayer.Destroy;
begin
  Stop;
  inherited;
end;

function TAudioPlayer.Play(const AFilePath, AMimeType: string): Boolean;
begin
  Result := False;
  if not TFile.Exists(AFilePath) then Exit;
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(AFilePath), nil, nil, SW_SHOWNORMAL);
  FCurrentFile := AFilePath;
  FIsPlaying   := True;
  Result := True;
  {$ELSE}
  ShowMessage('Play: ' + AFilePath);
  Result := True;
  {$ENDIF}
end;

procedure TAudioPlayer.Stop;
begin
  FIsPlaying   := False;
  FCurrentFile := '';
end;

function TAudioPlayer.IsPlaying: Boolean;
begin
  Result := FIsPlaying;
end;

end.
