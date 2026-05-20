unit AIChat.Types;

// MIT License — Gustavo Enríquez <gustavoeenriquez@gmail.com>
// Data model for TAIChatView: messages, attachments, theme.

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types, System.UITypes, System.JSON, System.IOUtils, System.Math,
  System.StrUtils, System.NetEncoding,
  uMakerAi.MD.Types;

type
  // ── Enums ────────────────────────────────────────────────────────────────

  TChatRole = (crUser, crAssistant, crSystem);

  TChatMessageStatus = (msComplete, msStreaming, msError);

  TChatFileKind = (fkUnknown, fkImage, fkAudio, fkVideo, fkPDF, fkDocument);

  // ── Forward declarations ─────────────────────────────────────────────────

  TAIChatAttachment = class;
  TAIChatMessage    = class;

  // ── Hit area — one clickable region inside a rendered message ────────────

  TChatHitKind = (hkCopyButton, hkAttachment, hkLink, hkText);

  TAIChatHitArea = record
    Kind        : TChatHitKind;
    DocRect     : TRectF;    // in document (pre-scroll) coordinates
    AttachIndex : Integer;   // hkAttachment: index in Message.Attachments
    URL         : string;    // hkLink: href; hkCopyButton: message text
  end;

  // ── Attachment ────────────────────────────────────────────────────────────

  TAIChatAttachment = class
  private
    FFileName     : string;
    FLocalPath    : string;
    FMimeType     : string;
    FFileKind     : TChatFileKind;
    FFileSize     : Int64;
    FDurationMs   : Int64;
    FThumbnailPath: string;
    FData         : TBytes;   // inline binary — set when PersistAttachments=True
    function GetDisplayName: string;
    function GetHasThumbnail: Boolean;
  public
    constructor Create(const ALocalPath: string = '');
    function Clone: TAIChatAttachment;
    function ToJSON(AIncludeData: Boolean = False): TJSONObject;
    procedure FromJSON(AObj: TJSONObject);

    property FileName     : string        read FFileName      write FFileName;
    property LocalPath    : string        read FLocalPath     write FLocalPath;
    property MimeType     : string        read FMimeType      write FMimeType;
    property FileKind     : TChatFileKind read FFileKind      write FFileKind;
    property FileSize     : Int64         read FFileSize      write FFileSize;
    property DurationMs   : Int64         read FDurationMs    write FDurationMs;
    property ThumbnailPath: string        read FThumbnailPath write FThumbnailPath;
    property Data         : TBytes        read FData          write FData;
    property DisplayName  : string        read GetDisplayName;
    property HasThumbnail : Boolean       read GetHasThumbnail;
  end;

  TAIChatAttachments = class(TObjectList<TAIChatAttachment>)
  public
    function ToJSON(AIncludeData: Boolean = False): TJSONArray;
    procedure FromJSON(AArr: TJSONArray);
  end;

  // ── Message ───────────────────────────────────────────────────────────────

  TAIChatMessage = class
  private
    FRole       : TChatRole;
    FText       : string;
    FTimestamp  : TDateTime;
    FStatus     : TChatMessageStatus;
    FAttachments: TAIChatAttachments;
    // Layout cache — computed by renderer, not serialized
    FDocY           : Single;
    FMeasuredHeight : Single;
    FLayoutWidth    : Single;
    FHitAreas       : TArray<TAIChatHitArea>;
  public
    constructor Create(ARole: TChatRole; const AText: string = '');
    destructor Destroy; override;

    procedure AppendText(const AToken: string); inline;
    procedure InvalidateLayout; inline;

    function ToJSON(AIncludeAttachments: Boolean = False): TJSONObject;
    procedure FromJSON(AObj: TJSONObject);

    property Role           : TChatRole          read FRole;
    property Text           : string             read FText            write FText;
    property Timestamp      : TDateTime          read FTimestamp       write FTimestamp;
    property Status         : TChatMessageStatus read FStatus          write FStatus;
    property Attachments    : TAIChatAttachments read FAttachments;
    // Layout cache
    property DocY           : Single             read FDocY            write FDocY;
    property MeasuredHeight : Single             read FMeasuredHeight  write FMeasuredHeight;
    property LayoutWidth    : Single             read FLayoutWidth     write FLayoutWidth;
    property HitAreas       : TArray<TAIChatHitArea> read FHitAreas   write FHitAreas;
  end;

  TAIChatMessages = TObjectList<TAIChatMessage>;

  // ── Chat theme ────────────────────────────────────────────────────────────

  TAIChatTheme = record
    // Base MD theme used for assistant bubble content (Markdown renderer)
    MD: TMDTheme;

    // Bubble backgrounds
    UserBubbleBg      : TAlphaColor;
    AssistantBubbleBg : TAlphaColor;
    SystemBubbleBg    : TAlphaColor;

    // Bubble text (plain, for user/system labels)
    UserTextColor      : TAlphaColor;
    AssistantTextColor : TAlphaColor;
    SystemTextColor    : TAlphaColor;

    // Bubble chrome
    BubbleBorderColor : TAlphaColor;
    BubbleRadius      : Single;
    BubblePadding     : Single;
    BubbleMarginV     : Single;  // vertical gap between bubbles
    MaxBubbleWidthPct : Single;  // 0..1 — max fraction of component width

    // Timestamp & avatar
    TimestampColor    : TAlphaColor;
    TimestampFontSize : Single;
    AvatarSize        : Single;

    // Attachment chip
    AttachChipBg      : TAlphaColor;
    AttachChipText    : TAlphaColor;
    AttachChipRadius  : Single;
    AttachChipHeight  : Single;

    // Component outer padding (mirrors AITextMD's OUTER_PADDING)
    OuterPadding      : Single;

    // Container rounded-rect border (drawn by TAIChatView)
    ViewBorderColor   : TAlphaColor;

    class function Default: TAIChatTheme; static;
    class function Dark: TAIChatTheme; static;
  end;

  // ── MIME / extension helpers ──────────────────────────────────────────────

  function MimeToFileKind(const AMime: string): TChatFileKind;
  function ExtToFileKind(const AExt: string): TChatFileKind;
  function ExtToMime(const AExt: string): string;
  function FileSizeLabel(ABytes: Int64): string;
  function DurationLabel(AMs: Int64): string;

implementation

// ── Helper tables ────────────────────────────────────────────────────────────

function MimeToFileKind(const AMime: string): TChatFileKind;
var
  M: string;
begin
  M := LowerCase(AMime);
  if M.StartsWith('image/')                then Exit(TChatFileKind.fkImage);
  if M.StartsWith('audio/')                then Exit(TChatFileKind.fkAudio);
  if M.StartsWith('video/')                then Exit(TChatFileKind.fkVideo);
  if M = 'application/pdf'                 then Exit(TChatFileKind.fkPDF);
  if M.StartsWith('text/')                 then Exit(TChatFileKind.fkDocument);
  if M.Contains('word')                    then Exit(TChatFileKind.fkDocument);
  if M.Contains('spreadsheet')             then Exit(TChatFileKind.fkDocument);
  if M.Contains('presentation')            then Exit(TChatFileKind.fkDocument);
  Result := TChatFileKind.fkUnknown;
end;

function ExtToFileKind(const AExt: string): TChatFileKind;
var
  E: string;
begin
  E := LowerCase(AExt);
  if E.StartsWith('.') then Delete(E, 1, 1);
  case AnsiIndexStr(E, [
    'jpg','jpeg','png','gif','bmp','webp','tiff','tif','svg','ico',
    'mp3','wav','ogg','flac','opus','m4a','aac','wma',
    'mp4','avi','mov','mkv','webm','m4v','flv',
    'pdf',
    'doc','docx','txt','rtf','xls','xlsx','csv','md','json','xml','htm','html'
  ]) of
    0..9  : Result := TChatFileKind.fkImage;
    10..17: Result := TChatFileKind.fkAudio;
    18..24: Result := TChatFileKind.fkVideo;
    25    : Result := TChatFileKind.fkPDF;
    26..36: Result := TChatFileKind.fkDocument;
  else      Result := TChatFileKind.fkUnknown;
  end;
end;

function ExtToMime(const AExt: string): string;
var
  E: string;
begin
  E := LowerCase(AExt);
  if E.StartsWith('.') then Delete(E, 1, 1);
  case AnsiIndexStr(E, [
    'jpg','jpeg','png','gif','bmp','webp','svg',
    'mp3','wav','ogg','flac','opus','m4a','aac',
    'mp4','avi','mov','mkv','webm',
    'pdf',
    'txt','md','csv','html','htm','xml','json',
    'doc','docx','xls','xlsx'
  ]) of
    0,1  : Result := 'image/jpeg';
    2    : Result := 'image/png';
    3    : Result := 'image/gif';
    4    : Result := 'image/bmp';
    5    : Result := 'image/webp';
    6    : Result := 'image/svg+xml';
    7    : Result := 'audio/mpeg';
    8    : Result := 'audio/wav';
    9    : Result := 'audio/ogg';
    10   : Result := 'audio/flac';
    11   : Result := 'audio/opus';
    12   : Result := 'audio/mp4';
    13   : Result := 'audio/aac';
    14   : Result := 'video/mp4';
    15   : Result := 'video/x-msvideo';
    16   : Result := 'video/quicktime';
    17   : Result := 'video/x-matroska';
    18   : Result := 'video/webm';
    19   : Result := 'application/pdf';
    20   : Result := 'text/plain';
    21   : Result := 'text/markdown';
    22   : Result := 'text/csv';
    23,24: Result := 'text/html';
    25   : Result := 'application/xml';
    26   : Result := 'application/json';
    27   : Result := 'application/msword';
    28   : Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
    29   : Result := 'application/vnd.ms-excel';
    30   : Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
  else     Result := 'application/octet-stream';
  end;
end;

function FileSizeLabel(ABytes: Int64): string;
begin
  if ABytes < 1024 then
    Result := Format('%d B', [ABytes])
  else if ABytes < 1024 * 1024 then
    Result := Format('%.1f KB', [ABytes / 1024])
  else
    Result := Format('%.1f MB', [ABytes / (1024 * 1024)]);
end;

function DurationLabel(AMs: Int64): string;
var
  S, M, H: Int64;
begin
  S := AMs div 1000;
  M := S div 60; S := S mod 60;
  H := M div 60; M := M mod 60;
  if H > 0 then
    Result := Format('%d:%2.2d:%2.2d', [H, M, S])
  else
    Result := Format('%d:%2.2d', [M, S]);
end;

// ── TAIChatAttachment ─────────────────────────────────────────────────────────

constructor TAIChatAttachment.Create(const ALocalPath: string);
begin
  inherited Create;
  FLocalPath  := ALocalPath;
  FDurationMs := -1;
  if ALocalPath <> '' then
  begin
    FFileName := TPath.GetFileName(ALocalPath);
    if TFile.Exists(ALocalPath) then
    begin
      var SR: TSearchRec;
      if FindFirst(ALocalPath, faAnyFile, SR) = 0 then
      begin
        FFileSize := SR.Size;
        FindClose(SR);
      end;
    end;
    FMimeType := ExtToMime(TPath.GetExtension(ALocalPath));
    FFileKind := ExtToFileKind(TPath.GetExtension(ALocalPath));
  end;
end;

function TAIChatAttachment.GetDisplayName: string;
begin
  Result := FFileName;
  if Result = '' then Result := TPath.GetFileName(FLocalPath);
end;

function TAIChatAttachment.GetHasThumbnail: Boolean;
begin
  Result := (FThumbnailPath <> '') and TFile.Exists(FThumbnailPath);
end;

function TAIChatAttachment.ToJSON(AIncludeData: Boolean = False): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('fileName',   FFileName);
  Result.AddPair('localPath',  FLocalPath);
  Result.AddPair('mimeType',   FMimeType);
  Result.AddPair('fileKind',   Integer(FFileKind));
  Result.AddPair('fileSize',   FFileSize);
  Result.AddPair('durationMs', FDurationMs);
  Result.AddPair('thumbPath',  FThumbnailPath);
  if AIncludeData and (Length(FData) > 0) then
    Result.AddPair('data', TNetEncoding.Base64.EncodeBytesToString(FData));
end;

function TAIChatAttachment.Clone: TAIChatAttachment;
begin
  Result               := TAIChatAttachment.Create;
  Result.FFileName     := FFileName;
  Result.FLocalPath    := FLocalPath;
  Result.FMimeType     := FMimeType;
  Result.FFileKind     := FFileKind;
  Result.FFileSize     := FFileSize;
  Result.FDurationMs   := FDurationMs;
  Result.FThumbnailPath:= FThumbnailPath;
  Result.FData         := FData;
end;

procedure TAIChatAttachment.FromJSON(AObj: TJSONObject);
var
  DataStr: string;
begin
  FFileName      := AObj.GetValue<string>('fileName',   '');
  FLocalPath     := AObj.GetValue<string>('localPath',  '');
  FMimeType      := AObj.GetValue<string>('mimeType',   '');
  FFileKind      := TChatFileKind(AObj.GetValue<Integer>('fileKind', 0));
  FFileSize      := AObj.GetValue<Int64>('fileSize',    0);
  FDurationMs    := AObj.GetValue<Int64>('durationMs',  -1);
  FThumbnailPath := AObj.GetValue<string>('thumbPath',  '');
  DataStr := AObj.GetValue<string>('data', '');
  if DataStr <> '' then
    FData := TNetEncoding.Base64.DecodeStringToBytes(DataStr)
  else
    SetLength(FData, 0);
end;

// ── TAIChatAttachments ────────────────────────────────────────────────────────

function TAIChatAttachments.ToJSON(AIncludeData: Boolean = False): TJSONArray;
var
  A: TAIChatAttachment;
begin
  Result := TJSONArray.Create;
  for A in Self do
    Result.AddElement(A.ToJSON(AIncludeData));
end;

procedure TAIChatAttachments.FromJSON(AArr: TJSONArray);
var
  I: Integer;
  Att: TAIChatAttachment;
begin
  Clear;
  if AArr = nil then Exit;
  for I := 0 to AArr.Count - 1 do
  begin
    Att := TAIChatAttachment.Create;
    Att.FromJSON(AArr.Items[I] as TJSONObject);
    Add(Att);
  end;
end;

// ── TAIChatMessage ────────────────────────────────────────────────────────────

constructor TAIChatMessage.Create(ARole: TChatRole; const AText: string);
begin
  inherited Create;
  FRole        := ARole;
  FText        := AText;
  FTimestamp   := Now;
  FStatus      := TChatMessageStatus.msComplete;
  FAttachments := TAIChatAttachments.Create(True);
  FMeasuredHeight := -1;
  FLayoutWidth    := -1;
end;

destructor TAIChatMessage.Destroy;
begin
  FAttachments.Free;
  inherited;
end;

procedure TAIChatMessage.AppendText(const AToken: string);
begin
  FText := FText + AToken;
  InvalidateLayout;
end;

procedure TAIChatMessage.InvalidateLayout;
begin
  FMeasuredHeight := -1;
  FLayoutWidth    := -1;
end;

function TAIChatMessage.ToJSON(AIncludeAttachments: Boolean = False): TJSONObject;
const
  ROLE_NAMES: array[TChatRole] of string = ('user', 'assistant', 'system');
  STATUS_NAMES: array[TChatMessageStatus] of string = ('complete', 'streaming', 'error');
begin
  Result := TJSONObject.Create;
  Result.AddPair('role',      ROLE_NAMES[FRole]);
  Result.AddPair('text',      FText);
  Result.AddPair('timestamp', FloatToStr(FTimestamp));
  Result.AddPair('status',    STATUS_NAMES[FStatus]);
  if AIncludeAttachments then
    Result.AddPair('attachments', FAttachments.ToJSON(True));
end;

procedure TAIChatMessage.FromJSON(AObj: TJSONObject);
var
  RoleStr: string;
begin
  RoleStr := LowerCase(AObj.GetValue<string>('role', 'user'));
  if RoleStr = 'assistant' then FRole := TChatRole.crAssistant
  else if RoleStr = 'system' then FRole := TChatRole.crSystem
  else FRole := TChatRole.crUser;

  FText      := AObj.GetValue<string>('text', '');
  FTimestamp := StrToFloatDef(AObj.GetValue<string>('timestamp', '0'), 0);

  var StatusStr := LowerCase(AObj.GetValue<string>('status', 'complete'));
  if StatusStr = 'streaming' then FStatus := TChatMessageStatus.msStreaming
  else if StatusStr = 'error' then FStatus := TChatMessageStatus.msError
  else FStatus := TChatMessageStatus.msComplete;

  var ArrVal := AObj.GetValue('attachments');
  if ArrVal is TJSONArray then
    FAttachments.FromJSON(ArrVal as TJSONArray);

  InvalidateLayout;
end;

// ── TAIChatTheme ──────────────────────────────────────────────────────────────

class function TAIChatTheme.Default: TAIChatTheme;
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.MD := TMDTheme.Default;

  Result.UserBubbleBg       := $FFD4EFFF;  // soft blue
  Result.AssistantBubbleBg  := $FFF5F5F5;  // near-white
  Result.SystemBubbleBg     := $FFEEEEEE;

  Result.UserTextColor      := $FF1A1A2E;
  Result.AssistantTextColor := $FF1A1A2E;
  Result.SystemTextColor    := $FF666666;

  Result.BubbleBorderColor  := $18000000;  // very subtle shadow
  Result.BubbleRadius       := 14;
  Result.BubblePadding      := 12;
  Result.BubbleMarginV      := 8;
  Result.MaxBubbleWidthPct  := 0.75;

  Result.TimestampColor     := $FF999999;
  Result.TimestampFontSize  := 10;
  Result.AvatarSize         := 28;

  Result.AttachChipBg       := $FFE8E8E8;
  Result.AttachChipText     := $FF444444;
  Result.AttachChipRadius   := 8;
  Result.AttachChipHeight   := 36;

  Result.OuterPadding       := 16;
  Result.ViewBorderColor    := $FFD0D0D0;
end;

class function TAIChatTheme.Dark: TAIChatTheme;
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.MD := TMDTheme.Dark;

  Result.UserBubbleBg       := $FF1A4A7A;  // deep blue
  Result.AssistantBubbleBg  := $FF2A2A2E;
  Result.SystemBubbleBg     := $FF222226;

  Result.UserTextColor      := $FFE8E8F0;
  Result.AssistantTextColor := $FFE0E0E8;
  Result.SystemTextColor    := $FF888899;

  Result.BubbleBorderColor  := $28FFFFFF;
  Result.BubbleRadius       := 14;
  Result.BubblePadding      := 12;
  Result.BubbleMarginV      := 8;
  Result.MaxBubbleWidthPct  := 0.75;

  Result.TimestampColor     := $FF666677;
  Result.TimestampFontSize  := 10;
  Result.AvatarSize         := 28;

  Result.AttachChipBg       := $FF333340;
  Result.AttachChipText     := $FFCCCCDD;
  Result.AttachChipRadius   := 8;
  Result.AttachChipHeight   := 36;

  Result.OuterPadding       := 16;
  Result.ViewBorderColor    := $FF444455;
end;

end.
