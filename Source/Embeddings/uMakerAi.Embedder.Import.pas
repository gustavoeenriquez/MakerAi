{ ===============================================================================
  uMakerAi.Embedder.Import

  Delphi wrapper for makerai.embedder.dll.
  Zero dependencies beyond uMakerAi.ErrorCodes.

  Usage:
    Emb := TEmbedder.Create;
    if Emb.ValidateModelFile('C:\models\mxbai.gguf') = llamaErrNone then
      Emb.Load('C:\models\mxbai-embed-large-v1.gguf', 99);
    Vec := Emb.Embed('The quick brown fox');
    Sim := TEmbedder.CosineSimilarity(VecA, VecB);
    Emb.Free;

  Required alongside your .exe:
    makerai.embedder.dll
    llama.dll, ggml.dll, ggml-base.dll, ggml-cpu.dll  (llama.cpp b8660+)
    ggml-cuda.dll  (optional, for GPU)

  License: MIT
=============================================================================== }
unit uMakerAi.Embedder.Import;

interface

uses
  System.SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Posix.Dlfcn,
  Posix.Errno,
{$ENDIF}
  uMakerAi.ErrorCodes;

{$IFDEF MSWINDOWS}
// Windows: DLL acepta wchar_t* — Delphi PChar = PWideChar, sin conversión.
type TNativeStr = PChar;
{$DEFINE EMB_NATIVE_WCHAR}
{$ELSE}
// POSIX/Android: .so espera const char* (UTF-8).
type TNativeStr = MarshaledAString;
{$ENDIF}

{$IFNDEF MSWINDOWS}
// ---------------------------------------------------------------------------
// POSIX shim — expose the Win32 module-loading API surface on Linux / macOS
// ---------------------------------------------------------------------------
type
  HMODULE = NativeUInt;  // matches dlopen/dlsym/dlclose signatures in Posix.Dlfcn

function LoadLibrary(LibName: PChar): HMODULE; inline;
begin
  Result := dlopen(MarshaledAString(UTF8String(LibName)), RTLD_NOW or RTLD_GLOBAL);
end;

function GetProcAddress(Lib: HMODULE; Name: PChar): Pointer; inline;
begin
  Result := dlsym(Lib, MarshaledAString(UTF8String(Name)));
end;

procedure FreeLibrary(Lib: HMODULE); inline;
begin
  if Lib <> 0 then dlclose(Lib);
end;

function GetLastError: Integer; inline;
begin
  Result := errno;
end;
// ---------------------------------------------------------------------------
{$ENDIF}

type
  EEmbedderError = class(Exception)
  public
    Code: TLlamaErrorCode;
    constructor Create(ACode: TLlamaErrorCode; const AMsg: string);
  end;

  TEmbedder = class
  private
    FLib: HMODULE;
    FHandle: Pointer;
    FDimensions: Integer;

    FnCreate:           function: Pointer; stdcall;
    FnLoad:             function(Inst: Pointer; Path: TNativeStr): LongBool; stdcall;
    FnLoadEx:           function(Inst: Pointer; Path: TNativeStr; NGPULayers: Integer): LongBool; stdcall;
    FnGetDims:          function(Inst: Pointer): Integer; stdcall;
    FnEmbed:            function(Inst: Pointer; Text: TNativeStr; Vec: PSingle; Size: Integer): Integer; stdcall;
    FnEmbedBatch:       function(Inst: Pointer; Texts: Pointer; Count: Integer;
                          OutVecs: PSingle; VecSize: Integer): LongBool; stdcall;
    FnIsGPU:            function(Inst: Pointer): LongBool; stdcall;
    FnGetCUDAInfo:      function: PAnsiChar; stdcall;  // siempre UTF-8 narrow
    FnFree:             procedure(Inst: Pointer); stdcall;
    FnGetLastError:     function: PAnsiChar; stdcall;  // siempre UTF-8 narrow
    FnGetLastErrorCode: function: Integer; stdcall;
    FnValidate:         function(Path: TNativeStr): Integer; stdcall;

    procedure BindDLL(const DLLPath: string);
    function  LastError: string;
    function  LastErrorCode: TLlamaErrorCode;
    procedure RaiseLastError;
    procedure CheckHandle;
  public
    constructor Create(const DLLPath: string =
      {$IFDEF MSWINDOWS}'makerai.embedder.dll'
      {$ELSEIF DEFINED(ANDROID) or DEFINED(ANDROID64)}'libmakerai_embedder.so'
      {$ELSE}'makerai.embedder.so'{$ENDIF});
    destructor  Destroy; override;

    { Validates the model file before loading. Returns llamaErrNone on success. }
    function ValidateModelFile(const Path: string): TLlamaErrorCode;

    { GGUFPath: path to a .gguf model file.
      NGPULayers: 99 = all layers on GPU (default), 0 = CPU only. }
    procedure Load(const GGUFPath: string; NGPULayers: Integer = 99);

    { Returns L2-normalized mean-pooled embedding vector. }
    function Embed(const Text: string): TArray<Single>;

    { Encodes multiple texts in sequence. }
    function EmbedBatch(const Texts: TArray<string>): TArray<TArray<Single>>;

    { Cosine similarity of two L2-normalized vectors (= dot product). }
    class function CosineSimilarity(const A, B: TArray<Single>): Single; static;

    { True if ggml-cuda.dll is loaded (GPU in use). }
    function IsGPUEnabled: Boolean;

    { Returns GPU/CPU status string. }
    function GetCUDAInfo: string;

    property Dimensions: Integer read FDimensions;
  end;

implementation

constructor EEmbedderError.Create(ACode: TLlamaErrorCode; const AMsg: string);
begin
  inherited Create(AMsg);
  Code := ACode;
end;

procedure TEmbedder.BindDLL(const DLLPath: string);
{$IFDEF MSWINDOWS}
var
  LDir: string;
{$ENDIF}

  function Bind(const Name: string): Pointer;
  begin
    Result := GetProcAddress(FLib, PChar(Name));
    if Result = nil then
      raise EEmbedderError.Create(llamaErrEntryPoint,
        Format('Entry point "%s" not found in %s', [Name, DLLPath]));
  end;

begin
{$IFDEF MSWINDOWS}
  // Agrega el directorio de makerai.embedder.dll al search path para que
  // Windows encuentre las dependencias (ggml-cuda.dll, cublas*.dll, etc.)
  LDir := ExtractFilePath(ExpandFileName(DLLPath));
  if LDir <> '' then
    SetDllDirectory(PChar(LDir));
{$ENDIF}
  FLib := LoadLibrary(PChar(DLLPath));
  if FLib = 0 then
    raise EEmbedderError.Create(llamaErrDllLoad,
      Format('Cannot load DLL: %s (error %d)', [DLLPath, GetLastError]));

  @FnCreate           := Bind('EmbedderCreate');
  @FnLoad             := Bind('EmbedderLoad');
  @FnLoadEx           := Bind('EmbedderLoadEx');
  @FnGetDims          := Bind('EmbedderGetDimensions');
  @FnEmbed            := Bind('EmbedderEmbed');
  @FnEmbedBatch       := Bind('EmbedderEmbedBatch');
  @FnIsGPU            := Bind('EmbedderIsGPUEnabled');
  @FnGetCUDAInfo      := Bind('EmbedderGetCUDAInfo');
  @FnFree             := Bind('EmbedderFree');
  @FnGetLastError     := Bind('EmbedderGetLastError');
  @FnGetLastErrorCode := Bind('EmbedderGetLastErrorCode');
  @FnValidate         := Bind('EmbedderValidateModelFile');
end;

function TEmbedder.LastError: string;
begin
  if Assigned(FnGetLastError) then Result := UTF8ToString(FnGetLastError())
  else Result := '(unknown)';
end;

function TEmbedder.LastErrorCode: TLlamaErrorCode;
begin
  if Assigned(FnGetLastErrorCode) then
    Result := TLlamaErrorCode(FnGetLastErrorCode())
  else
    Result := llamaErrUnknown;
end;

procedure TEmbedder.RaiseLastError;
begin
  raise EEmbedderError.Create(LastErrorCode, LastError);
end;

procedure TEmbedder.CheckHandle;
begin
  if FHandle = nil then
    raise EEmbedderError.Create(llamaErrInvalidArg, 'Embedder instance is nil');
end;

constructor TEmbedder.Create(const DLLPath: string);
begin
  inherited Create;
  BindDLL(DLLPath);
  FHandle := FnCreate();
  if FHandle = nil then
    RaiseLastError;
  FDimensions := 0;
end;

destructor TEmbedder.Destroy;
begin
  if Assigned(FnFree) and (FHandle <> nil) then FnFree(FHandle);
  if FLib <> 0 then FreeLibrary(FLib);
  inherited;
end;

function TEmbedder.ValidateModelFile(const Path: string): TLlamaErrorCode;
begin
{$IFDEF EMB_NATIVE_WCHAR}
  Result := TLlamaErrorCode(FnValidate(PChar(Path)));
{$ELSE}
  Result := TLlamaErrorCode(FnValidate(MarshaledAString(UTF8String(Path))));
{$ENDIF}
end;

procedure TEmbedder.Load(const GGUFPath: string; NGPULayers: Integer = 99);
begin
  CheckHandle;
{$IFDEF EMB_NATIVE_WCHAR}
  if not FnLoadEx(FHandle, PChar(GGUFPath), NGPULayers) then RaiseLastError;
{$ELSE}
  if not FnLoadEx(FHandle, MarshaledAString(UTF8String(GGUFPath)), NGPULayers) then RaiseLastError;
{$ENDIF}
  FDimensions := FnGetDims(FHandle);
  if FDimensions <= 0 then
    RaiseLastError;
end;

function TEmbedder.Embed(const Text: string): TArray<Single>;
var
  Written: Integer;
begin
  CheckHandle;
  if FDimensions <= 0 then
    raise EEmbedderError.Create(llamaErrInvalidArg,
      'Model not loaded. Call Load() first.');
  SetLength(Result, FDimensions);
{$IFDEF EMB_NATIVE_WCHAR}
  Written := FnEmbed(FHandle, PChar(Text), @Result[0], FDimensions);
{$ELSE}
  Written := FnEmbed(FHandle, MarshaledAString(UTF8String(Text)), @Result[0], FDimensions);
{$ENDIF}
  if Written < 0 then
    RaiseLastError;
  if Written <> FDimensions then
    SetLength(Result, Written);
end;

function TEmbedder.EmbedBatch(const Texts: TArray<string>): TArray<TArray<Single>>;
var
  Count:   Integer;
  FlatBuf: TArray<Single>;
  i:       Integer;
{$IFDEF EMB_NATIVE_WCHAR}
  Ptrs: TArray<PChar>;
{$ELSE}
  // En POSIX necesitamos UTF8String intermedios para mantener los punteros vivos
  U8:   TArray<UTF8String>;
  Ptrs: TArray<MarshaledAString>;
{$ENDIF}
begin
  CheckHandle;
  Count := Length(Texts);
  if Count = 0 then Exit(nil);
  if FDimensions <= 0 then
    raise EEmbedderError.Create(llamaErrInvalidArg,
      'Model not loaded. Call Load() first.');

{$IFDEF EMB_NATIVE_WCHAR}
  SetLength(Ptrs, Count);
  for i := 0 to Count - 1 do
    Ptrs[i] := PChar(Texts[i]);
  SetLength(FlatBuf, Count * FDimensions);
  if not FnEmbedBatch(FHandle, @Ptrs[0], Count, @FlatBuf[0], FDimensions) then
    RaiseLastError;
{$ELSE}
  // Convertir todos los strings a UTF-8 antes de construir el array de punteros;
  // U8 mantiene las cadenas vivas mientras FnEmbedBatch ejecuta.
  SetLength(U8, Count);
  SetLength(Ptrs, Count);
  for i := 0 to Count - 1 do
  begin
    U8[i]   := UTF8String(Texts[i]);
    Ptrs[i] := MarshaledAString(U8[i]);
  end;
  SetLength(FlatBuf, Count * FDimensions);
  if not FnEmbedBatch(FHandle, @Ptrs[0], Count, @FlatBuf[0], FDimensions) then
    RaiseLastError;
{$ENDIF}

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    SetLength(Result[i], FDimensions);
    Move(FlatBuf[i * FDimensions], Result[i][0], FDimensions * SizeOf(Single));
  end;
end;

function TEmbedder.IsGPUEnabled: Boolean;
begin
  CheckHandle;
  Result := FnIsGPU(FHandle);
end;

function TEmbedder.GetCUDAInfo: string;
begin
  Result := UTF8ToString(FnGetCUDAInfo());
end;

class function TEmbedder.CosineSimilarity(const A, B: TArray<Single>): Single;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to High(A) do
    Result := Result + A[i] * B[i];
end;

end.
