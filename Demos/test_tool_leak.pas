// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// test_tool_leak.pas — Demostración: Diseño no-propietario de tools
//
// Las tools NO son propiedad del chat (diseño real del framework).
// TAiChatSim.Destroy solo libera recursos propios (FSomeList).
// Las 11 tool instances deben liberarse explícitamente desde afuera
// mediante FreeTools, porque el chat solo tiene referencias NO
// propietarias a ellas.
//
// Este demo replica el patrón con contador de instancias para
// demostrar que no hay leak cuando se liberan las tools externamente.

program test_tool_leak;

{$mode objfpc}{$H+}

uses
  uDemoHelper,
  SysUtils, Classes;

type
  // ── Clases "tool" simuladas ──────────────────────────────────────────────
  TBaseTool = class
  private
    FName: string;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TSpeechToolSim      = class(TBaseTool);
  TImageToolSim       = class(TBaseTool);
  TVideoToolSim       = class(TBaseTool);
  TWebSearchToolSim   = class(TBaseTool);
  TVisionToolSim      = class(TBaseTool);
  TPdfToolSim         = class(TBaseTool);
  TReportToolSim      = class(TBaseTool);
  TShellToolSim       = class(TBaseTool);
  TTextEditorToolSim  = class(TBaseTool);
  TComputerUseToolSim = class(TBaseTool);
  TAiFunctionsSim     = class(TBaseTool);

var
  GAllocCount: Integer = 0;

constructor TBaseTool.Create(const AName: string);
begin
  FName := AName;
  InterlockedIncrement(GAllocCount);
  WriteLn('  [ALLOC] ', FName, '  -> vivas: ', GAllocCount);
end;

destructor TBaseTool.Destroy;
begin
  InterlockedDecrement(GAllocCount);
  WriteLn('  [FREE]  ', FName, '  -> vivas: ', GAllocCount);
  inherited Destroy;
end;

type
  // ── Clase que replica TAiChat (solo lo relevante) ───────────────────────
  TAiChatSim = class
  private
    FSpeechTool      : TSpeechToolSim;
    FImageTool       : TImageToolSim;
    FVideoTool       : TVideoToolSim;
    FWebSearchTool   : TWebSearchToolSim;
    FVisionTool      : TVisionToolSim;
    FPdfTool         : TPdfToolSim;
    FReportTool      : TReportToolSim;
    FShellTool       : TShellToolSim;
    FTextEditorTool  : TTextEditorToolSim;
    FComputerUseTool : TComputerUseToolSim;
    FAiFunctions     : TAiFunctionsSim;
    FSomeList        : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeTools;
    procedure AssignTools;
  end;

constructor TAiChatSim.Create;
begin
  FSomeList := TStringList.Create;
  FSomeList.Add('datos');
  // Tools empiezan en nil (como en TAiChat.Create)
end;

// ── Diseño no-propietario ────────────────────────────────────────────────
// El chat NO posee las tools. Destroy solo libera recursos propios.
// Las tools deben liberarse explícitamente desde afuera mediante FreeTools.
destructor TAiChatSim.Destroy;
begin
  FSomeList.Free;
  inherited Destroy;
end;

procedure TAiChatSim.FreeTools;
begin
  FreeAndNil(FSpeechTool);
  FreeAndNil(FImageTool);
  FreeAndNil(FVideoTool);
  FreeAndNil(FWebSearchTool);
  FreeAndNil(FVisionTool);
  FreeAndNil(FPdfTool);
  FreeAndNil(FReportTool);
  FreeAndNil(FShellTool);
  FreeAndNil(FTextEditorTool);
  FreeAndNil(FComputerUseTool);
  FreeAndNil(FAiFunctions);
end;

procedure TAiChatSim.AssignTools;
begin
  WriteLn;
  WriteLn('  Asignando 11 tool instances...');
  FSpeechTool      := TSpeechToolSim.Create('SpeechTool');
  FImageTool       := TImageToolSim.Create('ImageTool');
  FVideoTool       := TVideoToolSim.Create('VideoTool');
  FWebSearchTool   := TWebSearchToolSim.Create('WebSearchTool');
  FVisionTool      := TVisionToolSim.Create('VisionTool');
  FPdfTool         := TPdfToolSim.Create('PdfTool');
  FReportTool      := TReportToolSim.Create('ReportTool');
  FShellTool       := TShellToolSim.Create('ShellTool');
  FTextEditorTool  := TTextEditorToolSim.Create('TextEditorTool');
  FComputerUseTool := TComputerUseToolSim.Create('ComputerUseTool');
  FAiFunctions     := TAiFunctionsSim.Create('AiFunctions');
end;

var
  Chat: TAiChatSim;
begin
  WriteLn('══════════════════════════════════════════════════════════════');
  WriteLn('  Demo: Diseño no-propietario de tools');
  WriteLn('  Las tools NO son propiedad del chat; se liberan externamente');
  WriteLn('══════════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('  Escenario:');
  WriteLn('  1. Crear TAiChatSim');
  WriteLn('  2. Asignar 11 tool instances');
  WriteLn('  3. Liberar tools explícitamente (FreeTools) — el llamador es dueño');
  WriteLn('  4. Destruir el objeto (Destroy solo libera recursos propios)');
  WriteLn;

  Chat := TAiChatSim.Create;
  Chat.AssignTools;

  WriteLn;
  WriteLn('  Liberando tools explícitamente (FreeTools)...');
  Chat.FreeTools;

  WriteLn;
  WriteLn('  Destruyendo TAiChatSim (solo FSomeList + inherited)...');
  Chat.Free;

  WriteLn;
  if GAllocCount = 0 then
    WriteLn('  ✅ RESULTADO: 0 instancias vivas — SIN LEAK')
  else
    WriteLn('  ❌ RESULTADO: ', GAllocCount, ' instancias vivas — HAY LEAK');

  WriteLn;
  WriteLn('══════════════════════════════════════════════════════════════');
end.
