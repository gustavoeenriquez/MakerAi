// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Adaptaciones respecto a la version Delphi:
//   - System.Threading → Classes (TThread disponible en FCL)
//   - TThread.Queue(nil, procedure begin...end) → TAiQueueHelper (helper auto-free)
//     FPC no acepta anonymous procedures en TThread.Queue; usamos un objeto
//     helper con un método TThreadMethod que se auto-libera tras la ejecución.
//     El comportamiento es idéntico: llamada NO bloqueante al hilo principal.
//     En apps de consola FPC, llamar TThread.CheckSynchronize() en el loop
//     principal para que Queue sea procesado.
//   - TAiCustomTool hereda de TComponent (disponible en FCL/Classes, sin LCL)
//   - System.JSON → fpjson

unit uMakerAi.Chat.Tools;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson,
  uMakerAi.Core,
  uMakerAi.Chat.Messages;

type

  // ---------------------------------------------------------------------------
  //  IAiToolContext — interfaz para que una Tool reporte eventos al Chat
  //  principal sin crear dependencia circular.
  // ---------------------------------------------------------------------------
  IAiToolContext = interface
    ['{E1D2C3B4-A5F6-4B7C-9D8E-F0A1B2C3D4E5}']
    procedure DoData(Msg: TAiChatMessage; const Role, Text: string;
                     AResponse: TJSONObject = nil);
    procedure DoDataEnd(Msg: TAiChatMessage; const Role, Text: string;
                        AResponse: TJSONObject = nil);
    procedure DoError(const ErrorMsg: string; E: Exception);
    procedure DoStateChange(State: TAiChatState; const Description: string = '');
    function  GetAsynchronous: Boolean;
  end;

  // ---------------------------------------------------------------------------
  //  Interfaces de herramientas (ChatTools framework)
  // ---------------------------------------------------------------------------

  IAiSpeechTool = interface
    ['{B2C3D4E5-F6A7-4B6C-9D0E-F1A2B3C4D5E6}']
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile;
                                   ResMsg, AskMsg: TAiChatMessage);
    procedure ExecuteSpeechGeneration(const AText: string;
                                      ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiVisionTool = interface
    ['{7A8B9C0D-E1F2-4A3B-8C4D-5E6F7A8B9C0D}']
    procedure ExecuteImageDescription(aMediaFile: TAiMediaFile;
                                      ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiImageTool = interface
    ['{A1B2C3D4-E5F6-4A5B-8C9D-E0F1A2B3C4D5}']
    procedure ExecuteImageGeneration(const APrompt: string;
                                     ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiVideoTool = interface
    ['{E5F6A7B8-C9D0-4E1F-A2B3-C4D5E6F7A8B9}']
    procedure ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiDocumentTool = interface
    ['{D4E5F6A7-B8C9-4D0E-1F2A-B3C4D5E6F7A8}']
    procedure ExecuteDocumentAnalysis(aMediaFile: TAiMediaFile;
                                      ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiCodeInterpreterTool = interface
    ['{F6A7B8C9-D0E1-4F2A-B3C4-D5E6F7A8B9C0}']
    procedure ExecuteCode(const ACode, ALanguage: string;
                          ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiWebSearchTool = interface
    ['{C3D4E5F6-A7B8-4C7D-0E1F-A2B3C4D5E6F7}']
    procedure ExecuteSearch(const AQuery: string;
                            ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiPdfTool = interface
    ['{C1D2E3F4-B5A6-4C7D-8E9F-A0B1C2D3E4F5}']
    procedure ExecutePdfAnalysis(aMediaFile: TAiMediaFile;
                                 ResMsg, AskMsg: TAiChatMessage);
  end;

  IAiReportTool = interface
    ['{B9C0D1E2-F3A4-4B5C-6D7E-8F9A0B1C2D3E}']
    procedure ExecuteReport(ResMsg, AskMsg: TAiChatMessage);
  end;

  // ---------------------------------------------------------------------------
  //  TAiCustomTool — base de todas las herramientas de chat
  //  Hereda de TComponent (disponible en FCL sin necesidad de LCL)
  // ---------------------------------------------------------------------------
  TAiCustomTool = class(TComponent)
  protected
    FContext: IAiToolContext;

    procedure ReportData(Msg: TAiChatMessage; const Role, Text: string;
                         AResponse: TJSONObject = nil);
    procedure ReportDataEnd(Msg: TAiChatMessage; const Role, Text: string;
                            AResponse: TJSONObject = nil);
    procedure ReportError(const ErrorMsg: string; E: Exception);
    procedure ReportState(State: TAiChatState; const Description: string = '');
    function  IsAsync: Boolean;
  public
    procedure SetContext(AContext: IAiToolContext); virtual;
  end;

  // ---------------------------------------------------------------------------
  //  Clases base para cada tipo de herramienta
  // ---------------------------------------------------------------------------

  TAiSpeechToolBase = class(TAiCustomTool, IAiSpeechTool)
  protected
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile;
                                   ResMsg, AskMsg: TAiChatMessage); virtual;
    procedure ExecuteSpeechGeneration(const AText: string;
                                      ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiVisionToolBase = class(TAiCustomTool, IAiVisionTool)
  protected
    procedure ExecuteImageDescription(aMediaFile: TAiMediaFile;
                                      ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiImageToolBase = class(TAiCustomTool, IAiImageTool)
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
                                     ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiVideoToolBase = class(TAiCustomTool, IAiVideoTool)
  protected
    procedure ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiDocumentToolBase = class(TAiCustomTool, IAiDocumentTool)
  protected
    procedure ExecuteDocumentAnalysis(aMediaFile: TAiMediaFile;
                                      ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiCodeInterpreterToolBase = class(TAiCustomTool, IAiCodeInterpreterTool)
  protected
    procedure ExecuteCode(const ACode, ALanguage: string;
                          ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiWebSearchToolBase = class(TAiCustomTool, IAiWebSearchTool)
  protected
    procedure ExecuteSearch(const AQuery: string;
                            ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiPdfToolBase = class(TAiCustomTool, IAiPdfTool)
  protected
    procedure ExecutePdfAnalysis(aMediaFile: TAiMediaFile;
                                 ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

  TAiReportToolBase = class(TAiCustomTool, IAiReportTool)
  protected
    procedure ExecuteReport(ResMsg, AskMsg: TAiChatMessage); virtual;
  end;

implementation

// ===========================================================================
//  TAiQueueHelper — reemplaza TThread.Queue(nil, procedure begin...end)
//
//  Problema en FPC: TThread.Queue acepta TThreadMethod (procedure of object),
//  no anonymous procedures. Este helper encapsula el payload y se auto-libera
//  tras la ejecución, replicando el comportamiento no-bloqueante de Delphi.
//
//  Uso interno:
//    Helper := TAiQueueHelper.CreateData(Context, Msg, Role, Text, Response);
//    TThread.Queue(nil, @Helper.Execute);   // no bloquea el hilo actual
//
//  En apps de consola FPC: llamar TThread.CheckSynchronize() periódicamente
//  en el bucle principal para que los Queue pendientes sean procesados.
// ===========================================================================

type
  TAiQueueKind = (qkData, qkDataEnd, qkError, qkState);

  TAiQueueHelper = class
  private
    FContext  : IAiToolContext;
    FKind     : TAiQueueKind;
    FMsg      : TAiChatMessage;
    FRole     : string;
    FText     : string;
    FResponse : TJSONObject;
    FState    : TAiChatState;
    FDesc     : string;
    FErrorMsg : string;
  public
    // El método que se pasa a TThread.Queue; se auto-libera al terminar
    procedure Execute;

    class procedure QueueData(AContext: IAiToolContext;
                              Msg: TAiChatMessage;
                              const Role, Text: string;
                              AResponse: TJSONObject);

    class procedure QueueDataEnd(AContext: IAiToolContext;
                                 Msg: TAiChatMessage;
                                 const Role, Text: string;
                                 AResponse: TJSONObject);

    class procedure QueueError(AContext: IAiToolContext;
                               const ErrorMsg: string);

    class procedure QueueState(AContext: IAiToolContext;
                               State: TAiChatState;
                               const Description: string);
  end;

procedure TAiQueueHelper.Execute;
begin
  try
    case FKind of
      qkData    : FContext.DoData(FMsg, FRole, FText, FResponse);
      qkDataEnd : FContext.DoDataEnd(FMsg, FRole, FText, FResponse);
      qkError   : FContext.DoError(FErrorMsg, nil);
      qkState   : FContext.DoStateChange(FState, FDesc);
    end;
  finally
    Free; // auto-liberación: igual que los closures de Delphi que se destruyen
  end;
end;

class procedure TAiQueueHelper.QueueData(AContext: IAiToolContext;
                                         Msg: TAiChatMessage;
                                         const Role, Text: string;
                                         AResponse: TJSONObject);
var
  H: TAiQueueHelper;
begin
  H          := TAiQueueHelper.Create;
  H.FContext  := AContext;
  H.FKind     := qkData;
  H.FMsg      := Msg;
  H.FRole     := Role;
  H.FText     := Text;
  H.FResponse := AResponse;
  TThread.Queue(nil, @H.Execute);
end;

class procedure TAiQueueHelper.QueueDataEnd(AContext: IAiToolContext;
                                            Msg: TAiChatMessage;
                                            const Role, Text: string;
                                            AResponse: TJSONObject);
var
  H: TAiQueueHelper;
begin
  H          := TAiQueueHelper.Create;
  H.FContext  := AContext;
  H.FKind     := qkDataEnd;
  H.FMsg      := Msg;
  H.FRole     := Role;
  H.FText     := Text;
  H.FResponse := AResponse;
  TThread.Queue(nil, @H.Execute);
end;

class procedure TAiQueueHelper.QueueError(AContext: IAiToolContext;
                                          const ErrorMsg: string);
var
  H: TAiQueueHelper;
begin
  H           := TAiQueueHelper.Create;
  H.FContext   := AContext;
  H.FKind      := qkError;
  H.FErrorMsg  := ErrorMsg;
  TThread.Queue(nil, @H.Execute);
end;

class procedure TAiQueueHelper.QueueState(AContext: IAiToolContext;
                                          State: TAiChatState;
                                          const Description: string);
var
  H: TAiQueueHelper;
begin
  H          := TAiQueueHelper.Create;
  H.FContext  := AContext;
  H.FKind     := qkState;
  H.FState    := State;
  H.FDesc     := Description;
  TThread.Queue(nil, @H.Execute);
end;

// ===========================================================================
//  TAiCustomTool
// ===========================================================================

procedure TAiCustomTool.SetContext(AContext: IAiToolContext);
begin
  FContext := AContext;
end;

function TAiCustomTool.IsAsync: Boolean;
begin
  Result := Assigned(FContext) and FContext.GetAsynchronous;
end;

procedure TAiCustomTool.ReportData(Msg: TAiChatMessage; const Role, Text: string;
                                   AResponse: TJSONObject);
begin
  if Assigned(FContext) then
    TAiQueueHelper.QueueData(FContext, Msg, Role, Text, AResponse);
end;

procedure TAiCustomTool.ReportDataEnd(Msg: TAiChatMessage; const Role, Text: string;
                                      AResponse: TJSONObject);
begin
  if Assigned(FContext) then
    TAiQueueHelper.QueueDataEnd(FContext, Msg, Role, Text, AResponse);
end;

procedure TAiCustomTool.ReportError(const ErrorMsg: string; E: Exception);
begin
  if Assigned(FContext) then
    TAiQueueHelper.QueueError(FContext, ErrorMsg);
end;

procedure TAiCustomTool.ReportState(State: TAiChatState; const Description: string);
begin
  if Assigned(FContext) then
    TAiQueueHelper.QueueState(FContext, State, Description);
end;

// ===========================================================================
//  Implementaciones vacías de las clases base (overrides en descendientes)
// ===========================================================================

procedure TAiSpeechToolBase.ExecuteTranscription(aMediaFile: TAiMediaFile;
                                                 ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiSpeechToolBase.ExecuteSpeechGeneration(const AText: string;
                                                    ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiVisionToolBase.ExecuteImageDescription(aMediaFile: TAiMediaFile;
                                                    ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiImageToolBase.ExecuteImageGeneration(const APrompt: string;
                                                  ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiVideoToolBase.ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiDocumentToolBase.ExecuteDocumentAnalysis(aMediaFile: TAiMediaFile;
                                                      ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiCodeInterpreterToolBase.ExecuteCode(const ACode, ALanguage: string;
                                                 ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiWebSearchToolBase.ExecuteSearch(const AQuery: string;
                                             ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiPdfToolBase.ExecutePdfAnalysis(aMediaFile: TAiMediaFile;
                                            ResMsg, AskMsg: TAiChatMessage);
begin
end;

procedure TAiReportToolBase.ExecuteReport(ResMsg, AskMsg: TAiChatMessage);
begin
end;

end.
