// MakerAI Suite — Driver Gemini Realtime STT (stub)
// Protocolo: Gemini Live API via WebSocket (pendiente implementacion completa)
// Sample rate: 16kHz PCM16 mono
//
// Autor: Gustavo Enriquez
// Email: gustavoeenriquez@gmail.com

unit uMakerAi.Realtime.Gemini;

interface

uses
  System.SysUtils, System.Classes,
  uMakerAi.Realtime;

type
  TAiGeminiRealtimeSTT = class(TAiRealtimeBase)
  protected
    function  GetTargetSampleRate: Integer; override;
    procedure InternalSendAudio(const ResampledPCM16: TBytes); override;
    procedure InternalConnect;    override;
    procedure InternalDisconnect; override;
    procedure InternalCommitAudio; override;
    procedure InternalClearAudio;  override;
  public
    class function GetDriverName:   string; override;
    class function GetDefaultModel: string; override;
  end;

implementation

class function TAiGeminiRealtimeSTT.GetDriverName: string;
begin
  Result := 'Gemini';
end;

class function TAiGeminiRealtimeSTT.GetDefaultModel: string;
begin
  Result := 'gemini-realtime-preview';
end;

function TAiGeminiRealtimeSTT.GetTargetSampleRate: Integer;
begin
  Result := 16000; // Gemini Live API usa PCM16 a 16kHz
end;

procedure TAiGeminiRealtimeSTT.InternalConnect;
begin
  raise ENotImplemented.Create(
    'TAiGeminiRealtimeSTT: implementacion pendiente en v3.5');
end;

procedure TAiGeminiRealtimeSTT.InternalDisconnect;
begin
end;

procedure TAiGeminiRealtimeSTT.InternalSendAudio(const ResampledPCM16: TBytes);
begin
end;

procedure TAiGeminiRealtimeSTT.InternalCommitAudio;
begin
end;

procedure TAiGeminiRealtimeSTT.InternalClearAudio;
begin
end;

initialization
  TAiRealtimeFactory.Instance.RegisterDriver(
    TAiGeminiRealtimeSTT.GetDriverName, TAiGeminiRealtimeSTT);

end.
