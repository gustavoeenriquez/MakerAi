// MakerAI Suite — shim de compatibilidad Realtime.WebSocket
// Redirige los tipos legacy al nuevo TAiWSClient (Source/WebSocket/)
//
// La implementación real está en:
//   uMakerAi.WebSocket.Client   — TAiWSClient, ITlsTransport, TAiWSReaderThread
//   uMakerAi.WebSocket.SChannel — TSChannelTransport (Windows, secur32.dll)
//   uMakerAi.WebSocket.OpenSSL  — TOpenSSLTransport  (POSIX, libssl dlopen)

unit uMakerAi.Realtime.WebSocket;

interface

uses
  uMakerAi.WebSocket.Client;

const
  WS_DIAG_LOG = '';  // activar con 'C:\Temp\wsclient_diag.txt' para debug

type
  TAiRealtimeWSOpcode       = TAiWSOpcode;
  TAiRealtimeWSFrameEvent   = TAiWSFrameEvent;
  TAiRealtimeWSErrorEvent   = TAiWSErrorEvent;
  TAiRealtimeWSClient       = TAiWSClient;
  TAiRealtimeWSReaderThread = TAiWSReaderThread;

implementation

end.
