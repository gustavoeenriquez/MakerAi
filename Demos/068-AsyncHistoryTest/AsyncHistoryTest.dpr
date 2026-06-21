program AsyncHistoryTest;

{$APPTYPE CONSOLE}
{$R *.res}

// Test runtime del ISSUE #105: en async, Claude debe archivar sus respuestas en el
// historial para no repetir/olvidar. Conversacion multi-turno async; el ultimo turno
// debe recordar datos dados en los turnos previos.

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.JSON,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Initializations;

type
  TAsync = class
  public
    FDone: TEvent;
    FResult, FError: string;
    constructor Create;
    destructor Destroy; override;
    procedure OnEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
    procedure OnError(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
    function Wait: string;
  end;

constructor TAsync.Create;
begin
  inherited;
  FDone := TEvent.Create(nil, True, False, '');
end;

destructor TAsync.Destroy;
begin
  FDone.Free;
  inherited;
end;

procedure TAsync.OnEnd(const Sender: TObject; aMsg: TAiChatMessage; aResponse: TJSonObject; aRole, aText: string);
begin
  FResult := aText;
  FDone.SetEvent;
end;

procedure TAsync.OnError(Sender: TObject; const ErrorMsg: string; AException: Exception; const AResponse: IHTTPResponse);
begin
  FError := ErrorMsg;
  if Assigned(AResponse) and (AResponse.StatusCode > 0) then
    FError := 'HTTP=' + IntToStr(AResponse.StatusCode) + ' | ' + FError;
  FDone.SetEvent;
end;

function TAsync.Wait: string;
begin
  while FDone.WaitFor(0) <> wrSignaled do
    CheckSynchronize(100);
  if FError <> '' then
    Result := 'ERROR: ' + FError
  else
    Result := FResult;
end;

procedure RunTurn(Conn: TAiChatConnection; H: TAsync; const Q: string);
begin
  H.FResult := '';
  H.FError := '';
  H.FDone.ResetEvent;
  WriteLn('  Q: ' + Q);
  Conn.AddMessageAndRun(Q, 'user', []);
  WriteLn('  A: ' + Trim(H.Wait));
  WriteLn('');
end;

var
  Conn: TAiChatConnection;
  H: TAsync;
  LFinal: string;

begin
  try
    H := TAsync.Create;
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.DriverName := 'Claude';
      Conn.Model := 'claude-opus-4-8';
      Conn.Params.Values['ApiKey'] := '@CLAUDE_API_KEY';
      Conn.Params.Values['Asynchronous'] := 'True';
      Conn.Params.Values['Max_Tokens'] := '200';
      Conn.Params.Values['Tool_Active'] := 'False';
      Conn.OnReceiveDataEnd := H.OnEnd;
      Conn.OnError := H.OnError;

      WriteLn('=== ISSUE #105: historial en async (claude-opus-4-8) ===');
      WriteLn('');
      RunTurn(Conn, H, 'Mi color favorito es azul. Responde solo: ok.');
      RunTurn(Conn, H, 'Mi numero de la suerte es 42. Responde solo: ok.');
      RunTurn(Conn, H, 'En una sola frase, dime cual es mi color favorito y mi numero de la suerte.');
      LFinal := LowerCase(H.FResult);

      WriteLn('=== Veredicto ===');
      WriteLn(Format('  mensajes en historial: %d', [Conn.Messages.Count]));
      if (Pos('azul', LFinal) > 0) and (Pos('42', LFinal) > 0) then
        WriteLn('OK: recordo color (azul) y numero (42) del contexto previo -> historial async correcto.')
      else
        WriteLn('FALLO: no recordo el contexto previo (azul/42) -> historial async incompleto (#105).');
    finally
      Conn.Free;
      H.Free;
    end;
    WriteLn('=== FIN ===');
  except
    on E: Exception do
      WriteLn('FATAL: ' + E.ClassName + ': ' + E.Message);
  end;
end.
