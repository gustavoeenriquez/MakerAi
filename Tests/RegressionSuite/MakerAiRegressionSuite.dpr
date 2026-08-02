program MakerAiRegressionSuite;

// =============================================================================
// Suite de regresion de MakerAI
// =============================================================================
// Valida in-process, sin servicios externos ni API keys:
//   - MCP dual-era (spec 2026-07-28 stateless + fallback al handshake legacy)
//   - Patron MRTR (elicitation + reintento con requestState)
//   - Grafos de agentes
//   - Protocolo A2A 1.0 (Agent Card, SendMessage, federacion de agentes)
//   - Guardrails de tool calls
//   - El propio TAiEvalRunner (autoprueba)
//
// Construida sobre TAiEvalRunner: la suite es a la vez la red de seguridad del
// framework y el ejemplo canonico de uso del componente de evals.
//
// Uso:
//   MakerAiRegressionSuite.exe             ejecuta todo y devuelve exit code
//   MakerAiRegressionSuite.exe --json out.json   guarda el reporte para CI
//   MakerAiRegressionSuite.exe --otel      exporta trazas OTLP a :4318
//
// Exit code: 0 si todos los casos pasan, 1 si alguno falla, 2 si hubo un error
// no controlado.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  uMakerAi.Evals,
  uMakerAi.Telemetry,
  uRegression.Fixtures in 'uRegression.Fixtures.pas',
  uRegression.Suites in 'uRegression.Suites.pas';

var
  Suite: TRegressionSuite;
  Report: TAiEvalReport;
  Telemetry: TAiTelemetry;
  Json: TJSONObject;
  i: Integer;
  Param, JsonPath: string;
  UseOtel: Boolean;

begin
  Telemetry := nil;
  JsonPath := '';
  UseOtel := False;
  try
    i := 1;
    while i <= ParamCount do
    begin
      Param := LowerCase(ParamStr(i));
      if Param = '--otel' then
        UseOtel := True
      else if (Param = '--json') and (i < ParamCount) then
      begin
        Inc(i);
        JsonPath := ParamStr(i);
      end;
      Inc(i);
    end;

    if UseOtel then
    begin
      Telemetry := TAiTelemetry.Create(nil);
      Telemetry.ServiceName := 'MakerAiRegressionSuite';
      Telemetry.Enabled := True;
      Writeln('[otel] Exportando trazas a ' + Telemetry.Endpoint);
    end;

    Writeln('MakerAI — Suite de regresion');
    Writeln('=====================================');
    Writeln;

    Suite := TRegressionSuite.Create;
    try
      Report := Suite.Run;
      try
        Writeln(Report.ToText);

        if JsonPath <> '' then
        begin
          Json := Report.ToJSON;
          try
            TFile.WriteAllText(JsonPath, Json.Format(2), TEncoding.UTF8);
            Writeln;
            Writeln('Reporte JSON escrito en: ' + JsonPath);
          finally
            Json.Free;
          end;
        end;

        if not Report.AllPassed then
          ExitCode := 1;
      finally
        Report.Free;
      end;
    finally
      Suite.Free;
    end;

    if Assigned(Telemetry) then
      Telemetry.Flush;
  except
    on E: Exception do
    begin
      Writeln('ERROR NO CONTROLADO: ', E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
  Telemetry.Free;
end.
