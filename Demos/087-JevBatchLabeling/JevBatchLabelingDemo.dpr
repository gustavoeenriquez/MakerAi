program JevBatchLabelingDemo;

// =============================================================================
// DEMO 087 - Etiquetado masivo con Jev: movimientos contables contra el PUC
// =============================================================================
// Reproduce en Delphi el experimento del prototipo en Python: clasificar 126
// movimientos (casos.csv) en una de 225 cuentas del PUC colombiano (puc.csv)
// con TAiJevBatchLabeler, en paralelo, y medir:
//
//   - acierto contra la cuenta esperada (o una alternativa aceptable)
//   - acierto de lo que se automatizaria (confianza >= ReviewThreshold)
//   - filas que quedan para revision humana, con sus 3 sugerencias
//   - tokens, costo y tiempo
//
// La pregunta y los textos son los mismos del prototipo en Python (que dio 94%
// de acierto), para que la comparacion sea directa. El resultado por fila
// queda en resultados.csv.
//
// Requiere TYPESAFE_API_KEY. Costo: ~1.2M tokens de entrada, ~US$0.05.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  uMakerAi.Jev in '..\..\Source\Tools\uMakerAi.Jev.pas',
  uMakerAi.Jev.Batch in '..\..\Source\Tools\uMakerAi.Jev.Batch.pas';

const
  PREGUNTA = '¿En qué cuenta del PUC colombiano se registra la contrapartida de `movimiento`? ' +
    'La contrapartida es la cuenta que explica el concepto del movimiento, no la cuenta del banco de ' +
    'la empresa por donde entra o sale el dinero, ni la cuenta del proveedor en una causación.';

type
  TCaso = record
    Id, Flujo, Texto, Gold: string;
    Alt: TArray<string>;
  end;

function Sentido(const AFlujo: string): string;
begin
  if AFlujo = 'salida' then
    Result := 'Salida de dinero: se pagó desde el banco de la empresa.'
  else if AFlujo = 'entrada' then
    Result := 'Entrada de dinero: llegó al banco de la empresa.'
  else
    Result := 'Causación de una factura de proveedor (aún no se paga).';
end;

// Busca los CSV junto al .dpr (el .exe queda en Win64\Release)
function DataFile(const AName: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\' + AName);
  if not TFile.Exists(Result) then
    Result := AName;
end;

function LeerCasos: TArray<TCaso>;
var
  L: TStringList;
  i: Integer;
  F: TArray<string>;
  C: TCaso;
  Lista: TList<TCaso>;
begin
  L := TStringList.Create;
  Lista := TList<TCaso>.Create;
  try
    L.LoadFromFile(DataFile('casos.csv'), TEncoding.UTF8);
    for i := 1 to L.Count - 1 do // la primera linea es el encabezado
    begin
      if Trim(L[i]) = '' then
        Continue;
      F := L[i].Split([';']);
      C.Id := F[0];
      C.Flujo := F[1];
      C.Texto := F[2];
      C.Gold := F[3];
      if (Length(F) > 4) and (F[4] <> '') then
        C.Alt := F[4].Split([','])
      else
        C.Alt := nil;
      Lista.Add(C);
    end;
    Result := Lista.ToArray;
  finally
    Lista.Free;
    L.Free;
  end;
end;

procedure CargarCuentas(ALabeler: TAiJevBatchLabeler);
var
  L: TStringList;
  Q: TAiJevQuestion;
  i: Integer;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(DataFile('puc.csv'), TEncoding.UTF8);
    Q := ALabeler.Questions.Add;
    Q.Name := 'cuenta';
    Q.Kind := jqChoice;
    Q.Instructions := PREGUNTA;
    // 'codigo;ruta' -> 'codigo=ruta' (la ruta da contexto: ACTIVO > DISPONIBLE > ...)
    for i := 1 to L.Count - 1 do
      if Trim(L[i]) <> '' then
        Q.Criteria.Add(StringReplace(L[i], ';', '=', []));
  finally
    L.Free;
  end;
end;

function Aceptable(const APred: string; const C: TCaso): Boolean;
var
  A: string;
begin
  Result := APred = C.Gold;
  for A in C.Alt do
    if APred = A then
      Result := True;
end;

var
  Labeler: TAiJevBatchLabeler;
  Casos: TArray<TCaso>;
  States: TArray<TJSONObject>;
  Rep: TAiJevBatchReport;
  It: TAiJevBatchItem;
  i, Ok, Auto, AutoOk: Integer;
begin
  try
    if GetEnvironmentVariable('TYPESAFE_API_KEY') = '' then
    begin
      Writeln('Falta la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).');
      ExitCode := 2;
      Exit;
    end;

    Casos := LeerCasos;
    Labeler := TAiJevBatchLabeler.Create(nil);
    try
      CargarCuentas(Labeler);
      Labeler.ReviewThreshold := 0.8;
      Labeler.MaxParallel := 16;
      Writeln(Format('%d movimientos contra %d cuentas del PUC, %d en paralelo...',
        [Length(Casos), Labeler.Questions[0].Criteria.Count, Labeler.MaxParallel]));

      SetLength(States, Length(Casos));
      for i := 0 to High(Casos) do
        States[i] := TJSONObject.Create
          .AddPair('movimiento', Casos[i].Texto)
          .AddPair('tipo', Sentido(Casos[i].Flujo));
      try
        Rep := Labeler.Run(States);
      finally
        for i := 0 to High(States) do
          States[i].Free;
      end;

      try
        Ok := 0;
        Auto := 0;
        AutoOk := 0;
        for It in Rep.Items do
          if It.Error = '' then
          begin
            if Aceptable(It.Choice, Casos[It.Index]) then
              Inc(Ok);
            if not It.NeedsReview then
            begin
              Inc(Auto);
              if Aceptable(It.Choice, Casos[It.Index]) then
                Inc(AutoOk);
            end;
          end;

        Writeln;
        Writeln(Format('Acierto total:          %d/%d  (%.1f%%)', [Ok, Rep.Items.Count, 100 * Ok / Rep.Items.Count]));
        Writeln(Format('Automatico (conf>=%.1f): %d filas (%.0f%%), acierto %d/%d (%.1f%%)',
          [Labeler.ReviewThreshold, Auto, 100 * Auto / Rep.Items.Count, AutoOk, Auto,
           100 * AutoOk / Auto]));
        Writeln(Format('Para revision humana:   %d filas', [Rep.ReviewCount]));
        Writeln(Format('Errores:                %d', [Rep.ErrorCount]));
        Writeln(Format('Tokens: %d  Costo: US$%.4f  Tiempo: %.1f s',
          [Rep.InputTokens, Rep.CostUSD, Rep.ElapsedMs / 1000]));

        Writeln;
        Writeln('Errores del modelo (etiqueta no aceptable):');
        for It in Rep.Items do
          if (It.Error = '') and not Aceptable(It.Choice, Casos[It.Index]) then
            Writeln(Format('  %s  esperado %s -> %s (conf %.2f, sugerencias %s)  %s',
              [Casos[It.Index].Id, Casos[It.Index].Gold, It.Choice, It.Confidence,
               string.Join(',', It.Top), Copy(Casos[It.Index].Texto, 1, 50)]));

        Rep.SaveToCSV(DataFile('resultados.csv'));
        Writeln;
        Writeln('Detalle por fila: ', ExpandFileName(DataFile('resultados.csv')));
      finally
        Rep.Free;
      end;
    finally
      Labeler.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
