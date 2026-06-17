// MIT License
//
// Copyright (c) 2024 Gustavo Enríquez - CimaMaker
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
// Nombre: Gustavo Enríquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Memory.Decay;

// Motor de decay exponencial para TAiMemory.
//
// Modelo inspirado en la curva de olvido de Ebbinghaus:
//   decay = exp(-base_rate * importance_factor * access_factor * hours_elapsed)
//
// Factores:
//   importance_factor  = 1.0 / importance      → importance 10 = decay lento, 1 = rápido
//   access_factor      = 1.0 / log1p(accesses) → más accesos = decay más lento
//   hours_elapsed      = horas desde último acceso
//
// Resultado: Double en [0.0, 1.0] donde 1.0 = fresco, 0.0 = completamente decaído

interface

uses
  System.SysUtils, System.Math, System.DateUtils;

type
  TAiMemoryDecay = class
  private
    const BASE_RATE = 0.005; // tasa base — 0.005 ≈ mitad de vida ~200h para imp=5
  public
    // Calcula el decay score en base a importancia, accesos y tiempo
    class function Compute(
      AImportance:  Integer;
      AAccessCount: Integer;
      ALastAccess:  TDateTime
    ): Double;

    // Devuelve True si el entry debe considerarse "activo" (decay > threshold)
    class function IsAlive(
      AImportance:  Integer;
      AAccessCount: Integer;
      ALastAccess:  TDateTime;
      AThreshold:   Double = 0.1
    ): Boolean;
  end;

implementation

class function TAiMemoryDecay.Compute(
  AImportance:  Integer;
  AAccessCount: Integer;
  ALastAccess:  TDateTime
): Double;
var
  HoursElapsed:     Double;
  ImportanceFactor: Double;
  AccessFactor:     Double;
  EffectiveRate:    Double;
begin
  // Importance garantizado en rango 1–10
  AImportance  := Max(1, Min(10, AImportance));
  AAccessCount := Max(0, AAccessCount);

  HoursElapsed := Max(0.0, HourSpan(Now, ALastAccess));

  // importance 10 → factor 0.1 (decay muy lento)
  // importance  1 → factor 1.0 (decay máximo)
  ImportanceFactor := 1.0 / AImportance;

  // Más accesos → denominador mayor → decay más lento (log1p = Ln(1+x) en Delphi)
  AccessFactor := 1.0 / (1.0 + Ln(1.0 + AAccessCount));

  EffectiveRate := BASE_RATE * ImportanceFactor * AccessFactor;

  Result := Exp(-EffectiveRate * HoursElapsed);
  Result := Max(0.0, Min(1.0, Result));
end;

class function TAiMemoryDecay.IsAlive(
  AImportance:  Integer;
  AAccessCount: Integer;
  ALastAccess:  TDateTime;
  AThreshold:   Double
): Boolean;
begin
  Result := Compute(AImportance, AAccessCount, ALastAccess) >= AThreshold;
end;

end.
