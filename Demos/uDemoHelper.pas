unit uDemoHelper;

{$mode objfpc}{$H+}

interface

implementation

uses
  SysUtils,
  {$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  openssl
  {$ELSE}
  Winapi.Windows
  {$ENDIF};

initialization
  // ──────────────────────────────────────────────────────────────────────────
  // Fix 1: UTF-8 en consola Windows
  // Sin esto, los acentos (á, é, ñ, ¿, ¡) se ven corruptos.
  // ──────────────────────────────────────────────────────────────────────────
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}

  // ──────────────────────────────────────────────────────────────────────────
  // Fix 2: Separador decimal consistente (punto, no coma)
  // Sin esto, Temperature := 0.7 se serializa como "0,7" en Windows con locale
  // español, y el servidor lo rechaza. Esto puede causar respuestas vacías
  // intermitentes en proveedores como Cohere.
  // ──────────────────────────────────────────────────────────────────────────
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';
  {$ELSE}
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';
  {$ENDIF}

  // ──────────────────────────────────────────────────────────────────────────
  // Fix 3: Inicializar OpenSSL en FPC
  // FPC no inicializa OpenSSL automáticamente. Sin esto, las conexiones HTTPS
  // fallan silenciosamente o devuelven respuestas vacías.
  // ──────────────────────────────────────────────────────────────────────────
  {$IFDEF FPC}
  try
    InitSSLInterface;
  except
    on E: Exception do
    begin
      WriteLn(stderr, '[FATAL] Error inicializando OpenSSL: ', E.Message);
      WriteLn(stderr, '  En Linux:  apt install libssl-dev');
      WriteLn(stderr, '  En Windows: asegúrate de que libcrypto-3.dll y');
      WriteLn(stderr, '              libssl-3.dll estén en el PATH');
      Halt(1);
    end;
  end;
  {$ENDIF}

end.
