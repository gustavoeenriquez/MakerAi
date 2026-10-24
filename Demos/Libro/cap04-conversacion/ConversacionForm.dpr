program ConversacionForm;

// LIBRO "IA con Delphi", capitulo 4 - La conversacion, en un formulario FMX.
// Streaming asincrono (la respuesta aparece mientras se escribe, la ventana no se congela),
// eventos conectados desde el Object Inspector, y Nueva / Guardar / Abrir el historial.

uses
  System.StartUpCopy,
  FMX.Forms,
  uConversacionForm in 'uConversacionForm.pas' {FormConversacion};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormConversacion, FormConversacion);
  Application.Run;
end.
