program PrimerChatForm;

// LIBRO "IA con Delphi", capitulo 3 - El primer chat con Lucia, en un formulario FMX.
// El TAiChatConnection esta puesto en el formulario y configurado en el Object Inspector
// (ver uPrimerChatForm.fmx); el boton solo llama a AddMessageAndRun.

uses
  System.StartUpCopy,
  FMX.Forms,
  uPrimerChatForm in 'uPrimerChatForm.pas' {FormPrimerChat};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrimerChat, FormPrimerChat);
  Application.Run;
end.
