unit uMakerAi.Utils.PcmToWav;

interface

uses
  System.SysUtils, System.Classes;

type
  // Estructura del header WAV
  TWAVHeader = packed record
    // Chunk RIFF
    ChunkID: array [0 .. 3] of AnsiChar; // "RIFF"
    ChunkSize: Cardinal; // Tamaño del archivo - 8 bytes
    Format: array [0 .. 3] of AnsiChar; // "WAVE"

    // Subchunk fmt
    Subchunk1ID: array [0 .. 3] of AnsiChar; // "fmt "
    Subchunk1Size: Cardinal; // 16 para PCM
    AudioFormat: Word; // 1 para PCM
    NumChannels: Word; // Número de canales
    SampleRate: Cardinal; // Frecuencia de muestreo
    ByteRate: Cardinal; // SampleRate * NumChannels * BitsPerSample/8
    BlockAlign: Word; // NumChannels * BitsPerSample/8
    BitsPerSample: Word; // Bits por muestra

    // Subchunk data
    Subchunk2ID: array [0 .. 3] of AnsiChar; // "data"
    Subchunk2Size: Cardinal; // Tamaño de los datos PCM
  end;

  // Función principal para convertir PCM a WAV
function ConvertPCMToWAV(const PCMFilePath, WAVFilePath: string; SampleRate: Cardinal = 44100; Channels: Word = 2;
  BitsPerSample: Word = 16): Boolean;

// Función para convertir PCM a WAV desde TMemoryStream
function ConvertPCMStreamToWAVStream(const PCMStream: TMemoryStream; out WAVStream: TMemoryStream; SampleRate: Cardinal = 44100;
  Channels: Word = 2; BitsPerSample: Word = 16): Boolean;

implementation

function ConvertPCMToWAV(const PCMFilePath, WAVFilePath: string; SampleRate: Cardinal; Channels: Word; BitsPerSample: Word): Boolean;
var
  PCMFile, WAVFile: TFileStream;
  Header: TWAVHeader;
  PCMSize: Cardinal;
  Buffer: array [0 .. 4095] of Byte;
  BytesRead: Integer;
begin
  //Result := False;

  try
    // Verificar que el archivo PCM existe
    if not FileExists(PCMFilePath) then
    begin
      raise Exception.Create('El archivo PCM no existe: ' + PCMFilePath);
    end;

    // Abrir archivo PCM
    PCMFile := TFileStream.Create(PCMFilePath, fmOpenRead);
    try
      // Obtener tamaño del archivo PCM
      PCMSize := PCMFile.Size;

      // Crear archivo WAV
      WAVFile := TFileStream.Create(WAVFilePath, fmCreate);
      try
        // Configurar header WAV
        FillChar(Header, SizeOf(Header), 0);

        // Chunk RIFF
        Header.ChunkID := 'RIFF';
        Header.ChunkSize := 36 + PCMSize;
        Header.Format := 'WAVE';

        // Subchunk fmt
        Header.Subchunk1ID := 'fmt ';
        Header.Subchunk1Size := 16;
        Header.AudioFormat := 1; // PCM
        Header.NumChannels := Channels;
        Header.SampleRate := SampleRate;
        Header.ByteRate := SampleRate * Channels * (BitsPerSample div 8);
        Header.BlockAlign := Channels * (BitsPerSample div 8);
        Header.BitsPerSample := BitsPerSample;

        // Subchunk data
        Header.Subchunk2ID := 'data';
        Header.Subchunk2Size := PCMSize;

        // Escribir header WAV
        WAVFile.WriteBuffer(Header, SizeOf(Header));

        // Copiar datos PCM al archivo WAV
        PCMFile.Position := 0;
        while PCMFile.Position < PCMFile.Size do
        begin
          BytesRead := PCMFile.Read(Buffer, SizeOf(Buffer));
          if BytesRead > 0 then
            WAVFile.WriteBuffer(Buffer, BytesRead);
        end;

        Result := True;

      finally
        WAVFile.Free;
      end;

    finally
      PCMFile.Free;
    end;

  except
    on E: Exception do
    begin
      // Si hay error y el archivo WAV se creó parcialmente, eliminarlo
      if FileExists(WAVFilePath) then
        DeleteFile(WAVFilePath);
      raise;
    end;
  end;
end;

// Función para convertir PCM a WAV desde TMemoryStream
function ConvertPCMStreamToWAVStream(const PCMStream: TMemoryStream; out WAVStream: TMemoryStream; SampleRate: Cardinal = 44100;
  Channels: Word = 2; BitsPerSample: Word = 16): Boolean;
var
  Header: TWAVHeader;
  PCMSize: Cardinal;
  Buffer: array [0 .. 4095] of Byte;
  BytesRead: Integer;
begin
  //Result := False;
  WAVStream := nil; // Inicializar WAVStream

  try
    // Crear el stream de salida WAV
    WAVStream := TMemoryStream.Create;
    try
      // Obtener el tamaño del stream PCM
      PCMSize := PCMStream.Size;

      // Configurar el header WAV
      FillChar(Header, SizeOf(Header), 0);

      // Chunk RIFF
      Header.ChunkID := 'RIFF';
      Header.ChunkSize := 36 + PCMSize;
      Header.Format := 'WAVE';

      // Subchunk fmt
      Header.Subchunk1ID := 'fmt ';
      Header.Subchunk1Size := 16;
      Header.AudioFormat := 1; // PCM
      Header.NumChannels := Channels;
      Header.SampleRate := SampleRate;
      Header.ByteRate := SampleRate * Channels * (BitsPerSample div 8);
      Header.BlockAlign := Channels * (BitsPerSample div 8);
      Header.BitsPerSample := BitsPerSample;

      // Subchunk data
      Header.Subchunk2ID := 'data';
      Header.Subchunk2Size := PCMSize;

      // Escribir el header WAV en el stream de salida
      WAVStream.WriteBuffer(Header, SizeOf(Header));

      // Copiar los datos PCM al stream WAV
      PCMStream.Position := 0;
      while PCMStream.Position < PCMStream.Size do
      begin
        BytesRead := PCMStream.Read(Buffer, SizeOf(Buffer));
        if BytesRead > 0 then
          WAVStream.WriteBuffer(Buffer, BytesRead);
      end;

      // Resetear la posición del stream WAV a 0 para su posterior lectura
      WAVStream.Position := 0;

      Result := True;

    except
      on E: Exception do
      begin
        // Si hay un error, liberar el stream WAV y propagar la excepción
        WAVStream.Free;
        WAVStream := nil;
        raise;
      end;
    end;

  except
    on E: Exception do
    begin
      if Assigned(WAVStream) then
        WAVStream.Free;
      WAVStream := nil;
      raise;
    end;
  end;
end;

end.

// Ejemplo de uso:
{
  uses AudioConverter;

  procedure TForm1.Button1Click(Sender: TObject);
  begin
  try
  // Convertir archivo PCM mono de 24kHz y 16 bits
  if ConvertPCMToWAV('c:\temp\imagen.pcm', 'c:\temp\imagen.wav', 24000, 1, 16) then
  ShowMessage('Conversión exitosa')
  else
  ShowMessage('Error en la conversión');
  except
  on E: Exception do
  ShowMessage('Error: ' + E.Message);
  end;
  end;

  // Otros ejemplos:
  // Estéreo 44.1kHz, 16 bits (valores por defecto)
  // ConvertPCMToWAV('audio.pcm', 'audio.wav');

  // Mono 8kHz, 16 bits (calidad telefónica)
  // ConvertPCMToWAV('voice.pcm', 'voice.wav', 8000, 1, 16);

  // Estéreo 48kHz, 16 bits (calidad CD+)
  // ConvertPCMToWAV('music.pcm', 'music.wav', 48000, 2, 16);
}
