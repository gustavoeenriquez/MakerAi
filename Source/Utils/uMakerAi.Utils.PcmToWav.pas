{$mode objfpc}{$H+}
// =============================================================================
// uMakerAi.Utils.PcmToWav.pas
// FPC PORT of the MakerAI Delphi library (v3.3)
// Original author: Gustavo Enriquez
// FPC port: MakerAI FPC Port project
// License: MIT
//
// Utility functions to convert raw PCM audio data to WAV format,
// either file-to-file or stream-to-stream.
// =============================================================================
unit uMakerAi.Utils.PcmToWav;

interface

uses
  SysUtils, Classes;

type
  TWAVHeader = packed record
    ChunkID: array [0 .. 3] of AnsiChar;
    ChunkSize: Cardinal;
    Format: array [0 .. 3] of AnsiChar;
    Subchunk1ID: array [0 .. 3] of AnsiChar;
    Subchunk1Size: Cardinal;
    AudioFormat: Word;
    NumChannels: Word;
    SampleRate: Cardinal;
    ByteRate: Cardinal;
    BlockAlign: Word;
    BitsPerSample: Word;
    Subchunk2ID: array [0 .. 3] of AnsiChar;
    Subchunk2Size: Cardinal;
  end;

function ConvertPCMToWAV(const PCMFilePath, WAVFilePath: string;
  SampleRate: Cardinal = 44100; Channels: Word = 2; BitsPerSample: Word = 16): Boolean;

function ConvertPCMStreamToWAVStream(const PCMStream: TMemoryStream;
  out WAVStream: TMemoryStream; SampleRate: Cardinal = 44100;
  Channels: Word = 2; BitsPerSample: Word = 16): Boolean;

implementation

function ConvertPCMToWAV(const PCMFilePath, WAVFilePath: string;
  SampleRate: Cardinal = 44100; Channels: Word = 2; BitsPerSample: Word = 16): Boolean;
var
  PCMStream: TMemoryStream;
  WAVStream: TMemoryStream;
begin
  Result := False;
  PCMStream := TMemoryStream.Create;
  WAVStream := nil;
  try
    PCMStream.LoadFromFile(PCMFilePath);
    if ConvertPCMStreamToWAVStream(PCMStream, WAVStream, SampleRate, Channels, BitsPerSample) then
    begin
      WAVStream.SaveToFile(WAVFilePath);
      Result := True;
    end;
  finally
    PCMStream.Free;
    if Assigned(WAVStream) then
      WAVStream.Free;
  end;
end;

function ConvertPCMStreamToWAVStream(const PCMStream: TMemoryStream;
  out WAVStream: TMemoryStream; SampleRate: Cardinal = 44100;
  Channels: Word = 2; BitsPerSample: Word = 16): Boolean;
var
  Header: TWAVHeader;
  PCMDataSize: Cardinal;
begin
  Result := False;
  WAVStream := nil;

  if not Assigned(PCMStream) then
    Exit;

  PCMDataSize := PCMStream.Size;

  // Fill the WAV header
  Header.ChunkID[0]     := 'R';
  Header.ChunkID[1]     := 'I';
  Header.ChunkID[2]     := 'F';
  Header.ChunkID[3]     := 'F';
  Header.ChunkSize      := 36 + PCMDataSize;  // 4 (Format) + 24 (Subchunk1) + 8 (Subchunk2 hdr) + data
  Header.Format[0]      := 'W';
  Header.Format[1]      := 'A';
  Header.Format[2]      := 'V';
  Header.Format[3]      := 'E';
  Header.Subchunk1ID[0] := 'f';
  Header.Subchunk1ID[1] := 'm';
  Header.Subchunk1ID[2] := 't';
  Header.Subchunk1ID[3] := ' ';
  Header.Subchunk1Size  := 16;        // PCM — no extra params
  Header.AudioFormat    := 1;         // PCM = 1 (Linear quantization)
  Header.NumChannels    := Channels;
  Header.SampleRate     := SampleRate;
  Header.ByteRate       := SampleRate * Channels * (BitsPerSample div 8);
  Header.BlockAlign     := Channels * (BitsPerSample div 8);
  Header.BitsPerSample  := BitsPerSample;
  Header.Subchunk2ID[0] := 'd';
  Header.Subchunk2ID[1] := 'a';
  Header.Subchunk2ID[2] := 't';
  Header.Subchunk2ID[3] := 'a';
  Header.Subchunk2Size  := PCMDataSize;

  WAVStream := TMemoryStream.Create;
  try
    // Write header
    WAVStream.Write(Header, SizeOf(TWAVHeader));
    // Append PCM data
    PCMStream.Position := 0;
    WAVStream.CopyFrom(PCMStream, PCMDataSize);
    WAVStream.Position := 0;
    Result := True;
  except
    FreeAndNil(WAVStream);
  end;
end;

end.
