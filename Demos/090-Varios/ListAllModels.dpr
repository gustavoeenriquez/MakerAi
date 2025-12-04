// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


program ListAllModels;



{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,

  uMakerAi.Core, uMakerAi.Chat, uMakerAi.Chat.Ollama, uMakerAi.Chat.OpenAi, uMakerAi.Tools.Functions, uMakerAi.Chat.Grok, uMakerAi.Chat.Groq,
  uMakerAi.Chat.Mistral, uMakerAi.Chat.Claude, uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Gemini, uMakerAi.Embeddings,
  uMakerAi.Chat.Kimi, uMakerAi.chat.LMStudio, uMakerAi.chat.Initializations,
  uMakerAi.Chat.AiConnection;

procedure ListarModelos;
var
  AiConn: TAiChatConnection;
  Drivers, Modelos: TStringList;
  i, j: Integer;
begin
  AiConn := TAiChatConnection.Create(nil);
  Drivers := TStringList.Create;
  Modelos := TStringList.Create;
  try
    Writeln('--- LISTADO DE DRIVERS Y MODELOS DISPONIBLES ---');
    Writeln;

    // Obtener los drivers registrados
    Drivers.Assign(AiConn.GetDriversNames);
    Drivers.Sort;

    for i := 0 to Drivers.Count - 1 do
    begin
      Writeln('Driver: ', Drivers[i]);
      AiConn.DriverName := Drivers[i];

      try
        Modelos.Assign(AiConn.GetModels);

        if Modelos.Count = 0 then
          Writeln('   (Sin modelos detectados)')
        else
          for j := 0 to Modelos.Count - 1 do
            Writeln('   - ', Modelos[j]);
      except
        on E: Exception do
          Writeln('   [Error al obtener modelos: ', E.Message, ']');
      end;

      Writeln;
    end;

  finally
    AiConn.Free;
    Drivers.Free;
    Modelos.Free;
  end;
end;

begin
  try
    ListarModelos;
  except
    on E: Exception do
      Writeln('Error general: ', E.Message);
  end;

  Writeln;
  Writeln('Presione ENTER para salir...');
  Readln;
end.

