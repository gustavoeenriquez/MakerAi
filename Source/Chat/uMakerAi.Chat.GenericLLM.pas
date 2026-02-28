// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Driver GenericLLM: endpoint compatible con OpenAI /v1/chat/completions.
// Hereda toda la implementacion de TAiChat sin sobrescribir ningun metodo.
// Util para Ollama (OpenAI-compat), LMStudio, vLLM, etc.

unit uMakerAi.Chat.GenericLLM;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMakerAi.Chat;

type
  TAiGenericChat = class(TAiChat)
  private
    FCustomDriverName: string;
    procedure SetCustomDriverName(const Value: string);
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;

  published
    // Permite registrar el mismo driver bajo un nombre alternativo en tiempo
    // de ejecucion (por ejemplo: 'Ollama', 'LMStudio', 'vLLM').
    // Cuando se asigna un valor, el driver se registra con ese nombre adicional.
    property CustomDriverName: string
        read FCustomDriverName write SetCustomDriverName;
  end;

implementation

uses
  UMakerAi.ParamsRegistry;

// ===========================================================================
//  TAiGenericChat
// ===========================================================================

constructor TAiGenericChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  // Defaults para endpoint OpenAI-compatible generico (p.ej. Ollama local)
  ApiKey  := '1234';
  Model   := 'generic-local';
  Url     := 'http://127.0.0.1:1234/v1/';
end;

destructor TAiGenericChat.Destroy;
begin
  inherited Destroy;
end;

class function TAiGenericChat.GetDriverName: string;
begin
  Result := 'GenericLLM';
end;

class procedure TAiGenericChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Values['ApiKey']      := '1234';
  Params.Values['Model']       := 'generic-local';
  Params.Values['Max_Tokens']  := '4096';
  Params.Values['Temperature'] := '1';
  Params.Values['URL']         := 'http://127.0.0.1:1234/v1/';
  Params.Values['Asynchronous']:= 'False';
  Params.Values['Tool_Active'] := 'False';
end;

class function TAiGenericChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiGenericChat.Create(Sender);
end;

procedure TAiGenericChat.SetCustomDriverName(const Value: string);
begin
  if FCustomDriverName = Value then
    Exit;
  FCustomDriverName := Value;
  // Registrar el driver con el nombre personalizado tambien
  if Value <> '' then
    TAiChatFactory.Instance.RegisterDriver(TAiGenericChat, Value);
end;

// ===========================================================================
//  initialization — auto-registro en la factory
// ===========================================================================

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiGenericChat);

end.
