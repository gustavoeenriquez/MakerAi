// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - Stub for uMakerAi.Tools.Functions
unit uMakerAi.Tools.Functions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMakerAi.Core,          // TToolFormat defined here
  uMakerAi.Chat.Messages;

type

  TAiFunctions = class(TComponent)
  public
    function GetTools(aToolFormat: TToolFormat): string; virtual;
    function DoCallFunction(ToolCall: TAiToolsFunction): Boolean; virtual;
  end;

implementation

function TAiFunctions.GetTools(aToolFormat: TToolFormat): string;
begin
  Result := '';
end;

function TAiFunctions.DoCallFunction(ToolCall: TAiToolsFunction): Boolean;
begin
  Result := False;
end;

end.
