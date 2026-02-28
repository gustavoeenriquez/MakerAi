// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - Stub for uMakerAi.Embeddings
unit uMakerAi.Embeddings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TAiEmbeddings = class(TComponent)
  public
    class function GetDriverName: string; virtual; abstract;
    class procedure RegisterDefaultParams(Params: TStrings); virtual; abstract;
    class function CreateInstance(Sender: TComponent): TAiEmbeddings; virtual; abstract;
  end;

implementation

end.
