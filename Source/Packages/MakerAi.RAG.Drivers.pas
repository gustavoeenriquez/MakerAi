{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MakerAi.RAG.Drivers;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMakerAi.RAG.Graph.Driver.Postgres, uMakerAi.RAG.Vector.Driver.Postgres, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uMakerAi.RAG.Graph.Driver.Postgres', 
    @uMakerAi.RAG.Graph.Driver.Postgres.Register);
end;

initialization
  RegisterPackage('MakerAi.RAG.Drivers', @Register);
end.

