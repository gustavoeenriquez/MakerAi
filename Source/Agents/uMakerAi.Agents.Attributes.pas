// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Atributos RTTI para herramientas de agentes.
// En FPC los atributos personalizados requieren {$modeswitch customattributes}
// pero se usan como clases simples (no se leen via TypInfo en tiempo de ejecucion).
// El registro en EngineRegistry se hace explicitamente en initialization.

unit uMakerAi.Agents.Attributes;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type

  TAgentCustomAttribute = class
    // Clase base para atributos de agentes
  end;

  // ---------------------------------------------------------------------------
  // TToolAttribute -- marca la clase de herramienta con nombre/descripcion/categoria
  // ---------------------------------------------------------------------------
  TToolAttribute = class(TAgentCustomAttribute)
  private
    FName:        string;
    FDescription: string;
    FCategory:    string;
  public
    constructor Create(const AName, ADescription, ACategory: string);
    property Name:        string read FName;
    property Description: string read FDescription;
    property Category:    string read FCategory;
  end;

  // ---------------------------------------------------------------------------
  // TToolParameterAttribute -- marca propiedades published de la herramienta
  // ---------------------------------------------------------------------------
  TToolParameterAttribute = class(TAgentCustomAttribute)
  private
    FDisplayName:  string;
    FHint:         string;
    FDefaultValue: string;
  public
    constructor Create(const ADisplayName, AHint: string;
                       const ADefaultValue: string = '');
    property DisplayName:  string read FDisplayName;
    property Hint:         string read FHint;
    property DefaultValue: string read FDefaultValue;
  end;

implementation

{ TToolAttribute }

constructor TToolAttribute.Create(const AName, ADescription, ACategory: string);
begin
  inherited Create;
  FName        := AName;
  FDescription := ADescription;
  FCategory    := ACategory;
end;

{ TToolParameterAttribute }

constructor TToolParameterAttribute.Create(const ADisplayName, AHint: string;
  const ADefaultValue: string);
begin
  inherited Create;
  FDisplayName  := ADisplayName;
  FHint         := AHint;
  FDefaultValue := ADefaultValue;
end;

end.
