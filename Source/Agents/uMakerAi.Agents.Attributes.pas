unit uMakerAi.Agents.Attributes;

interface

uses
  System.SysUtils;

type
  // ---------------------------------------------------------------------------
  // 1. ATRIBUTO PARA LA CLASE DE LA HERRAMIENTA ([Tool])
  //    Define las propiedades generales de la herramienta.
  // ---------------------------------------------------------------------------
  [AttributeUsage(ClassServices, AllowMultiple = false, Inherited = true)]
  TToolAttribute = class(TCustomAttribute)
  private
    FName: string;
    FDescription: string;
    FCategory: string;
  public
    constructor Create(const AName, ADescription, ACategory: string);
    property Name: string read FName;
    property Description: string read FDescription;
    property Category: string read FCategory;
  end;

  // ---------------------------------------------------------------------------
  // 2. ATRIBUTO PARA LOS PARÁMETROS (PROPIEDADES) DE LA HERRAMIENTA ([ToolParameter])
  //    Define cómo se muestra un parámetro en el diálogo de configuración.
  // ---------------------------------------------------------------------------
  // Nota de estilo: Renombrado a TToolParameterAttribute por convención de Delphi.
  [AttributeUsage(PropertyServices, AllowMultiple = false, Inherited = true)]
  TToolParameterAttribute = class(TCustomAttribute)
  private
    FDisplayName: string;
    FHint: string;
    FDefaultValue: string;
  public
    constructor Create(const ADisplayName, AHint: string; const ADefaultValue: string = '');
    property DisplayName: string read FDisplayName;
    property Hint: string read FHint;
    property DefaultValue: string read FDefaultValue;
  end;

implementation

{ TToolAttribute }

constructor TToolAttribute.Create(const AName, ADescription, ACategory: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FCategory := ACategory;
end;

{ TToolParameterAttribute }

constructor TToolParameterAttribute.Create(const ADisplayName, AHint: string; const ADefaultValue: string);
begin
  inherited Create;
  FDisplayName := ADisplayName;
  FHint := AHint;
  FDefaultValue := ADefaultValue;
end;

end.
