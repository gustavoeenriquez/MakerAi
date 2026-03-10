unit uTool.WorldTime;

interface

uses
  uMakerAi.MCPServer.Core,
  System.SysUtils, System.StrUtils,
  System.JSON;

type
  // =============================================================================
  // HERRAMIENTA PARA OBTENER HORA DE UNA CIUDAD
  // =============================================================================
  
  TGetCityTimeParams = class
  private
    FCountry: string;
    FCity: string;
  public
    [AiMCPSchemaDescription('PaÃ­s de la ciudad (ej: "EspaÃ±a", "MÃ©xico", "Argentina")')]
    property Country: string read FCountry write FCountry;
    
    [AiMCPSchemaDescription('Nombre de la ciudad (ej: "Madrid", "Ciudad de MÃ©xico", "Buenos Aires")')]
    property City: string read FCity write FCity;
  end;

  TGetCityTimeTool = class(TAiMCPToolBase<TGetCityTimeParams>)
  protected
    function ExecuteWithParams(const AParams: TGetCityTimeParams; const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

procedure RegisterWorldTimeTool(ALogicServer: TAiMCPServer);

implementation

{ TGetCityTimeTool }

constructor TGetCityTimeTool.Create;
begin
  inherited;
  FName := 'get_city_time';
  FDescription := 'Obtiene la fecha y hora actual de una ciudad especÃ­fica en un paÃ­s determinado';
end;

function TGetCityTimeTool.ExecuteWithParams(const AParams: TGetCityTimeParams; 
  const AuthContext: TAiAuthContext): TJSONObject;
var
  ResultObject: TJSONObject;
  CurrentDateTime: TDateTime;
  TimeZoneOffset: Integer;
  CityDateTime: TDateTime;
begin
  try
    // Validar parÃ¡metros obligatorios
    if AParams.Country.IsEmpty then
      raise Exception.Create('El parÃ¡metro "Country" es obligatorio');
      
    if AParams.City.IsEmpty then
      raise Exception.Create('El parÃ¡metro "City" es obligatorio');

    // =========================================================================
    // AQUÃ IMPLEMENTA TU LÃ“GICA PARA OBTENER LA HORA DE LA CIUDAD
    // =========================================================================
    // Puedes:
    // 1. Consultar una API externa de zonas horarias (WorldTimeAPI, TimeZoneDB, etc.)
    // 2. Usar una base de datos local con informaciÃ³n de zonas horarias
    // 3. Implementar tu propia lÃ³gica de conversiÃ³n de zonas horarias
    //
    // Ejemplo de estructura que podrÃ­as retornar:
    // - DateTime: fecha y hora actual en esa ciudad
    // - TimeZone: nombre de la zona horaria (ej: "UTC-5", "Europe/Madrid")
    // - UTCOffset: diferencia en horas respecto a UTC
    // - IsDST: si estÃ¡ en horario de verano
    
    // EJEMPLO TEMPORAL (reemplazar con implementaciÃ³n real):                 ewrqwr
    CurrentDateTime := Now;
    TimeZoneOffset := 0; // Calcular el offset real segÃºn la ciudad
    CityDateTime := CurrentDateTime; // Ajustar segÃºn zona horaria

    // =========================================================================
    // FIN DEL ESPACIO PARA IMPLEMENTACIÃ“N
    // =========================================================================

    // Construir respuesta estructurada
    ResultObject := TJSONObject.Create;
    try
      ResultObject.AddPair('country', AParams.Country);
      ResultObject.AddPair('city', AParams.City);
      ResultObject.AddPair('datetime', FormatDateTime('yyyy-mm-dd hh:nn:ss', CityDateTime));
      ResultObject.AddPair('date', FormatDateTime('yyyy-mm-dd', CityDateTime));
      ResultObject.AddPair('time', FormatDateTime('hh:nn:ss', CityDateTime));
      ResultObject.AddPair('timezone', 'UTC' + IfThen(TimeZoneOffset >= 0, '+', '') + IntToStr(TimeZoneOffset));
      ResultObject.AddPair('utc_offset', TJSONNumber.Create(Int64(TimeZoneOffset)));
      ResultObject.AddPair('message', Format('Hora actual en %s, %s', [AParams.City, AParams.Country]));

      Result := TAiMCPResponseBuilder.New
        .AddText(ResultObject.ToJSON)
        .Build;
    finally
      ResultObject.Free;
    end;

  except
    on E: Exception do
      Result := TAiMCPResponseBuilder.New
        .AddText('âŒ Error obteniendo hora de la ciudad: ' + E.Message)
        .Build;
  end;
end;

procedure RegisterWorldTimeTool(ALogicServer: TAiMCPServer);
begin
  if not Assigned(ALogicServer) then
    raise Exception.Create('LogicServer no puede ser nulo para registrar herramientas.');
    
  ALogicServer.RegisterTool('get_city_time', 
    function: IAiMCPTool 
    begin 
      Result := TGetCityTimeTool.Create; 
    end);
end;

end.
