{
  Archivo central de Directivas de Compilación para MakerAI.
  Este archivo se incluye en todas las unidades del proyecto para asegurar
  una configuración consistente entre Delphi y Lazarus (FPC).
}

// -----------------------------------------------------------------------------
// SECCIÓN DELPHI
// -----------------------------------------------------------------------------
{$ifndef FPC}
  // Estas directivas SOLO afectan cuando se compila con Delphi.
  {$RTTI INHERIT} // Asegura que la RTTI se genere según la herencia (necesario para Bindings/MVVM)
{$endif}

// -----------------------------------------------------------------------------
// SECCIÓN LAZARUS / FREE PASCAL (FPC)
// -----------------------------------------------------------------------------
{$ifdef FPC}
  // Nota: {$mode delphiunicode} causa error con overrides de TCollectionItem 
  // (GetDisplayName, SetDisplayName, GetNamePath) porque la clase base RTL
  // usa ShortString, no UnicodeString. Se mantiene {$mode delphi}.
  {$mode delphi}
  
  // Declarar explícitamente que el archivo .pas debe interpretarse como codificado en UTF-8.
  {$codepage UTF8}
  
  // 'typehelpers': Permite extender tipos simples (Integer, String, etc.) con métodos.
  // Esencial para compatibilidad con métodos como '10.ToString' o 'String.IsEmpty'.
  {$modeswitch typehelpers}

  // Características Modernas (FPC 3.3.1+ / Trunk):
  // Estas características son necesarias para soportar callbacks anónimos y sintaxis moderna
  // que utiliza la librería, similares a los 'anonymous methods' de Delphi.
  // Requiere FPC 3.3+ (Trunk) - FPC 3.2 NO es soportado.
  {$modeswitch functionreferences} // Permite variables de tipo referencia a función
  {$modeswitch anonymousfunctions} // Permite declarar funciones anónimas inline
  
  // Supresión de advertencias comunes que no afectan la funcionalidad:
  // - 4104/4105: Conversiones implícitas entre tipos de cadena (UnicodeString/AnsiString)
  // - 5028: Constantes locales no utilizadas
  // - 5033: Function result does not seem to be set (falsos positivos comunes)
  // - 5093: Function result variable not initialized (falsos positivos comunes)
  // NOTA: Con {$mode delphi} + {$codepage UTF8}, los warnings 4104/4105 
  // (conversión implícita de cadenas) NO aparecen en la compilación actual.
  // Las siguientes supresiones son preventivas por si futuras versiones de FPC
  // o cambios en el código las generan de nuevo.
  // IMPORTANTE: Estas supresiones SON necesarias.
  // Análisis 2024-12-22: Al compilar demos aparecen ~23 warnings en:
  //   - uMakerAi.MCPClient.Core.pas (6 warnings)
  //   - uMakerAi.Chat.Gemini.pas (4 warnings)
  //   - uMakerAi.Chat.OpenAi.pas, Ollama.pas, Mistral.pas, Groq.pas, Grok.pas
  // Las conversiones implícitas son seguras porque todo el proyecto usa UTF-8.
  {$WARN 4104 OFF} // Implicit string conversion
  {$WARN 4105 OFF} // Implicit string conversion with data loss
  {$WARN 5028 OFF} // Local const not used
  {$WARN 5033 OFF} // Function result does not seem to be set
  {$WARN 5093 OFF} // Function result variable of a managed type does not seem to be initialized
{$endif}

// -----------------------------------------------------------------------------
// SECCIÓN DEBUG (Aplica a Delphi y FPC)
// -----------------------------------------------------------------------------
// APIDEBUG: Activa el guardado de archivos de depuración para llamadas API.
// Cuando está activo, guarda en /tmp/ (Linux) o c:\temp\ (Windows):
//   - peticion.json.txt   : El JSON que se envía a la API
//   - respuesta.json.txt  : El JSON que devuelve la API
// 
// Útil para diagnosticar problemas de comunicación con proveedores de AI.
// Ver documentación completa en: GEMINI.md (Sección 7)
//
// NOTA: Descomentar SOLO para depuración, mantener comentado en producción.

//{$DEFINE APIDEBUG}

// -----------------------------------------------------------------------------
// SECCIÓN DEPENDENCIAS OPCIONALES
// -----------------------------------------------------------------------------
// USE_MORMOT2: Habilita soporte para mORMot2 framework.
// Si NO está definida, las unidades que requieren mORMot2 quedarán vacías (stub)
// y compilarán sin errores, pero no tendrán funcionalidad.
//
// Unidades afectadas:
//   - uMakerAi.MCPServer.mORMot2 (Servidor MCP con mORMot2 + SSE)
//
// Descomentar SOLO si tienes mORMot2 instalado y deseas usarlo:

{$DEFINE USE_MORMOT2}

// -----------------------------------------------------------------------------
// USE_ZEOS: Habilita soporte para Zeos Database Objects en lugar de SQLDB (FPC).
// Si NO está definida, los drivers PostgreSQL usarán SQLDB (incluido con FPC).
// Con USE_ZEOS, usarán TZConnection y TZQuery de ZeosLib.
//
// Unidades afectadas:
//   - uMakerAi.RAG.Vector.Driver.Postgres (TFDConnection -> TZConnection/TPQConnection)
//   - uMakerAi.RAG.Graph.Driver.Postgres (TFDQuery -> TZQuery/TSQLQuery)
//
// Requisitos previos:
//   - Tener instalado el paquete 'zcomponent' en Lazarus
//   - El paquete MakerAi.RAG.Drivers.lpk debe incluir zcomponent como dependencia
//
// Descomentar SOLO si tienes Zeos instalado:
{$IFDEF FPC}
  {$DEFINE USE_ZEOS}
{$ENDIF}

// -----------------------------------------------------------------------------
// SECCIÓN PYTHON (Python4Delphi / Python4Lazarus)
// -----------------------------------------------------------------------------
// USE_PYTHON4DELPHI: Habilita soporte nativo para Python usando los componentes
// Python4Delphi (o su port Python4Lazarus en FPC).
//
// Requisitos previos:
//   - Tener instalado Python 3.10+ (o la versión que corresponda a la DLL configurada)
//   - Tener instalado el paquete 'Python4Delphi' (o 'python4lazarus') en el IDE
//
// Si NO está definida (y se compila con FPC), se usará una implementación 'dummy'
// que permite compilar el proyecto sin errores pero sin funcionalidad real de Python.
// En Delphi siempre se asume que los componentes están disponibles.
//
// Descomentar la siguiente línea tras instalar el paquete:
{$DEFINE USE_PYTHON4DELPHI}