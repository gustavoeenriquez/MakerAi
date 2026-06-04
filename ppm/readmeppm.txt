================================================================================
  PPM — PascalAI Package Manager
  Guía para IA (Claude) — Cómo usar, instalar y publicar paquetes
  Última actualización: 2026-03-23  |  Versión registry: 1.3.0
================================================================================

ÍNDICE
------
 1.  Qué es PPM
 2.  Servidor / Registry
 3.  Tipos de paquetes
 4.  CLI — Comandos disponibles
 5.  Instalar paquetes pai/clib en Delphi (MakerAI)
 6.  Usar paquetes MCP en MakerAI
 7.  Usar Skills de Claude Code
 8.  Usar Prompts del registry
 9.  Publicar un paquete
10.  Estructura interna de un paquete (.paipkg)
11.  Crear un nuevo MCP Tool (flujo completo)
12.  Código fuente — dónde está todo
13.  Convenciones de nomenclatura
14.  Errores frecuentes y soluciones
15.  Referencia rápida

================================================================================
1. QUÉ ES PPM
================================================================================

PPM (PascalAI Package Manager) es el gestor de paquetes oficial del ecosistema
PascalAI / MakerAI. Permite instalar, publicar y gestionar cinco tipos de
paquetes:

  Tipo    Descripción
  ------  ------------------------------------------------------------------
  pai     Librerías escritas en PascalAI (lenguaje del ecosistema)
  clib    Wrappers Delphi sobre librerías C nativas (.obj, .a, .lib)
  mcp     MCP Tools — servidores de herramientas para LLMs (protocolo MCP)
  prompt  Plantillas de prompts reutilizables con variables {{var}}
  skill   Skills para Claude Code — instrucciones Markdown con frontmatter

Estado actual del registry:
  - 158+ MCP Tools (Fases 1-10, v1.3.0)
  - 200+ paquetes total (pai, clib, prompt, mcp, skill)
  -   4  Skills publicados (skill-code-review, skill-test-writer, etc.)
  - Web:  https://ppm.pascalai.org
  - Docs: https://ppm.pascalai.org/ppm/docs/

================================================================================
2. SERVIDOR / REGISTRY
================================================================================

URL del registry:     https://ppm.pascalai.org
API base:             https://ppm.pascalai.org/v1/

VPS (servidor de producción):
  Host:    187.33.145.146
  Usuario: gustavo
  Pass:    C1maMaker*
  Web:     /var/www/ppm/web/index.html
  API:     proceso PPMServer escuchando en 127.0.0.1:8765 (proxied por nginx)
  Fuentes: /home/gustavo/ppm/api/PPMServer  (binario Delphi Linux64)

Endpoints — Paquetes generales:
  GET  /v1/health                               → estado del servidor y DB
  GET  /v1/search?q=<q>&type=<tipo>&page=N      → buscar paquetes
  GET  /v1/packages/:name                       → info de un paquete
  GET  /v1/packages/:name/:version              → info de una versión
  GET  /v1/packages/:name/:version/download     → descargar .paipkg
  PUT  /v1/packages/:name/:version              → publicar (requiere auth)
  POST /v1/packages/:name/:version/yank         → retirar versión (requiere auth)

Endpoints — MCP Tools:
  GET  /v1/packages/:name/:version/schema       → JSON Schema del tool
  GET  /v1/mcp/discover?q=<keyword>             → descubrir tools + schema inline

Endpoints — Prompts:
  GET  /v1/packages/:name/:version/raw          → texto plano del prompt
  GET  /v1/packages/:name/:version/render?var=val → prompt con variables sustituidas

Endpoints — Skills (NUEVO):
  GET  /v1/packages/:name/:version/skill        → Markdown raw del skill
  GET  /v1/packages/:name/:version/frontmatter  → frontmatter YAML como JSON
  GET  /v1/skill/discover?q=<q>&tools=Read,Write → descubrir skills + frontmatter inline

Endpoints — Autores:
  POST /v1/authors/register                     → registrar cuenta
  POST /v1/authors/login                        → login (devuelve api_key)
  POST /v1/authors/keys                         → gestionar API keys (requiere auth)
  GET  /v1/authors/:username                    → perfil público

Auth para publicar:
  Header: Authorization: Bearer ppm_skillpub_pascalai2026skillpublish01
  (API key del usuario pascalai con permisos de escritura)

  API key histórica (también válida para pascalai):
  Header: Authorization: Bearer ppm_3eca51c5_893080112c079195cc0ea076e759d623

Nginx config (en VPS): /etc/nginx/sites-available/ppm.pascalai.org

================================================================================
3. TIPOS DE PAQUETES
================================================================================

mcp (MCP Tools) — el tipo más importante para MakerAI
------------------------------------------------------
Herramientas que implementan el Model Context Protocol. Cada una es un
ejecutable standalone (Win64/Linux64) con soporte para tres modos:
  --protocol stdio   → comunicación por stdin/stdout (Claude Desktop, MakerAI)
  --protocol http    → servidor HTTP en un puerto fijo
  --protocol sse     → Server-Sent Events

Puertos asignados por fase:
  Tier 1  (Productividad): Jira(8601), Trello(8602), Asana(8603), HubSpot(8604),
                           Stripe(8605), Twilio(8606), Notion(8607),
                           Sendgrid(8608), Zapier(8609)
  Tier 2  (Datos):         DuckDB(8610), Elasticsearch(8611), Airtable(8612)
  Tier 3  (Comunicación):  Discord(8613), WhatsApp(8614), Teams(8615)
  Fase 4  (Finance):       mcp-coingecko(8623), mcp-alphavantage(8624), mcp-paypal(8625)
  Fase 5  (AI):            mcp-ollama(8626), mcp-huggingface(8627), mcp-replicate(8628)
  Fase 6  (SaaS):          mcp-zoom(8629), mcp-zendesk(8630), mcp-salesforce(8631)
                           mcp-shopify(8632), mcp-freshdesk(8633), mcp-mailchimp(8634)
  Fase 7  (DevOps):        mcp-kubernetes(8635), mcp-kafka(8636), mcp-rabbitmq(8637)
                           mcp-terraform(8638), mcp-ansible(8639), mcp-prometheus(8640)
  Fase 8  (Cloud):         mcp-azure-blob(8641), mcp-azure-openai(8642), mcp-gcs(8643)
                           mcp-bigquery(8644), mcp-s3(8645), mcp-dynamodb(8646)
  Fase 9  (AI Utils):      mcp-memory(8647), mcp-rag(8648), mcp-sequential-thinking(8649)
                           mcp-workflow(8650), mcp-code-exec(8651)
  Fase 10 (Protocolos):    mcp-mqtt(8652), mcp-websocket(8653), mcp-graphql(8654)
                           mcp-grpc(8655)
  Puertos libres desde:    8656+

pai (PascalAI libraries)
------------------------
Librerías en lenguaje PascalAI. Se instalan en ~/.ppm/packages/ y se importan
con la cláusula uses del lenguaje.

clib (C libraries / wrappers)
------------------------------
Wrappers Delphi sobre librerías nativas: OpenCV, FFmpeg, SQLite, DuckDB, etc.
Se instalan como archivos .obj/.lib junto con una unit de interfaz Delphi.

prompt (Prompt templates)
--------------------------
Plantillas de prompts con variables {{variable}}. Se sirven por /raw y /render.
Útiles para estandarizar prompts entre agentes de un equipo.

skill (Claude Code Skills) — NUEVO en v1.3.0
---------------------------------------------
Archivos Markdown con frontmatter YAML que definen comportamiento especializado
para Claude Code. Se instalan en ~/.ppm/skills/ y se activan en Claude Code
copiándolos a ~/.claude/skills/ (o .claude/skills/ en el proyecto).

Formato del frontmatter:
  ---
  description: Hace X tarea
  allowed-tools:
    - Read
    - Write
    - Bash
  model: claude-opus-4-6   # opcional
  ---
  Instrucciones del skill en Markdown...

Skills publicados actualmente:
  skill-code-review   → revisión de seguridad, bugs y buenas prácticas
  skill-test-writer   → genera tests unitarios e integración
  skill-doc-writer    → crea documentación técnica y READMEs
  skill-refactor      → refactoring para claridad y mantenibilidad

================================================================================
4. CLI — COMANDOS DISPONIBLES
================================================================================

El cliente PPM (ppm.exe / ppm-linux-x64) es la herramienta de línea de comandos.

Instalación del cliente:
  Linux:   curl -fsSL https://ppm.pascalai.org/install.sh | bash
  Windows: irm https://ppm.pascalai.org/install.ps1 | iex

Binarios directos:
  https://ppm.pascalai.org/releases/latest/ppm-linux-x64
  https://ppm.pascalai.org/releases/latest/ppm-windows-x64.exe
  https://ppm.pascalai.org/releases/latest/version.txt  (versión actual)

Configuración del cliente (~/.ppm/config, formato INI):
  [registry]
  url = https://ppm.pascalai.org

  [auth]
  api_key = ppm_xxxx_yyyy

Comandos generales:
  ppm login                           Autenticarse en el registry
  ppm register                        Crear cuenta
  ppm search <query> [--type <tipo>]  Buscar paquetes (tipo: pai|clib|mcp|prompt|skill)
  ppm info <pkg>[@version]            Ver info detallada de un paquete
  ppm install <pkg>[@version]         Instalar paquete
  ppm uninstall <pkg>                 Desinstalar paquete
  ppm update <pkg>[@version]          Actualizar a última versión
  ppm list                            Listar paquetes instalados
  ppm outdated                        Ver paquetes con actualización disponible
  ppm lock                            Generar pai.lock desde instalados
  ppm restore                         Instalar paquetes desde pai.lock
  ppm publish [--dir <dir>]           Publicar paquete (requiere login)
  ppm init                            Crear pai.package en directorio actual
  ppm version / --version             Mostrar versión del cliente

Comandos MCP:
  ppm install <mcp-tool>              Instala en ~/.ppm/mcp/
  ppm search <q> --type mcp          Busca solo MCP tools
  ppm mcp list                        Lista MCP tools instalados (con runtime y descripción)
  ppm mcp register                    Registra todos los MCPs en Claude Desktop
                                      (escribe claude_desktop_config.json)

Comandos Skill (NUEVO):
  ppm install <skill>                 Instala en ~/.ppm/skills/
  ppm search <q> --type skill        Busca solo skills
  ppm skill list                      Lista skills instalados (con tools y descripción)
  ppm skill install-claude            Copia .skill/.md a ~/.claude/skills/ (o .claude/skills/)
                                      Claude Code los detecta automáticamente por directorio

Ejemplos completos:
  ppm search postgres --type mcp
  ppm install mcp-postgres
  ppm mcp list
  ppm mcp register

  ppm search review --type skill
  ppm install skill-code-review
  ppm skill list
  ppm skill install-claude           # activa en Claude Code

  ppm install json-parser            # paquete pai
  ppm install sqlite                 # paquete clib

================================================================================
5. INSTALAR PAQUETES PAI/CLIB EN DELPHI (MakerAI)
================================================================================

Los paquetes pai/clib se instalan en ~/.ppm/packages/ y están listos para usar
en proyectos Delphi una vez agregada la ruta a Library Path.

  ppm install sqlite          # instala el wrapper SQLite en ~/.ppm/packages/sqlite/

  En tu código Delphi:
    uses
      sqlite;                 // unit instalada por ppm

    var DB := sqlite_open('mi.db', SQLITE_OPEN_READWRITE);

Para agregar a Library Path en Delphi:
  Tools > Options > Language > Delphi > Library > Library path:
    C:\Users\<usuario>\.ppm\packages\sqlite\

================================================================================
6. USAR PAQUETES MCP EN MAKERKAI (Delphi)
================================================================================

MakerAI (E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker) tiene soporte nativo MCP.
Las unidades relevantes están en Source/MCPServer/ y Source/MCPClient/.

Unidades clave:
  uMakerAi.MCPServer.Core.pas    — Base classes: TAiMCPServer, TAiMCPToolBase
  UMakerAi.MCPServer.Stdio.pas   — Protocolo stdio
  UMakerAi.MCPServer.Http.pas    — Protocolo HTTP
  UMakerAi.MCPServer.SSE.pas     — Server-Sent Events

OPCIÓN A — MCP Tool standalone (la forma más común)
---------------------------------------------------
Cada mcp-xxx es un ejecutable independiente. Instalar con PPM y usar desde Delphi:

  ppm install mcp-github
  ppm mcp register          // actualiza claude_desktop_config.json

  En código Delphi (TAiMCPServer):
    var
      MCPServer: TAiMCPServer;
    begin
      MCPServer := TAiMCPServer.Create;
      MCPServer.Protocol := 'stdio';
      MCPServer.Command  := 'mcp-github.exe';
      MCPServer.Args     := '--protocol stdio';
      // TAiChatConnection usará las herramientas automáticamente
    end;

  O lanzarlo como servidor HTTP:
    mcp-github.exe --protocol http --port 8600
    // luego conectar desde Delphi a http://localhost:8600

OPCIÓN B — Descubrimiento automático desde el registry
------------------------------------------------------
MakerAI puede descubrir herramientas MCP en el registry:

  // Obtener tools disponibles con schema inline
  GET https://ppm.pascalai.org/v1/mcp/discover?q=github
  GET https://ppm.pascalai.org/v1/packages/mcp-github/1.2.2/schema

OPCIÓN C — Definir un tool desde cero con MakerAI
-------------------------------------------------
  type
    TMiTool = class(TAiMCPToolBase<TMiParams>)
    protected
      function ExecuteWithParams(const AParams: TMiParams;
        const AuthContext: TAiAuthContext): TJSONObject; override;
    end;

  var
    MCPServer: TAiMCPServer;
  begin
    MCPServer := TAiMCPServer.Create(nil);
    MCPServer.RegisterTool(TMiTool.Create(MCPServer));
    MCPServer.Start('stdio', 8600);
  end;

Configuración de MCPs en Claude Desktop (~/.claude/.mcp.json o claude_desktop_config.json):
  {
    "mcpServers": {
      "mcp-shell": {
        "command": "C:/Users/<usuario>/.ppm/mcp/mcp-shell/mcp-shell.exe",
        "args": ["--protocol", "stdio"]
      },
      "mcp-telegram": {
        "command": "C:/Users/<usuario>/.ppm/mcp/mcp-telegram/mcp-telegram.exe",
        "args": ["--protocol", "stdio"],
        "env": {
          "TELEGRAM_BOT_TOKEN": "...",
          "TELEGRAM_CHAT_ID": "..."
        }
      }
    }
  }

  ppm mcp register   // genera este archivo automáticamente

================================================================================
7. USAR SKILLS DE CLAUDE CODE
================================================================================

Los skills son instrucciones Markdown que Claude Code carga automáticamente
desde el directorio de skills. Permiten definir comportamiento especializado.

Flujo completo:
  1. Buscar un skill en el registry:
       ppm search code-review --type skill

  2. Instalar el skill (se guarda en ~/.ppm/skills/):
       ppm install skill-code-review

  3. Listar skills instalados:
       ppm skill list

  4. Activar en Claude Code (copia a ~/.claude/skills/):
       ppm skill install-claude

  5. En la siguiente sesión de Claude Code, el skill estará disponible
     como un comando /skill-code-review

Skills disponibles actualmente en el registry:
  skill-code-review   → Tools: Read, Glob, Grep, Bash
  skill-test-writer   → Tools: Read, Glob, Grep, Write, Edit
  skill-doc-writer    → Tools: Read, Glob, Grep, Write
  skill-refactor      → Tools: Read, Glob, Grep, Write, Edit, Bash

Crear y publicar un skill propio:
  1. ppm init                    // elegir tipo = skill, ingresar tools y modelo
                                 // genera pai.package + mi-skill.skill

  2. Editar el archivo .skill:
       ---
       description: Mi skill personalizado
       allowed-tools:
         - Read
         - Write
       model: claude-opus-4-6
       ---
       Instrucciones en Markdown...

  3. ppm publish                 // requiere api_key configurada

Descubrimiento via API (para agentes que quieran auto-instalar skills):
  GET /v1/skill/discover?q=review
  GET /v1/skill/discover?q=test&tools=Read,Write
  GET /v1/packages/skill-code-review/1.0.0/frontmatter  // solo YAML como JSON
  GET /v1/packages/skill-code-review/1.0.0/skill        // Markdown completo

Estructura de ~/.claude/skills/ (detectada automáticamente por Claude Code):
  ~/.claude/skills/
    skill-code-review.skill   ← copiado por ppm skill install-claude
    skill-test-writer.skill
    skill-refactor.skill

================================================================================
8. USAR PROMPTS DEL REGISTRY
================================================================================

Los prompts son plantillas de texto con variables {{variable}} reutilizables.

  // Obtener texto plano del prompt:
  GET /v1/packages/commit-message/1.0.0/raw

  // Obtener con variables sustituidas:
  GET /v1/packages/commit-message/1.0.0/render?tipo=feat&descripcion=login

  // Desde código Python/Delphi HTTP:
  import requests
  r = requests.get('https://ppm.pascalai.org/v1/packages/code-reviewer/1.0.0/raw')
  prompt_text = r.text

  // Instalar localmente:
  ppm install code-reviewer
  // El archivo .prompt queda en ~/.ppm/packages/code-reviewer/

================================================================================
9. PUBLICAR UN PAQUETE
================================================================================

Formato: archivo .paipkg = ZIP con estructura interna

Contenido mínimo de un .paipkg:
  pai.package          — manifiesto (formato INI) — OBLIGATORIO
  <nombre>.pai         — código fuente (paquetes pai)
  <nombre>.tool        — schema JSON (paquetes mcp)
  <nombre>.prompt      — texto del prompt (paquetes prompt)
  <nombre>.skill       — Markdown del skill (paquetes skill)

Manifiesto pai.package (ejemplo para MCP):
  [package]
  name        = mcp-miherramienta
  version     = 1.2.2
  description = Descripción de la herramienta
  author      = pascalai
  license     = MIT
  tags        = mcp,api,tools
  type        = mcp

  [mcp]
  runtime          = binary
  entrypoint       = mcp-miherramienta.exe
  entrypoint_lin64 = mcp-miherramienta

Manifiesto para skill:
  [package]
  name        = skill-mi-skill
  version     = 1.0.0
  description = Descripción del skill
  author      = pascalai
  license     = MIT
  tags        = skill,code,review
  type        = skill

  [skill]
  allowed_tools = Read,Glob,Grep
  model         = claude-opus-4-6
  file          = skill-mi-skill.skill

Publicar con el cliente PPM:
  cd mi-paquete/         // directorio con pai.package
  ppm publish            // usa api_key de ~/.ppm/config

Publicar vía API directa (Python):
  import requests
  url = 'https://ppm.pascalai.org/v1/packages/{name}/{version}'
  headers = {
      'Authorization':     'Bearer ppm_skillpub_pascalai2026skillpublish01',
      'Content-Type':      'application/octet-stream',
      'X-PPM-Type':        'mcp',           # o 'skill', 'prompt', 'pai', 'clib'
      'X-PPM-Description': 'Descripción',
      'X-PPM-Tags':        'mcp,api',
      'X-PPM-Changelog':   'Initial release',
  }
  requests.put(url, data=open('mi-tool.paipkg','rb').read(), headers=headers)

Scripts de empaquetado y publicación (MCP standalone):
  E:\Copilot\spas\mcpservice\standalone\pack_mcp_tools.py    — crea los .paipkg
  E:\Copilot\spas\mcpservice\standalone\publish_mcp_tools.py — los sube al registry

================================================================================
10. ESTRUCTURA INTERNA DE UN PAQUETE (.paipkg)
================================================================================

A) Paquete MCP standalone:
   pai.package            → manifiesto INI
   mcp-nombre.exe         → binario Windows x64
   mcp-nombre             → binario Linux x64 (opcional)
   mcp-nombre.tool        → JSON Schema del tool (describe parámetros y funciones)

B) Paquete skill:
   pai.package            → manifiesto INI
   skill-nombre.skill     → archivo Markdown con frontmatter YAML

C) Paquete prompt:
   pai.package            → manifiesto INI
   nombre.prompt          → texto del prompt con variables {{var}}

D) Paquete pai:
   pai.package            → manifiesto INI
   src/                   → archivos .pai (código fuente)

Todos son ZIPs renombrados a .paipkg. Para inspeccionar:
  unzip -l mi-paquete-1.0.0.paipkg

================================================================================
11. CREAR UN NUEVO MCP TOOL (FLUJO COMPLETO)
================================================================================

Para añadir un nuevo tool (p.ej. mcp-stripe2 en puerto 8656):

PASO 1 — Crear la unit Delphi:
  Archivo: E:\Copilot\spas\mcpservice\src\tools\Tier3\MCPTool.Stripe2.pas
  Plantilla: copiar estructura de MCPTool.Zoom.pas o MCPTool.GitHub.pas

  Patrón obligatorio:
    - TMcpStripe2Params = class con propiedades y atributos [AiMCPSchemaDescription]
    - TMcpStripe2Tool = class(TAiMCPToolBase<TMcpStripe2Params>)
    - override ExecuteWithParams → devuelve TJSONObject
    - Usar TAiMCPResponseBuilder.New.AddText(R.ToJSON).Build como resultado

PASO 2 — Generar .dpr y .dproj:
  python gen_standalone.py mcp-stripe2 8656
  (gen_standalone.py hace byte-replacement en mcp-prometheus.dproj como template)

PASO 3 — Compilar:
  Usar skill delphi-build: target=Win64, config=Release
  Para Linux64: compilar y copiar al VPS con SCP

PASO 4 — Crear el manifiesto pai.package

PASO 5 — Empaquetar:
  python pack_mcp_tools.py mcp-stripe2

PASO 6 — Publicar:
  python publish_mcp_tools.py mcp-stripe2

PASO 7 — Actualizar listas:
  - Añadir "mcp-stripe2" a TOOLS en pack_mcp_tools.py
  - Añadir "mcp-stripe2" a TOOLS en publish_mcp_tools.py

================================================================================
12. CÓDIGO FUENTE — DÓNDE ESTÁ TODO
================================================================================

MakerAI framework (Delphi components):
  E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\
  ├── Source\
  │   ├── Core\          — TAiChat, TAiCapabilities, TAiMessage...
  │   ├── Chat\          — drivers LLM (OpenAI, Claude, Gemini, Ollama...)
  │   ├── Agents\        — TAiAgentManager, grafos, checkpoints
  │   ├── RAG\           — Retrieval Augmented Generation (VQL/GQL)
  │   ├── MCPClient\     — cliente MCP (consumir tools externos)
  │   ├── MCPServer\     — servidor MCP (publicar tools)
  │   │   ├── uMakerAi.MCPServer.Core.pas   ← base class
  │   │   ├── UMakerAi.MCPServer.Http.pas
  │   │   ├── UMakerAi.MCPServer.Stdio.pas
  │   │   └── UMakerAi.MCPServer.SSE.pas
  │   ├── Tools\         — function calling, vision, audio, shell
  │   ├── ChatUI\        — componentes FMX (TChatList, TChatInput)
  │   └── Packages\      — paquetes compilables Delphi (.dpk)
  ├── Demos\             — 18+ demos de ejemplo
  └── ppm\               ← ESTE DIRECTORIO (guía IA)

MCPService — los 158 MCP tools standalone:
  E:\Copilot\spas\mcpservice\
  ├── src\tools\Tier3\        — .pas de cada tool (MCPTool.XXX.pas)
  └── standalone\
      ├── pack_mcp_tools.py    — crea los .paipkg
      ├── publish_mcp_tools.py — publica al registry
      ├── gen_standalone.py    — genera .dpr/.dproj para tool nuevo
      └── packages\            — los .paipkg generados

PPM Client + Registry + Web:
  E:\Copilot\spas\ppm\
  ├── client\            — código Free Pascal (ppm.lpr)
  │   ├── PPM.Commands.pas  — todos los comandos CLI
  │   ├── PPM.Config.pas    — config ~/.ppm/config + SkillsDir
  │   ├── PPM.Package.pas   — parsing de .paipkg + campos skill
  │   ├── PPM.HTTP.pas      — HTTP client
  │   └── PPM.Registry.pas  — comunicación con el registry
  ├── server\            — código Delphi del registry (PPMServer.dpr)
  │   └── PPM.Handlers.pas  — endpoints REST incl. /skill, /frontmatter, /skill/discover
  ├── web\               — web estática (index.html con sección Skills)
  ├── sql\
  │   ├── schema.sql          — schema completo PostgreSQL
  │   └── migration_001_skill_type.sql  — migración que añade tipo 'skill'
  ├── pkgs\              — directorios de los skills publicados
  │   ├── skill-code-review/
  │   ├── skill-test-writer/
  │   ├── skill-doc-writer/
  │   └── skill-refactor/
  ├── release.sh         — build + deploy del cliente ppm
  └── deploy_ppm_site.sh — despliega la web al VPS

================================================================================
13. CONVENCIONES DE NOMENCLATURA
================================================================================

Nombres de paquetes:
  mcp-<servicio>           → MCP tools (mcp-github, mcp-postgres, mcp-llm)
  skill-<funcion>          → Skills (skill-code-review, skill-refactor)
  <libreria>               → clib/pai (sqlite, opencv, http-client)
  <categoria>-<funcion>    → prompts (code-reviewer, commit-message)

Nombres en Delphi:
  MCPTool.<NombrePascalCase>.pas   — unit del tool
  TMcp<Nombre>Tool                 — clase del tool
  TMcp<Nombre>Params               — clase de parámetros

Nombres de ejecutables:
  mcp-nombre.dpr           → programa Delphi (guion, NO guion-bajo)
  program mcp_nombre;      → identificador Pascal (guion-bajo)
  mcp-nombre.exe           → binario Windows
  mcp-nombre               → binario Linux

================================================================================
14. ERRORES FRECUENTES Y SOLUCIONES (NOTAS PARA IA)
================================================================================

Al crear MCP tools en Delphi:

1. {$R *.res} en .dpr files — NO incluirlo (no existe .res para standalone tools)

2. Comentarios con llaves que contienen {placeholder}:
   MAL:  { GET /topics/{topic} }
   BIEN: (* GET /topics/:topic *)

3. Resp.Headers.Count — TNetHeaders es TArray<TNameValuePair>, NO un objeto:
   MAL:  for i := 0 to Resp.Headers.Count - 1 do
   BIEN: for i := 0 to Length(Resp.Headers) - 1 do

4. TNameValuePair en interface section — requiere System.Net.URLClient en uses

5. HTTP.CustomHeaders — NO EXISTE en THTTPClient de Delphi:
   BIEN: HTTP.Get(URL, nil, [TNameValuePair.Create('Authorization', 'Bearer ' + Token)])

6. c.IsLetterOrDigit — usar CharInSet(c, ['A'..'Z','a'..'z','0'..'9'])

7. IfThen — puede requerir import; mejor usar if/else directamente

8. .dproj MainSource — usar nombre con GUION:
   <MainSource>mcp-zoom.dpr</MainSource>   ← correcto

9. IHTTPResponse.ContentAsBytes — NO EXISTE:
   BIEN: HTTP.Get(URL, TBytesStream) o Resp.ContentAsString

Al crear skills / código FPC (cliente ppm):

10. var inline (Delphi): `var T := ...` no existe en FPC objfpc mode
    BIEN: declarar la variable en el bloque var:
      var T: string;
    ...
      T := algo;

11. for var T in arr — no existe en FPC; usar: for T in arr do

12. Single-char string inferido como Char con var inline:
    MAL (Delphi): var s := '[';   // puede inferirse como Char
    BIEN:         var s: string := '[';

13. CopyFile — no existe en FPC SysUtils; usar streams manuales:
    FS.CopyFrom(FD, FD.Size)

================================================================================
15. REFERENCIA RÁPIDA
================================================================================

Registry:         https://ppm.pascalai.org
API key publish:  ppm_skillpub_pascalai2026skillpublish01
                  ppm_3eca51c5_893080112c079195cc0ea076e759d623  (también válida)
Versión registry: 1.3.0 (158 MCP + 4 Skills + 200+ total)
PPM CLI versión:  1.1.0
Próximo puerto:   8656+ (libre)

VPS:      187.33.145.146  |  gustavo / C1maMaker*
DB:       PostgreSQL, usuario avance, pass Avance2024*, base ppm_registry
Servidor: PPMServer escuchando en 127.0.0.1:8765, proxied por nginx en :443

Directorios clave:
  MakerAI framework:   E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker\
  MCP tools .pas:      E:\Copilot\spas\mcpservice\src\tools\Tier3\
  MCP standalone:      E:\Copilot\spas\mcpservice\standalone\
  PPM client/web:      E:\Copilot\spas\ppm\
  Skills publicados:   E:\Copilot\spas\ppm\pkgs\

Instalación local de paquetes:
  Pai/clib/prompt:  ~/.ppm/packages/<nombre>/
  MCP tools:        ~/.ppm/mcp/<nombre>/
  Skills:           ~/.ppm/skills/<nombre>/
  Skills activos:   ~/.claude/skills/   (Claude Code los detecta automáticamente)

Comandos de un vistazo:
  ppm install mcp-postgres          → instala MCP tool
  ppm mcp register                  → activa MCPs en Claude Desktop
  ppm install skill-code-review     → instala skill
  ppm skill install-claude          → activa skills en Claude Code
  ppm search <q> --type skill       → busca skills
  ppm search <q> --type mcp         → busca MCP tools
  ppm publish                       → publica desde directorio actual

================================================================================
