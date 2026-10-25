# El registry corporativo de Café La Ladera, en tu máquina (capítulo 33)

1. PostgreSQL local. Crear la base y aplicar el esquema del repo de PPM (`sql/`):

       psql -U postgres -c "CREATE DATABASE ppm_ladera"
       psql -U postgres -d ppm_ladera -f sql\schema.sql
       psql -U postgres -d ppm_ladera -f sql\migration_001_skill_type.sql
       psql -U postgres -d ppm_ladera -f sql\migration_002_skill_name.sql

2. `PPMServer.exe` (Win64) compilado desde `server/PPMServer.dproj` del repo
   https://github.com/gustavoeenriquez/ppm, copiado a esta carpeta.

3. En el entorno, `PPM_DB_PASS` (la del usuario de PostgreSQL) y `PPM_SECRET_KEY`
   (cualquier cadena larga al azar). Luego `iniciar_registry.cmd`: el registry queda en
   http://127.0.0.1:8766 (`GET /v1/health`).

4. Crear la cuenta y la clave de publicación (una sola vez):

       curl -X POST http://127.0.0.1:8766/v1/authors/register -H "Content-Type: application/json" -d "{\"username\":\"ladera\",\"email\":\"ti@cafelaladera.co\",\"password\":\"...\",\"full_name\":\"Café La Ladera\"}"
       curl -X POST http://127.0.0.1:8766/v1/authors/login    -H "Content-Type: application/json" -d "{\"username\":\"ladera\",\"password\":\"...\"}"

   La respuesta del login trae `api_key` (se muestra una sola vez).

5. Apuntar el CLI al corporativo (`~\.ppm\config`: `[registry] url=http://127.0.0.1:8766`,
   `[auth] api_key=...`) y publicar desde cada carpeta de `pkgs\`:

       cd pkgs\lucia-barista
       ppm publish

6. Desde Delphi, `RegistryLadera.exe` (o `REGISTRY_LADERA=http://...` para otra URL).

En producción (capítulo 33): Linux, `systemd`, nginx con TLS delante del puerto interno, y el
archivo de entorno con permisos 600. Café La Ladera: empresa, productos y precios ficticios.
