# Capítulo 34 — componentes anexos

`AnexosLadera.dproj` usa dos repositorios que no están en este:

- https://github.com/gustavoeenriquez/delphi-libraries → variable de entorno `DELPHI_LIBRARIES`
- https://github.com/gustavoeenriquez/MakerAi.ChatTools  → variable de entorno `MAKERAI_CHATTOOLS`

El `.dproj` arma las rutas de unidades con esas dos variables (`$(DELPHI_LIBRARIES)\audio\src`, …).
Compilar desde la línea de comandos con ambas definidas, o agregar las carpetas al Library Path del IDE.
`entrada\` trae lo que la consola lee (la factura del capítulo 10, dos voces del 27, el manual, las
tripletas y un skill); `salida\` se crea al correr. Café La Ladera: empresa, productos y precios ficticios.
