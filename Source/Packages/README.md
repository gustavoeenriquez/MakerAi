# Paquete Lazarus — `makerai_fpc`

Paquete `RunAndDesignTime` que agrupa las 53 unidades de `Source/` y registra en
la paleta **MakerAI** los 12 componentes que exponen `procedure Register`
(`TAiFunctions`, `TAiShell`, `TAiTextEditor`, `TAiOpenChat`, `TAiClaudeChat`,
`TAIAgentManager`, `TAiRAGGraph`, `TAiWhisper`, `TAiDalle`, `TAiOpenAIAudio`,
`TAiGeminiSpeech`, `TAiComputerUse`).

La librería sigue siendo utilizable **sin el paquete**: basta con añadir los
`-Fu` de cada carpeta de `Source/` como documenta `CLAUDE.md`. El paquete solo
hace falta para trabajar con los componentes en el diseñador de Lazarus.

## Compilar sin instalar

```powershell
lazbuild --build-all Source\Packages\makerai_fpc.lpk
```

## Instalar en el IDE

1. `Paquete → Abrir archivo de paquete (.lpk)` → `Source\Packages\makerai_fpc.lpk`
2. `Usar → Instalar` → Lazarus reconstruye la IDE y se reinicia.
3. Los componentes aparecen en la pestaña **MakerAI**.

Para usarlo desde un proyecto sin instalarlo en la IDE, registra el enlace y
añádelo como dependencia:

```powershell
lazbuild --add-package-link Source\Packages\makerai_fpc.lpk
```

## Notas

- Requiere el paquete `FCL` (viene con Lazarus). No depende de LCL: las unidades
  de `Source/` no usan nada visual.
- `IncludeFiles` apunta a `..\Core` porque varias unidades hacen
  `{$I uMakerAi.Version.inc}`.
- Salida de unidades en `Source\Packages\lib\$(TargetCPU)-$(TargetOS)`.
- Verificado con **FPC 3.2.2 / Lazarus (i386-win32)**: 48.504 líneas, 0 errores.
