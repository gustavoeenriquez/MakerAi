# Capítulo 36 — La voz por dentro

- `LaderaApp.dproj`: la app con la fila de audio (micrófono y parlante elegibles: `Microfono.DeviceId`,
  `Parlante.DeviceId`). `--captura-audio x.png` en `--prueba` guarda esa fila.
- `VozPorDentroLadera.dproj` (consola): `--dispositivos`, `--loopback` (transcripción en vivo de lo que suena +
  `Muted`), `--llamada` (un cliente en inglés por VB-CABLE, traducido en vivo en los dos sentidos). Sin argumentos
  corre los tres. Si hay VB-CABLE nada suena en los parlantes; si no, se usan los predeterminados.
- `media\`: las voces TTS (`cliente-ingles.wav`, `lucia-respuesta.wav`, y las de la clienta del cap 27).
  `salida\` se crea con las voces traducidas.

Requiere `OPENAI_API_KEY`. VB-CABLE (vb-audio.com, donationware; no se redistribuye con la app): ver
`Docs/Version 3/uMakerAi-AudioBridge.md`. Café La Ladera: empresa, productos y precios ficticios.
