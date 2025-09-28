# MakerAI v3.0 Beta 🚀

## Rama de Desarrollo - Versión 3.0.000 Beta

Esta es la rama de desarrollo de MakerAI v3.0, una evolución significativa del proyecto original con nuevas funcionalidades y mejoras arquitectónicas.

> ⚠️ **AVISO**: Esta es una versión BETA en desarrollo activo. No recomendada para uso en producción.

## 🌟 Novedades en v3.0

- Nueva arquitectura de componentes
- Mejoras en la interfaz de usuario
- Optimizaciones de rendimiento
- Nuevas funcionalidades de IA
- Soporte mejorado para múltiples plataformas

## 🛠️ Requisitos de Desarrollo

### Software Necesario
- **Embarcadero Delphi 13** (RAD Studio 13)
- **Git** para control de versiones
- **Windows 10/11** (recomendado)

### Componentes Requeridos
- FireMonkey (FMX) Components
- [Lista específica de componentes necesarios - pendiente de documentar]

## 🚀 Instalación para Desarrolladores

### 1. Clonar el Repositorio
```bash
git clone https://github.com/gustavoeenriquez/MakerAi.git
cd MakerAi
git checkout development-v3.0-beta
```

### 2. Configuración del Entorno
1. Abrir Delphi 13
2. Cargar el proyecto principal: `[nombre_proyecto].dproj`
3. Configurar las rutas de librerías si es necesario
4. Compilar en modo Debug para desarrollo

### 3. Estructura del Proyecto
```
AiMaker/
├── Demos/                  # Aplicaciones de demostración
│   ├── 011-ChatBasicCalls/ # Demo de llamadas básicas de chat
│   ├── 012-ChatAllFunctions/ # Demo completo de funciones
│   └── ...
├── Source/                 # Código fuente principal
├── Components/             # Componentes personalizados
├── Docs/                   # Documentación
└── Tests/                  # Pruebas unitarias
```

## 🤝 Colaboración y Desarrollo

### Flujo de Trabajo con Git

#### Para Nuevas Funcionalidades:
```bash
# Crear una nueva rama desde development-v3.0-beta
git checkout development-v3.0-beta
git pull origin development-v3.0-beta
git checkout -b feature/nueva-funcionalidad

# Desarrollar y hacer commits
git add .
git commit -m "feat: descripción de la nueva funcionalidad"

# Subir la rama y crear Pull Request
git push origin feature/nueva-funcionalidad
```

#### Para Corrección de Bugs:
```bash
# Crear rama para fix
git checkout -b fix/descripcion-del-bug

# Hacer los cambios necesarios
git add .
git commit -m "fix: corrección del bug específico"

# Push y Pull Request
git push origin fix/descripcion-del-bug
```

### Convenciones de Commits
- `feat:` para nuevas funcionalidades
- `fix:` para corrección de bugs
- `docs:` para cambios en documentación
- `refactor:` para refactorización de código
- `test:` para añadir o modificar tests
- `style:` para cambios de formato/estilo

## 📋 Tareas Pendientes

- [ ] Documentar componentes específicos requeridos
- [ ] Crear guías de estilo de código
- [ ] Implementar tests automatizados
- [ ] Configurar CI/CD pipeline
- [ ] Crear templates para Issues y Pull Requests

## 🐛 Reportar Bugs

Para reportar bugs, por favor crear un Issue en GitHub con:
- Descripción detallada del problema
- Pasos para reproducir
- Versión de Delphi utilizada
- Screenshots si aplica

## 📞 Contacto y Soporte

- **Desarrollador Principal**: Gustavo Enríquez
- **Email**: [tu-email@ejemplo.com]
- **GitHub Issues**: Para bugs y sugerencias
- **Discussions**: Para preguntas generales

## 📄 Licencia

[Especificar la licencia del proyecto]

---

## 🔄 Historial de Versiones

### v3.0.000 Beta (Actual)
- Migración inicial a nueva estructura
- Configuración de rama de desarrollo
- Implementación de .gitignore optimizado

### Versiones Anteriores
- Ver rama `master` para historial de v2.x

---

**¡Gracias por contribuir al desarrollo de MakerAI v3.0!** 🎉