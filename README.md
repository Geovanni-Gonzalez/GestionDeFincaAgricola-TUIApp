# Gestión de Finca Agrícola - TUI App

Sistema de gestión para operaciones agrícolas desarrollado en Haskell bajo el paradigma funcional.

## Requisitos

- GHC (8.10+) o GHCup
- Cabal (3.x+)

## Instalación y Compilación

1. Clona el repositorio:

   ```bash
   git clone https://github.com/geova/GestionDeFincaAgricola-TUIApp.git
   ```

2. Compila el proyecto:

   ```bash
   cabal build
   ```

## Ejecución

Inicia la aplicación con:

```bash
cabal run finca-agricola
```

## Funcionalidades Principales

- **Menú Operativo**: Registro de herramientas y parcelas (requiere login por cédula).
- **Gestión de Cosechas**: Registro de producción con validación de disponibilidad de parcelas.
- **Estadísticas**: Generación de reportes de volumen, ventas y rendimiento por trabajador.
- **Persistencia**: Los datos se guardan automáticamente en `farm_data.txt`.

## Créditos

Desarrollado para el curso de Lenguajes de Programación.
