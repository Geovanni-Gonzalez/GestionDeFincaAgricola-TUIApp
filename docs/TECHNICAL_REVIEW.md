# TECHNICAL_REVIEW — GestionDeFincaAgricola-TUIApp

Fecha de revisión: 2026-07-16
Método: análisis estático, enunciado (`docs/Proyecto Programado 2 - 2025 I.md`, IC-4700), cabal, CI y git. Sin ejecución en esta pasada; CI compila con GHC 9.4.8 (`cabal build all`).

## 1. Comprensión del proyecto

Sistema TUI de gestión de finca agrícola en **Haskell** (~670 LOC, 6 módulos): parcelas, herramientas, trabajadores, cosechas, disponibilidad por fechas y reportes estadísticos, con persistencia en `farm_data.txt` (Show/Read) y `tools.csv`. Proyecto del paradigma funcional del curso Lenguajes de Programación. Uno de los dos proyectos Haskell del portafolio (junto a `AnalisisDeVentas`).

## 2. Cumplimiento del enunciado

| Requisito | Estado | Evidencia |
|---|---|---|
| Menú recurrente con submenús operativos/estadísticos y navegación | 🟦 | `src/Menu.hs` (395 LOC) |
| Registro/consulta de parcelas, herramientas, trabajadores, cosechas | 🟦 | `Types.hs` (ADTs `Plot`, `Tool`, `Worker`, `Harvest`), `Menu.hs` |
| Disponibilidad de parcelas por rango de fechas | ✅ estático | `Logic.hs` → `isPlotAvailable` (solapamiento de rangos con `Data.Time.Day`), `plotDailyStatus` |
| Reportes estadísticos (top parcelas por volumen/venta, etc.) | 🟦 | `Logic.hs` → `topPlotByVolumeAgg` (groupBy+sortBy+aggregate), etc. |
| Paradigma funcional: inmutabilidad, funciones puras | ✅ | `Logic.hs` es puro (sin IO); IO confinado a `Menu.hs`/`Files.hs` |
| Persistencia en archivos | 🟦 | `Files.hs` (Show/Read + CSV) |

## 3. Fortalezas

1. Modelado con ADTs y tipos suma (`Role`, `ToolType`) con derivación `Show/Read/Eq` — persistencia sin dependencias.
2. Separación pura/impura correcta: la lógica de negocio (`Logic.hs`) no toca IO, exactamente lo que el enunciado evalúa.
3. Lógica de fechas real (solapamiento de rangos, expansión día a día con `[start..end]`).
4. `-Wall` activado en el cabal y CI con GHC/cabal pineados.

## 4. Debilidades y riesgos

| Hallazgo | Severidad | Nota |
|---|---|---|
| ~~`author: Antigravity / maintainer: antigravity@google.com` en el `.cabal`~~ | Alta (credibilidad) | **Corregido en esta pasada** — era un resto de herramienta AI, riesgo serio ante un reclutador |
| Comentarios conversacionales residuales ("Actually, let's aggregate as requested") en `Logic.hs` | Media (credibilidad) | Limpiar; misma causa que lo anterior |
| ~~10 artefactos `.hi`/`.o` trackeados en `src/`~~ | — | Corregido: `git rm --cached` + `.gitignore` |
| Sin tests (0) | Media | La lógica pura de `Logic.hs` es trivial de testear con HUnit/QuickCheck |
| `Menu.hs` concentra 60% del código | Baja | Aceptable para TUI |

## 5. Evaluación profesional

- Nivel demostrado: **Junior+ en programación funcional** — correcto y idiomático a nivel básico-medio (ADTs, pureza, HOFs, composición); sin typeclasses propias, monads avanzadas ni property testing.
- Rol en el portafolio: evidencia del **paradigma funcional**, valiosa por diversidad de paradigmas (imperativo/OO/funcional/ensamblador).

## 6. Recomendaciones

Ver `IMPROVEMENT_ROADMAP.md`. P0: limpiar comentarios conversacionales; commitear las correcciones aplicadas.
