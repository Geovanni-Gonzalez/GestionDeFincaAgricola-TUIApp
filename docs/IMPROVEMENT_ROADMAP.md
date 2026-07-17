# IMPROVEMENT_ROADMAP — GestionDeFincaAgricola-TUIApp

Backlog priorizado. Impacto/Esfuerzo: Alto/Medio/Bajo.

## Quick Wins

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 1 | Commitear las correcciones aplicadas en esta revisión: autor del `.cabal` (era "Antigravity"), untracking de `.hi`/`.o`, link de imagen del README | Alto | Bajo | P0 |
| 2 | Limpiar comentarios conversacionales de IA en `Logic.hs` ("Actually, let's aggregate...", "Note: Re-implementing...") y decidir entre `topPlotByVolume` y `topPlotByVolumeAgg` (hay dos versiones) | Alto (credibilidad) | Bajo | P0 |
| 3 | GitHub Topics: `haskell`, `functional-programming`, `tui`, `cabal` + descripción | Medio | Bajo | P1 |

## Mejoras técnicas

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 4 | ✅ Aplicado: suite Hspec + QuickCheck (`test/Spec.hs`) cubriendo las 7 funciones puras de `Logic.hs`, incl. propiedades de solapamiento de fechas | Alto | Medio | P1 |
| 5 | ✅ Aplicado: CI ejecuta `cabal test`. **Verificar la primera corrida de Actions** — la suite se escribió sin GHC local | Medio | Bajo | P1 |
| 6 | Manejo de errores de parsing en `Files.hs` (hoy `read` puede lanzar excepción con datos corruptos) — `readMaybe` + `Either` | Medio | Bajo | P2 |

## Mejoras arquitectónicas

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 7 | Introducir `StateT`/`ReaderT` para el estado global en lugar de pasarlo manualmente (evidenciaría monads transformers) | Medio | Medio | P3 |

## Mejoras de GitHub

Ya presentes: CI, LICENSE (MIT), enunciado en docs, imagen. Faltan: Topics (item 3), badge CI en README.
