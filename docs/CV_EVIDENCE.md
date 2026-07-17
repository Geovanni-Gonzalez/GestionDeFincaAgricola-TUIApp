# CV_EVIDENCE — GestionDeFincaAgricola-TUIApp

Verifiable, interview-defensible material. All claims map to files in this repository.

## Resume bullets (pick & adapt)

- Built a farm-management terminal application in Haskell (~670 LOC) modeling plots, tools, workers, and harvests as algebraic data types, with file-based persistence via derived Show/Read instances and CSV.
- Kept business logic 100% pure (date-range overlap detection for plot availability, production statistics via groupBy/sortBy aggregation), confining all IO to the menu and file layers — demonstrating functional-programming discipline (immutability, pure functions, higher-order functions).
- Set up a Cabal build with `-Wall` and GitHub Actions CI on pinned GHC 9.4.8.

## Skills matrix

| Skill | Evidence | Depth | Confidence |
|---|---|---|---|
| Haskell (ADTs, records, sum types, deriving) | `src/Types.hs` | Basic-Medium | High |
| Pure functional design (IO segregation) | `src/Logic.hs` (no IO) vs `Menu.hs`/`Files.hs` | Medium | High |
| Higher-order functions & composition | `Logic.hs` — `maximumBy`, `groupBy`, `comparing`, `on` | Medium | High |
| Date/time logic | `Data.Time` range overlaps, day expansion | Basic-Medium | High |
| Cabal + GHC toolchain, CI | `finca-agricola.cabal`, `.github/workflows/ci.yml` | Basic-Medium | High |

## What this project proves

- First appearance of: **functional paradigm** (Haskell), ADT modeling, pure/impure separation.
- Portfolio role: paradigm-diversity evidence — together with imperative (C/ASM), OO (Java/Python), and web (TS/JS) projects it shows multi-paradigm range, a talking point for languages/PL-theory interviews.
- Reinforces: CI, file persistence, TUI design.

## ATS keywords

Haskell, functional programming, algebraic data types, immutability, pure functions, higher-order functions, pattern matching, Cabal, GHC, type systems, multi-paradigm programming.
