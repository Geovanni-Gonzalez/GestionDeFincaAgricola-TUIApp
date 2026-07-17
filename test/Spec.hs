-- Suite Hspec + QuickCheck para la lógica pura de la finca.
-- Ejecutar: cabal test
module Main (main) where

import Data.Time.Calendar (Day, addDays, diffDays, fromGregorian)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Logic
import Types

-- Constructores de datos de prueba -----------------------------------------

d :: Int -> Day
d n = addDays (fromIntegral n) (fromGregorian 2025 1 1)

mkHarvest :: String -> String -> Day -> Day -> Double -> Double -> String -> Bool -> Harvest
mkHarvest hid pid s e target actual wid closed = Harvest
    { harvestId = hid
    , hPlotId   = pid
    , veggieType = "tomate"
    , startDate = s
    , endDate   = e
    , targetKg  = target
    , actualKg  = actual
    , workerIdH = wid
    , isClosed  = closed
    }

mkPlot :: String -> Double -> Plot
mkPlot pid price = Plot
    { plotId     = pid
    , plotName   = "Parcela " ++ pid
    , plotZone   = "Z1"
    , plotArea   = 100.0
    , veggieTypes = ["tomate"]
    , pricePerKg = price
    , toolCodes  = []
    }

-- Suite ---------------------------------------------------------------------

main :: IO ()
main = hspec $ do

  describe "isPlotAvailable" $ do
    it "detecta solapamiento con una cosecha abierta" $
      isPlotAvailable "p1" (d 5) (d 10) [mkHarvest "h1" "p1" (d 0) (d 7) 100 0 "w1" False]
        `shouldBe` False

    it "es disponible cuando los rangos no se tocan" $
      isPlotAvailable "p1" (d 10) (d 15) [mkHarvest "h1" "p1" (d 0) (d 7) 100 0 "w1" False]
        `shouldBe` True

    it "ignora cosechas cerradas" $
      isPlotAvailable "p1" (d 5) (d 10) [mkHarvest "h1" "p1" (d 0) (d 7) 100 90 "w1" True]
        `shouldBe` True

    it "ignora cosechas de otras parcelas" $
      isPlotAvailable "p1" (d 5) (d 10) [mkHarvest "h1" "p2" (d 0) (d 30) 100 0 "w1" False]
        `shouldBe` True

    prop "cualquier dia dentro de una cosecha abierta deja la parcela no disponible" $
      forAll (chooseInt (0, 20)) $ \len ->
      forAll (chooseInt (0, len)) $ \k ->
        let h = mkHarvest "h1" "p1" (d 0) (d len) 100 0 "w1" False
        in not (isPlotAvailable "p1" (d k) (d k) [h])

  describe "plotDailyStatus" $ do
    it "produce una entrada por dia del rango con todas las parcelas" $ do
      let plots = [mkPlot "p1" 1.0, mkPlot "p2" 2.0]
          status = plotDailyStatus (d 0) (d 4) plots []
      length status `shouldBe` 5
      map (length . snd) status `shouldBe` replicate 5 2

    prop "el largo del reporte es (diffDays end start) + 1" $
      forAll (chooseInt (0, 30)) $ \len ->
        let start = d 0
            end   = d len
        in length (plotDailyStatus start end [mkPlot "p1" 1.0] [])
             == fromInteger (diffDays end start) + 1

  describe "topPlotByVolumeAgg" $ do
    it "retorna Nothing sin cosechas" $
      topPlotByVolumeAgg [] `shouldBe` Nothing

    it "agrega por parcela antes de comparar" $ do
      -- p1: 10+10 = 20 supera al maximo individual de p2 (15)
      let hs = [ mkHarvest "h1" "p1" (d 0) (d 1) 10 10 "w1" True
               , mkHarvest "h2" "p1" (d 2) (d 3) 10 10 "w1" True
               , mkHarvest "h3" "p2" (d 0) (d 1) 15 15 "w2" True
               ]
      topPlotByVolumeAgg hs `shouldBe` Just "p1"

  describe "top3PlotsBySales" $ do
    it "ordena por volumen * precio y toma 3" $ do
      let plots = [mkPlot "pA" 2.0, mkPlot "pB" 1.0, mkPlot "pC" 5.0, mkPlot "pD" 9.0]
          hs = [ mkHarvest "h1" "pA" (d 0) (d 1) 10 10 "w1" True  -- 20
               , mkHarvest "h2" "pB" (d 0) (d 1) 30 30 "w1" True  -- 30
               , mkHarvest "h3" "pC" (d 0) (d 1) 1 1 "w1" True    -- 5
               ]                                                   -- pD: 0
      top3PlotsBySales hs plots `shouldBe` ["pB", "pA", "pC"]

  describe "topWorkerByCount" $ do
    it "retorna Nothing sin cosechas" $
      topWorkerByCount [] `shouldBe` Nothing

    it "elige al trabajador con mas cosechas" $ do
      let hs = [ mkHarvest "h1" "p1" (d 0) (d 1) 10 10 "w1" True
               , mkHarvest "h2" "p1" (d 2) (d 3) 10 10 "w2" True
               , mkHarvest "h3" "p2" (d 4) (d 5) 10 10 "w2" True
               ]
      topWorkerByCount hs `shouldBe` Just "w2"

  describe "topMonthByVolume" $ do
    it "agrupa por mes-anio de inicio" $ do
      let enero1  = mkHarvest "h1" "p1" (fromGregorian 2025 1 5)  (fromGregorian 2025 1 9)  10 10 "w1" True
          enero2  = mkHarvest "h2" "p1" (fromGregorian 2025 1 20) (fromGregorian 2025 1 25) 10 10 "w1" True
          febrero = mkHarvest "h3" "p1" (fromGregorian 2025 2 1)  (fromGregorian 2025 2 5)  15 15 "w1" True
      topMonthByVolume [enero1, enero2, febrero] `shouldBe` Just "1-2025"

  describe "productionStats" $ do
    it "cuenta sub y sobreproduccion solo de cosechas cerradas" $ do
      let hs = [ mkHarvest "h1" "p1" (d 0) (d 1) 100 80  "w1" True   -- sub
               , mkHarvest "h2" "p1" (d 0) (d 1) 100 120 "w1" True   -- sobre
               , mkHarvest "h3" "p1" (d 0) (d 1) 100 100 "w1" True   -- exacta
               , mkHarvest "h4" "p1" (d 0) (d 1) 100 10  "w1" False  -- abierta: no cuenta
               ]
      productionStats hs `shouldBe` (1, 1)

    prop "sub + sobre nunca excede el total de cerradas" $
      forAll (listOf (chooseInt (0, 200))) $ \actuals ->
        let hs = [ mkHarvest ("h" ++ show i) "p1" (d 0) (d 1) 100 (fromIntegral a) "w1" True
                 | (i, a) <- zip [(1 :: Int) ..] actuals ]
            (sub, over) = productionStats hs
        in sub + over <= length hs
