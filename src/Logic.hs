module Logic where

import Types
import Data.Time (Day)
import Data.Time.Calendar (toGregorian)
import Data.List (maximumBy, sortBy, groupBy)
import Data.Ord (comparing, Down(..))
import Data.Function (on)

-- | Check if a plot is available in a given date range.
-- A plot is unavailable if there is a harvest that overlaps with the range.
isPlotAvailable :: String -> Day -> Day -> [Harvest] -> Bool
isPlotAvailable pId start end allHarvests =
    not $ any overlaps (filter (\h -> hPlotId h == pId) allHarvests)
  where
    overlaps h = (hPlotId h == pId) && (not (isClosed h)) && (start <= endDate h) && (end >= startDate h)

-- | Get status of all plots for each day in a range.
-- Returns [(Day, [(PlotId, Bool)])]
plotDailyStatus :: Day -> Day -> [Plot] -> [Harvest] -> [(Day, [(String, Bool)])]
plotDailyStatus start end allPlots allHarvests =
    [ (d, [(plotId p, isPlotAvailable (plotId p) d d allHarvests) | p <- allPlots])
    | d <- [start..end]]

-- | Calculate Statistics

-- 1. Parcela con mayor volumen de cosecha.
topPlotByVolume :: [Harvest] -> Maybe String
topPlotByVolume [] = Nothing
topPlotByVolume hs = Just $ hPlotId $ maximumBy (comparing (\h -> actualKg h)) hs 
-- Note: Re-implementing aggregation if needed, but for now simple top one
-- Actually, let's aggregate as requested:
topPlotByVolumeAgg :: [Harvest] -> Maybe String
topPlotByVolumeAgg [] = Nothing
topPlotByVolumeAgg hs = Just $ fst $ maximumBy (comparing snd) aggregated
  where
    groups = groupBy ((==) `on` hPlotId) $ sortBy (comparing hPlotId) hs
    aggregated = [(hPlotId (head g), sum (map actualKg g)) | g <- groups]

-- 2. Top 3 de parcelas con mayor venta (Volume * Price)
top3PlotsBySales :: [Harvest] -> [Plot] -> [String]
top3PlotsBySales hs ps = take 3 $ map fst $ sortBy (comparing (Down . snd)) sales
  where
    sales = [(plotId p, plotSales hs p) | p <- ps]
    plotSales harvests plot = sum [actualKg h * pricePerKg plot | h <- harvests, hPlotId h == plotId plot]

-- 3. Trabajador con más cosechas realizadas.
topWorkerByCount :: [Harvest] -> Maybe String
topWorkerByCount [] = Nothing
topWorkerByCount hs = Just $ fst $ maximumBy (comparing snd) counts
  where
    groups = groupBy ((==) `on` workerIdH) $ sortBy (comparing workerIdH) hs
    counts = [(workerIdH (head g), length g) | g <- groups]

-- 4. Mes-Año con mayor recolección acumulada.
topMonthByVolume :: [Harvest] -> Maybe String
topMonthByVolume [] = Nothing
topMonthByVolume hs = Just $ fst $ maximumBy (comparing snd) aggregated
  where
    toMonth h = let (y, m, _) = toGregorian (startDate h) in show m ++ "-" ++ show y
    groups = groupBy ((==) `on` fst) $ sortBy (comparing fst) [(toMonth h, actualKg h) | h <- hs]
    aggregated = [(fst (head g), sum (map snd g)) | g <- groups]

-- 5. Conteo de subproducción y sobreproducción.
productionStats :: [Harvest] -> (Int, Int)
productionStats hs = (length sub, length over)
  where
    closed = filter isClosed hs
    sub  = filter (\h -> actualKg h < targetKg h) closed
    over = filter (\h -> actualKg h > targetKg h) closed
