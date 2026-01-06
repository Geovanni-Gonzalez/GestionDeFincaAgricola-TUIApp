module Menu where

import Types
import Logic
import Files
import UIUtils
import System.IO
import Data.Time (Day, parseTimeOrError, defaultTimeLocale)
import Control.Monad (when)

-- | Main application loop
runApp :: AppState -> IO ()
runApp state = do
    clearScreen
    printBanner
    printHeader green "MENÚ PRINCIPAL"
    printBox white
        [ "1. Opciones Operativas (Acceso Trabajadores)"
        , "2. Opciones Generales (Gestión Global)"
        , "3. Salir y Guardar Estado"
        ]
    putStr (cyan ++ b ++ " > " ++ res ++ "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> loginWorker state
        "2" -> generalMenu state
        "3" -> do
            clearScreen
            printHeader yellow "SALIENDO DEL SISTEMA"
            printInfo "Guardando estado en farm_data.txt..."
            saveState state
            printSuccess "¡Información guardada con éxito! Hasta pronto."
        _   -> do
            printError "Opción inválida. Intente de nuevo."
            hFlush stdout
            _ <- getLine -- Wait for user
            runApp state

-- | Worker Login for Operational Menu
loginWorker :: AppState -> IO ()
loginWorker state = do
    clearScreen
    printHeader green "ACCESO OPERATIVO"
    putStr (cyan ++ b ++ " ? " ++ res ++ "Ingrese su cédula: ")
    hFlush stdout
    cedula <- getLine
    case filter (\w -> workerId w == cedula) (workers state) of
        (w:_) -> do
            printSuccess $ "Bienvenido(a), " ++ b ++ workerName w ++ res ++ " (" ++ show (workerRole w) ++ ")"
            _ <- getLine -- Pause
            operationalMenu state w
        [] -> do
            printError "Cédula no registrada en el sistema de la finca."
            _ <- getLine -- Pause
            runApp state

-- | Operational Menu (Requires Login)
operationalMenu :: AppState -> Worker -> IO ()
operationalMenu state worker = do
    clearScreen
    printHeader green $ "MENÚ OPERATIVO - " ++ workerName worker
    printBox green
        [ "1. Cargar y Mostrar Herramientas de Campo"
        , "2. Registrar y Mostrar Parcelas de Cultivo"
        , "3. Informe de Cosechas"
        , "4. Volver al Menú Principal"
        ]
    putStr (cyan ++ b ++ " > " ++ res ++ "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> toolsFlow state worker
        "2" -> plotsFlow state worker
        "3" -> harvestsFlow state worker
        "4" -> runApp state
        _   -> operationalMenu state worker

-- | Tools Flow
toolsFlow :: AppState -> Worker -> IO ()
toolsFlow state worker = do
    clearScreen
    printHeader green "GESTIÓN DE HERRAMIENTAS"
    putStr (cyan ++ b ++ " ? " ++ res ++ "Ingrese la ruta del archivo CSV: ")
    hFlush stdout
    path <- getLine
    newTools <- loadToolsFromCSV path
    let existingCodes = map toolCode (tools state)
    let filteredNew = filter (\t -> not (toolCode t `elem` existingCodes)) newTools
    let updatedTools = tools state ++ filteredNew
    let newState = state { tools = updatedTools }
    
    printInfo "Sincronizando herramientas..."
    
    putStrLn $ "\n" ++ b ++ u ++ "Inventario Completo de Herramientas:" ++ res
    mapM_ (putStrLn . show) updatedTools
    
    when (not (null filteredNew)) $ do
        putStrLn $ "\n" ++ green ++ b ++ "[NUEVAS HERRAMIENTAS CONFIGURADAS]" ++ res
        mapM_ (putStrLn . (green ++) . show) filteredNew
        
    saveState newState
    printInfo "Presione ENTER para volver..."
    _ <- getLine
    operationalMenu newState worker

-- | Plots Flow
plotsFlow :: AppState -> Worker -> IO ()
plotsFlow state worker = do
    clearScreen
    printHeader green "PARCELAS DE CULTIVO"
    printBox green
        [ "1. Registrar Nueva Parcela"
        , "2. Consultar Parcela por Código"
        , "3. Volver"
        ]
    putStr (cyan ++ b ++ " > " ++ res ++ "Opción: ")
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> registerPlot state worker
        "2" -> consultPlot state worker
        "3" -> operationalMenu state worker
        _   -> plotsFlow state worker

registerPlot :: AppState -> Worker -> IO ()
registerPlot state worker = do
    putStrLn $ blue ++ b ++ "Complete los datos de la nueva parcela:" ++ res
    putStr " Identificador (ID): " >> hFlush stdout
    pId <- getLine
    putStr " Nombre: " >> hFlush stdout
    pName <- getLine
    putStr " Zona: " >> hFlush stdout
    pZone <- getLine
    putStr " Área (m2): " >> hFlush stdout
    areaStr <- getLine
    putStr " Vegetales (Papa, Tomate...): " >> hFlush stdout
    vegStr <- getLine
    putStr " Precio por kilo ($): " >> hFlush stdout
    priceStr <- getLine
    putStr " Códigos de herramientas (HR001,...): " >> hFlush stdout
    toolStr <- getLine
    
    let newPlot = Plot pId pName pZone (read areaStr) (splitComma vegStr) (read priceStr) (splitComma toolStr)
    let newState = state { plots = plots state ++ [newPlot] }
    printSuccess $ "Parcela registrada con éxito. ID Asignado: " ++ pId
    saveState newState
    _ <- getLine
    operationalMenu newState worker

consultPlot :: AppState -> Worker -> IO ()
consultPlot state worker = do
    putStr (cyan ++ b ++ " ? " ++ res ++ "Ingrese código de parcela: ")
    hFlush stdout
    pId <- getLine
    case filter (\p -> plotId p == pId) (plots state) of
        (p:_) -> do
            printBox white [show p]
            let associatedTools = filter (\t -> toolCode t `elem` toolCodes p) (tools state)
            putStrLn $ b ++ "Herramientas asignadas a esta parcela:" ++ res
            if null associatedTools
                then putStrLn "  (Ninguna herramienta asignada)"
                else mapM_ (putStrLn . ("  - " ++) . show) associatedTools
        [] -> printError "La parcela indicada no existe."
    _ <- getLine
    operationalMenu state worker

-- | Harvests Flow (Operational)
harvestsFlow :: AppState -> Worker -> IO ()
harvestsFlow state worker = do
    clearScreen
    printHeader green "REPORTE DE COSECHAS REGISTRADAS"
    if null (harvests state)
        then printInfo "No hay cosechas registradas en el sistema."
        else mapM_ (putStrLn . show) (harvests state)
    
    printHeader yellow "ESTADÍSTICAS RÁPIDAS"
    putStrLn $ " Parcela con mayor volumen histórico: " ++ b ++ show (topPlotByVolumeAgg (harvests state)) ++ res
    
    printInfo "Presione ENTER para volver..."
    _ <- getLine
    operationalMenu state worker

-- | General Menu
generalMenu :: AppState -> IO ()
generalMenu state = do
    clearScreen
    printHeader blue "OPCIONES GENERALES DE GESTIÓN"
    printBox blue
        [ "1. Registrar Cosecha (Gestión)"
        , "2. Cierre de Cosecha (Kilos reales)"
        , "3. Consulta Detallada de Cosecha"
        , "4. Consulta disponibilidad de Parcela"
        , "5. Informe Estadístico Completo"
        , "6. Cancelación de Cosecha"
        , "7. Modificación de Cosecha"
        , "8. Volver al Menú Principal"
        ]
    putStr (cyan ++ b ++ " > " ++ res ++ "Seleccione una opción: ")
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> registerHarvest state
        "2" -> closeHarvest state
        "3" -> consultHarvest state
        "4" -> availabilityMenu state
        "5" -> showFullStats state
        "6" -> cancelHarvest state
        "7" -> modifyHarvest state
        "8" -> runApp state
        _   -> generalMenu state

registerHarvest :: AppState -> IO ()
registerHarvest state = do
    printHeader blue "REGISTRO DE COSECHA"
    putStr " ID Trabajador Responsable: " >> hFlush stdout
    wId <- getLine
    putStr " ID Parcela a Cultivar: " >> hFlush stdout
    pId <- getLine
    putStr " Fecha Inicio (YYYY-MM-DD): " >> hFlush stdout
    startStr <- getLine
    putStr " Fecha Fin (YYYY-MM-DD): " >> hFlush stdout
    endStr <- getLine
    putStr " Tipo Vegetal: " >> hFlush stdout
    veg <- getLine
    putStr " Cantidad Objetivo (kg): " >> hFlush stdout
    kgStr <- getLine
    
    let startD = parseDate startStr
    let endD = parseDate endStr
    let kg = read kgStr
    
    if not (any (\p -> plotId p == pId && veg `elem` veggieTypes p) (plots state))
        then do
            printError "La parcela no es válida o el cultivo no está permitido allí."
            _ <- getLine
            generalMenu state
        else if not (isPlotAvailable pId startD endD (harvests state))
            then do
                printError "Conflicto de fechas: La parcela ya está ocupada en ese rango."
                _ <- getLine
                generalMenu state
            else do
                let hId = "CO" ++ show (length (harvests state) + 1)
                let newH = Harvest hId pId veg startD endD kg 0 wId False
                let newState = state { harvests = harvests state ++ [newH] }
                printSuccess $ "Cosecha programada exitosamente. ID: " ++ hId
                saveState newState
                _ <- getLine
                generalMenu newState

closeHarvest :: AppState -> IO ()
closeHarvest state = do
    putStr (cyan ++ b ++ " ? " ++ res ++ "ID de la Cosecha a cerrar: ")
    hFlush stdout
    hId <- getLine
    putStr (cyan ++ b ++ " ? " ++ res ++ "Cantidad real recolectada (kg): ")
    hFlush stdout
    kgStr <- getLine
    let kg = read kgStr
    let updatedHs = map (\h -> if harvestId h == hId then h { actualKg = kg, isClosed = True } else h) (harvests state)
    let newState = state { harvests = updatedHs }
    printSuccess "Los datos de recolección han sido actualizados y la cosecha cerrada."
    saveState newState
    _ <- getLine
    generalMenu newState

consultHarvest :: AppState -> IO ()
consultHarvest state = do
    putStr (cyan ++ b ++ " ? " ++ res ++ "ID de la Cosecha: ")
    hFlush stdout
    hId <- getLine
    case filter (\h -> harvestId h == hId) (harvests state) of
        (h:_) -> printBox white [show h]
        [] -> printError "El identificador de cosecha no existe."
    _ <- getLine
    generalMenu state

cancelHarvest :: AppState -> IO ()
cancelHarvest state = do
    putStr (cyan ++ b ++ " ? " ++ res ++ "ID de Cosecha a eliminar: ")
    hFlush stdout
    hId <- getLine
    let toCancel = filter (\h -> harvestId h == hId) (harvests state)
    case toCancel of
        (h:_) | not (isClosed h) -> do
            let newState = state { harvests = filter (\x -> harvestId x /= hId) (harvests state) }
            printSuccess "La cosecha ha sido eliminada permanentemente del sistema."
            saveState newState
            generalMenu newState
        (h:_) -> do
            printError "Error: No se pueden eliminar cosechas que ya han sido cerradas."
            _ <- getLine
            generalMenu state
        [] -> do
            printError "No se encontró ninguna cosecha con ese ID."
            _ <- getLine
            generalMenu state

modifyHarvest :: AppState -> IO ()
modifyHarvest state = do
    putStr (cyan ++ b ++ " ? " ++ res ++ "ID de Cosecha a modificar: ")
    hFlush stdout
    hId <- getLine
    let toMod = filter (\h -> harvestId h == hId) (harvests state)
    case toMod of
        (h:_) | not (isClosed h) -> do
            putStrLn $ i ++ "(Deje en blanco para conservar el valor actual)" ++ res
            putStr " Nueva Parcela: " >> hFlush stdout
            pId <- getLine
            putStr " Nueva Fecha Inicio: " >> hFlush stdout
            sStr <- getLine
            putStr " Nueva Fecha Fin: " >> hFlush stdout
            eStr <- getLine
            putStr " Nuevo Vegetal: " >> hFlush stdout
            veg <- getLine
            
            let finalPId = if null pId then hPlotId h else pId
            let finalStart = if null sStr then startDate h else parseDate sStr
            let finalEnd = if null eStr then endDate h else parseDate eStr
            let finalVeg = if null veg then veggieType h else veg
            
            if not (isPlotAvailable finalPId finalStart finalEnd (filter (\x -> harvestId x /= hId) (harvests state)))
                then do
                    printError "Error: Los nuevos datos generan un conflicto de disponibilidad."
                    _ <- getLine
                    generalMenu state
                else do
                    let newH = h { hPlotId = finalPId, startDate = finalStart, endDate = finalEnd, veggieType = finalVeg }
                    let newState = state { harvests = map (\x -> if harvestId x == hId then newH else x) (harvests state) }
                    printSuccess "La planificación de la cosecha ha sido actualizada."
                    saveState newState
                    _ <- getLine
                    generalMenu newState
        (h:_) -> do
            printError "No es posible modificar una cosecha que ya está cerrada."
            _ <- getLine
            generalMenu state
        [] -> do
            printError "ID de cosecha no encontrado."
            _ <- getLine
            generalMenu state

availabilityMenu :: AppState -> IO ()
availabilityMenu state = do
    clearScreen
    printHeader blue "CONSULTA DE DISPONIBILIDAD"
    printBox blue
        [ "1. Listar Parcelas Libres en Rango"
        , "2. Ver Calendario de Estado diario"
        ]
    putStr (cyan ++ b ++ " > " ++ res)
    hFlush stdout
    op <- getLine
    putStr " Fecha Inicio: " >> hFlush stdout
    sStr <- getLine
    putStr " Fecha Fin: " >> hFlush stdout
    eStr <- getLine
    let start = parseDate sStr
    let end = parseDate eStr
    case op of
        "1" -> do
            let avail = filter (\p -> isPlotAvailable (plotId p) start end (harvests state)) (plots state)
            printHeader green "PARCELAS DISPONIBLES"
            if null avail then putStrLn " Ninguna parcela libre en este rango."
            else mapM_ (putStrLn . (" - " ++) . plotName) avail
        "2" -> do
            let status = plotDailyStatus start end (plots state) (harvests state)
            printHeader white "CALENDARIO DE OCUPACIÓN"
            mapM_ (\(d, ps) -> putStrLn (show d ++ ": " ++ show ps)) status
        _ -> printError "Opción no reconocida."
    _ <- getLine
    generalMenu state

showFullStats :: AppState -> IO ()
showFullStats state = do
    clearScreen
    printHeader magenta "INFORME ESTADÍSTICO DE PRODUCCIÓN"
    let (sub, over) = productionStats (harvests state)
    printBox white 
        [ "Parámetro                        | Resultado"
        , "---------------------------------|---------------------------"
        , "1. Parcela con Mayor Volumen     | " ++ maybe "N/A" id (topPlotByVolumeAgg (harvests state))
        , "2. Ranking Top 3 Ventas          | " ++ show (top3PlotsBySales (harvests state) (plots state))
        , "3. Trabajador con más Cosechas   | " ++ maybe "N/A" id (topWorkerByCount (harvests state))
        , "4. Mes de Máxima Recolección     | " ++ maybe "N/A" id (topMonthByVolume (harvests state))
        , "5. Eficiencia de Producción      | Sub: " ++ show sub ++ " | Sobre: " ++ show over
        ]
    printInfo "Presione ENTER para continuar..."
    _ <- getLine
    generalMenu state

-- Helper for date parsing
parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"
