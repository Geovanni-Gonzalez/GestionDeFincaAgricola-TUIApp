module Types where

import Data.Time (Day)

-- | Worker information
data Role = Agronomo | Supervisor | Operario
    deriving (Show, Read, Eq)

data Worker = Worker
    { workerId    :: String
    , workerName  :: String
    , workerRole  :: Role
    } deriving (Show, Read, Eq)

-- | Tool information
data ToolType = Manual | Motorizada | Automatizada
    deriving (Show, Read, Eq)

data Tool = Tool
    { toolCode    :: String
    , toolName    :: String
    , toolDesc    :: String
    , toolType    :: ToolType
    } deriving (Show, Read, Eq)

-- | Plot information
data Plot = Plot
    { plotId      :: String
    , plotName    :: String
    , plotZone    :: String
    , plotArea    :: Double
    , veggieTypes :: [String]
    , pricePerKg  :: Double
    , toolCodes   :: [String]
    } deriving (Show, Read, Eq)

-- | Harvest information
data Harvest = Harvest
    { harvestId   :: String
    , hPlotId     :: String
    , veggieType  :: String
    , startDate   :: Day
    , endDate     :: Day
    , targetKg    :: Double
    , actualKg    :: Double -- 0 if not closed
    , workerIdH   :: String
    , isClosed    :: Bool
    } deriving (Show, Read, Eq)

-- | Global State for the application
data AppState = AppState
    { workers     :: [Worker]
    , tools       :: [Tool]
    , plots       :: [Plot]
    , harvests    :: [Harvest]
    } deriving (Show, Read)

-- | Initial workers as required by the documentation
initialWorkers :: [Worker]
initialWorkers =
    [ Worker "1" "Juan Perez" Agronomo
    , Worker "2" "Maria Lopez" Supervisor
    , Worker "3" "Carlos Ruiz" Operario
    , Worker "4" "Ana Garcia" Operario
    , Worker "5" "Luis Torres" Operario
    ]

initialState :: AppState
initialState = AppState
    { workers  = initialWorkers
    , tools    = []
    , plots    = []
    , harvests = []
    }
