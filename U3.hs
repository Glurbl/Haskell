import Data.List (sort)

data Prozess = Prozess { pid :: String
                       , arrival :: Int
                       , computing :: Int } deriving (Show)

instance Eq Prozess where
    Prozess { computing = a } == Prozess { computing = b } = a == b

instance Ord Prozess where
    compare x y
        | computing x < computing y = LT
        | computing x > computing y = GT
        | otherwise                 = EQ

data State = State { new   :: [Prozess]
                   , run   :: Prozess
                   , ready :: [Prozess]
                   , time  :: Int
                   , chart :: String } 

-- Idle Prozess
idle :: Prozess
idle = Prozess { pid = "IDLE", arrival = -1, computing = -1 }

--Test ohne show mit "print start" ausgabe funktionert ist aber super unübersichtlich, deshalb Aufgabe 1 


ps :: [Prozess]
ps = [ Prozess { pid = "P1", arrival = 0,  computing = 6 }
     , Prozess { pid = "P2", arrival = 2,  computing = 6 }
     , Prozess { pid = "P3", arrival = 4,  computing = 5 }
     , Prozess { pid = "P4", arrival = 12, computing = 4 }
     , Prozess { pid = "P5", arrival = 16, computing = 3 }
     , Prozess { pid = "P6", arrival = 19, computing = 6 } ]

start :: State
start = State { new = ps, run = idle, ready = [], time = 0, chart = "" }

--Aufgabe 1 --anstelle von "deriving (Show)" definieren wir selber wie "State" ausgegeben wird
instance Show State where
    show s = unlines 
        ([ "-- new" ]
        ++ map show (new s)
        ++ [ "-- run" ]
        ++ [ show (run s) ]
        ++ [ "-- ready" ]
        ++ map show (ready s)
        ++ [ "-- time: " ++ show (time s) ]
        ++ [ "-- chart: " ++ chart s ])
    

    --Aufgabe 2 
    update_ready :: State -> State      --Funktion nicht "State " und gibt aktualisiertn State zurück
    update_ready s = s { new   = nichtDa, ready = ready s ++ angekommen } --kopiert State s und erstetzt new und ready 
    where (angekommen, nichtDa) = splitByTime (time s) (new s)

    --Hilfsfunktion
    splitByTime :: Int -> [Prozess] -> ([Prozess], [Prozess])
splitByTime t [] = ([], [])
splitByTime t (x:xs)
    | arrival x == t = (x : angekommen, nichtDa)
    | otherwise      = (angekommen, x : nichtDa)
    where (angekommen, nichtDa) = splitByTime t xs