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
    --Hilfsfunktion
splitByTime :: Int -> [Prozess] -> ([Prozess], [Prozess])
splitByTime t [] = ([], [])
splitByTime t (x:xs)
    | arrival x == t = (x : angekommen, nichtDa)
    | otherwise      = (angekommen, x : nichtDa)
    where (angekommen, nichtDa) = splitByTime t xs

    --Hauptfunktion
update_ready :: State -> State
update_ready s = s { new   = nichtDa
                   , ready = ready s ++ angekommen }
    where (angekommen, nichtDa) = splitByTime (time s) (new s)

--Aufgabe 3             
update_run :: State -> State
update_run s
    | pid (run s) == "IDLE" && not (null (ready s)) =
        s { run   = head (ready s)
          , ready = tail (ready s) }
    | otherwise = s

                {-zum starten Schritt 1: update_ready testen
                         let s1 = update_ready start

                Schritt 2: update_run darauf anwenden
                        let s2 = update_run s1
                Ausgeben
                      print s2-}

--Aufgabe 4
update_time :: State -> State
update_time s = s { run   = neuerRun
                  , time  = time s + 1
                  , chart = chart s ++ pid (run s) ++ " " }
    where
        neuzeit  = computing (run s) - 1
        neuerRun
            | neuzeit == 0 = idle
            | otherwise    = (run s) { computing = neuzeit }
            {-Ausführen: let s1 = update_ready start
            let s2 = update_run s1
            let s3 = update_time s2
            print s3-}

--Aufgabe 5

-- Ein einzelner Schritt
step :: State -> State
step s = (update_time . update_run) (update_ready s)

-- Prüft ob alle Prozesse fertig sind
fertig :: State -> Bool
fertig s = null (new s)
        && null (ready s)
        && pid (run s) == "IDLE"

-- Wiederholt step bis alle fertig sind
fcfs :: State -> State
fcfs s
    | fertig s  = s
    | otherwise = fcfs (step s) --Ausführen putStrLn (chart (fcfs start))

--Aufgabe 7
update_ready_sjf :: State -> State
update_ready_sjf s = s { new   = nichtDa
                       , ready = sort (ready s ++ angekommen) }
    where (angekommen, nichtDa) = splitByTime (time s) (new s)




update_ready_srtf :: State -> State
update_ready_srtf s = s { new   = nichtDa
                        , run   = neuerRun
                        , ready = sort neueReady }
    where
        (angekommen, nichtDa) = splitByTime (time s) (new s)
        alleReady  = ready s ++ angekommen
        neuerRun
            | not (null alleReady) &&
              head (sort alleReady) < run s = head (sort alleReady)
            | otherwise                     = run s
        neueReady
            | not (null alleReady) &&
              head (sort alleReady) < run s = run s : tail (sort alleReady)
            | otherwise                     = alleReady

step_sjf :: State -> State
step_sjf = update_time . update_run . update_ready_sjf

sjf :: State -> State
sjf s
    | fertig s  = s
    | otherwise = sjf (step_sjf s)

step_srtf :: State -> State
step_srtf = update_time . update_run . update_ready_srtf

srtf :: State -> State
srtf s
    | fertig s  = s
    | otherwise = srtf (step_srtf s)