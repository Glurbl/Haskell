{- HLINT ignore "Use camelCase" -}
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


ps :: [Prozess]                                                 --Testeingabe 
ps = [ Prozess { pid = "P1", arrival = 0,  computing = 6 }
     , Prozess { pid = "P2", arrival = 2,  computing = 6 }
     , Prozess { pid = "P3", arrival = 4,  computing = 5 }
     , Prozess { pid = "P4", arrival = 12, computing = 4 }
     , Prozess { pid = "P5", arrival = 16, computing = 3 }
     , Prozess { pid = "P6", arrival = 19, computing = 6 } ]

start :: State
start = State { new = ps, run = idle, ready = [], time = 0, chart = "" }

--Aufgabe 1 "print start"
instance Show State where   --anstelle von "deriving (Show)" definieren wir selber wie "State" ausgegeben wird
    show s = unlines        --s ist State der ausgegeben wird, unlines verbindet liste von strings mit zeilenumbrüchen
        ([ "-- new" ]          --Überschriften der Prozesse 
        ++ map show (new s)    -- ++ hängt es and die Liste, map show wandelt Prozess in String um, (new s) nimmt Liste der noch nicht angekommenen Prozesse aus dem "State" 
        ++ [ "-- run" ]         --eckige Klammern, da unlines eine Liste erwartet !
        ++ [ show (run s) ]
        ++ [ "-- ready" ]
        ++ map show (ready s)
        ++ [ "-- time: " ++ show (time s) ]
        ++ [ "-- chart: " ++ chart s ])
    

    --Aufgabe 2 
    --Hilfsfunktion
splitByTime :: Int -> [Prozess] -> ([Prozess], [Prozess])   --nimmt int und Liste und gibt Tupel mit 2 Listen zurück -> Prozesse die jetzt ankommen und Prozesse die noch nicht da sind
splitByTime t [] = ([], []) --basisfall für leere Liste
splitByTime t (x:xs)
    | arrival x == t = (x : angekommen, nichtDa)    --prüft ob x ankommt, falls ja x in ankommen sonst x in nichtda
    | otherwise      = (angekommen, x : nichtDa)
    where (angekommen, nichtDa) = splitByTime t xs  --ruft Splitbytime rekursiv für den rest von xs auf 

    --Hauptfunktion
update_ready :: State -> State      --funktion nimmt State und gibt aktualisierten State zurück, print (update_ready start)-> P1 wurde in ready verschoben
update_ready s = s { new   = nichtDa        --kopiert State s und ersetzt new und ready 
                   , ready = ready s ++ angekommen }
    where (angekommen, nichtDa) = splitByTime (time s) (new s)  --where definiert Hilfsvariable für diese Funktion und gibt einen Tupel zurück, splitBytime entpackt den Tupel in die beiden Variablen

--Aufgabe 3             
update_run :: State -> State    --gibt erneut aktualisierten State zurück
update_run s
    | pid (run s) == "IDLE" && not (null (ready s)) =   --prüft, dass gerade kein Prozess läuft und das liste nicht leer ist -> not
        s { run   = head (ready s)  --nimmt ersten Prozess aus ready -> wird laufender Prozess
          , ready = tail (ready s) }    --entfernt ersten Prozess aus ready 
    | otherwise = s                     -- falls && nicht greift bleibt State unverändert
    {-bsp ready = [P1, P2, P3]

    head [P1, P2, P3] = P1      -- P1 wird run
    tail [P1, P2, P3] = [P2, P3] -- P2, P3 bleiben in ready -}

                {-zum starten 
                         let s1 = update_ready start
                         print (update_run s1)

                -}

--Aufgabe 4
update_time :: State -> State   
update_time s = s { run   = neuerRun        --laufender Prozess wird durch neuerRun ersetzt 
                  , time  = time s + 1      --aktuelle Zeit um 1 erhöht
                  , chart = chart s ++ pid (run s) ++ " " }  --chart s scheduling String pid(run s) holt laufenden Prozess
    where
        neuzeit  = computing (run s) - 1    --rechenzeit des laufenden Prozesses wird um 1 reduziert 
        neuerRun
            | neuzeit == 0 = idle       --wenn rechenzeit 0 -> idle
            | otherwise    = (run s) { computing = neuzeit }    --sonst Prozess weiter mit neuer rechenzeit
            {-Ausführen: let s1 = update_ready start
            let s2 = update_run s1
            let s3 = update_time s2
            print s3-}

--Aufgabe 5

step :: State -> State
step s = update_time (update_run (update_ready s))   -- verbindet funktionen, von rechts nach links

-- Prüft ob alle Prozesse fertig sind
fertig :: State -> Bool
fertig s = null (new s)
        && null (ready s)
        && pid (run s) == "IDLE"

-- Wiederholt step bis alle fertig sind
fcfs :: State -> State
fcfs s
    | fertig s  = s
    | otherwise = fcfs (step s) --Ausführen print (fcfs start)
                                

--Aufgabe 6
update_ready_sjf :: State -> State
update_ready_sjf s = s { new   = nichtDa
                       , ready = sort (ready s ++ angekommen) } --für shortest job First wird die Liste sortiert angehängt
    where (angekommen, nichtDa) = splitByTime (time s) (new s)


step_sjf :: State -> State  --rest ist wie in aufgabe 5
step_sjf s = update_time  (update_run  (update_ready_sjf s))

sjf :: State -> State
sjf s
    | fertig s  = s
    | otherwise = sjf (step_sjf s)    




update_ready_srtf :: State -> State
update_ready_srtf s = s { new   = nichtDa
                        , run   = neuerRun
                        , ready = sort neueReady }
    where
        (angekommen, nichtDa) = splitByTime (time s) (new s)
        alleReady  = ready s ++ angekommen
        neuerRun
            | not (null alleReady) &&       -- 2bedingungen, es gibt wartende Prozesse und kürzester wartender Prozess hat weniger Rechenzeit als aktuelle -> falls beide zutreffen wir der neue Prozess ausgeführt
              head (sort alleReady) < run s = head (sort alleReady)
            | otherwise                     = run s
        neueReady
            | not (null alleReady) &&           --falls unterbrochen wird, kommt Prozess zurück in ready da der kürzeste Prozess bereits als neuerRun genommen wurde, nimmt Tail ihn aus ready 
              head (sort alleReady) < run s = run s : tail (sort alleReady)
            | otherwise                     = alleReady



step_srtf :: State -> State --print (srtf start)
step_srtf = update_time . update_run . update_ready_srtf

srtf :: State -> State
srtf s
    | fertig s  = s
    | otherwise = srtf (step_srtf s)