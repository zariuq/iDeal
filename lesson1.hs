import Control.Monad

experiments = ["e1","e2"]
results = ["r1","r2"]
moods = ["BORED","SELF-SATISFIED","FRUSTRATED"]
memory :: [(String,String)]
memory = zip experiments results
satCount = 0

-- And these were completely useless, err except for experiments.
-- I think if it gets more complicated, they could become more useful.

defaultStart :: IO ()
defaultStart = mapM_ putStrLn (action "e1" 13 0 [] "" [])

paramStart :: String -> Int -> IO ()
paramStart exp num = mapM_ putStrLn (action exp num 0 [] "" [])

action :: String -> Int -> Int -> [(String,String)] -> String -> [String] -> [String]
action _ 0 _ _ _ out = out 
action e n s mem mood out = do 
    let r = getResult e
    let a = anticipateResult $ startsWith e mem
    let me = recordTuple e r mem
    let sa | mood == "BORED" = 0
           | r == a = s + 1 
           | otherwise = 0
    --let mo | sa > 3 = "BORED"
    --       | r == a = "SELF-SATISFIED"
    --       | otherwise = "FRUSTRATED"
    let mo = setMood sa r a
    let ex | mo == "SELF-SATISFIED" || mo == "FRUSTRATED" = e 
           | mo == "BORED" = head $ filter (/= e) experiments
    let o = out++("Cycle " ++ show n ++ " " ++ ex ++ " " ++ r ++ " " ++ mo ++ " sat " ++ show sa):[]
    action ex (n-1) sa me mo o

-- out as a list of tuples may be easier.  and you can pattern match the tuple (e,n,s,mem,mood) out as the only
-- input

-- Cool!
setMood :: Int -> String -> String -> String
setMood sa _ _ | sa > 3 = "BORED"
setMood _ r a = if r == a then "SELF-SATISFIED" else "FRUSTRATED"

-- Interesting. The basic 'functions' were very easy to write in Haskell.
-- But I had a hard time getting the 'declarative loop' to work.
-- The key seems to be that 'conditions' have to be broken up by the variables they effect
-- rather than the 'conditions.' Not necessarily more complex, but focused on the values rather than conditions :o

getResult :: String -> String
getResult ex
    | ex == experiments !! 0 = results !! 0
    | ex == experiments !! 1 = results !! 1
    | otherwise = ""
    
recordTuple :: String -> String -> [(String,String)] -> [(String,String)]
recordTuple a b mem 
    | not $ elem (a,b) mem = (a,b):mem
    | otherwise = mem

anticipateResult :: [(String,String)] -> String
anticipateResult [] = "" -- I think this can work
anticipateResult (x:_) = snd x


startWith :: (Eq a) => a -> (a,b) -> Bool
startWith x t = x == (fst t)

startsWith :: String -> [(String,String)] -> [(String,String)]
startsWith x mem = filter (startWith x) mem
