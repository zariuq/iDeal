import Data.List
import Data.Char
import qualified Data.Map as M
import Control.Monad

-- data types with names instead of lists and strings!
type Interaction = (Experiment,Result)
type InteractionPair = (Interaction, Float)
type Valence = M.Map Interaction Float
data Mood = SELF_SATISFIED | FRUSTRATED | PLEASED | PAINED | BORED
    deriving (Eq, Ord, Show, Read, Enum, Bounded) -- 'cuz why not

data Experiment = E1 | E2 deriving (Eq, Ord, Show, Read, Enum, Bounded) -- not sure I actually want Ord in there.
data Result = R1 | R2 deriving (Eq, Ord, Show, Read, Enum, Bounded) -- same

experiments = [E1,E2] -- useful afterall -_-


defaultStart :: Int ->  IO ()
defaultStart steps = mapM_ putStrLn (action [1..steps] [] E1 (E1,R2) 0 PLEASED initVal)
-- No null Interaction?

initVal :: Valence
initVal = foldl addPrimitiveInteraction M.empty [((E1,R1), -0.5),((E2,R2), 0.5)] -- initializing

-- This is probably a bit clunky.
addPrimitiveInteraction :: Valence -> InteractionPair -> Valence
addPrimitiveInteraction v (i,f) = M.insert i f v -- With overriding...

-- Not having static values, everything has to be passed along each time, right?
-- No need for "get previous experience" calls though!
-- Adding in memory and anticipation when the interactions and valence are static feels.. odd.
-- I'll wait for the next lesson for that :-D
action :: [Int] -> [String] -> Experiment -> Interaction -> Int -> Mood -> Valence -> [String]
action [] output _ _ _ _ _ = output -- When the count list becomes empty, return the output
action count output experiment previousInteraction boredCount mood val = do
    let experiment'
            | mood == PLEASED = experiment
            | mood == PAINED || mood == BORED = getOtherExperiment experiment
            | otherwise = experiment
        result = getResult experiment'
        enactedInteraction = (experiment', result) -- I can lookup the valence later
        boredCount'
            | enactedInteraction == previousInteraction = succ boredCount
            | otherwise = 0 -- if you mess up, just reset the bored count
        mood' = getMood enactedInteraction val boredCount'
        output' = output ++ ("Cycle: " ++ (show $ head count) ++ "  " ++ (show experiment') 
                             ++ " " ++ (show result) ++ " " ++ (show mood') ++ " " ++ show boredCount'):[]
    action (tail count) output' experiment' enactedInteraction boredCount' mood' val

getResult :: Experiment -> Result
getResult E1 = R1
getResult E2 = R2

getMood :: Interaction -> Valence -> Int -> Mood
getMood _ _ bored | bored > 3 = BORED
getMood inter val _ = case M.lookup inter val of
                        Just n -> if n >= 0 then PLEASED
                                            else PAINED
                        Nothing -> PAINED

-- I could write this as I did getResult...
getOtherExperiment :: Experiment -> Experiment
getOtherExperiment e = head (experiments \\ [e])



