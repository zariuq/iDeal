import Data.List
import Data.Char
import qualified Data.Map as Map
import Control.Monad

data Mood = SELF_SATISFIED | FRUSTRATED | PLEASED | PAINED | BORED
    deriving (Eq, Ord, Show, Read, Enum, Bounded) -- 'cuz why not
data Experiment = E1 | E2 | E3 | E4 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Result = R1 | R2 | R3 deriving (Eq, Ord, Show, Read, Enum, Bounded)
type Interaction = (Experiment, Result)
type CompositeInteraction = (Interaction,Interaction)
type ValencedInteraction = Map.Map Interaction Float
type KnownInteraction = Map.Map CompositeInteraction Float -- interactions + proclivities
type Anticipation = Map.Map Experiment Float -- same, but proclivity instead of valence
-- whoops, anticipation was first Interaction based, not Experiment based.
primitiveInteractions :: ValencedInteraction
primitiveInteractions = Map.fromList -- I decided to just hardcode them this time -_-
    [((E1, R1),-1)
    ,((E1, R2), 1)
    ,((E2, R1),-1)
    ,((E2, R2), 1)
    ,((E3, R1),-1)
    ,((E3, R2), 1)
    ,((E4, R1),-1)
    ,((E4, R2), 1)]
    

experiments = [minBound .. maxBound] :: [Experiment]


-- Odd, when I start it at (E2,R1), it's pained on step 9 (whereas the official Java is pleased) with Env31
-- (E1,R1) gives it the same output.  (E2,R1) gives it the same output on Env30. 
-- Initial conditions change how the agent learns at first =]
-- The simple version (Table 33-2/Existence030) isn't implemented.
main = do
    action [0..20] (E3,R1) PAINED Map.empty -- default values


action :: [Int] -> Interaction -> Mood -> KnownInteraction -> IO ()
action [] _ _ _ = return ()
action count previousInteraction mood knownInteractions = do
    let (experiment, sortedAnticipations) = selectExperiment $ anticipate31 previousInteraction knownInteractions
        --result = getResult10 experiment -- This experiment switching should go in a function too!
        result = getResult30 (fst previousInteraction) experiment
        --result = getResult31 experiment (head count)
        --result = getResultE4 experiment
        --result = getResultE3 experiment
        
        enactedInteraction = (experiment, result)
        valence = getValence enactedInteraction
    mapM_ printProposal (reverse sortedAnticipations)
    putStrLn ("Enacted " ++ (show experiment) ++ (show result) ++ " valence " ++ (show valence)) 
    let mood' = getMood $ getValence enactedInteraction
        knownInteractions' = learnCompositeInteraction31 (previousInteraction, enactedInteraction) knownInteractions
    putStrLn ("learn " ++ (show previousInteraction) ++ (show enactedInteraction))
    putStrLn ("Step "  ++ (show $ head count) ++  " " ++ (show mood'))
    action (tail count) enactedInteraction mood' knownInteractions'



-- There should be a way to merge this sorting and reshuffling...
selectExperiment :: Anticipation -> (Experiment,[(Float, Experiment)])
selectExperiment anticipations
    | Map.null anticipations = (E1, []) -- why not?
    | otherwise = 
        let proposedExperiments = Map.keys anticipations
            sortedAnticipations = sort $ zip (Map.elems anticipations) (proposedExperiments)
            bestBet = last sortedAnticipations
            bestExperiment = snd bestBet -- (valence,e)
        in if fst bestBet >= 0 -- the proclivity
            then (bestExperiment, sortedAnticipations)
            else let otherExperiments = experiments \\ proposedExperiments -- messy and should go in another function
                 in if otherExperiments /= []
                        then (head otherExperiments, sortedAnticipations)
                        else (maxBound :: Experiment, sortedAnticipations)

-- dictionary filter, map and map... the last one being O(n * logn), maybe this is too slow? :(
anticipate31 :: Interaction -> KnownInteraction -> Anticipation
anticipate31 previousInteraction knownInteractions =
    let affordedInteractions = Map.filterWithKey (\k _ -> fst k == previousInteraction) knownInteractions
        calcProclivity postInt weight = weight * (getValence $ snd postInt)
        anticipations = Map.mapWithKey calcProclivity affordedInteractions
    in Map.mapKeysWith (+) (\k -> fst $ snd k) anticipations


-- compositeInteraction = (previousInteraction,enactedInteraction)
learnCompositeInteraction31 :: CompositeInteraction -> KnownInteraction -> KnownInteraction
learnCompositeInteraction31 compositeInteraction knownInteractions =
    if Map.member compositeInteraction knownInteractions
        then Map.adjust (1+) compositeInteraction knownInteractions
        else Map.insert compositeInteraction 1 knownInteractions

printProposal :: (Float, Experiment) -> IO ()
printProposal (proclivity, experiment) = 
    putStrLn ("propose " ++ (show experiment) ++ " proclivity " ++ (show proclivity))

getValence :: Interaction-> Float -- Because dealing with the Maybe is a pain
getValence interaction = case Map.lookup interaction primitiveInteractions of
    Just valence -> valence
    Nothing -> -0.1

getMood :: Float -> Mood
getMood valence 
    | valence >= 0 = PLEASED
    | otherwise = PAINED
    
getNextExperiment :: Experiment -> Experiment
getNextExperiment exp
    | exp == (maxBound :: Experiment) = E1
    | otherwise = succ exp

-- Environment 10
getResult10 :: Experiment -> Result
getResult10 E1 = R1
getResult10 E2 = R2
getResult10 _  = R1

-- Environment 30
getResult30 :: Experiment -> Experiment -> Result
getResult30 previous current
    | previous == current = R1
    | previous /= current = R2
    | otherwise = R3
    
-- Environment 31 (Altered a bit to be the same as my step-counter.)
getResult31 :: Experiment -> Int -> Result
getResult31 experiment cycle
    | cycle < 8 || cycle >= 15 = if experiment == E1 then R1 else R2
    | otherwise = if experiment == E1 then R2 else R1

-- There are many possible compositeInteractions buliding up to E4, so the learning curve is high.
-- Learns to always do E4 by Step 15.
getResultE4 :: Experiment -> Result
getResultE4 E4 = R2
getResultE4 _  = R1

getResultE3 :: Experiment -> Result
getResultE3 E3 = R2
getResultE3 _  = R1

{-
-- For testing.

i11 = (E1,R1)
i12 = (E1,R2)
i21 = (E2,R1)
i22 = (E2,R2) 

knownInteractions :: KnownInteraction
knownInteractions = Map.fromList
    [((i11,i11),1)
    ,((i11,i21),2)
    ,((i22,i22),-1)
    ,((i12,i21),-5)]
    
-- Eek, now I have to mod it to output all the various steps.
-}
