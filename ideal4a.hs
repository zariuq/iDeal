import Data.List
import Data.Char
import qualified Data.Map as Map
import Control.Monad
import System.Random
import Control.Monad.State

data Experiment = E1 | E2 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Result = R1 | R2 deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Mood = SELF_SATISFIED | FRUSTRATED | PLEASED | PAINED | BORED deriving (Eq, Ord, Show, Read)

--type Interaction = (Experiment,Result)
data Inter = None | Primitive Experiment Result | Composite Inter Inter deriving (Show, Read, Eq, Ord)

type ValenceMap = Map.Map Inter Float
type MemoryMap = Map.Map Inter Float

test = (Composite (Composite (Primitive E1 R2) (Primitive E2 R2)) (Primitive E1 R1))

-- Seems a waste to do this when everything is in Inter form.
{-primitiveInteractions :: ValenceMap
primitiveInteractions = Map.fromList -- I decided to just hardcode them this time -_-
    [((E1, R1),-1)
    ,((E1, R2), 1)
    ,((E2, R1),-1)
    ,((E2, R2), 1)] -}

-- Hardcoded primitives
primitiveInteractions :: ValenceMap
primitiveInteractions = Map.fromList 
    [((Primitive E1 R1),-1)
    ,((Primitive E1 R2), 2.5)
    ,((Primitive E2  R1),-1)
    ,(( Primitive E2  R2), 2.5)]
    
primitiveSuggestions :: Map.Map Inter Float
primitiveSuggestions = Map.fromList
    [((Primitive E1 R1),0)
    ,((Primitive E2 R1),0)]

-- Conversion... maybe not used.
{-
toInteraction :: Inter -> [Interaction]
toInteraction (Primitive e r) = (e,r):[]
toInteraction (Composite pre post) = (toInteraction pre) ++ (toInteraction post)
toInteraction None = []
-}

-- Recursively looks up composite interaction valence.
getValence :: Inter -> Float
getValence (Composite pre post) = (getValence pre) + (getValence post)
getValence (Primitive e r) = case Map.lookup (Primitive e r) primitiveInteractions of 
    --(head (toInteraction (Primitive e r))) primitiveInteractions of
    Just valence -> valence
    Nothing -> 0
getValence None = 0

-- Because if statements are a bit uglier.
getMood :: Float -> Mood
getMood valence 
    | valence >= 0 = PLEASED
    | otherwise = PAINED

-- Prints primitives and composites, using < > to show grouping.
printInter :: Inter -> String
printInter (Primitive e r) = fmap toLower (show e ++ show r)
printInter (Composite pre post) = "<" ++ (printInter pre) ++ (printInter post) ++ ">"
printInter None = ""

-- To ignore the superfluous result :)
printProposal :: (Float, Inter) -> IO ()
printProposal (proclivity, Primitive e _) = 
    putStrLn ("propose " ++ (fmap toLower $ show e) ++ " proclivity " ++ (show proclivity))
printProposal (proclivity, Composite pre post) =
    putStrLn ("propose " ++ (printInter (Composite pre post)) ++ " proclivity " ++ (show proclivity))
printProposal _ = return ()

-- Cool way to do if statements. Embedding them elsewhere seems harder!
getContext :: [Inter] -> [Inter]
getContext [] = []
getContext [None] = [None]
getContext [(Primitive e r)] = [(Primitive e r)]
getContext [(Composite a b)] = [(Composite a b), b]
getContext (pre:pen:xs) = getContext [pre] ++ [(Composite pen pre)]

-- Adds a composite interaction to memory
learn :: MemoryMap -> Inter -> MemoryMap
learn memory (Composite pre post) = Map.insertWith (+) (Composite pre post) 1 memory
learn memory _ = memory 

-- Adds <pre, enacted>, <pen,<pre,enacted>> and <<pen, pre>, enacted> to memory
learner :: MemoryMap -> [Inter] -> Inter -> MemoryMap
learner memory (pre:pen:xs) enacted = do
    let memory' = learn memory (Composite pen (Composite pre enacted))
        memory'' = learn memory' (Composite (Composite pen pre) enacted)
    learn memory'' (Composite pre enacted)
learner memory [pre] enacted = learn memory (Composite pre enacted)
learner memory _ _ = memory

-- Memory and context to a Map of suggestions (Inters).
-- <e2r2> and <e2r1> not being merged MAY be a serious problem.
anticipate :: MemoryMap -> [Inter] -> Map.Map Inter Float
anticipate memory context =
    -- get a Map with <pre post> : weight where pre is in context
    let afforded = Map.filterWithKey (\(Composite pre post) _ -> pre `elem` context) memory
    -- multiply weight by getValence post (the one we're examining).
        proclived = Map.mapWithKey (\(Composite pre post) weight -> {-(weight)/(weight+1) + -}weight * (getValence post)) afforded
    -- get rid of pre and combine proclivities of identical posts
        pruned = Map.mapKeysWith (+) (\(Composite pre post) -> post) proclived
    -- merge identical primitives with 'different results'
        merged = Map.mapKeysWith (+) (\inter -> mergePrimitives inter) pruned
    -- make an experiment-only map
        expMap = Map.mapKeysWith (+) (\inter -> getExp inter) merged
    -- update original values by looking up in expMap
        updated = Map.mapWithKey (\inter _ -> expValUp inter expMap) merged
    -- add proclivity 0 suggestions for e1 and e2
    in Map.unionWith (+) updated primitiveSuggestions

-- For comparing all interactions with the same experiment sequence... surely not efficient!
expValUp :: Inter -> Map.Map [Experiment] Float -> Float
expValUp inter expMap = case Map.lookup (getExp inter) expMap of
    Just valence -> valence 
    Nothing -> 0

getExp :: Inter -> [Experiment]
getExp None = [E1]
getExp (Primitive e _) = [e]
getExp (Composite pre post) = (getExp pre) ++ (getExp post)

-- To make <e2r2> and <e2r1> be added together. The 'result' won't be used later.
mergePrimitives :: Inter -> Inter
mergePrimitives (Primitive e r) = (Primitive e R1)
mergePrimitives other = other
    
-- Takes anticipations and returns 'experiment' with highest proclivity. List for printing.
selectExperiment :: Map.Map Inter Float -> [(Float, Inter)]
selectExperiment anticipations = reverse $ sort $ zip (Map.elems anticipations) (Map.keys anticipations)

-- If anticipation fails, create new composite <what we tried to do, what we got>
-- Currently not used.
failCheck :: Inter -> Inter -> Inter
failCheck (Composite pre post) intended = Composite intended (Composite pre post)
failCheck other _ = other

-- Enacts composite interactions recursively. Hard to pass environment history through.
enact :: Inter -> Inter
enact (Composite pre post) =
    let enacted = enact pre
    -- add to eHist here, pass it on. Reconstruct it after you finish :x.
    in if enacted /= pre
        then enacted
        else (Composite pre (enact post))
enact (Primitive e _) = (Primitive e (getResult10 e))
enact None = None

-- Takes an interaction and is/returns a state (environment history) and interaction-value
enactE :: Inter -> State [Experiment] Inter
enactE (Composite pre post) = do
    enacted <- enactE pre
    if enacted /= pre
        then return enacted
        else do
            post' <- enactE post
            return $ Composite pre post'
enactE (Primitive e _) = do
    envHist <- get
    put (e:envHist)
    return $ Primitive e (getResult40 envHist e)
    --return $ Primitive e (getResult10 e)
    --return $ Primitive e (getResult30 (head envHist) e)
    --return $ Primitive e (getResult41 envHist e)
enactE None = return None

main = do
    step [0..30] [Primitive E1 R1, Primitive E1 R1] [E1,E1,E1] Map.empty -- default values

-- I was thinking it'd be easier to just keep a list of everything we did -_-;
-- hist = [previous, pre-1,pre-2,...]
step :: [Int] -> [Inter] -> [Experiment] -> MemoryMap -> IO ()
step [] _ envHist _ = print envHist
step count hist envHist memory = do
    let context = getContext hist
    --putStrLn $ "Context: " ++ (unwords $ fmap printInter context)
    
    let anticipations = anticipate memory context
    --print anticipations
    let    experiments = selectExperiment anticipations
    mapM_ printProposal experiments
    
    let intendedInteraction' = snd $ head experiments -- highest proclivity
        intendedInteraction = if (fst $ head experiments) < 0
                              then (Primitive (getRandomExperiment (head count)) R1)
                              else intendedInteraction'
        (enacted, envHist') = runState (enactE intendedInteraction) envHist 
        valence = getValence enacted
    --putStrLn $ "Enacted: " ++ (printInter enacted) ++ " valence " ++ (show valence)
    
    let enacted' = enacted -- failCheck enacted intendedInteraction
    
    let memory' = learner memory hist enacted'
        hist' = enacted' : hist
        mood = getMood valence
    
    putStrLn ("Step " ++ (show $ head count) ++ " " ++ (printInter enacted) ++ " " ++ (show mood))
    step (tail count) hist' envHist' memory' 

-- Because I don't like just choosing E1 or E2
getRandomExperiment :: Int -> Experiment
getRandomExperiment n = let (r,g) = randomR (1,2) (mkStdGen (n^2)) :: (Int, StdGen)
    in case r of
        1 -> E1
        2 -> E2

-- To test system before adding in the threaded experiments =.=;
-- Environment 10
getResult10 :: Experiment -> Result
getResult10 E1 = R1
getResult10 E2 = R2

-- Environment 30
getResult30 :: Experiment -> Experiment -> Result
getResult30 previous current
    | previous == current = R1
    | previous /= current = R2
    | otherwise = R1

-- Environment040
getResult40 :: [Experiment] -> Experiment -> Result
getResult40 (pre:pen:_) cur
    | pen /= cur && pre == cur = R2
    | otherwise = R1

--Environment041
getResult41 :: [Experiment] -> Experiment -> Result
getResult41 (pre:pen:ant:_) cur
    | ant /= cur && cur == pre && cur == pen = R2
    | otherwise = R1
