
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE DeriveGeneric  #-}

module Submission2 where
import Lib

  hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (unfoldr)
import Data.List
import Data.Maybe
import Text.Printf
import Control.DeepSeq
import GHC.Generics

deriving instance (Integral Growth)
deriving instance (Enum Growth)
deriving instance (Real Growth)

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

logic :: Strategy -> GameState -> AIState -> ([Order], Log, AIState)
logic strat gs ai
  = let logic' = case strat of
          Pacifist       -> pacifist
          ZergRush       -> zergRush
          PlanetRankRush -> planetRankRush
          Skynet         -> skynet
    in logic' gs ai {turn = turn ai + 1}

data AIState = AIState
  { turn :: Turns,
    rushTarget :: Maybe PlanetId,
    rank :: Maybe PlanetRanks
  } deriving Generic

initialState :: AIState
initialState = AIState
  { turn = 0,
    rushTarget = Nothing,
    rank = Nothing
  }

type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

enemyPlanets :: GameState -> Planets
enemyPlanets (GameState ps _ _) = M.filter enemyPlanet ps

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet gs
  | null mapList   = Nothing
  | otherwise      = Just (fst (head mapList))
  where
    mapList = M.toList (enemyPlanets gs)

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st
  | not (ourPlanet planet) = []
  | (isNothing mShips) || (fromJust mShips > totalShips)  = [Order wId totalShips]
  | otherwise    = [Order wId (fromJust mShips)]
  where
    Wormhole (Source src) _ _ = lookupWormhole wId st
    planet@(Planet _ totalShips _) = lookupPlanet src st

shortestPath :: PlanetId -> PlanetId -> GameState
             -> Maybe (Path (WormholeId, Wormhole))
shortestPath src dst st
  = case filter ((== dst) . target) (shortestPaths st src) of
      [] -> Nothing
      (x : _) -> Just x

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _)
  = wormholes M.! wId

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState planets _ _)
  = planets M.! pId

attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs
  = concatMap (\x->case x of
                  Nothing -> []
                  Just (Path _ []) -> []
                  Just (Path _ xs) -> send (fst (last xs)) Nothing gs) paths
  where
    paths = map (\x->shortestPath x targetId gs) (M.keys (ourPlanets gs))

tryAttackFromAll :: Maybe PlanetId -> GameState -> [Order]
tryAttackFromAll Nothing _ = []
tryAttackFromAll (Just pid) gs = attackFromAll pid gs

zergRush :: GameState -> AIState
         -> ([Order], Log, AIState)
zergRush gs ai = (tryAttackFromAll (rushTarget ai') gs, rushLog, ai')
  where
    rushT = rushTarget ai
    ai' = if (isNothing rushT || ourPlanet (lookupPlanet (fromJust rushT) gs))
          then ai {rushTarget = findEnemyPlanet gs} else ai
    rushLog = ["Zerg Rush: "] ++ [show (rushTarget ai')]

newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)

type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId)
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n))
                             | p <- ps ]
  where ps = vertices g
        n  = length ps

example1 :: [(String, String, Integer)]
example1 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1), ("c","d",1)]

initPageRank' :: Map pageId a -> PageRanks pageId
initPageRank' m = M.map (const (1 / fromIntegral n)) m
  where
    n = M.size m


nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = (1 - d) / n + d * sum [ pr M.! j / t j
                                            | j <- s i ]
 where
  d   = 0.85
  n   = fromIntegral (length (vertices g))
  t j = fromIntegral (length (edgesFrom g j))
  s i = map source (edgesTo g i)

nextPageRanks :: Ord pageId => Graph g e pageId =>
  g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
 where
   pri' = nextPageRank g pr i

nextPageRanks' :: Ord pageId => Graph g e pageId =>
  g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr = case M.mapAccumWithKey nextPageRank'' True pr of
                           (True,  pr)  -> Nothing
                           (False, pr') -> Just pr'
  where
    nextPageRank'' converged i pri = case nextPageRank' g pr k i pri of
                            Nothing   -> (converged, pri)
                            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId)
  => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank' g = last (take 200 (pageRanks' g 0.0001))


example2 :: GameState
example2 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 7))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 2))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 3))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 6))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole (Source 0) (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole (Source 0) (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole (Source 0) (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole (Source 1) (Target 0) (Turns 1))
    , (WormholeId 4, Wormhole (Source 2) (Target 0) (Turns 1))
    , (WormholeId 5, Wormhole (Source 3) (Target 0) (Turns 1))
    , (WormholeId 6, Wormhole (Source 2) (Target 3) (Turns 1))
    ]
  fleets = []

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional)

type PlanetRanks = Map PlanetId PlanetRank

instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [ (p, PlanetRank (1 / fromIntegral n))
                               | p <- ps ]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 200

planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks
               -> PlanetId -> PlanetRank
nextPlanetRank g@(GameState planets _ _) pr i =
 (1 - d) / n + d * sum [ pr M.! j * growth i / growths j
                       | j <- targets i ]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = (\(Planet _ _ g) -> fromIntegral g)
                                  (planets M.! i)
  targets :: PlanetId -> [PlanetId]
  targets i = map (target) (edgesFrom g i)

  growths :: PlanetId -> PlanetRank
  growths j = sum (map (growth . source) (edgesTo g j))

checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems

planetRankRush :: GameState -> AIState
               -> ([Order], Log, AIState)
planetRankRush gs ai
  | isNothing (rank ai) = planetRankRush gs (ai {rank = Just (planetRank gs)})
  | otherwise   = (tryAttackFromAll (rushTarget ai') gs, rushLog, ai')
  where
    rushT = rushTarget ai
    ai' = if (isNothing rushT || ourPlanet (lookupPlanet (fromJust rushT) gs))
          then ai {rushTarget = findNextPlanet gs (fromJust (rank ai))} else ai
    rushLog = ["Planet Rank Rush: "] ++ [show (rushTarget ai')]

{- Try to find next planet to rush in the planet rank -}
findNextPlanet :: GameState -> PlanetRanks -> Maybe PlanetId
findNextPlanet gs prs = findNextPlanet' invalidPid minRank (M.toList prs)
  where
    invalidPid = -1
    minRank = 0 -- all ranks > 0
    findNextPlanet' :: PlanetId -> PlanetRank -> [(PlanetId, PlanetRank)]
     -> Maybe PlanetId
    findNextPlanet' pid maxRank []
      | pid == invalidPid        = Nothing
      | otherwise                = Just pid
    findNextPlanet' pid maxRank ((pid', rank):xs)
      | notOurPlanet && rank > maxRank   = findNextPlanet' pid' rank xs
      | otherwise                        = findNextPlanet' pid maxRank xs
      where
        notOurPlanet = not (ourPlanet (lookupPlanet pid' gs))


----------------- Whole Skynet strategy design below this line -----------------

{- This skynet strategy design contains four parts
 - 1. In the first 200 turns, use planetRankRush strategy to start and grow,
 - until we got enough number of ships to attack neutral planet easily.
 - 2. From turns 200 - 400, use fakeRush strategy to spread our power.
 - 3. From turns 400 - 600, use planetRankRush strategy to gain advantage.
 - 4. For the last 400 turns, use catchMaxFleet strategy to crush the enemy. -}

{- For most cases, this design could beat the planetRankRush strategy.
 - We use planetRankRush to start, so most times both sides would behave
 - symmetrically at the beginning. And for these cases, our design would always
 - win. But we found for some cases, two PlanetRankRush strategies would behave
 - differently for some reasons. And for these cases, our design would win
 - for most times and fail sometimes. -}
skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet gs ai
  | isNothing (rank ai)         = skynet gs (ai {rank = Just (planetRank gs)})
  | turn ai >= growBeginTime && turn ai <= growEndTime
                                = fakeRush gs ai
  | turn ai >= terminateTime    = catchMaxFleet gs ai
  | otherwise                   = planetRankRush gs ai
  where
    growBeginTime = 200
    growEndTime = 400
    terminateTime = 600


---------------- Implementation of fakeRush sub-strategy below -----------------

{- Import from CW1, modified (we need one more ship to hold the planet) -}
bknapsack :: (Ord weight, Num weight, Ord value, Num value)
  => [(name, weight, value)] -> weight -> (value, [name])
bknapsack [] c = (0, [])
bknapsack ((n, w, v):xs) c
  | c - w > 0   = maxBy fst (bknapsack xs c) (v'+v, n:ns)
  | otherwise    = bknapsack xs c
  where
    (v', ns) = bknapsack xs (c - (w + 1))

{- Pick up all the neutral plants nearby the source planet,
 - Return a list of planetId, ships number and growth number -}
targetNeutralPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetNeutralPlanets st s
  = map (planetDetails . target)
   (filter (neutralPlanetId st . target) (M.elems (wormholesFrom s st)))
  where
    planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
    planetDetails pId = (pId, ships, growth)
      where Planet _ ships growth = lookupPlanet pId st

{- Use a bounded knapsack algorithm to pick up some neutral planets
 - nearby the source planet and return a pair of 1.the total ships of the source
 - 2.a list of pairs which contains the WormholeId to the neutralPlanet and
 - the number of enough ships for successfully attacking the neutralPlanet
 - Notice the "+ 1", it is because we need one more ship to own the planet -}
prepareOrders :: GameState -> Source -> (Ships, [(WormholeId, Ships)])
prepareOrders st s@(Source p)
  = (shipsOnPlanet st p, map (\x->(fst x, shipsOnPlanet st (target x) + 1))
   (filterWormholeId (M.toList (wormholesFrom s st)) ps))
  where
    (_, ps) = bknapsack (targetNeutralPlanets st s) (shipsOnPlanet st p)
    {- There are wormholes with totally the same target and source planets
     - This filter function avoids the "not enough ships" problem which is
     - caused by sending ships to all the same wormholes -}
    filterWormholeId :: [(WormholeId, Wormhole)] -> [PlanetId]
     -> [(WormholeId, Wormhole)]
    filterWormholeId _ [] = []
    filterWormholeId (w:ws) pid
      | (target w) `elem` pid  = w:filterWormholeId ws (delete (target w) pid)
      | otherwise              = filterWormholeId ws pid

{- Produce orders with the given info pair list (the third parameter)
 - Return the orders together with the number of remaining ships (unused ships)
 - The snd parameter given is the total number of ships -}
makeOrders :: GameState -> Ships -> [(WormholeId, Ships)] -> ([Order], Ships)
makeOrders gs ts [] = ([], ts)
makeOrders gs ts ((wid, s):xs) = ((send wid (Just s) gs) ++ ords, ts')
  where
    (ords, ts') = makeOrders gs (ts - s) xs

{- Modified version of attackFromAll function, this time all the plants
 - would not send all ships to the target planet. Instead, they would try to
 - pick up some neutralPlanets using the bounded knapsack algorithm and
 - send the remaining ships to the target planet -}
attackFromAll' :: PlanetId -> GameState -> [Order]
attackFromAll' targetId gs
  = concatMap
    (\x->let (ords, remains) = fst x in case (snd x) of
         Nothing -> ords
         Just (Path _ []) -> ords
         Just (Path _ xs) -> ords ++ send (fst (last xs)) (Just remains) gs)
    paths -- snd arg of the concatMap
  where
    paths = map (\x->
                let (ships, pairs) = prepareOrders gs (Source x) in
                (makeOrders gs ships pairs , shortestPath x targetId gs))
                (M.keys (ourPlanets gs)) -- snd arg of the map

tryAttackFromAll' :: Maybe PlanetId -> GameState -> [Order]
tryAttackFromAll' Nothing _ = []
tryAttackFromAll' (Just pid) gs = attackFromAll' pid gs

{- Modified version of planetRankRush, every planet would try to pick up
 - neutral planets nearby and only planetRankRush using the remaining ships -}
fakeRush :: GameState -> AIState
         -> ([Order], Log, AIState)
fakeRush gs ai = (tryAttackFromAll' (rushTarget ai') gs, rushLog, ai')
  where
    rushT = rushTarget ai
    ai' = if (isNothing rushT || ourPlanet (lookupPlanet (fromJust rushT) gs))
          then ai {rushTarget = findNextPlanet gs (fromJust (rank ai))} else ai
    rushLog = ["Fake Rush: "] ++ [show (rushTarget ai')]


-------------- Implementation of catchMaxFleet sub-strategy below --------------

{- Try to find the target planetId of the enemy fleet with max ships number
 - Return Nothing if there is no such fleet -}
findMaxFleetTarget :: GameState -> Maybe PlanetId
findMaxFleetTarget (GameState _ _ []) = Nothing
findMaxFleetTarget (GameState p w ((Fleet Player1 _ _ _):fs))
  = findMaxFleetTarget (GameState p w fs)
findMaxFleetTarget gs@(GameState _ _ (f:fs))
  = Just (target (lookupWormhole wid gs))
  where
    (Fleet _ _ wid _) = (maxShipsFleet fs f)
    maxShipsFleet :: Fleets -> Fleet -> Fleet
    maxShipsFleet [] f = f
    maxShipsFleet ((Fleet Player1 _ _ _):fs) f = maxShipsFleet fs f
    maxShipsFleet (f@(Fleet _ s _ _):fs) f'@(Fleet _ s' _ _)
      | s > s'     = maxShipsFleet fs f
      | otherwise  = maxShipsFleet fs f'

{- All planets would try to block the enemy fleet with max ships number
 - by rushing to the destination planet of the fleet -}
catchMaxFleet :: GameState -> AIState
               -> ([Order], Log, AIState)
catchMaxFleet gs ai = (tryAttackFromAll maybePid gs, catchLog, ai)
  where
    maybePid = findMaxFleetTarget gs
    catchLog = ["Catch: "] ++ [show maybePid]


------------------------ Some Util functions below -----------------------------

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
  where Planet _ ships _ = lookupPlanet pId st

neutralPlanet :: Planet -> Bool
neutralPlanet (Planet (Neutral) _ _) = True
neutralPlanet _ = False

neutralPlanetId :: GameState -> PlanetId -> Bool
neutralPlanetId gs pid = neutralPlanet (lookupPlanet pid gs)



deriving instance Generic PlanetRank
deriving instance Generic PageRank

instance NFData PageRank
instance NFData PlanetRank
