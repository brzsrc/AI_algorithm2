
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
    rank :: Maybe PlanetRanks,
    scatterZone :: Maybe [PlanetId]
  } deriving Generic

initialState :: AIState
initialState = AIState
  { turn = 0,
    rushTarget = Nothing,
    rank = Nothing,
    scatterZone = Nothing
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
    paths = map (\x->shortestPath (fst x) targetId gs) (M.toList (ourPlanets gs))

tryAttackFromAll :: Maybe PlanetId -> GameState -> [Order]
tryAttackFromAll Nothing _ = []
tryAttackFromAll (Just pid) gs = attackFromAll pid gs

zergRush :: GameState -> AIState
         -> ([Order], Log, AIState)
zergRush gs ai
  | isNothing rushT || ourPlanet (lookupPlanet (fromJust rushT) gs)
                = (tryAttackFromAll rushT' gs, rushLog', ai')
  | otherwise   = (tryAttackFromAll rushT gs, rushLog, ai)
  where
    ai' = ai {rushTarget = findEnemyPlanet gs}
    rushT = rushTarget ai
    rushT' = rushTarget ai'
    rushLog = ["Zerg Rush"] ++ if (isNothing rushT) then []
      else [show (fromJust rushT) ++ show (lookupPlanet (fromJust rushT) gs)]
    rushLog' = ["Zerg Rush"] ++ if (isNothing rushT') then []
      else [show (fromJust rushT') ++ show (lookupPlanet (fromJust rushT') gs)]

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
  | isNothing rushT || ourPlanet (lookupPlanet (fromJust rushT) gs)
                = (tryAttackFromAll rushT' gs, rushLog', ai')
  | otherwise   = (tryAttackFromAll rushT gs, rushLog, ai)
  where
    ai' = ai {rushTarget = findNextPlanet True gs (fromJust (rank ai))}
    rushT = rushTarget ai
    rushT' = rushTarget ai'
    rushLog = ["Planet Rank Rush"] ++ if (isNothing rushT) then []
      else [show (fromJust rushT) ++ show (lookupPlanet (fromJust rushT) gs)]
    rushLog' = ["Planet Rank Rush"] ++ if (isNothing rushT') then []
      else [show (fromJust rushT') ++ show (lookupPlanet (fromJust rushT') gs)]

findNextPlanet :: Bool -> GameState -> PlanetRanks -> Maybe PlanetId
findNextPlanet attackOrDefense gs prs = findNextPlanet' (-1) (-1) (M.toList prs)
  where
    findNextPlanet' :: PlanetId -> PlanetRank -> [(PlanetId, PlanetRank)] -> Maybe PlanetId
    findNextPlanet' pid maxRank []
      | maxRank == -1    = Nothing
      | otherwise        = Just pid
    findNextPlanet' pid maxRank ((pid', rank):xs)
      | cond && rank > maxRank   = findNextPlanet' pid' rank xs
      | otherwise                        = findNextPlanet' pid maxRank xs
      where
        cond = if attackOrDefense then notOurPlanet else notEnemyPlanet
        notOurPlanet = not (ourPlanet (lookupPlanet pid' gs))
        notEnemyPlanet = not (enemyPlanet (lookupPlanet pid' gs))


skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet gs ai
  | isNothing (scatterZone ai)        = skynet gs (ai {scatterZone = Just zone})
  | foldl (||) False (map (neutralPlanetId gs) (fromJust (scatterZone ai)))
                                      = neutralScatter gs ai
  | otherwise                         = planetRankRush gs ai
  where
    (zone, _, _) = conflictZones gs p1 p2
    p1 = fst (head (M.toList (ourPlanets gs)))
    p2 = fst (head (M.toList (enemyPlanets gs)))

neutralPlanet :: Planet -> Bool
neutralPlanet (Planet (Neutral) _ _) = True
neutralPlanet _ = False

neutralPlanets :: GameState -> Planets
neutralPlanets (GameState ps _ _) = M.filter neutralPlanet ps

neutralPlanetId :: GameState -> PlanetId -> Bool
neutralPlanetId gs pid = neutralPlanet (lookupPlanet pid gs)

neutralScatter :: GameState -> AIState
               -> ([Order], Log, AIState)
neutralScatter gs ai
  | isNothing (rank ai)  = neutralScatter gs (ai {rank = Just (planetRank gs)})
  | isNothing rushT || enemyPlanet (lookupPlanet (fromJust rushT) gs)
                         = (orders', ["Neutral Scatter"], ai')
  | otherwise            = (orders, ["Neutral Scatter"], ai)
  where
    rushT = rushTarget ai
    ai' = ai {rushTarget = findNextPlanet False gs (fromJust (rank ai))}
    orders = scatter mapList ++ defense mapList ai
    orders' = scatter mapList ++ defense mapList ai'
    mapList = M.toList (ourPlanets gs)

    scatter :: [(PlanetId, Planet)] -> [Order]
    scatter ps = concatMap (\p -> makeOrders (prepareOrders gs (Source (fst p)))) ps

    makeOrders :: [(WormholeId, Ships)] -> [Order]
    makeOrders [] = []
    makeOrders ((wid, s):xs) = (send wid (Just s) gs) ++ makeOrders xs

    defense :: [(PlanetId, Planet)] -> AIState -> [Order]
    defense ps a
      | isNothing rushT ||
        not ((fromJust rushT) `elem` (fromJust (scatterZone a))) = []
      | otherwise = concatMap (\x->case x of
                    Nothing -> []
                    Just (Path _ []) -> []
                    Just (Path _ xs) -> send (fst (last xs)) Nothing gs) paths
      where
        rushT = rushTarget a
        paths = map (\x->shortestPath (fst x) (fromJust rushT) gs) ps

-- Import from CW1, modified (we need one more ship to hold the planet)
bknapsack :: (Ord weight, Num weight, Ord value, Num value)
  => [(name, weight, value)] -> weight -> (value, [name])
bknapsack [] c = (0, [])
bknapsack ((n, w, v):xs) c
  | c - w > 0   = maxBy fst (bknapsack xs c) (v'+v, n:ns)
  | otherwise    = bknapsack xs c
  where
    (v', ns) = bknapsack xs (c - (w + 1))

-- add a filter to take only neutral plants
targetNeutralPlanets :: GameState -> Source -> [(PlanetId, Ships, Growth)]
targetNeutralPlanets st s
  = map (planetDetails . target)
   (filter (neutralPlanetId st . target) (M.elems (wormholesFrom s st)))
  where
    planetDetails :: PlanetId -> (PlanetId, Ships, Growth)
    planetDetails pId = (pId, ships, growth)
      where Planet _ ships growth = lookupPlanet pId st

shipsOnPlanet :: GameState -> PlanetId -> Ships
shipsOnPlanet st pId = ships
    where Planet _ ships _ = lookupPlanet pId st

-- maybe merge with makeOrders func later
prepareOrders :: GameState -> Source -> [(WormholeId, Ships)]
prepareOrders st s@(Source p)
  = map (\x->(fst x, shipsOnPlanet st (target x) + 1))  --need one more ship
   (filterWormholeId (M.toList (wormholesFrom s st)) ps)
  where
    (_, ps) = bknapsack (targetNeutralPlanets st s) (shipsOnPlanet st p)
    filterWormholeId :: [(WormholeId, Wormhole)] -> [PlanetId] -> [(WormholeId, Wormhole)]
    filterWormholeId _ [] = []
    filterWormholeId (w:ws) pid
      | (target w) `elem` pid  = w:filterWormholeId ws (delete (target w) pid)
      | otherwise              = filterWormholeId ws pid



-- import from CW1 to get the conflictZones
conflictZones :: GameState -> PlanetId -> PlanetId
  -> ([PlanetId], [PlanetId], [PlanetId])
conflictZones st p q = checkPaths (vertices(st)) ps qs
  where
    ps  = shortestPaths st p
    qs  = shortestPaths st q

checkPaths :: [PlanetId] -> [Path (WormholeId, Wormhole)]
  -> [Path (WormholeId, Wormhole)] -> ([PlanetId], [PlanetId], [PlanetId])
checkPaths [] _ _ = ([], [], [])
checkPaths (x:xs) p q
  | pl == Nothing && ql == Nothing   = (ps, qs, pqs)
  | pl == Nothing                    = (ps, x:qs, pqs)
  | ql == Nothing                    = (x:ps, qs, pqs)
  | fromJust pl < fromJust ql        = (x:ps, qs, pqs)
  | fromJust pl > fromJust ql        = (ps, x:qs, pqs)
  | otherwise                        = (ps, qs, x:pqs)
  where
    pl = (getPathLen p x)
    ql = (getPathLen q x)
    (ps, qs, pqs) = checkPaths xs p q

getPathLen :: [Path (WormholeId, Wormhole)] -> PlanetId -> Maybe Integer
getPathLen [] _ = Nothing
getPathLen ((Path n ((wid, Wormhole s t _):xs)):ys) t'
  | t == (Target t')    = Just n
  | otherwise  = getPathLen ys t'



deriving instance Generic PlanetRank
deriving instance Generic PageRank

instance NFData PageRank
instance NFData PlanetRank
