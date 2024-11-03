
import Data.Array qualified
import Data.Bits qualified
import Data.List (intercalate, minimumBy)
import Data.List qualified

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

--  returns all the cities in the graph.

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

cities :: RoadMap -> [City]
cities x = removeDuplicates (concat [[c1, c2] | (c1, c2, _) <- x])

printCities :: RoadMap -> IO ()
printCities x = putStrLn ("The cities in the graph are: " ++ intercalate ", " (cities x))

-- returns a boolean indicating whether two cities are linked directly.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent x city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) x

printAreAdjacent :: RoadMap -> City -> City -> IO ()
printAreAdjacent x city1 city2 =
  if areAdjacent x city1 city2
    then putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are adjacent.")
    else putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are not adjacent.")

-- returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.

distance :: RoadMap -> City -> City -> Maybe Distance
distance x city1 city2 =
  if areAdjacent x city1 city2
    then Just (head [d | (c1, c2, d) <- x, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)])
    else Nothing

-- returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them.

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent x city = [(c, d) | c <- cities x, areAdjacent x city c, Just d <- [distance x city c]]

printAdjacentCities :: RoadMap -> City -> IO ()
printAdjacentCities x city = do
  putStrLn ("The Cities adjacent to " ++ city ++ " are:\n")
  putStrLn (unlines ["City " ++ c ++ " with distance " ++ show d | (c, d) <- adjacent x city])

-- returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads.
-- Otherwise, it returns a Nothing.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1 : c2 : cs) = case distance roadmap c1 c2 of
  Nothing -> Nothing
  Just d -> case pathDistance roadmap (c2 : cs) of
    Nothing -> Nothing
    Just ds -> Just (d + ds)

-- returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)

rome :: RoadMap -> [City]
rome [] = []
rome roadmap = [city | city <- cities roadmap, length (adjacent roadmap city) == maxDegree]
  where
    maxDegree = maximum [length (adjacent roadmap city) | city <- cities roadmap]

-- returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city)
dfs :: RoadMap -> City -> [City]
dfs x start = dfs' [start] []
  where
    dfs' [] visited = visited
    dfs' (c : cs) visited
      | c `elem` visited = dfs' cs visited
      | otherwise = dfs' (adjacentCities ++ cs) (c : visited)
      where
        adjacentCities = [c2 | (c1, c2, _) <- x, c1 == c] ++ [c1 | (c1, c2, _) <- x, c2 == c]

reachableFrom :: RoadMap -> City -> Bool
reachableFrom x city = length (dfs x city) == length (cities x)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected x = all (reachableFrom x) (cities x)

printIsStronglyConnected :: RoadMap -> IO ()
printIsStronglyConnected x =
  if isStronglyConnected x
    then putStrLn ("The graph is strongly connected.")
    else putStrLn ("The graph is not strongly connected.")



--USES DFS (NOT OPTIMAL)
-- Returns the shortest paths based on distance between two cities in a roadmap.
--shortestPath :: RoadMap -> City -> City -> [Path]
--shortestPath roadmap start end
--  | start == end = [[start]]  -- The shortest path to itself
--  | null uniquePaths = []  -- No paths found
--  | otherwise = shortestPaths
--  where
--    -- List of all paths found with their total distances
--    allPaths = findPaths [[start]]  -- Start with the initial path
--
--    -- Find all paths using depth-first search
--    findPaths :: [Path] -> [(Path, Distance)]  -- Returns a list of (Path, Distance) tuples
--    findPaths [] = []  -- No more paths to process
--    findPaths (p:ps)
--      | last p == end = (p, distanceToEnd p) : findPaths ps  -- If we reach the end, add the path with its distance
--      | otherwise = findPaths (extendPath p ++ ps)  -- Extend the current path
--      where
--        -- Extend the current path by exploring adjacent cities
--        extendPath :: Path -> [Path]
--        extendPath path =
--          let currentCity = last path
--          in [path ++ [nextCity] | (nextCity, dist) <- adjacent roadmap currentCity, nextCity `notElem` path]
--
--    -- Calculate the distance from the start city to the end city for a given path
--    distanceToEnd :: Path -> Distance
--    distanceToEnd path = case pathDistance roadmap path of
--      Just d  -> d
--      Nothing -> maxBound  -- Use maxBound for unreachable paths
--
--    -- Get all unique paths to the end city with their distances
--    uniquePaths = filter (\(p, _) -> last p == end) (findPaths [[start]])
--
--    -- Find the minimum distance from the collected paths
--    minDistance = minimum [d | (_, d) <- uniquePaths]
--
--    -- Filter paths to only include those with the minimum distance
--    shortestPaths = [p | (p, d) <- uniquePaths, d == minDistance]

--dijkstra
type DistanceTable = [(City, Distance)] -- table of distances from the starting city to each city
type PredecessorTable = [(City, Maybe City)] -- table of predecessors for each city

initDataStructure :: RoadMap -> City -> (DistanceTable, PredecessorTable)
initDataStructure roadMap start = ([(c, if c == start then 0 else maxBound) | c <- cities roadMap], [(c, Nothing) | c <- cities roadMap])



findCityWithSmallestDistance :: [City] -> DistanceTable -> (City, Distance)
findCityWithSmallestDistance cities distTable = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) [(c, d) | (c, d) <- distTable, c `elem` cities]


updateTables :: DistanceTable -> PredecessorTable -> [(City, Distance)] -> City -> RoadMap -> (DistanceTable, PredecessorTable)
updateTables distTable predsTable neighbors currentCity roadmap=
  foldl update (distTable, predsTable) neighbors
  where

  update (dTable, pTable) (neighbor, weight) =
    let oldDistance = lookupDistance neighbor dTable
        newDistance = case distance roadmap currentCity neighbor of
                        Nothing -> oldDistance
                        Just d -> if oldDistance == maxBound then d else oldDistance + d
    in if newDistance < oldDistance
      then (replace dTable neighbor newDistance, replace pTable neighbor (Just currentCity))
      else (dTable, pTable)

  lookupDistance city table = maybe maxBound id (lookup city table)

  replace table city newValue = map (\(c, v) -> if c == city then (c, newValue) else (c, v)) table
  -- Helper function to get the distance between two cities



-- Dijkstra's algorithm to find the shortest path to a specific destination
dijkstra :: RoadMap -> City -> City -> (DistanceTable, PredecessorTable)
dijkstra roadmap start destination =
    let (initialDistTable, initialPredTable) = initDataStructure roadmap start
        dijkstra' [] distTable predsTable = (distTable, predsTable)  -- No more cities to visit
        dijkstra' unvisitedCities distTable predsTable =
            let (currentCity, currentDistance) = findCityWithSmallestDistance unvisitedCities distTable
            in if currentCity == destination
                then (distTable, predsTable)  -- Stop if we reached the destination
                else let neighbors = adjacent roadmap currentCity
                         (updatedDistTable, updatedPredsTable) = updateTables distTable predsTable neighbors currentCity roadmap
                         remainingCities = filter (/= currentCity) unvisitedCities
                     in dijkstra' remainingCities updatedDistTable updatedPredsTable
    in dijkstra' (cities roadmap) initialDistTable initialPredTable



-- Function to print Distance and Predecessor Tables
printTables :: DistanceTable -> PredecessorTable -> IO ()
printTables distTable predsTable = do
    putStrLn "Distance Table:"
    mapM_ (\(city, dist) -> putStrLn $ city ++ ": " ++ show dist) distTable
    putStrLn "\nPredecessor Table:"
    mapM_ (\(city, preds) -> putStrLn $ city ++ ": " ++ maybe "Nothing" id preds) predsTable
    putStrLn ""

testDijkstra :: RoadMap -> City -> City -> IO ()
testDijkstra roadmap start destination = do
    let (distTable, predsTable) = dijkstra roadmap start destination
    printTables distTable predsTable
    putStrLn $ "Shortest path from " ++ start ++ " to " ++ destination ++ ": " ++ show (reconstructPath predsTable start destination)

reconstructPath :: PredecessorTable -> City -> City -> [City]
reconstructPath predsTable start destination = reverse $ go destination []
  where
    go city path
      | city == start = start : path
      | otherwise = case lookup city predsTable of
                      Just (Just prev) -> go prev (city : path)
                      _ -> []  -- If no predecessor, return empty path




-- given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route
-- whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting
-- city (which is also the ending city) is left to be chosen by each group.
-- Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.

travelSales :: RoadMap -> Path
travelSales = undefined

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]


