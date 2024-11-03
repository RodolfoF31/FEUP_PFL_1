
import Data.Array qualified
import Data.Bits qualified
import Data.List (intercalate, minimumBy)
import Data.List qualified
import Data.List (permutations)

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String

type Path = [City]

type Distance = Int

type RoadMap = [(City, City, Distance)]

--  Remove duplicate elements from a list.
--  This function takes a list of elements and returns a new list with duplicates removed.

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

--  Extract all unique cities from the road map.
--  The function returns a list of cities present in the road map.

cities :: RoadMap -> [City]
cities x = removeDuplicates (concat [[c1, c2] | (c1, c2, _) <- x])

--  Print all cities in the graph.
--  This function takes a road map and outputs the cities to the console.

printCities :: RoadMap -> IO ()
printCities x = putStrLn ("The cities in the graph are: " ++ intercalate ", " (cities x))

--  Check if two cities are adjacent in the road map.
--  This function returns True if there is a direct road between the two cities; otherwise, it returns False.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent x city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) x

--  Print a message indicating whether two cities are adjacent.
--  This function takes a road map and two cities, outputting the adjacency status to the console.

printAreAdjacent :: RoadMap -> City -> City -> IO ()
printAreAdjacent x city1 city2 =
  if areAdjacent x city1 city2
    then putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are adjacent.")
    else putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are not adjacent.")

--  Calculate the distance between two cities.
--  This function returns the distance as a 'Just Distance' if the cities are adjacent; otherwise, it returns 'Nothing'.

distance :: RoadMap -> City -> City -> Maybe Distance
distance x city1 city2 =
  if areAdjacent x city1 city2
    then Just (head [d | (c1, c2, d) <- x, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)])
    else Nothing

--  Get all cities adjacent to a given city along with their distances.
--  This function returns a list of tuples containing adjacent cities and their distances.

adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent x city = [(c, d) | c <- cities x, areAdjacent x city c, Just d <- [distance x city c]]

--  Print all cities adjacent to a specified city.
--  This function outputs the list of adjacent cities and their distances to the console.

printAdjacentCities :: RoadMap -> City -> IO ()
printAdjacentCities x city = do
  putStrLn ("The Cities adjacent to " ++ city ++ " are:\n")
  putStrLn (unlines ["City " ++ c ++ " with distance " ++ show d | (c, d) <- adjacent x city])

--  Calculate the total distance of a given path.
--  This function returns the total distance as 'Just Distance' if all city pairs are adjacent; otherwise, it returns 'Nothing'.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1 : c2 : cs) = case distance roadmap c1 c2 of
  Nothing -> Nothing
  Just d -> case pathDistance roadmap (c2 : cs) of
    Nothing -> Nothing
    Just ds -> Just (d + ds)

--  Find cities with the highest degree of adjacency (most connections).
--  This function returns a list of cities that are connected to the most other cities.

rome :: RoadMap -> [City]
rome [] = []
rome roadmap = [city | city <- cities roadmap, length (adjacent roadmap city) == maxDegree]
  where
    maxDegree = maximum [length (adjacent roadmap city) | city <- cities roadmap]

--  Perform a depth-first search (DFS) from a starting city.
--  This function returns a list of visited cities.

dfs :: RoadMap -> City -> [City]
dfs x start = dfs' [start] []
  where
    dfs' [] visited = visited
    dfs' (c : cs) visited
      | c `elem` visited = dfs' cs visited
      | otherwise = dfs' (adjacentCities ++ cs) (c : visited)
      where
        adjacentCities = [c2 | (c1, c2, _) <- x, c1 == c] ++ [c1 | (c1, c2, _) <- x, c2 == c]

--  Check if all cities are reachable from a given city.
--  This function returns True if all cities can be reached; otherwise, it returns False.        

reachableFrom :: RoadMap -> City -> Bool
reachableFrom x city = length (dfs x city) == length (cities x)

--  Determine if the graph is strongly connected.
--  This function returns True if every city is reachable from every other city; otherwise, it returns False.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected x = all (reachableFrom x) (cities x)

--  Print a message indicating whether the graph is strongly connected.
--  This function outputs the connection status of the graph to the console.

printIsStronglyConnected :: RoadMap -> IO ()
printIsStronglyConnected x =
  if isStronglyConnected x
    then putStrLn ("The graph is strongly connected.")
    else putStrLn ("The graph is not strongly connected.")


--Dijkstra
type DistanceTable = [(City, Distance)] 
type PredecessorTable = [(City, [Maybe City])] 

--  Initialize distance and predecessor tables for Dijkstra's algorithm.
--  This function takes a road map and a starting city, returning initialized distance and predecessor tables.

initDataStructure :: RoadMap -> City -> (DistanceTable, PredecessorTable)
initDataStructure roadMap start = ([(c, if c == start then 0 else maxBound) | c <- cities roadMap], [(c, [Nothing]) | c <- cities roadMap])

--  Find the city with the smallest distance from the distance table.
--  This function returns a tuple containing the city and its distance.

findCityWithSmallestDistance :: [City] -> DistanceTable -> (City, Distance)
findCityWithSmallestDistance cities distTable = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) [(c, d) | (c, d) <- distTable, c `elem` cities]

--  Update the distance and predecessor tables based on the neighbors of the current city.
--  This function returns updated distance and predecessor tables.

updateTables :: DistanceTable -> PredecessorTable -> [(City, Distance)] -> City -> RoadMap -> (DistanceTable, PredecessorTable)
updateTables distTable predsTable neighbors currentCity roadmap = foldl update (distTable, predsTable) neighbors
  where
    update (dTable, pTable) (neighbor, weight) =
      case distance roadmap currentCity neighbor of
        Just currentDistance ->
          let sourceDist = lookupDistance currentCity dTable
              neighborDist = lookupDistance neighbor dTable
              alt = sourceDist + currentDistance
          in if alt < neighborDist && sourceDist /= maxBound
             then (replace dTable neighbor alt, replace pTable neighbor [Just currentCity])
             else if alt == neighborDist && sourceDist /= maxBound
                  then (dTable, addPredecessor pTable neighbor (Just currentCity))
                  else (dTable, pTable)
    lookupDistance city table = maybe maxBound id (lookup city table)
    replace table city newValue = map (\(c, v) -> if c == city then (c, newValue) else (c, v)) table
    addPredecessor table city pred = map (\(c, preds) -> if c == city then (c, pred : preds) else (c, preds)) table

--  Implement Dijkstra's algorithm to find the shortest path.
--  This function returns the final distance and predecessor tables.

dijkstra :: RoadMap -> City -> City -> (DistanceTable, PredecessorTable)
dijkstra roadmap start destination =
    let (initialDistTable, initialPredTable) = initDataStructure roadmap start
        dijkstra' [] distTable predsTable = (distTable, predsTable)  
        dijkstra' unvisitedCities distTable predsTable =
            let (currentCity, currentDistance) = findCityWithSmallestDistance unvisitedCities distTable
            in if currentCity == destination
                then (distTable, predsTable)  -- Stop if we reached the destination
                else let neighbors = adjacent roadmap currentCity
                         (updatedDistTable, updatedPredsTable) = updateTables distTable predsTable neighbors currentCity roadmap
                         remainingCities = filter (/= currentCity) unvisitedCities
                     in dijkstra' remainingCities updatedDistTable updatedPredsTable
    in dijkstra' (cities roadmap) initialDistTable initialPredTable


--  Find all shortest paths between two cities using Dijkstra's algorithm.
--  This function returns a list of all shortest paths.

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start destination =
    let (distTable, predsTable) = dijkstra roadmap start destination
        shortestDistance = lookupDistance destination distTable
    in if shortestDistance == maxBound
       then []  -- No path found
       else if start == destination
            then [[start]]  -- Path to itself
            else reconstructAllPaths predsTable start destination
  where
    lookupDistance city table = maybe maxBound id (lookup city table)

--  Reconstruct all paths from the start city to the destination city using the predecessor table.
--  This function takes a PredecessorTable, a starting city, and a destination city, and returns a list of all possible paths from the start to the destination.


reconstructAllPaths :: PredecessorTable -> City -> City -> [Path]
reconstructAllPaths predsTable start destination = go [Just destination]
  where
    go path@(Just current : _)
      | current == start = [map (\(Just c) -> c) path]
      | otherwise = case lookup current predsTable of
          Just preds -> concatMap (\pred -> go (pred : path)) preds
          Nothing -> []
    go _ = []  

--  Find the optimal travel sales path for a given roadmap.
--  This function takes a RoadMap and returns the shortest path that visits all cities exactly once and returns to the starting city, implementing a brute-force approach.

travelSales :: RoadMap -> Path
travelSales roadmap =
  let
    allCities = cities roadmap
  in
    if null allCities then [] else 
        let
          start = head allCities
          rest = tail allCities
          
          possiblePaths = map (\p -> start : p ++ [start]) (permutations rest)
          
          validPaths = [(p, d) | p <- possiblePaths, Just d <- [pathDistance roadmap p]]
          
          findMinPath [p] = p
          findMinPath (p1@(path1, dist1) : p2@(path2, dist2) : ps)
            | dist1 <= dist2 = findMinPath (p1 : ps)
            | otherwise      = findMinPath (p2 : ps) 
        in
          if null validPaths then [] else fst (findMinPath validPaths)

--  Print the optimal travel sales path for a given roadmap.
--  This function takes a RoadMap, computes the travel sales path, and prints the result to the console.

printTravelSales :: RoadMap -> IO ()
printTravelSales x = do
  let path = travelSales x
  if null path
    then putStrLn "No TSP path found."
    else putStrLn ("The TSP path for the graph is -> " ++ show path)


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]

