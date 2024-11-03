import Data.Array qualified
import Data.Bits qualified
import Data.List (intercalate)
import Data.List qualified
import Data.List (permutations)

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City, City, Distance)]

-- Remove duplicates from a list.
-- Takes a list of elements and returns a list with duplicate elements removed.
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

-- List all unique cities in a roadmap.
-- Takes a roadmap and returns a list of cities without duplicates.
cities :: RoadMap -> [City]
cities x = removeDuplicates (concat [[c1, c2] | (c1, c2, _) <- x])

-- Print all cities in the roadmap.
-- Takes a roadmap and prints each city in a comma-separated list.
printCities :: RoadMap -> IO ()
printCities x = putStrLn ("The cities in the graph are: " ++ intercalate ", " (cities x))

-- Check if two cities are adjacent in the roadmap.
-- Takes a roadmap and two cities, returns True if they are directly connected.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent x city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) x

-- Print adjacency status between two cities.
-- Takes a roadmap and two cities, prints if they are adjacent or not.
printAreAdjacent :: RoadMap -> City -> City -> IO ()
printAreAdjacent x city1 city2 =
  if areAdjacent x city1 city2
    then putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are adjacent.")
    else putStrLn ("City " ++ city1 ++ " and " ++ "City " ++ city2 ++ " are not adjacent.")

-- Find the distance between two cities if they are adjacent.
-- Takes a roadmap and two cities, returns the distance as Maybe Distance or Nothing if not adjacent.
distance :: RoadMap -> City -> City -> Maybe Distance
distance x city1 city2 =
  if areAdjacent x city1 city2
    then Just (head [d | (c1, c2, d) <- x, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)])
    else Nothing

-- List all cities adjacent to a given city with their distances.
-- Takes a roadmap and a city, returns a list of adjacent cities with distances.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent x city = [(c, d) | c <- cities x, areAdjacent x city c, Just d <- [distance x city c]]

-- Print all adjacent cities of a given city with distances.
-- Takes a roadmap and a city, prints each adjacent city with the distance to it.
printAdjacentCities :: RoadMap -> City -> IO ()
printAdjacentCities x city = do
  putStrLn ("The Cities adjacent to " ++ city ++ " are:\n")
  putStrLn (unlines ["City " ++ c ++ " with distance " ++ show d | (c, d) <- adjacent x city])

-- Calculate the total distance of a path.
-- Takes a roadmap and a path, returns the distance of the path as Maybe Distance or Nothing if path is invalid.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1 : c2 : cs) = case distance roadmap c1 c2 of
  Nothing -> Nothing
  Just d -> case pathDistance roadmap (c2 : cs) of
    Nothing -> Nothing
    Just ds -> Just (d + ds)

-- Print the total distance of a path.
-- Takes a roadmap and a path, prints the distance of the path or indicates if the path is invalid.
printPathDistance :: RoadMap -> Path -> IO ()
printPathDistance x path =
  case pathDistance x path of
    Nothing -> putStrLn "The path is not valid."
    Just d -> putStrLn ("The distance of the path is " ++ show d)

-- List cities with the highest degree (most adjacent connections).
-- Takes a roadmap and returns a list of cities with the maximum number of adjacent cities.
rome :: RoadMap -> [City]
rome [] = []
rome roadmap = [city | city <- cities roadmap, length (adjacent roadmap city) == maxDegree]
  where
    maxDegree = maximum [length (adjacent roadmap city) | city <- cities roadmap]

-- Print cities with the highest degree.
-- Takes a roadmap and prints each city with the maximum number of adjacent connections.
printRome :: RoadMap -> IO ()
printRome x = do
  let romeCities = rome x
  if null romeCities
    then putStrLn "There are no cities with the highest degree."
    else putStrLn ("The cities with the highest degree are: " ++ intercalate ", " romeCities)

-- Depth-first search (DFS) to list reachable cities from a starting city.
-- Takes a roadmap and a starting city, returns a list of all cities reachable from the start.
dfs :: RoadMap -> City -> [City]
dfs x start = dfs' [start] []
  where
    dfs' [] visited = visited
    dfs' (c : cs) visited
      | c `elem` visited = dfs' cs visited
      | otherwise = dfs' (adjacentCities ++ cs) (c : visited)
      where
        adjacentCities = [c2 | (c1, c2, _) <- x, c1 == c] ++ [c1 | (c1, c2, _) <- x, c2 == c]

-- Check if a city can reach all other cities in the graph.
-- Takes a roadmap and a city, returns True if all cities are reachable from the given city.
reachableFrom :: RoadMap -> City -> Bool
reachableFrom x city = length (dfs x city) == length (cities x)

-- Check if the entire graph is strongly connected.
-- Takes a roadmap, returns True if all cities are mutually reachable.
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected x = all (reachableFrom x) (cities x)

-- Print if the graph is strongly connected.
-- Takes a roadmap and prints if all cities are mutually reachable.
printIsStronglyConnected :: RoadMap -> IO ()
printIsStronglyConnected x =
  if isStronglyConnected x
    then putStrLn ("The graph is strongly connected.")
    else putStrLn ("The graph is not strongly connected.")

-- Find the shortest path between two cities (currently undefined).
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

-- Solve the Traveling Salesman Problem (TSP) by finding the shortest round-trip path.
-- Takes a roadmap, returns the shortest path that visits all cities and returns to the starting city.
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

-- Print the shortest TSP path.
-- Takes a roadmap and prints the shortest round-trip path that visits all cities.
printTravelSales :: RoadMap -> IO ()
printTravelSales x = do
  let path = travelSales x
  if null path
    then putStrLn "No TSP path found."
    else putStrLn ("The TSP path for the graph is -> " ++ show path)

-- Example roadmaps for testing
gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]

