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
  
printPathDistance :: RoadMap -> Path -> IO ()
printPathDistance x path =
  case pathDistance x path of
    Nothing -> putStrLn "The path is not valid."
    Just d -> putStrLn ("The distance of the path is " ++ show d)

-- returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)

rome :: RoadMap -> [City]
rome [] = []
rome roadmap = [city | city <- cities roadmap, length (adjacent roadmap city) == maxDegree]
  where
    maxDegree = maximum [length (adjacent roadmap city) | city <- cities roadmap]
  
printRome :: RoadMap -> IO ()
printRome x = do
  let romeCities = rome x
  if null romeCities
    then putStrLn "There are no cities with the highest degree."
    else putStrLn ("The cities with the highest degree are: " ++ intercalate ", " romeCities)

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

-- computes all shortest paths [RL99, BG20] connecting the two cities given as input. Note that there may be more than one path with the same total distance.
-- If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c].


-- Main shortestPath function to find all shortest paths with minimum distance
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined


  
-- given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route
-- whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting
-- city (which is also the ending city) is left to be chosen by each group.
-- Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.


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


