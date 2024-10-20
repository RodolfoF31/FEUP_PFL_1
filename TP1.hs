import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

--  returns all the cities in the graph.

cities :: RoadMap -> [City]
cities = undefined 

-- returns a boolean indicating whether two cities are linked directly.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

-- returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.

distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined

-- returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them.

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

-- returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. 
-- Otherwise, it returns a Nothing.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

-- returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)

rome :: RoadMap -> [City]
rome = undefined

-- returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

-- computes all shortest paths [RL99, BG20] connecting the two cities given as input. Note that there may be more than one path with the same total distance.
-- If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c].

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

-- given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route
-- whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting
-- city (which is also the ending city) is left to be chosen by each group. 
-- Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]