# PFL - Haskell project

## ShortestPath Function

The code we've written  aims at finding all the shortest paths between two cities in a given roadmap using Dijkstra's algorithm.The algorithm calculates both the minimum distance and predecessor relationships to reconstruct the shortest path.

### Key Data Structures
1. **DistanceTable**: This is a list of tuples where each tuple contains a city and its current known distance from the start city. The distance is initialized to zero for the start city and set to a maximum bound (representing infinity) for all other cities. This table will be updated as the algorithm progresses.
   
2. **PredecessorTable**: This is a list of tuples where each tuple consists of a city and a list of possible predecessor cities. The predecessor cities store the previous city on the path leading to this city, allowing path reconstruction.

### Algorithm Workflow
#### 1. **Initialization**
The `initDataStructure` function sets up the initial distance and predecessor tables:
   - For each city, the distance is initialized to zero if it's the start city, otherwise to a very large value (maxBOUND).
   - Each city’s predecessor list starts with a single `Nothing` entry.

#### 2. **Finding the City with the Smallest Distance**
The `findCityWithSmallestDistance` function searches for the city with the smallest known distance from the  unvisited cities. This is achieved using Haskell’s `minimumBy` function, which compares distances from the `DistanceTable` and selects the city with the lowest distance. This city is then used as the "current" city to explore neighbors.

#### 3. **Updating Distance and Predecessor Tables**
The `updateTables` function is the heart of Dijkstra's algorithm, as it updates both the `DistanceTable` and `PredecessorTable`:
   - For each neighboring city of the current city, it calculates the alternative distance (`alt`) by adding the distance of the current city to the edge weight connecting this two cities.
   - If this new distance (`alt`) is shorter than the neighbor's existing distance in the table, the table is updated with this new shorter distance, and the predecessor for this city is updated.
   - If `alt` is equal to the current shortest distance to the neighbor, the `PredecessorTable` appends the current city as an additional predecessor, allowing for multiple shortest paths.

#### 4. **Dijkstra's Algorithm Execution**
The `dijkstra` function manages the execution flow by repeatedly calling helper functions:
   - Starting with the initial distance and predecessor tables, it iterates over all cities in the roadmap.
   - In each iteration, it finds the city with the smallest known distance, explores its neighbors, and updates tables as necessary.
   - The algorithm stops when it reaches the destination city (the shortest path is found) or has visited all cities.
   
#### 5. **Path Reconstruction**
Once Dijkstra’s algorithm completes, the `shortestPath` function calculates the shortest path:
   - If the destination city’s distance is `infinity`, no path exists.
   - If the start city is the same as the destination, the path is simply the start city itself.
   - For all other cases, `reconstructAllPaths` recursively traces back from the destination to the start using the `PredecessorTable`.
   - The result is a list of all possible shortest paths between the start and destination cities, if multiple shortest paths exist.

#### Summary of Core Functions

| Function Name                   | Purpose                                                                                           |
|---------------------------------|---------------------------------------------------------------------------------------------------|
| `initDataStructure`             | Initializes the distance and predecessor tables.                                                  |
| `findCityWithSmallestDistance`  | Finds the city with the minimum distance in the unvisited set.                                    |
| `updateTables`                  | Updates the distance and predecessor tables for each neighboring city.                            |
| `dijkstra`                      | Executes Dijkstra's algorithm to find shortest paths from start to destination.                   |
| `shortestPath`                  | Returns the shortest path(s) by reconstructing paths from the `PredecessorTable`.                 |
| `reconstructAllPaths`           | Recursively traces paths from destination back to the start using predecessors.                   |




## TravelSales Function

The travelSales function aims to solve the Traveling Salesman Problem (TSP) for a given roadmap of cities. In this problem the person must visit each city exactly once and return to the starting city, minimizing the total travel distance.

This function takes a RoadMap as input, which represents a weighted undirected graph where each tuple (City1, City2, Distance) represents a road between City1 and City2 with a given Distance. The function returns the shortest possible round-trip path (starting and ending at the same city) that visits all cities exactly once.

The function follows a brute-force approach to solve the TSP by:
1. Generating all permutations of cities to explore all possible paths.
2. Filtering out the paths that cannot be completed (Due to the lack of direct connections between the cities)
3. Calculating the total distance for each valid path.
4. Selecting the path with the minimum distance.

The function starts by extracting all unique cities in the roadmap using a helper function, cities. This list, allCities, allows us to build all possible paths that visit each city once.

To generate the paths, we first set the inicial city as the starting and ending city. Using permutations from Data.List, we created a list of potential paths, each beginning and ending with `start`.

For each generated path, we use the pathDistance function to calculate the total distance if all cities in the path are connected by direct roads. Paths with missing connections are automatically filtered out, returning "Nothing".

With all valid paths and their corresponding distances, we can identify the path with the minimum distance by recursively comparing distances between paths to determine the shortest one.

We decided to use a list of tuples as an auxiliary data structure. This allows each path to be paired directly with its distance, simplifying the task of finding the minimum distance path later. We also used list comprehensions which are used to filter valid paths.











## Group T07_G01
| Name             | Number    | E-Mail             | Contribution |
| ---------------- | --------- | ------------------ | -------------- |
| António Ferreira         | 202108834 | up202108834@up.pt              | 50%
| José Ferreira        | 202108836 | up202108836@up.pt                | 50%

António Ferreira was responsible for the TSP algorithm and José Ferreira was responsible for the shortestPath Algorithm. The rest of the functions were coded together.