# PFL - Haskell project

## ShortestPath Function




## TravelSales Function

The travelSales function aims to solve the Traveling Salesman Problem (TSP) for a given roadmap of cities. In this problem the person must visit each city exactly once and return to the starting city, minimizing the total travel distance.

This function takes a RoadMap as input, which represents a weighted undirected graph where each tuple (City1, City2, Distance) represents a road between City1 and City2 with a given Distance. The function returns the shortest possible round-trip path (starting and ending at the same city) that visits all cities exactly once.

The function follows a brute-force approach to solve the TSP by:
1. Generating all permutations of cities to explore all possible paths.
2. Filtering out the paths that cannot be completed (Due to the lack of direct connections between the cities)
3. Calculating the total distance for each valid path.
4. Selecting the path with the minimum distance.

The function starts by extracting all unique cities in the roadmap using a helper function, cities. This list, allCities, allows us to build all possible paths that visit each city once.

To generate the paths, we first set the inicial city as the starting and ending city. Using permutations from Data.List, we created a list of potential paths, each beginning and ending with "start".

For each generated path, we use the pathDistance function to calculate the total distance if all cities in the path are connected by direct roads. Paths with missing connections are automatically filtered out, returning "Nothing".

With all valid paths and their corresponding distances, we can identify the path with the minimum distance by recursively comparing distances between paths to determine the shortest one.

We decided to use a list of tuples as an auxiliary data structure. This allows each path to be paired directly with its distance, simplifying the task of finding the minimum distance path later. We also used list comprehensions which are used to filter valid paths.











## Group T07_G01
| Name             | Number    | E-Mail             | Contribution |
| ---------------- | --------- | ------------------ | -------------- |
| António Ferreira         | 202108834 | up202108834@up.pt              | 50%
| José Ferreira        | 202108836 | up202108836@up.pt                | 50%

