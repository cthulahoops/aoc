import example from "./examples/7.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { Grid, Point } from "./grid";
import { sum } from "./lib";
import { GridCell } from "./GridCell";

type InputGrid = Grid<"S" | "^" | ".">;

async function solve(input: string) {
  console.log("Solving problem");

  const grid = Grid.parse(input, (x) => {
    if (x === "S") {
      return "S";
    } else if (x === "^") {
      return "^";
    } else if (x === ".") {
      return ".";
    }
  });

  const star = findStar(grid);

  const cache1 = new Grid<Set<string>>();
  const part1 = reachable(
    star,
    grid,
    cache1,
    () => new Set(),
    (lefts, rights, location) =>
      new Set([...lefts, location.toPair(), ...rights]),
  );

  const visits = reachable(
    star,
    grid,
    new Grid<Map<Point, number>>(),
    (point) => new Map([[point, 1]]),
    (lefts, rights, _location) => sumMap(lefts, rights),
  );

  return {
    star: star,
    part1: part1.size,
    part2: sum([...visits.values()]),
    grid: grid,
    pathCount: cache1,
    output: visits,
  };
}

function sumMap<K>(map1: Map<K, number>, map2: Map<K, number>): Map<K, number> {
  const result: Map<K, number> = new Map([...map1]);
  for (const [key, value] of map2.entries()) {
    result.set(key, (result.get(key) || 0) + value);
  }
  return result;
}

function reachable<T>(
  location: Point,
  grid: InputGrid,
  cache: Grid<T>,
  empty: (p: Point) => T,
  combine: (left: T, right: T, location: Point) => T,
): T {
  const cached = cache.get(location);
  if (cached) {
    return cached;
  }
  const value = grid.get(location);
  if (value === "." || value === "S") {
    const result = reachable(location.down(), grid, cache, empty, combine);
    cache.set(location, result);

    return result;
  } else if (value === "^") {
    const lefts = reachable(location.left(), grid, cache, empty, combine);
    const rights = reachable(location.right(), grid, cache, empty, combine);
    const result = combine(lefts, rights, location);
    cache.set(location, result);
    return result;
  }
  return empty(location);
}

function findStar(grid: InputGrid): Point {
  for (const [location, value] of grid) {
    if (value === "S") {
      return location;
    }
  }

  throw new Error("Star not found");
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day7", input],
    queryFn: () => solve(input),
  });

  if (isLoading || !data) {
    return <div>Loading</div>;
  }

  const { star, part1, part2, grid, pathCount, output } = data;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <div className="tree">
        <GridCell position={star}>🌟</GridCell>
        {[...grid]
          .filter((item) => item[1] === "^")
          .map(([pos, _value], idx) => (
            <GridCell position={pos} key={`split-${idx}`}>
              🪩
            </GridCell>
          ))}
        {[...pathCount].map(([pos, _value], idx) => (
          <GridCell position={pos} key={idx}>
            ⚡
          </GridCell>
        ))}

        {[...output].map(([pos, value], idx) => (
          <GridCell position={pos} key={`output-${idx}`} className="tall-cell">
            <div style={{ transform: "rotate(90deg)" }}>{value}</div>
          </GridCell>
        ))}
      </div>
    </>
  );
}

renderApp(7, example, <Solution />);
