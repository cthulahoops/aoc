import example from "./examples/7.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { Grid, Point } from "./grid";

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
    new Set(),
    (lefts, rights, location) =>
      new Set([...lefts, location.toPair(), ...rights]),
  );

  const cache2 = new Grid<number>();
  const part2 = reachable(
    star,
    grid,
    cache2,
    0,
    (lefts, rights, _location) => lefts + rights + 1,
  );

  return {
    part1: part1.size,
    part2: part2 + 1,
    output: [],
  };
}

function reachable<T>(
  location: Point,
  grid: InputGrid,
  cache: Grid<T>,
  empty: T,
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
  return empty;
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

  const { part1, part2, output } = data;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <div>{JSON.stringify(output)}</div>
    </>
  );
}

renderApp(7, example, <Solution />);
