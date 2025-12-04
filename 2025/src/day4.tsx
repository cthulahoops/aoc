import example from "./examples/4.txt?raw";
import { useContext, useEffect, useState } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Grid, Point } from "./grid";
import { Solutions } from "./Solutions";

async function solve(input: string) {
  console.log("Solving problem");
  const grid = Grid.parse(input, (x: string) => (x === "@" ? "@" : undefined));
  const removed = new Grid<number>();
  const removeable: Point[][] = [];
  let round = 0;
  while (true) {
    round += 1;
    const accessible = accessibleRolls(grid);
    if (accessible.length === 0) {
      break;
    }
    removeable.push(accessible);

    for (const item of accessible) {
      grid.delete(item);
      removed.set(item, round);
    }
    if (round > 200) {
      // Infinite loop defences.
      break;
    }
  }

  return {
    part1: removeable[0].length,
    part2: sum(removeable.map((x) => x.length)),
    removed,
    roundCount: round,
    grid,
  };
}

function useFrames(interval: number) {
  const [currentFrame, setCurrentFrame] = useState(0);
  useEffect(() => {
    const timer = setInterval(() => setCurrentFrame((x) => x + 1), interval);
    () => clearInterval(timer);
  }, [interval]);

  return currentFrame;
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day4", input],
    queryFn: () => solve(input),
  });

  const t = useFrames(100);

  if (isLoading || !data) {
    return <div>Loading</div>;
  }

  const { part1, part2, roundCount, grid, removed } = data;

  const frame = t % (roundCount + 3);
  const radius = 6;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <svg width={grid.maxX * radius * 2} height={grid.maxY * radius * 2}>
        {[...grid].map((item) => (
          <circle
            cx={item[0].x * radius * 2 + radius}
            cy={item[0].y * radius * 2 + radius}
            r={radius}
            key={item[0].toPair()}
            fill="black"
          />
        ))}
        {[...removed].map((item) => (
          <circle
            cx={item[0].x * radius * 2 + radius}
            cy={item[0].y * radius * 2 + radius}
            r={radius}
            key={item[0].toPair()}
            fill={item[1] <= frame ? color(item[1], roundCount) : "black"}
          />
        ))}
      </svg>
    </>
  );
}

function color(item: number, rounds: number) {
  const round = (item * 360) / rounds;
  return `hsl(${round}, 50%, 50%)`;
}

function sum(numbers: number[]) {
  return numbers.reduce((a, b) => a + b, 0);
}

function accessibleRolls(grid: Grid<"@">) {
  const accessible: Point[] = [];
  for (const [location, _value] of grid) {
    let count = 0;
    location.neighbours().forEach((neighbour: Point) => {
      if (grid.get(neighbour)) {
        count++;
      }
    });
    if (count >= 4) {
      continue;
    }

    accessible.push(location);
  }
  return accessible;
}

renderApp(4, example, <Solution />);
