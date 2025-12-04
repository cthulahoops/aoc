import example from "./examples/4.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { Grid, Point } from "./grid";
import { Solutions } from "./Solutions";

import "./day4.css";

function Solution() {
  const input = useContext(InputContext);
  const grid = Grid.parse(input);

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
      grid.set(item, `${round}`);
    }
    if (round > 200) {
      // Infinite loop defences.
      break;
    }
  }

  const part1 = removeable[0].length;
  const part2 = sum(removeable.map((x) => x.length));

  const radius = 6;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <svg width={1500} height={1500}>
        {[...grid]
          .filter((item) => item[1] !== ".")
          .map((item, idx) => (
            <circle
              cx={item[0].x * radius * 2 + radius}
              cy={item[0].y * radius * 2 + radius}
              r={radius}
              key={idx}
              fill={color(item[1], round)}
            />
          ))}
      </svg>
    </>
  );
}

function color(item: string, rounds: number) {
  if (item === "@") {
    return "black";
  }
  const round = (Number(item) * 360) / rounds;
  return `hsl(${round}, 50%, 50%)`;
}

function sum(numbers: number[]) {
  return numbers.reduce((a, b) => a + b, 0);
}

function accessibleRolls(grid: Grid) {
  const accessible: Point[] = [];
  for (const [location, value] of grid) {
    if (value !== "@") {
      continue;
    }
    let count = 0;
    location.neighbours().forEach((neighbour: Point) => {
      if (grid.get(neighbour) === "@") {
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
