import example from "./examples/4.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { Grid, Point } from "./grid";
import { Solutions } from "./Solutions";

function Solution() {
  const input = useContext(InputContext);
  const grid = Grid.parse(input);

  const removeable: Point[][] = [];
  while (true) {
    const accessible = accessibleRolls(grid);
    if (accessible.length === 0) {
      break;
    }
    removeable.push(accessible);
    for (const item of accessible) {
      grid.set(item, ".");
    }
  }

  const part1 = removeable[0].length;
  const part2 = sum(removeable.map((x) => x.length));

  return (
    <>
      <Solutions part1={part1} part2={part2} />
    </>
  );
}

function sum(numbers: number[]) {
  return numbers.reduce((a, b) => a + b, 0);
}

function accessibleRolls(grid: Grid) {
  const accessible: Point[] = [];
  for (const [location, value] of grid) {
    if (value === ".") {
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
