import example from "./examples/9.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { Point2 } from "./point2";
import { GridCell } from "./GridCell";
import { SMap } from "./smap";

type Grid = SMap<Point2, "#" | ".">;
type Compressed = { points: Point2[]; xs: number[]; ys: number[] };

async function solve(input: string) {
  console.log("Solving problem");
  const points = parseInput(input);

  const compressed = compressCoords(points);

  const grid = new SMap<Point2, "#" | ".">(Point2);
  for (let i = 0; i < compressed.points.length; i++) {
    const point = compressed.points[i];
    const next = compressed.points[(i + 1) % compressed.points.length];
    const delta = next.sub(point);
    const distance = Math.abs(delta.x) + Math.abs(delta.y);
    const step = delta.div(distance);
    console.log(point, step, next);
    for (let edge = point; !edge.equals(next); edge = edge.add(step)) {
      grid.set(edge, "#");
    }
  }

  floodFill(
    grid,
    new Point2(-3, -3),
    new Point2(compressed.xs.length + 3, compressed.ys.length + 3),
  );

  console.log(grid);

  return {
    part1: part1(points),
    part2: part2(compressed, grid),
    grid: grid,
    points: compressed.points,
    output: input,
  };
}

function floodFill(grid: Grid, start: Point2, max: Point2) {
  const queue = [start];
  let ptr = 0;
  while (ptr < queue.length) {
    const point = queue[ptr];
    ptr++;
    if (
      point.x < start.x ||
      point.y < start.y ||
      point.x >= max.x ||
      point.y >= max.y
    ) {
      continue;
    }
    if (grid.get(point) !== undefined) {
      continue;
    }
    grid.set(point, ".");
    queue.push(new Point2(point.x + 1, point.y));
    queue.push(new Point2(point.x - 1, point.y));
    queue.push(new Point2(point.x, point.y + 1));
    queue.push(new Point2(point.x, point.y - 1));
  }
}

function compressCoords(points: Point2[]) {
  const xs = uniqueSorted(points.map((p) => p.x));
  const ys = uniqueSorted(points.map((p) => p.y));

  const xIndex = makeIndex(xs);
  const yIndex = makeIndex(ys);

  const compressed = points.map(
    (p) => new Point2(xIndex.get(p.x)!, yIndex.get(p.y)!),
  );

  return {
    points: compressed,
    xs,
    ys,
  };
}

function uniqueSorted(list: number[]) {
  return [...new Set(list.toSorted((a, b) => a - b))];
}

function makeIndex(list: number[]): Map<number, number> {
  return new Map(list.map((value, idx) => [value, idx]));
}

function uncompress(point: Point2, xs: number[], ys: number[]) {
  return new Point2(xs[point.x], ys[point.y]);
}

function part2({ points, xs, ys }: Compressed, grid: Grid) {
  const originals = points.map((p) => uncompress(p, xs, ys));
  let best = 0;
  for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < i; j++) {
      const area = originals[i].area(originals[j]);
      if (area <= best) {
        continue;
      }
      const p1 = points[i];
      const p2 = points[j];

      if (hasPointOutside(grid, p1, p2)) {
        continue;
      }

      console.log("New best: ", originals[i], originals[j], area);

      best = area;
    }
  }
  return best;
}

function hasPointOutside(grid: Grid, p1: Point2, p2: Point2) {
  let xMin = Math.min(p1.x, p2.x);
  let xMax = Math.max(p1.x, p2.x);
  let yMin = Math.min(p1.y, p2.y);
  let yMax = Math.max(p1.y, p2.y);

  for (let x = xMin; x <= xMax; x++) {
    for (let y = yMin; y <= yMax; y++) {
      if (grid.get(new Point2(x, y)) === ".") {
        return true;
      }
    }
  }
  return false;
}

function part1(points: Point2[]) {
  let best = 0;
  for (const p1 of points) {
    for (const p2 of points) {
      const area = p1.area(p2);
      if (area > best) {
        best = area;
      }
    }
  }
  return best;
}

function parseInput(input: string): Point2[] {
  const lines = parseLines(input);
  return lines.map(Point2.fromString);
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day6", input],
    queryFn: () => solve(input),
  });

  if (isLoading || !data) {
    return <div>Loading</div>;
  }

  const { part1, part2, grid, points } = data;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <div>{JSON.stringify(points)}</div>
      <div className="tree">
        {[...grid].map(([p, v], idx) => (
          <GridCell position={p.add(new Point2(4, 4))} key={idx}>
            {v}
          </GridCell>
        ))}
      </div>
    </>
  );
}

renderApp(9, example, <Solution />);
