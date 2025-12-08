import example from "./examples/8.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { Point3 } from "./point3";
import { SMap } from "./smap";
import { product } from "./lib";

async function solve(input: string) {
  console.log("Solving problem");
  console.log("Parse", parseInput(input));
  const points = parseInput(input);

  // const shortest = new MinList<[Point3, Point3]>(
  //   points.length > 50 ? 1000 : 10,
  // );
  //
  const part1Length = points.length > 50 ? 1000 : 10;

  const shortest: [[Point3, Point3], number][] = [];

  for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < i; j++) {
      const p1 = points[i];
      const p2 = points[j];
      shortest.push([[p1, p2], p1.sqDist(p2)]);
    }
  }
  shortest.sort((a, b) => a[1] - b[1]);

  console.log("Short: ", shortest);

  const groups = new SMap(Point3);
  let group_count = 0;
  for (const p of points) {
    group_count++;
    groups.set(p, group_count);
  }

  console.log("groups: ", group_count);
  for (const [[p1, p2], _] of shortest.slice(0, part1Length)) {
    const p1_id = groups.get(p1);
    const p2_id = groups.get(p2);
    if (p1_id == p2_id) {
      continue;
    }

    group_count--;

    for (const [p, group_id] of groups) {
      if (group_id == p2_id) {
        groups.set(p, p1_id);
      }
    }
  }

  console.log("Group count", group_count);

  const counts = new Map();
  for (const [_, group_id] of groups) {
    counts.set(group_id, (counts.get(group_id) || 0) + 1);
  }

  console.log("Counts: ", counts);

  let connected = [...counts.values()];
  connected.sort((a, b) => a - b);
  connected = connected.slice(connected.length - 3);

  let part2 = 0;

  for (const [[p1, p2], _] of shortest.slice(part1Length)) {
    const p1_id = groups.get(p1);
    const p2_id = groups.get(p2);
    if (p1_id == p2_id) {
      continue;
    }
    group_count--;
    for (const [p, group_id] of groups) {
      if (group_id == p2_id) {
        groups.set(p, p1_id);
      }
    }

    if (group_count == 1) {
      console.log("Break!");
      part2 = p1.x * p2.x;
      break;
    }
  }

  return {
    part1: product(connected),
    part2: part2,
    output: connected,
  };
}

function parseInput(input: string): Point3[] {
  const lines = parseLines(input);
  return lines.map((x) => Point3.fromString(x));
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day8", input],
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

renderApp(6, example, <Solution />);
