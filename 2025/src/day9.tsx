import example from "./examples/9.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { Point2 } from "./point2";

async function solve(input: string) {
  console.log("Solving problem");
  const points = parseInput(input);

  let best = 0;
  for (const p1 of points) {
    for (const p2 of points) {
      const area = p1.area(p2);
      if (area > best) {
        best = area;
      }
    }
  }

  return {
    part1: best,
    part2: 0,
    output: input,
  };
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

  const { part1, part2, output } = data;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <div>{JSON.stringify(output)}</div>
    </>
  );
}

renderApp(9, example, <Solution />);
