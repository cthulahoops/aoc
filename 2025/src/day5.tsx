import example from "./examples/5.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { sum } from "./lib";
import { Range } from "./range";

async function solve(input: string) {
  console.log("Solving problem");
  const [ranges, inventory] = parseInput(input);

  const fresh = inventory.filter((ingredient) => isFresh(ingredient, ranges));

  const disjointRanges = computeDisjoint(ranges);

  const part2 = sum(disjointRanges.map((x) => x.length));

  return {
    part1: fresh.length,
    part2: part2,
    output: disjointRanges,
  };
}

function computeDisjoint(ranges: Range[]) {
  const sorted = ranges.toSorted((a: Range, b: Range) => a.start - b.start);
  const disjoint: Range[] = [];
  for (const range of sorted) {
    const last_idx = disjoint.length - 1;
    const last = disjoint[last_idx];
    if (last && last.intersects(range)) {
      disjoint[last_idx] = last.merge(range);
    } else {
      disjoint.push(range);
    }
  }
  return disjoint;
}

function isFresh(ingredient: number, ranges: Range[]) {
  return ranges.some((range) => range.contains(ingredient));
}

type Inventory = number[];

function parseInput(input: string): [Range[], Inventory] {
  const [ranges, inventory] = input.split("\n\n");
  return [
    parseLines(ranges).map(Range.parse),
    parseLines(inventory).map(Number),
  ];
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day4", input],
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

renderApp(5, example, <Solution />);
