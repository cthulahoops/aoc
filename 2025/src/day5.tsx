import example from "./examples/5.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { sum } from "./lib";

async function solve(input: string) {
  console.log("Solving problem");
  const [ranges, inventory] = parseInput(input);

  const fresh = inventory.filter((ingredient) => isFresh(ingredient, ranges));

  const disjointRanges = computeDisjoint(ranges);

  const part2 = sum(disjointRanges.map((x) => x.length()));

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
    const idx = disjoint.findIndex((existing) => existing.intersects(range));
    if (idx >= 0) {
      disjoint[idx] = disjoint[idx].merge(range);
    } else {
      disjoint.push(range);
    }
  }
  return disjoint;
}

class Range {
  start: number;
  end: number;

  constructor(start: number, end: number) {
    this.start = start;
    this.end = end;
  }

  intersects(other: Range) {
    return (
      this.contains(other.start) ||
      this.contains(other.end) ||
      other.contains(this.start) ||
      other.contains(this.end)
    );
  }

  contains(value: number) {
    return value >= this.start && value <= this.end;
  }

  static parse(text: string) {
    const [start, end] = text.split("-");
    return new Range(Number(start), Number(end));
  }

  length() {
    return this.end - this.start + 1;
  }

  merge(other: Range) {
    if (!this.intersects(other)) {
      throw new Error("Ranges do not overlap");
    }
    return new Range(
      Math.min(this.start, other.start),
      Math.max(this.end, other.end),
    );
  }
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
