import example from "./examples/6.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { sum } from "./lib";

const OPERATORS = {
  "+": {
    identity: 0,
    operation: (a: number, b: number) => a + b,
  },
  "*": {
    identity: 1,
    operation: (a: number, b: number) => a * b,
  },
};

type Operator = keyof typeof OPERATORS;

async function solve(input: string) {
  console.log("Solving problem");
  const lines = parseLines(input);
  const totals1 = part1(lines);
  const totals2 = part2(lines);

  return {
    part1: sum(totals1),
    part2: sum(totals2),
    output: { totals1, totals2 },
  };
}

function part1(lines: string[]) {
  const homework = lines.map((x) => x.trim().split(/ +/));
  const totals = [];

  for (let i = 0; i < homework[0].length; i++) {
    const operator = OPERATORS[last(homework)[i] as "*" | "+"];
    let result = operator.identity;

    for (let j = 0; j < homework.length - 1; j++) {
      const value = Number(homework[j][i]);
      result = operator.operation(result, value);
    }

    totals.push(result);
  }
  return totals;
}

function part2(lines: string[]) {
  const sheet = [];
  for (let i = 0; i < lines[0].length; i++) {
    const column = [];
    for (let j = 0; j < lines.length; j++) {
      column.push(lines[j][i]);
    }
    sheet.push(
      ...column
        .join("")
        .trim()
        .split(/ *\b */)
        .toReversed(),
    );
  }
  const sums = splitOn(sheet, "").map((x) => x.toReversed());

  const totals = [];
  for (const sum of sums) {
    const operator = OPERATORS[sum.pop() as Operator];
    const terms = sum.slice(0).map((x) => Number(x));
    const total = terms.reduce(operator.operation, operator.identity);
    totals.push(total);
  }

  return totals;
}

function last<T>(items: T[]): T {
  return items[items.length - 1];
}

function splitOn<T>(items: T[], splitter: T) {
  const result: T[][] = [[]];
  for (const item of items) {
    if (item === splitter) {
      result.push([]);
    } else {
      result[result.length - 1].push(item);
    }
  }
  return result;
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

renderApp(6, example, <Solution />);
