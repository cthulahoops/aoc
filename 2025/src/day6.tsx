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

async function solve(input: string) {
  console.log("Solving problem");
  const homework = parseInput(input);

  const totals = [];

  for (let i = 0; i < homework[0].length; i++) {
    const operator = OPERATORS[homework[homework.length - 1][i] as "*" | "+"];
    let result = operator.identity;

    for (let j = 0; j < homework.length - 1; j++) {
      const value = Number(homework[j][i]);
      result = operator.operation(result, value);
    }

    totals.push(result);
  }

  return {
    part1: sum(totals),
    part2: 0,
    output: totals,
  };
}

function parseInput(input: string): string[][] {
  const lines = parseLines(input);
  return lines.map((x) => x.trim().split(/ +/));
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
