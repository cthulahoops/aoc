import example from "./examples/10.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { sum } from "./lib";
import solver from "javascript-lp-solver";

async function solve(input: string) {
  console.log("Solving problem");
  const parsed = parseInput(input);

  return {
    part1: part1(parsed),
    part2: part2(solver, parsed),
    output: parsed,
  };
}

function part2(solver: any, machines: Machine[]) {
  const results: number[] = [];
  for (const machine of machines) {
    const model = createModel(machine);
    const solution = solver.Solve(model);
    console.log("Solution:", solution);
    results.push(solution.result);
  }
  return sum(results);
}

function createModel(machine: Machine) {
  const constraints: Record<string, { equal: number }> = {};
  for (let i = 0; i < machine.joltages.length; i++) {
    constraints[String(i)] = { equal: machine.joltages[i] };
  }

  const variables: Record<string, Record<string, number>> = {};
  const ints: Record<string, 1> = { presses: 1 };
  for (let i = 0; i < machine.wiring.length; i++) {
    const variable: Record<string, number> = { presses: 1 };
    for (const item of machine.wiring[i]) {
      variable[String(item)] = 1;
    }
    variables[String(i)] = variable;
    ints[String(i)] = 1;
  }

  return {
    optimize: "presses",
    opType: "min",
    constraints: constraints,
    variables: variables,
    ints: ints,
  };
}

function part1(machines: Machine[]) {
  const results: number[] = [];
  for (const machine of machines) {
    const result = bfs(
      [],
      (x: ButtonPresses) => isTarget(x, machine),
      (x: ButtonPresses) => getNext(x, machine),
    );
    results.push(result?.length || 0);
  }
  return sum(results);
}

type ButtonPresses = number[];
type Wiring = number[][];
type Machine = { target: string; wiring: Wiring; joltages: number[] };

function isTarget(buttons: ButtonPresses, machine: Machine) {
  return (
    machine.target ===
    applyButtons(buttons, machine.wiring, machine.target.length)
  );
}

function getNext(buttons: ButtonPresses, machine: Machine): ButtonPresses[] {
  if (buttons.length > 50) {
    console.log("DEPTH LIMIT!!!");
    return [];
  }
  const last = buttons[buttons.length - 1] || 0;
  const result = [];
  for (let i = last; i < machine.wiring.length; i++) {
    result.push([...buttons, i]);
  }
  return result;
}

function applyButtons(
  buttons: ButtonPresses,
  wiring: Wiring,
  outputSize: number,
) {
  const state = new Map<number, boolean>();
  for (const button of buttons) {
    const effect = wiring[button];
    for (const point of effect) {
      const current = state.get(point) || false;
      state.set(point, !current);
    }
  }

  const result = [];
  for (let i = 0; i < outputSize; i++) {
    result.push(state.get(i) ? "#" : ".");
  }
  return result.join("");
}

function bfs<T>(
  start: T,
  isTarget: (node: T) => boolean,
  getNext: (node: T) => T[],
) {
  const queue = [start];
  while (queue.length > 0) {
    const current = queue.shift()!;

    if (isTarget(current)) {
      return current;
    }

    for (const next of getNext(current)) {
      queue.push(next);
    }
  }
}

function parseInput(input: string) {
  const lines = parseLines(input);
  return lines.map(parseLine);
}

function parseLine(line: string) {
  const match = /\[([#.]+)\] ((\([0-9,]+\) )+)\{([0-9,]+)\}/.exec(line);
  if (!match) {
    throw new Error("No match of line");
  }

  return {
    target: match[1],
    wiring: match[2].trim().split(" ").map(parseWires),
    joltages: match[4].trim().split(",").map(Number),
  };
}

function parseWires(wires: string) {
  const match = /\(([^)]+)\)/.exec(wires);
  if (!match) {
    throw new Error("No match");
  }
  return match[1].split(",").map(Number);
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading, error } = useQuery({
    queryKey: ["day10", input],
    queryFn: () => solve(input),
  });

  if (error) {
    console.error(error);
  }

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

renderApp(10, example, <Solution />);
