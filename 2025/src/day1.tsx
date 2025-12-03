import { useContext, useEffect, useState } from "react";
import { InputContext } from "./contexts";
import { parseLines } from "./parse.ts";
import { renderApp } from "./App";

import example from "./examples/1.txt?raw";

function parseCommand(instruction: string) {
  const direction = instruction.substr(0, 1);
  const distance = Number(instruction.substr(1));
  if (direction === "L") {
    return -1 * distance;
  } else if (direction === "R") {
    return distance;
  }
  throw new Error(`Invalid instruction: ${instruction}`);
}

import "react";

declare module "react" {
  interface CSSProperties {
    [key: `--${string}`]: string | number;
  }
}

type State = {
  commands: string[];
  offset: number;
  part1: number;
  part2: number;
  value: number;
  steps: number;
};

function activeCommand(state: State): string | undefined {
  return state.commands[state.offset];
}

function updateState(state: State): State {
  const command = activeCommand(state);
  if (!command) {
    return state;
  }
  const distance = parseCommand(command);

  if (state.steps < Math.abs(distance)) {
    state.value += Math.sign(distance);
    state.steps++;

    if (state.value % 100 === 0) {
      state.part2++;
    }
  } else {
    state.offset++;
    state.steps = 0;
    if (state.value % 100 === 0) {
      state.part1++;
    }
  }

  return { ...state };
}

function solve_fast(lines: string[]) {
  const distances = lines.map(parseCommand);

  let value = 50;
  let password = 0;
  let password2 = 0;

  for (const distance of distances) {
    if (distance > 0) {
      password2 += Math.floor(((((value % 100) + 100) % 100) + distance) / 100);
    } else {
      password2 +=
        -1 * Math.ceil(((((value % 100) - 100) % 100) + distance) / 100);
    }

    value = value + distance;
    if (value % 100 === 0) {
      password += 1;
    }
  }
  return [password, password2];
}

function Day1() {
  const input = useContext(InputContext);

  const [state, setState] = useState<State>({
    commands: [],
    offset: 0,
    part1: 0,
    part2: 0,
    value: 50,
    steps: 0,
  });

  useEffect(
    () =>
      setState({
        commands: parseLines(input),
        offset: 0,
        part1: 0,
        part2: 0,
        value: 50,
        steps: 0,
      }),
    [input],
  );

  useEffect(() => {
    const timerId = setInterval(() => setState(updateState), 30);
    () => clearInterval(timerId);
  }, []);

  const lines = parseLines(input);

  const [part1, part2] = solve_fast(lines);

  return (
    <>
      <div className="solutions box">
        <div className="part1">Part 1: {part1}</div>
        <div className="part2">Part 2: {part2}</div>
      </div>
      <div className="solution" style={{ "--value": String(state.value) }}>
        <div>{((state.value % 100) + 100) % 100}</div>
        <div
          className="dial"
          style={{ transform: `rotate(calc(3.6deg * var(--value)))` }}
        ></div>
        <div>{activeCommand(state)}</div>
        <div id="part1">{state.part1}</div>
        <div id="part2">{state.part2}</div>
      </div>
    </>
  );
}

renderApp(1, example, <Day1 />);
