import example from "./examples/3.txt?raw";
import { useContext, useState } from "react";
import { digits, parseLines } from "./parse";
import { InputContext } from "./contexts";
import { Solutions } from "./Solutions";

import { renderApp } from "./App";

function bestJoltage(line: number[], count: number): number {
  return joltage(line, bestBatteries(line, count));
}

function joltage(line: number[], batteries: number[]) {
  const digits = [];
  for (const battery of batteries) {
    digits.push(line[battery]);
  }
  return Number(digits.map(String).join(""));
}

function bestBatteries(line: number[], count: number): number[] {
  let start = 0;
  const batteries: number[] = [];
  for (let i = count; i > 0; i--) {
    const slice = line.slice(start, line.length - i + 1);
    const highest = Math.max(...slice);
    const batteryChoice = slice.indexOf(highest) + start;
    start = batteryChoice + 1;
    batteries.push(batteryChoice);
  }
  return batteries;
}

function sum(numbers: number[]) {
  return numbers.reduce((a, b) => a + b, 0);
}

function Solution() {
  const input = useContext(InputContext);
  const [count, setCount] = useState("2");

  const batteries = parseLines(input).map(digits);
  const part1 = batteries.map((x) => bestJoltage(x, 2));
  const part2 = batteries.map((x) => bestJoltage(x, 12));
  console.log(part1);
  return (
    <>
      <Solutions part1={sum(part1)} part2={sum(part2)} />
      <div className="box">
        <input value={count} onChange={(e) => setCount(e.target.value)} />
      </div>
      <ul>
        {batteries.map((x) => (
          <li>
            <Batteries useCount={Number(count)} batteries={x} />
          </li>
        ))}
      </ul>
    </>
  );
}

function Batteries({
  useCount,
  batteries,
}: {
  useCount: number;
  batteries: number[];
}) {
  const best = bestBatteries(batteries, useCount);
  return (
    <>
      <div className="battery-box">
        {batteries.map((x, idx) => (
          <span className={best.includes(idx) ? "best battery" : "battery"}>
            {x}
          </span>
        ))}
      </div>
      = {joltage(batteries, best)}
    </>
  );
}

renderApp(3, example, <Solution />);
