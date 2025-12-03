import { InputContext } from "./contexts";
import { useContext } from "react";
import { renderApp } from "./App";

import example from "./examples/2.txt?raw";

function parse(input: string): [number, number][] {
  return input.split(",").map((range) => {
    const [start, end] = range.split("-");
    return [Number(start), Number(end)];
  });
}

function isValid1(input: number): boolean {
  const invalid = /^([0-9]*)\1$/;
  return !String(input).match(invalid);
}

function isValid2(input: number): boolean {
  const invalid = /^([0-9]*)\1+$/;
  return !String(input).match(invalid);
}

function invalidLength(input: number): number {
  const invalid = /^([0-9]*)\1+$/;
  const match = invalid.exec(String(input));
  return match ? match[1].length : 0;
}

function solve(input: string) {
  const ranges = parse(input);
  let part1 = 0;
  let part2 = 0;

  const result: Range[] = [];
  for (const [start, end] of ranges) {
    const invalids: Invalid[] = [];
    const range = { start, end, invalids };
    result.push(range);
    for (let i = start; i <= end; i++) {
      if (!isValid2(i)) {
        console.log("Invalid: ", i);
        invalids.push({
          value: String(i),
          patternLength: invalidLength(i),
        });

        if (!isValid1(i)) {
          part1 += i;
        }
        part2 += i;
      }
    }
  }
  return {
    part1,
    part2,
    ranges: result,
  };
}

type Range = { start: number; end: number; invalids: Invalid[] };
type Invalid = { value: string; patternLength: number };

function Solution() {
  const input = useContext(InputContext);
  const { ranges, part1, part2 } = solve(input);
  return (
    <div className="solution">
      <div className="solutions">
        <div>
          Part 1: <span>{part1}</span>
        </div>
        <div>
          Part 2: <span>{part2}</span>
        </div>
      </div>
      {ranges.map((range: Range, idx: number) => (
        <Range key={idx} {...range} />
      ))}
    </div>
  );
}

function Range({
  start,
  end,
  invalids,
}: {
  start: number;
  end: number;
  invalids: Invalid[];
}) {
  return (
    <div className="range">
      <div>
        {start}-{end}
      </div>
      <ul>
        {invalids.map(({ value, patternLength }, idx) => (
          <li
            key={idx}
            className={
              patternLength === value.length / 2 ? "invalid-1" : "invalid"
            }
          >
            <span className="pattern">{value.substring(0, patternLength)}</span>
            {value.substring(patternLength)}
          </li>
        ))}
      </ul>
    </div>
  );
}

renderApp(2, example, <Solution />);
