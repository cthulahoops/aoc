import { createRoot } from "react-dom/client";
import { useContext, useRef, useEffect } from "react";
import { InputContext } from "./contexts";
import { InputProvider } from "./InputProvider";
import { parseLines } from "./parse.ts";

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

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function day1(
  elementRef: React.RefObject<HTMLDivElement | null>,
  commands: string,
) {
  const lines = commands.split("\n").filter((item) => item);
  const distances = lines.map(parseCommand);

  let value = 50;
  let part1 = 0;
  let part2 = 0;

  for (const distance of distances) {
    for (let i = 0; i < Math.abs(distance); i++) {
      await sleep(30);
      if (distance < 0) {
        value -= 1;
      } else {
        value += 1;
      }

      if (elementRef.current) {
        elementRef.current.style.setProperty("--value", String(value));
      }
      if (value % 100 === 0) {
        part2 += 1;
        if (elementRef.current) {
          document.getElementById("part2")!.textContent = String(part2);
        }
      }
    }
    await sleep(100);
    if (value % 100 === 0) {
      part1 += 1;
      if (elementRef.current) {
        document.getElementById("part1")!.textContent = String(part1);
      }
    }
  }

  console.log("Finished ", part1, part2);
  return { part1, part2 };
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
  const liveRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    day1(liveRef, input);
  }, [input]);

  const lines = parseLines(input);

  const [part1, part2] = solve_fast(lines);

  return (
    <>
      <div className="solutions box">
        <div className="part1">Part 1: {part1}</div>
        <div className="part2">Part 2: {part2}</div>
      </div>
      <div className="solution" ref={liveRef}>
        <div
          className="dial"
          style={{ transform: `rotate(calc(3.6deg * var(--value)))` }}
        ></div>
        <>
          <div id="part1"></div>
          <div id="part2"></div>
        </>
      </div>
    </>
  );
}

function App() {
  return (
    <InputProvider storageKey="day1/input" example={example}>
      <Day1 />
    </InputProvider>
  );
}

const root = createRoot(document.getElementById("root")!);
root.render(<App />);
