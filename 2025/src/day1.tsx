/// <reference types="./vite-env.d.ts" />
import { createRoot } from "react-dom/client";
import { useRef } from "react";
import {
  useQuery,
  QueryClientProvider,
  QueryClient,
} from "@tanstack/react-query";

import example from "./examples/1.txt?raw";
// import input from "./inputs/1.txt?raw";

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

async function day1(state: React.RefObject<State>, commands: string) {
  const lines = commands.split("\n").filter((item) => item);
  const distances = lines.map(parseCommand);

  for (const distance of distances) {
    for (let i = 0; i < Math.abs(distance); i++) {
      await sleep(30);
      if (distance < 0) {
        state.current.value -= 1;
      } else {
        state.current.value += 1;
      }
      if (state.current.value % 100 === 0) {
        state.current.part2 += 1;
      }
    }
    await sleep(100);
    if (state.current.value % 100 === 0) {
      state.current.part1 += 1;
    }
  }

  // console.log(password);
  return { part1: state.current.part1, part2: state.current.part2 };
}

type State = { value: number; part1: number; part2: number };

// Efficient version:?
// for (const distance of distances) {
//   if (distance > 0) {
//     password2 += Math.floor(((((value % 100) + 100) % 100) + distance) / 100);
//   } else {
//     password2 +=
//       -1 * Math.ceil(((((value % 100) - 100) % 100) + distance) / 100);
//   }

//   value = value + distance;
//   console.log(value);
//   if (dial.current) {
//     dial.current.style.transform = `rotate(${3.6 * value}deg)`;
//   }
//   if (value % 100 === 0) {
//     password += 1;
//   }
// }

function Day1() {
  const state = useRef<State>({
    value: 50,
    part1: 0,
    part2: 0,
  });

  const { data, isLoading } = useQuery({
    queryKey: ["example"],
    queryFn: () => day1(state, example),
  });

  return (
    <div className="solution">
      <div
        className="dial"
        style={{ transform: `rotate(${3.6 * state.current.value}deg)` }}
      ></div>
      {!isLoading && data && (
        <>
          <div className="part1">{state.current.part1}</div>
          <div className="part2">{state.current.part2}</div>
        </>
      )}
    </div>
  );
}

function App() {
  const queryClient = new QueryClient();
  return (
    <QueryClientProvider client={queryClient}>
      <Day1 />
    </QueryClientProvider>
  );
}

const root = createRoot(document.getElementById("root")!);
root.render(<App />);
