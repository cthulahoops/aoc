import example from "./examples/11.txt?raw";
import { useContext } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { sum } from "./lib";

type Node = string;
type Graph = Map<Node, Node[]>;

async function solve(input: string) {
  const inputParts = input.split("---\n");

  const graph1 = parseInput(inputParts[0]);
  const graph2 = parseInput(inputParts[1] || inputParts[0]);

  const countPaths = memoize(
    (start: Node, target: Node, graph: Graph) =>
      `${start},${target},${[...graph.keys()]}`,
    (start: Node, target: Node, graph: Graph): number => {
      if (start === target) {
        return 1;
      }

      return sum(
        graph.get(start)!.map((next) => countPaths(next, target, graph)),
      );
    },
  );

  const countPathsVia = memoize(
    (start: Node, target: Node, via: Node[], graph: Graph) =>
      `${start},${target},${via},${[...graph.keys()]}`,
    (start: Node, target: Node, via: Node[], graph: Graph): number => {
      if (start === target) {
        if (via.length === 0) {
          return 1;
        }
        return 0;
      }

      const newVia = via.filter((x) => x !== start);

      return sum(
        graph
          .get(start)!
          .map((next) => countPathsVia(next, target, newVia, graph)),
      );
    },
  );

  return {
    part1: countPaths("you", "out", graph1),
    part2: countPathsVia("svr", "out", ["fft", "dac"], graph2),
    output: graph2.get("svr"),
  };
}

function memoize<Args extends any[], Result>(
  keyFn: (...args: Args) => string,
  fn: (...args: Args) => Result,
) {
  const cache = new Map<string, Result>();
  const memoizedFunction = (...args: Args): Result => {
    const key = keyFn(...args);
    const cached = cache.get(key);
    if (cached !== undefined) {
      return cached;
    }
    const result = fn(...args);
    cache.set(key, result);
    return result;
  };
  return memoizedFunction;
}

function parseInput(input: string) {
  const lines = parseLines(input);
  return new Map(
    lines.map((line) => {
      const [from, to] = line.split(": ");
      return [from, to.split(" ")];
    }),
  );
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day11", input],
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

renderApp(11, example, <Solution />);
