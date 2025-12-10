declare module "javascript-lp-solver" {
  interface Constraint {
    max?: number;
    min?: number;
    equal?: number;
  }

  interface Variable {
    [attribute: string]: number;
  }

  interface Model {
    optimize: string | { [key: string]: "max" | "min" };
    opType: "max" | "min";
    constraints: { [key: string]: Constraint };
    variables: { [key: string]: Variable };
    ints?: { [key: string]: number };
    options?: {
      timeout?: number;
      tolerance?: number;
    };
  }

  interface Solution {
    feasible: boolean;
    result: number;
    [variableName: string]: number | boolean;
  }

  export function Solve(model: Model): Solution;
  export { Solve as solve };
}
