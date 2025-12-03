import example from "./examples/2.txt?raw";
import input from "./inputs/2.txt?raw";

function parse(input: string): [string, string][] {
  return input.split(",").map((range) => range.split("-") as [string, string]);
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
  const [_, pattern] = invalid.exec(String(input));
  return pattern.length;
}

function solve(input: string, element: HTMLElement) {
  element.innerHTML = "";
  const ranges = parse(input);
  let part1 = 0;
  let part2 = 0;
  const solutions = document.createElement("div");
  solutions.classList.add("solutions");
  element.appendChild(solutions);

  for (const [start, end] of ranges) {
    const rangeElement = document.createElement("div");
    element.appendChild(rangeElement);
    const heading = document.createElement("div");
    heading.innerHTML = `${start}-${end}`;
    rangeElement.appendChild(heading);
    const ul = document.createElement("ul");
    rangeElement.appendChild(ul);
    for (let i = Number(start); i <= Number(end); i++) {
      if (!isValid2(i)) {
        console.log("Invalid: ", i);

        const item = document.createElement("li");
        const matchLength = invalidLength(i);
        const pattern = String(i).substring(0, matchLength);
        const rest = String(i).substring(matchLength);
        item.innerHTML = `<span class="pattern">${pattern}</span>${rest}`;
        item.classList.add("invalid");

        if (!isValid1(i)) {
          item.classList.add("invalid-1");
          part1 += i;
        }
        ul.appendChild(item);
        part2 += i;
      }
    }
  }
  solutions.innerHTML = `<div>Part 1: ${part1}</div><div>Part 2: ${part2}</div>`;
  //  element.innerText = solution;
}

const solutionElement = document.getElementById("solution")!;
(
  document.querySelectorAll(
    'input[name="dataset"]',
  ) as NodeListOf<HTMLInputElement>
).forEach((radio: HTMLInputElement) => {
  radio.addEventListener("change", (event: Event) => {
    const target = event.target as HTMLInputElement;
    if (target.checked) {
      if (target.value == "input") {
        solve(input, solutionElement);
      } else if (target.value == "example") {
        solve(example, solutionElement);
      }
    }
  });
});

solve(example, solutionElement);
