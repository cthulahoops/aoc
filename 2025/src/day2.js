import { jsx as _jsx } from "react/jsx-runtime";
/// <reference types="./vite-env.d.ts" />
import { createRoot } from "react-dom/client";
import example from "./examples/2.txt?raw";
function parse(input) {
  return input.split(",").map((range) => range.split("-"));
}
function isValid1(input) {
  const invalid = /^([0-9]*)\1$/;
  return !String(input).match(invalid);
}
function isValid2(input) {
  const invalid = /^([0-9]*)\1+$/;
  return !String(input).match(invalid);
}
function invalidLength(input) {
  const invalid = /^([0-9]*)\1+$/;
  const match = invalid.exec(String(input));
  return match ? match[1].length : 0;
}
function solve(input, element) {
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
const solutionElement = document.getElementById("solution");
document.querySelectorAll('input[name="dataset"]').forEach((radio) => {
  radio.addEventListener("change", (event) => {
    const target = event.target;
    if (target.checked) {
      if (target.value == "input") {
        const input = localStorage.getItem("day2/input");
        if (input) {
          solve(input, solutionElement);
        }
      } else if (target.value == "example") {
        solve(example, solutionElement);
      }
    }
  });
});
const input = localStorage.getItem("day2/input");
if (!input) {
  const textArea = document.createElement("textarea");
  textArea.setAttribute("width", "120");
  textArea.setAttribute("height", "50");
  textArea.addEventListener("change", (event) => {
    const target = event.target;
    localStorage.setItem("day2/input", target.value);
  });
  document.body.appendChild(textArea);
}
function App() {
  return _jsx("div", { children: "Hello, World" });
}
const root = createRoot(document.getElementById("root"));
root.render(_jsx(App, {}));
solve(example, solutionElement);
