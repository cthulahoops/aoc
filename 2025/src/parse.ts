export function parseLines(input: string) {
  return input.split("\n").filter((x: string) => !!x);
}

export function digits(string: string) {
  return [...string].map((char: string) => Number(char));
}
