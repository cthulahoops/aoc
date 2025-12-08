export function sum(numbers: number[]) {
  return numbers.reduce((a, b) => a + b, 0);
}

export function product(numbers: number[]) {
  return numbers.reduce((a, b) => a * b, 1);
}
