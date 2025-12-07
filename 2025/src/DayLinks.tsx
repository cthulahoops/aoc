export function DayLinks() {
  return (
    <ul>
      {sequence(7).map((day) => (
        <li>
          <a href={`/day${day}.html`}>Day {day}</a>
        </li>
      ))}
    </ul>
  );
}

function sequence(max: number): number[] {
  const result: number[] = [];
  for (let i = 1; i <= max; i++) {
    result.push(i);
  }
  return result;
}
