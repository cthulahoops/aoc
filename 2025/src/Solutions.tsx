export function Solutions({ part1, part2 }: { part1: number; part2: number }) {
  return (
    <div className="solutions box">
      <div>
        Part 1: <span>{part1}</span>
      </div>
      <div>
        Part 2: <span>{part2}</span>
      </div>
    </div>
  );
}
