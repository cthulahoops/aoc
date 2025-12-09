interface GridCellProps extends React.HTMLAttributes<HTMLDivElement> {
  position: { x: number; y: number };
  children: React.ReactNode;
}

export function GridCell({ position, children, ...rest }: GridCellProps) {
  return (
    <div
      style={{ gridColumn: position.x + 1, gridRow: position.y + 1 }}
      {...rest}
    >
      {children}
    </div>
  );
}
