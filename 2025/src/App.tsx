import { createRoot } from "react-dom/client";
import { InputProvider } from "./InputProvider";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { DayLinks } from "./DayLinks";

export function renderApp(
  day: number,
  example: string,
  solution: React.ReactNode,
) {
  const root = createRoot(document.getElementById("root")!);
  root.render(<App day={day} children={solution} example={example} />);
}

function App({
  day,
  example,
  children,
}: {
  day: number;
  example: string;
  children: React.ReactNode;
}) {
  const queryClient = new QueryClient();
  return (
    <QueryClientProvider client={queryClient}>
      <nav>
        <DayLinks />
      </nav>
      <InputProvider storageKey={`day${day}/input`} example={example}>
        {children}
      </InputProvider>
      <a
        href={`https://github.com/cthulahoops/aoc/blob/main/2025/src/day${day}.tsx`}
        target="_blank"
      >
        Github
      </a>
    </QueryClientProvider>
  );
}
