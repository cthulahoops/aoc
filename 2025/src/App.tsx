import { createRoot } from "react-dom/client";
import { InputProvider } from "./InputProvider";

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
  return (
    <>
      <nav>
        <ul>
          <li>
            <a href="/day1.html">Day 1</a>
          </li>
          <li>
            <a href="/day2.html">Day 2</a>
          </li>
          <li>
            <a href="/day3.html">Day 3</a>
          </li>
          <li>
            <a href="/day4.html">Day 4</a>
          </li>
        </ul>
      </nav>
      <InputProvider storageKey={`day${day}/input`} example={example}>
        {children}
      </InputProvider>
    </>
  );
}
