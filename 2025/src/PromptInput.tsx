import { useState, useEffect } from "react";

export function PromptInput({ storageKey }: { storageKey: string }) {
  const [value, setValue] = useState(
    () => localStorage.getItem(storageKey) || "",
  );

  useEffect(() => localStorage.setItem(storageKey, value), [value]);

  return (
    <textarea
      onChange={(event) => {
        setValue(event.target.value);
      }}
      value={value}
      cols={80}
      rows={20}
    />
  );
}
