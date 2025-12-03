import { useState, useMemo } from "react";
import { InputContext } from "./contexts";
import { InputSelection, type Input } from "./InputSelection";
import { PromptInput } from "./PromptInput";

export function InputProvider({
  example,
  storageKey,
  children,
}: {
  example: string;
  storageKey: string;
  children: React.ReactNode;
}) {
  const [selectedInput, setSelectedInput] = useState<Input>("example");

  const input = useMemo(() => {
    if (selectedInput === "example") {
      return example;
    } else {
      return localStorage.getItem(storageKey);
    }
  }, [selectedInput]);
  return (
    <>
      <InputSelection
        selectedInput={selectedInput}
        setSelectedInput={setSelectedInput}
      />

      {input && (
        <InputContext.Provider value={input}>{children}</InputContext.Provider>
      )}
      <PromptInput storageKey={storageKey} />
    </>
  );
}
