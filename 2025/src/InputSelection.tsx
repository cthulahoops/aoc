import "./app.css";

export type Input = "example" | "input";

export function InputSelection({
  selectedInput,
  setSelectedInput,
}: {
  selectedInput: Input;
  setSelectedInput: (value: Input) => void;
}) {
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const value = event.target.value;

    if (value !== "example" && value !== "input") {
      return;
    }
    setSelectedInput(value);
  };

  return (
    <div id="input-selection" className="box">
      <div>
        <input
          type="radio"
          id="example"
          name="dataset"
          value="example"
          checked={selectedInput === "example"}
          onChange={handleChange}
        />
        <label htmlFor="example">Example Data</label>
      </div>
      <div>
        <input
          type="radio"
          id="input"
          name="dataset"
          value="input"
          checked={selectedInput === "input"}
          onChange={handleChange}
        />
        <label htmlFor="input">Input File</label>
      </div>
    </div>
  );
}
