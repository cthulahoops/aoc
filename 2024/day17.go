package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type OpCode int

const (
	ADV OpCode = iota // 0
	BXL               // 1
	BST               // 2
	JNZ               // 3
	BXC               // 4
	OUT               // 5
	BDV               // 6
	CDV               // 7
)

type Registers struct {
	A int
	B int
	C int
}

func parseRegisters(line string) (int, error) {
	parts := strings.Split(line, ":")
	if len(parts) != 2 {
		return 0, fmt.Errorf("invalid register line format: %s", line)
	}

	valueStr := strings.TrimSpace(parts[1])
	value, err := strconv.Atoi(valueStr)
	if err != nil {
		return 0, fmt.Errorf("invalid register value: %s", valueStr)
	}

	return value, nil
}

func parseProgram(line string) ([]int, error) {
	parts := strings.Split(line, ":")
	if len(parts) != 2 {
		return nil, fmt.Errorf("invalid program line format: %s", line)
	}

	numberStrs := strings.Split(strings.TrimSpace(parts[1]), ",")
	numbers := make([]int, 0, len(numberStrs))

	for _, numStr := range numberStrs {
		num, err := strconv.Atoi(strings.TrimSpace(numStr))
		if err != nil {
			return nil, fmt.Errorf("invalid program number: %s", numStr)
		}
		numbers = append(numbers, num)
	}

	return numbers, nil
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <input_file>\n", os.Args[0])
		os.Exit(1)
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	var registers Registers
	var program []int

	scanner := bufio.NewScanner(file)
	lineNum := 0

	for scanner.Scan() {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" {
			continue
		}

		lineNum++

		switch {
		case strings.HasPrefix(line, "Register A:"):
			registers.A, err = parseRegisters(line)
		case strings.HasPrefix(line, "Register B:"):
			registers.B, err = parseRegisters(line)
		case strings.HasPrefix(line, "Register C:"):
			registers.C, err = parseRegisters(line)
		case strings.HasPrefix(line, "Program:"):
			program, err = parseProgram(line)
		default:
			err = fmt.Errorf("unknown line format at line %d: %s", lineNum, line)
		}

		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing line %d: %v\n", lineNum, err)
			os.Exit(1)
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	// Print parsed values
	fmt.Printf("Registers: A=%d, B=%d, C=%d\n", registers.A, registers.B, registers.C)
	fmt.Printf("Program: %v\n", program)
	outputs := executeProgram(&registers, program)

	var stringOutputs []string
	for _, num := range outputs {
		stringOutputs = append(stringOutputs, strconv.Itoa(num))
	}
	fmt.Println(strings.Join(stringOutputs, ","))

	inputs := findInputsByOutput(program, program)
	println("Smallest self-inputs: ", minimum(inputs))
}

func minimum(nums []int) int {
    if len(nums) == 0 {
        panic("empty slice")
    }
    min := nums[0]
    for _, num := range nums[1:] {
        if num < min {
            min = num
        }
    }
    return min
}

func findInputsByOutput(program []int, output []int) []int {
	if len(output) == 0 {
		return []int{0}
	}
	tail := output[1:]
	tailInputs := findInputsByOutput(program, tail)

	var results []int
	for _, tailInput := range tailInputs {
		for i := 0; i < 8; i++ {
			input := 8*tailInput + i
			//registers := Registers{A: input, B: 0, C: 0}
			if equalSlices(processNumber(input), output) {
				fmt.Printf("found input: %d for output: %v\n", input, output)
				results = append(results, input)
			}
		}
	}

	if len(results) == 0 {
		return []int{} // return empty slice if no matches found
	}
	return results
}

func processNumber(a int) []int {
    var outputs []int
    
    for a != 0 {
        b := a % 8
        b ^= 5
        c := a / (1 << b)
        b ^= 6
        b ^= c
        outputs = append(outputs, b%8)
        a = a / 8
    }
    
    return outputs
}

func equalSlices(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func executeProgram(registers *Registers, program []int) []int {
	var outputs []int
	instPtr := 0

	println(registers.A, registers.B, registers.C)

	for instPtr < len(program) {
		opcode, err := parseOpCode(program[instPtr])
		if err != nil {
			break
		}

		operand := program[instPtr+1]

		/*
			println(registers.A, registers.B, registers.C)
			println(opcode, operand)
		*/

		switch opcode {
		case ADV:
			registers.A = opDivide(registers, operand)
		case BXL:
			registers.B ^= operand

		case BST:
			registers.B = combo(registers, operand) % 8

		case JNZ:
			if registers.A != 0 {
				instPtr = operand
				continue
			}

		case BXC:
			registers.B ^= registers.C

		case OUT:
			outputs = append(outputs, combo(registers, operand)%8)

		case BDV:
			registers.B = opDivide(registers, operand)

		case CDV:
			registers.C = opDivide(registers, operand)

		}
		instPtr += 2
	}

	return outputs
}

func opDivide(registers *Registers, operand int) int {
	power := combo(registers, operand)
	if power == 0 {
		return registers.A
	}
	return registers.A / (2 << (power - 1))
}

func combo(registers *Registers, operand int) int {
	if operand <= 3 {
		return operand
	} else if operand == 4 {
		return registers.A
	} else if operand == 5 {
		return registers.B
	} else if operand == 6 {
		return registers.C
	} else {
		panic("Invalid operand")
	}
}

func parseOpCode(code int) (OpCode, error) {
	if code >= 0 && code <= 7 {
		return OpCode(code), nil
	}
	return 0, fmt.Errorf("invalid opcode: %d", code)
}
