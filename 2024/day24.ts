const fs = require('fs');

type Operation = 'AND' | 'OR' | 'XOR';

type Variable = {
    name: string;
    value: number;
};

type Expression = {
    input1: string;
    input2: string;
    operation: Operation;
    output: string;
};

type System = Map<string, Variable | Expression>;

function parseVariable(line: string): Variable {
    const [name, valueStr] = line.split(':').map(s => s.trim());
    const value = parseInt(valueStr);
    
    if (isNaN(value) || (value !== 0 && value !== 1)) {
        throw new Error(`Invalid variable value in line: ${line}`);
    }
    
    return { name, value };
}

function parseExpression(line: string): Expression {
    const [leftSide, rightSide] = line.split('->').map(s => s.trim());
    const parts = leftSide.split(' ');
    
    if (parts.length !== 3) {
        throw new Error(`Invalid expression format in line: ${line}`);
    }

    const input1 = parts[0];
    const operation = parts[1] as Operation;
    const input2 = parts[2];
    const output = rightSide;

    if (!['AND', 'OR', 'XOR'].includes(operation)) {
        throw new Error(`Invalid operation ${operation} in line: ${line}`);
    }

    return { input1, input2, operation, output };
}

function parseFile(filename: string): { variables: Variable[], expressions: Expression[] } {
    const content = fs.readFileSync(filename, 'utf-8');
    const lines = content.split('\n').filter((line: string) => line.trim());
    
    const variables: Variable[] = [];
    const expressions: Expression[] = [];
    let parsingVariables = true;

    for (const line of lines) {
        if (line.includes('->')) {
            parsingVariables = false;
            expressions.push(parseExpression(line));
        } else if (parsingVariables) {
            variables.push(parseVariable(line));
        }
    }

    return { variables, expressions };
}

function isVariable(obj: Variable | Expression): obj is Variable {
    return 'value' in obj;
}

function buildSystem(variables: Variable[], expressions: Expression[]): System {
    const system = new Map<string, Variable | Expression>();

    variables.forEach(v => system.set(v.name, v));
    expressions.forEach(e => system.set(e.output, e));

    return system;
}

function evaluate(variable: string, system: System): number {
    const expr = system.get(variable);

    if (!expr) {
        throw new Error(`Variable ${variable} not found in system`);
    }

    if (isVariable(expr)) {
        return expr.value;
    }

    const input1 = evaluate(expr.input1, system);
    const input2 = evaluate(expr.input2, system);

    switch (expr.operation) {
        case 'AND':
            return input1 & input2;
        case 'OR':
            return input1 | input2;
        case 'XOR':
            return input1 ^ input2;
    }
}

function computeValue(variable: string, system: System): BigInt {
    let bit = BigInt(0);
    let result = BigInt(0);
    while (true) {
        const name = `${variable}${bit.toString().padStart(2, '0')}`;
        if (!system.has(name)) {
            break;
        }
        const value = evaluate(name, system);

        const bitValue = BigInt(value) * (BigInt(1) << bit);

        result |= bitValue;
        bit++;
    }
    return result;
}

// Main execution
if (process.argv.length !== 3) {
    console.error('Usage: ts-node script.ts <input-file>');
    process.exit(1);
}

const { variables, expressions } = parseFile(process.argv[2]);

/*
console.log('Variables:');
variables
.sort((a, b) => a.name.localeCompare(b.name))
.forEach(v => console.log(`  ${v.name}: ${v.value}`));

console.log('\nExpressions:');
expressions.forEach(expr => 
	  console.log(`  ${expr.input1} ${expr.operation} ${expr.input2} -> ${expr.output}`));
*/

const system = buildSystem(variables, expressions);

const result = evaluate('z02', system);

// console.log(`Result: ${result}\n`);
//
console.log(`x = ${computeValue('x', system)}`)
console.log(`y = ${computeValue('y', system)}`)
console.log(`z = ${computeValue('z', system)}`)
