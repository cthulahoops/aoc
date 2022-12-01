import aoc

class ALU:
    def __init__(self, input_stream):
        self.vars = dict(x = 0, y = 0, z = 0, w = 0)
        self.input_stream = list(input_stream)

    def run(self, program):
        for instruction in program:
            self.execute(instruction)

    def execute(self, instruction):
        (ins, *args) = instruction.split()

        match ins:
            case 'inp':
                self.vars[args[0]] = self.read()
            case 'add':
                self.vars[args[0]] += self.value(args[1])
            case 'mul':
                self.vars[args[0]] *= self.value(args[1])
            case 'div':
                self.vars[args[0]] = int(self.vars[args[0]] / self.value(args[1]))
            case 'mod':
                self.vars[args[0]] = self.vars[args[0]] % self.value(args[1])
            case 'eql':
                self.vars[args[0]] = 1 if self.vars[args[0]] == self.value(args[1]) else 0

    def read(self):
        return self.input_stream.pop(0)

    def value(self, arg):
        if arg in 'xyzw':
            return self.vars[arg]
        return int(arg)

    @property
    def x(self):
        return self.vars['x']

    @property
    def y(self):
        return self.vars['y']

    @property
    def z(self):
        return self.vars['z']

    @property
    def w(self):
        return self.vars['w']


def split_blocks(program):
    blocks = []
    current = None
    for line in program:
        if line == 'inp w':
            current = []
            blocks.append(current)
        current.append(line)
    return blocks

def format_base26(x):
    digits = []
    while x:
        digits.append(x % 26)
        x //= 26
    return list(reversed(digits))

def transpile(program):
    for line in program:
        cmd, *args = line.split()
        match cmd:
            case 'inp':
                print(f"\n{args[0]} = read()")
            case 'add':
                print(f"{args[0]} += {args[1]}")
            case 'mul':
                print(f"{args[0]} *= {args[1]}")
            case 'div':
                print(f"{args[0]} = int({args[0]} / {args[1]})")
            case 'mod':
                print(f"{args[0]} = {args[0]} % {args[1]}")
            case 'eql':
                print(f"{args[0]} = 1 if {args[0]} == {args[1]} else 0")

def get_op(block):
    if block[4] == "div z 26":
        return "pop"
    else:
        return "push"

def main():
    program = aoc.lines(24)

    stack = []
    blocks = split_blocks(program)
    assert program == sum(blocks, [])

    ops = [get_op(block) for block in blocks]

    pairs = []
    for i, op in enumerate(ops):
        if op == 'push':
            stack.append(i)
        elif op == 'pop':
            pair = stack.pop()
            pairs.append((pair, i))

    print(pairs)

    monad = [0] * 14
    retro = [0] * 14
    for push, pop in pairs:
        print(push, pop)
        program = blocks[push] + blocks[pop]
        nums = []
        for a in range(1, 10):
            for b in range(1, 10):
                alu = ALU([a, b])
                alu.run(program)
                if alu.z == 0:
                    nums.append((a, b))
        print(nums)
        best_a, best_b = max(nums)
        monad[push] = best_a
        monad[pop] = best_b

        best_a, best_b = min(nums)
        retro[push] = best_a
        retro[pop] = best_b

    alu = ALU(monad)
    for b in blocks:
        alu.run(b)
    assert alu.z == 0
    print("Part 1: ", ''.join(str(x) for x in monad))

    alu = ALU(retro)
    for b in blocks:
        alu.run(b)
    assert alu.z == 0
    print("Part 2: ", ''.join(str(x) for x in retro))

if __name__ == '__main__':
    main()

def test_simple():
    program = ["inp x", "mul x -1"]
    alu = ALU([8])
    alu.run(program)
    assert alu.x == -8

def test_triple():
    program = ["inp z", "inp x", "mul z 3", "eql z x"]
    alu = ALU([8, 24])
    alu.run(program)
    assert alu.z == 1
    alu = ALU([8, 23])
    alu.run(program)
    assert alu.z == 0

def test_bin():
    program = """inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"""
    alu = ALU([10])
    alu.run(program.splitlines())
    assert alu.w == 1
    assert alu.x == 0
    assert alu.y == 1
    assert alu.z == 0

def test_base26():
    assert format_base26(5) == [5]
    assert format_base26(31) == [1, 5]
