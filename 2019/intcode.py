import pytest
import re

POSITION_MODE = 0
IMMEDIATE_MODE = 1
RELATIVE_MODE = 2

def parse_program(program):
    return list(map(int, re.split(r'\s*,\s*', program)))

class VirtualMachine(object):
    def __init__(self, program):
        self.program = program
        self.ptr = 0
        self.relative_base = 0

    def get_param(self, n):
        instruction = self.program[self.ptr]
        mode = (instruction // 10 ** (n + 2)) % 10
        param = self.program[self.ptr + n + 1]
        if mode == IMMEDIATE_MODE:
            return param
        elif mode == POSITION_MODE:
            return self.program[param]
        elif mode == RELATIVE_MODE:
            return self.program[param + self.relative_base]
        else:
            raise ValueError("Invalid parameter mode.")

    def get_params(self, n):
        return tuple([self.get_param(i) for i in range(n)])

    def put_param(self, n, value):
        self.program[self.program[self.ptr + n + 1]] = value

    def advance(self, n):
        self.ptr += n + 1

    def run(self):
        while True:
            opcode = self.program[self.ptr] % 100
            if opcode == 1:
                in1, in2 = self.get_params(2)
                self.put_param(2, in1 + in2) 
                self.advance(3)
            elif opcode == 2:
                in1, in2 = self.get_params(2)
                self.put_param(2, in1 * in2) 
                self.advance(3)
            elif opcode == 3:
                param = self.program[self.ptr+1]
                self.put_param(0, int(input()))
                self.advance(1)
            elif opcode == 4:
                param = self.get_param(0)
                print(self.program[param])
                self.advance(1)
            elif opcode == 9:
                self.relative_base += self.get_param(0)
                self.advance(1)
            elif opcode == 99:
                break
            else:
                raise ValueError("Unexpected instruction")

def eval_program(program):
    program = parse_program(program)
    vm = VirtualMachine(program)
    vm.run()
    return program

def exec_program(program):
    eval_program(program)

if __name__ == '__main__':
    exec_program("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
