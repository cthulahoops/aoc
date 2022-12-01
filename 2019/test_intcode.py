import pytest
from intcode import parse_program, eval_program, exec_program

def test_parse():
    assert parse_program(" 1,9,10,70, \n 15,99") == [1,9,10,70,15,99]

def test_exit():
    assert [99] == eval_program("99")

def test_add():
    assert [2 + 99,4,2,0,99] == eval_program("1,4,2,0,99")

def test_mul():
    assert [2 * 99,4,2,0,99] == eval_program("2,4,2,0,99")

def test_mul_immediate():
    assert [1101,100,-1,4,99] == eval_program("1101,100,-1,4,0")

def test_mul_relative():
    assert [4402,2,2201,0,0,0,99] == eval_program("109,2,2201,0,0,0,99")
