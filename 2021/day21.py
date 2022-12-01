from collections import namedtuple, Counter
from functools import lru_cache
import pytest
import aoc

Player = namedtuple('Player', ('score', 'position'))

def main():
    (player1, player2) = aoc.lines(21)

    player1 = parse_starting_position(player1)
    player2 = parse_starting_position(player2)

    part1 = deterministic_game(player1, player2)
    print("Part 1:", part1)

    (p1, p2) = dirac_game(Player(position=player1, score=0), Player(position=player2, score=0))
    print("Part 2", max(p1, p2))

@lru_cache(maxsize=None)
def dirac_game(player1, player2):
    if player2.score >= 21:
        return (0, 1)

    player1_wins, player2_wins = 0, 0
    for outcome, universes in roll_dirac().items():
        new_position = mod10(player1.position + outcome)
        new_score = player1.score + new_position
        p2, p1 = dirac_game(player2, Player(score=new_score, position=new_position))
        player1_wins += p1 * universes
        player2_wins += p2 * universes

    return player1_wins, player2_wins


def deterministic_game(player1, player2):
    die = DeterministicDie()

    player1_score = 0
    player2_score = 0

    while True:
        player1 = mod10(player1 + die.times(3))
        player1_score += player1

        if player1_score >= 1000:
            break

        player1, player2 = player2, player1
        player1_score, player2_score = player2_score, player1_score

    return player2_score * die.rolls

def roll_dirac():
    faces = [1, 2, 3]
    return Counter([a + b + c for a in faces for b in faces for c in faces])

class DeterministicDie:
    def __init__(self):
        self.rolls = 0
        self.next = 1

    def __next__(self):
        self.rolls += 1
        result = self.next
        self.next += 1
        if self.next > 100:
            self.next = 1
        return result

    def times(self, n):
        result = 0
        for _ in range(n):
            result += next(self)
        return result

def parse_starting_position(line):
    _, position = line.split(': ')
    return int(position)

def mod10(x):
    return ((x - 1) % 10) + 1

if __name__ == '__main__':
    main()

def test_die():
    die = DeterministicDie()
    assert next(die) == 1
    assert next(die) == 2
    assert next(die) == 3

    assert die.rolls == 3

    for _ in range(100):
        next(die)
    assert next(die) == 4
    assert die.rolls == 104

    assert die.times(3) == 5 + 6 + 7

def test_mod10():
    assert mod10(5) == 5
    assert mod10(15) == 5
