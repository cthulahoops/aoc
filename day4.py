import aoc

def is_winner(board, calls):
    return (
        any(all(x in calls for x in line) for line in board)
        or
        any(all(line[i] in calls for line in board) for i in range(5))
        # DIAGONALS DO NOT COUNT!
        # or
        # all(board[i][i] in calls for i in range(5))
        # or
        # all(board[4-i][i] in calls for i in range(5))
    )

def uncalled_numbers(board, calls):
    return sum(x for line in board for x in line if x not in calls)

def parse_board(board):
    return [[int(x) for x in line.split()] for line in board.split('\n') if line]

def print_board(board, calls):
    green = '\033[92m'
    red = '\033[91m'
    for line in board:
        formatted = []
        for x in line:
            if x == calls[-1]:
                color = red
            elif x in calls:
                color = green
            else:
                color = ''
            formatted.append(f"{color}{x:2}\033[0m")
        print(' '.join(formatted))

def simulate_game(board, calls):
    called = set()
    for call in calls:
        called.add(call)

        if is_winner(board, called):
            uncalled = uncalled_numbers(board, called)
            return len(called), uncalled, call, uncalled * call, board

def main():
    calls, *boards = aoc.blocks(4)
    calls = [int(x) for x in calls.split(',')]
    boards = [parse_board(board) for board in boards]

    results = [simulate_game(board, calls) for board in boards]
    results.sort()

    for turns, uncalled, call, score, board in results:
        print(f"After {turns} turns:")
        print_board(board, calls[:turns])
        print(f"Final score: {uncalled} * {call} = {score}\n")

    print("Part 1: ", results[0])
    print("Part 2: ", results[-1])


if __name__ == '__main__':
    main()
