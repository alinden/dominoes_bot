"""Convert a CppDepthMatch scoreboard and its decision logs into labeler input."""
import argparse
from pathlib import Path
import re
import sys

GAME = re.compile(r"^Pair (\d+) \| deal seed (\d+) \| Bayes seat (\d+)[^\n]*\n(.*?)^Result:", re.M | re.S)
TURN = re.compile(r"^\s*(\d+) (Bayes|Baseline) (Draw|Pass|Move\(\[(\d),(\d)\],(\w+))", re.M)
GAME_HEADER = re.compile(r"^GAME \| seed (\d+) \| C\+\+ seat ([12])\s*$", re.M)


def deals(seed, count):
    # Scala Random.shuffle uses Java's 48-bit RNG and Fisher–Yates.
    state = (seed ^ 25214903917) % (1 << 48)
    for _ in range(count):
        tiles = list(range(28))
        for n in range(28, 1, -1):
            while True:
                state = (state * 25214903917 + 11) % (1 << 48)
                bits = state >> 17
                index = bits % n if n & (n - 1) else (bits * n) >> 31
                if bits - index + n - 1 < 1 << 31:
                    break
            tiles[n - 1], tiles[index] = tiles[index], tiles[n - 1]
        yield tiles


def decision_positions(path, max_hands):
    text = path.read_text()
    headers = list(GAME_HEADER.finditer(text))
    for header, limit in zip(headers, [match.start() for match in headers[1:]] + [len(text)]):
        seed, seat = map(int, header.groups())
        blocks = re.split(r"(?m)^TURN (\d+)\s*$", text[header.end():limit])
        for turn, block in zip(blocks[1::2], blocks[2::2]):
            count = re.search(r"(?m)^(\d+) possible hands", block)
            scores = sorted(map(float, re.findall(r"(?m)^Move .* mean=(-?[\d.]+)", block)), reverse=True)
            if count and len(scores) >= 2 and int(count[1]) <= max_hands:
                yield (path.stem.capitalize(), seed, seat, int(turn)), (int(count[1]), scores[0] - scores[1])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("scoreboard", type=Path)
    parser.add_argument("decision_logs", type=Path, nargs=2)
    parser.add_argument("--max-hands", type=int, default=200)
    args = parser.parse_args()
    positions = dict(item for path in args.decision_logs for item in decision_positions(path, args.max_hands))
    for match in GAME.finditer(args.scoreboard.read_text()):
        pair, seed, bayes, body = match.groups()
        pair, seed, bayes = int(pair), int(seed), int(bayes)
        actions = {}
        for turn, label, action, high, low, direction in TURN.findall(body):
            seat = bayes if label == "Bayes" else 3 - bayes
            hands, gap = positions.get((label, seed, seat, int(turn)), (0, 0))
            if high:
                high, low = sorted((int(high), int(low)), reverse=True)
                verb, tile = direction[0], high * (high + 1) // 2 + low
            else:
                verb, tile = {"Draw": "W", "Pass": "P"}[action], -1
            actions[int(turn)] = (verb, tile, seat, hands, gap)
        deal_count = 32
        print(pair, seed, bayes, max(actions), deal_count)
        for deal in deals(seed, deal_count):
            print(*deal)
        for turn in range(1, max(actions) + 1):
            print(*actions.get(turn, ("H", -1, 0, 0, 0)))


if __name__ == "__main__":
    try:
        main()
    except BrokenPipeError:
        sys.exit(1)
