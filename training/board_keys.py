"""Print the distinct board layouts in a settlement-label file."""
import argparse
import json
from pathlib import Path


def board_key(board):
    spinner = board["spinner"] if board["spinner"] >= 0 else 28
    chains = ["".join(chr(65 + 7 * outer + inner) for outer, inner in chain)
              for chain in board["chains"]]
    return chr(65 + spinner) + "r".join(chains) + "r"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("labels", type=Path)
    args = parser.parse_args()
    with args.labels.open() as source:
        keys = {board_key(json.loads(line)["before"]) for line in source}
    for key in sorted(keys):
        print(key)


if __name__ == "__main__":
    main()
