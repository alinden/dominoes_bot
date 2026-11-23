"""Export a CPU value model as input-major dense layers for the C++ labeler."""
import argparse

import torch


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("checkpoint")
    args = parser.parse_args()
    state = torch.load(args.checkpoint, map_location="cpu", weights_only=True)
    keys = [key.removesuffix(".weight") for key in state if key.endswith(".weight")]
    print(len(keys))
    for key in keys:
        weight, bias = state[f"{key}.weight"], state[f"{key}.bias"]
        print(weight.shape[1], weight.shape[0])
        print(*bias.tolist())
        print(*weight.T.flatten().tolist())


if __name__ == "__main__":
    main()
