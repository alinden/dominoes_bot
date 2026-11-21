# Potential improvements

## Revisit earlier moves when fewer opponent hands are feasible

Late in a round, public evidence may leave only a few dozen feasible opponent hands. Use those candidates to revisit an earlier move that Bayesian inference skipped or timed out on. For each candidate, reconstruct what the opponent could have held when they made that move, then evaluate their observed choice on the board and score from that turn.

- To reverse an opponent play, add the played tile back to their hand.
- To reverse an opponent draw, consider which tiles could have been drawn. A forced draw followed by an immediate play identifies the last tile drawn. Any preceding draws in that sequence must have been unplayable on that board.
- Use later observations to narrow the possible hand histories. When scoring the earlier decision, sample the hidden tiles and draw orders from the information the opponent could have had at the time.
- When several earlier histories lead to the same current hand, average their likelihoods using their conditional probabilities. Apply each observed move's evidence only once.

Start with the most recent skipped or timed-out decision that can be reached without crossing an ambiguous draw. Evaluate each distinct reconstructed hand once and reuse that result across current candidates. Keep the existing requirement that all sampled branches reach round settlement before applying an update. A small candidate pool limits the number of hands to evaluate, though an earlier board can still be expensive to search.

This is an optional experiment. Compare the current Bayes bot with and without look-back inference in seat-swapped matches. Track actual-hand probability, move changes, and inference time alongside wins to see whether the extra search is worthwhile.

## Learn a value function for search cutoffs

Train a small network to evaluate positions where search stops before round settlement. It could learn when exposed pips, mobility, or a path to dominoes outweigh an immediate score deficit. Use the board's exposed ends, both hands within the sampled scenario, match scores, the active seat, and boneyard size as inputs. Exclude the unrevealed draw order. At settlement, use the engine's existing evaluation of the settled position.

Generate positions through self-play and games between the existing bots. Label a selection with deeper search values, using round-end searches where feasible. Start with a small CPU-trained model on the ThinkPad and measure how long it takes to generate the labels. Hold out whole deal seeds and compare the learned evaluator with the current heuristic in seat-swapped matches under equal time budgets. Measure playing strength, search latency, and how often each version reaches settlement.

### Training and inference tools

Use Python and [PyTorch](https://docs.pytorch.org/tutorials/beginner/basics/quickstart_tutorial.html) to train on the ThinkPad's CPU. Start with one or two small fully connected layers with ReLU activations and a single value output. Have the C++ engine generate and label positions, then fit the model in Python and export its weights and biases.

Implement the forward pass in C++ so the evaluator can run inside the native search and compile into the same WebAssembly module as the engine. Verify that the Python and C++ outputs agree on saved positions, then profile the cost per leaf evaluation and measure playing strength at equal time budgets. Consider [ONNX Runtime](https://onnxruntime.ai/docs/get-started/with-cpp.html) if the model grows or we need to swap architectures without changing the C++ forward pass.

## Use learned move ordering with alpha-beta

The search already uses alpha-beta once a sampled branch reaches an empty boneyard. Test a cheap move ordering heuristic there, then compare it with ranking moves using the learned evaluator. Search the most promising moves first and use alpha-beta bounds to decide which siblings can be skipped. A predicted value can guide the order. Pruning across uncertain draws requires valid bounds on the weighted average of the draw outcomes.

Compare search scores at fixed depth when changing only the move order, and check whether the new order settles more positions within the same time budget. Include evaluation time in the comparison, since extra network calls can outweigh the savings from pruning.
