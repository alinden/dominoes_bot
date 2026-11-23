# Potential improvements

## Revisit earlier moves when fewer opponent hands are feasible

Late in a round, public evidence may leave only a few dozen feasible opponent hands. Use those candidates to revisit an earlier move that Bayesian inference skipped or timed out on. For each candidate, reconstruct what the opponent could have held when they made that move, then evaluate their observed choice on the board and score from that turn.

- To reverse an opponent play, add the played tile back to their hand.
- To reverse an opponent draw, consider which tiles could have been drawn. A forced draw followed by an immediate play identifies the last tile drawn. Any preceding draws in that sequence must have been unplayable on that board.
- Use later observations to narrow the possible hand histories. When scoring the earlier decision, sample the hidden tiles and draw orders from the information the opponent could have had at the time.
- When several earlier histories lead to the same current hand, average their likelihoods using their conditional probabilities. Apply each observed move's evidence only once.

Start with the most recent skipped or timed-out decision that can be reached without crossing an ambiguous draw. Evaluate each distinct reconstructed hand once and reuse that result across current candidates. Use the browser's shallow neural inference and confirmation pass as a starting point. A small candidate pool limits the number of hands to evaluate, though an earlier board can still be expensive to search.

This is an optional experiment. Compare the current Bayes bot with and without look-back inference in seat-swapped matches. Track actual-hand probability, move changes, and inference time alongside wins to see whether the extra search is worthwhile.

## Improve opening labels and retrain

The current opening specialist was trained on a mix of nine-tile cutoff
estimates and exact four- and five-tile labels. Add more exact labels at
those two board sizes, then retrain and compare architectures against the
same held-out deal seeds. Measure ranking and regret on exact labels and
benchmark three-tile searches under a fixed time cap. Track boards settled
per hour, including timeouts, to see whether better ordering offsets the
cost of a larger model. See the [training guide](training/README.md).

## Measure shallow Bayesian inference

Replay logged decisions with known opponent hands. Record when inference
first updates, how often it times out, and how often it downweights the
actual hand. Compare against hard constraints alone and the existing
full-settlement inference in paired matches.
