# Potential improvements

## Revisit earlier moves when fewer opponent hands are feasible

Late in a round, public evidence may leave only a few dozen feasible opponent hands. Use those candidates to revisit an earlier move that Bayesian inference skipped or timed out on. For each candidate, reconstruct what the opponent could have held when they made that move, then evaluate their observed choice on the board and score from that turn.

- To reverse an opponent play, add the played tile back to their hand.
- To reverse an opponent draw, consider which tiles could have been drawn. A forced draw followed by an immediate play identifies the last tile drawn. Any preceding draws in that sequence must have been unplayable on that board.
- Use later observations to narrow the possible hand histories. When scoring the earlier decision, sample the hidden tiles and draw orders from the information the opponent could have had at the time.
- When several earlier histories lead to the same current hand, average their likelihoods using their conditional probabilities. Apply each observed move's evidence only once.

Start with the most recent skipped or timed-out decision that can be reached without crossing an ambiguous draw. Evaluate each distinct reconstructed hand once and reuse that result across current candidates. Keep the existing requirement that all sampled branches reach round settlement before applying an update. A small candidate pool limits the number of hands to evaluate, though an earlier board can still be expensive to search.

This is an optional experiment. Compare the current Bayes bot with and without look-back inference in seat-swapped matches. Track actual-hand probability, move changes, and inference time alongside wins to see whether the extra search is worthwhile.
