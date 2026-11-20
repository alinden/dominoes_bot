# C++ game engine rewrite plan

This is stage 1 of [the C++ rewrite](rewrite_plan.md). Work on `feature/cpp_rewrite` with the C++ project under `cpp/`. Use Scala commit `a5f8cbed46002bf0d16ae7f3ab5771d758637899` as the behavioral reference. If the reference changes, update the SHA and review any changes to expected fixture results together.

The deliverable is a headless C++ engine that can play a complete scripted match, including redeals and round scoring. Its API will also serve the terminal and search stages. Terminal commands and rendering, AI policies, and Bayesian inference belong to those later stages.

## 1. Set up the build and tile model

- Create a C++20 library and a CTest test target with CMake under `cpp/`. Keep build output outside the source tree, for example in `cpp/build/debug/` with an appropriate ignore entry. Start with the standard library and compiler warnings. Use value types to make copying a position and inspecting it in GDB straightforward.
- Read `domino.scala`, `played_domino.scala`, and their specs. Represent tile identity as canonical high and low pips. Keep the exposed pip on the placed tile so it can be reversed without changing its identity. Enumerate the double-six set in Scala order. Preserve the mapping `high * (high + 1) / 2 + low` from `hand_mask.scala` so stage 4 can use the same IDs.
- Represent players by seat, with ordered hands and integer match scores. Keep the order of the deal and prepend newly drawn tiles, matching Scala and preserving terminal hand indices later. Use an ordered boneyard with an explicit next tile. Check that a valid live round accounts for each of the 28 tiles exactly once.

## 2. Implement board transitions and scoring

Use `board.scala`, `played_domino.scala`, `Game.updateBoard`, and `BoardSpec.scala` as the primary references. Model the board as an optional spinner and oriented chains. Derive board shape and exposed ends from those fields.

| Board shape | Placement and scoring to reproduce |
| --- | --- |
| Empty | Any direction requests the same opening placement. A double becomes the spinner. |
| Snake | Left and right extend opposite ends of the same chain. The first double attaches as the spinner and reorients the chain if needed. Both free ends count toward the board total. |
| Spinner only | Every compatible direction starts the left chain. The spinner contributes both pips until two arms are occupied. |
| Tee | Only left and right are available. The exposed left end and spinner contribute to the total. |
| Cross | Left, right, up, and down are available. Sum the occupied chain ends. Empty arms contribute zero. |

Keep the raw board total separate from awarded match points. An exposed non-spinner double counts both pips for scoring and matches on its single pip value. Award the board total to the player who placed the tile when the total is a multiple of five. Use `Game` and `Player` for match scoring. Scala's `Score` and `ScoreSpec` cover AI evaluation and belong to the search stage.

## 3. Implement legal actions and round transitions

- Provide operations to list legal actions and apply one action to a state. An invalid action returns a failure with the original state intact. Keep the full set of accepted directional moves available separately from the canonical list used by search. Preserve Scala's left, right, up, down ordering and ordered-hand traversal. Check equivalent empty arms, equal snake ends, and cross endpoints where an exposed nonzero double changes scoring and prevents canonicalization with a matching single-pip end.
- Start from an explicit ordered deal of seven tiles per player and fourteen in the boneyard. The first round and the round after a lock use the highest double in either hand. Place it automatically, apply ordinary Fives scoring, and give the next turn to the other seat. If neither hand contains a double, request another deal and preserve accumulated scores. After a player goes out, give that seat the opening choice in the next round.
- Draw only when there are no legal plays and the boneyard has tiles. Draw exactly one tile and keep the same active seat. Pass only when there are no legal plays and the boneyard is empty. Pass advances the turn. A play or draw clears the previous-pass flag. Two consecutive passes lock the round.
- On going out, award the opponent's remaining pips rounded to the nearest five. On a lock, award the lower-pip hand the rounded difference, following Scala's `Game.endGame`.
- Check for a match winner only after settling the round, including any round-end bonus. The winner must have at least 150 points and more points than the opponent, matching Scala's `Game.matchIsOver`. Start a new round if the scores are tied, even at or above 150. Continue the current round if a player reaches or exceeds 150 during play.
- Make round settlement and the next deal observable as distinct steps. The match runner can advance between them automatically. This gives fixtures access to the settled scores and lets later search evaluate a round boundary without consuming a random deal. Use explicit lifecycle states for awaiting the opening double, active play, awaiting a new deal, and match complete.

## 4. Define the engine boundary for later stages

Keep terminal I/O and shuffling at the caller boundary. Tests should inject ordered deals, including multiple successive deals for the no-double and round-restart cases. Runtime can supply shuffled deals through the same entry point. The two implementations can replay the same tile sequence without using the same random generator.

Expose enough structured transition data for terminals, replays, and later inference. Use the events in `game_log.scala` as a reference. Record the acting seat, played tile and direction, passes, and forced opening doubles. For draws, retain the public evidence available in Scala, including the pre-draw hand size and the tiles that could fit the board before the draw. Keep the actual drawn tile private to the drawing seat. The terminal and inference can consume player-specific views, while headless fixtures can inspect the full engine state. Keep accumulated event history outside the state copied by search.

## 5. Verify behavior and practice GDB

1. Run `sbt test` at the pinned reference and record the result. Translate engine cases from `DominoSpec`, `PlayedDominoSpec`, `BoardSpec`, and `GameSpec`. Leave the input-parser cases for the terminal stage.
2. Start with small table-driven fixtures for placement, orientation, raw board totals, and legal action order. Add round cases for repeated draws, rejected actions, pass reset after a play, forced opening and redeal, a last-tile bonus, and a locked-round bonus. For match completion, check that 150–145 wins after settlement, 150–150 and 155–155 start another round, and reaching or exceeding 150 during play leaves the round in progress. Include a round-end bonus that produces a winning score of 155–150. Include both seats as round winners or starters. Check the bonus after rounding and confirm that rejected actions leave hands, scores, board, and turns intact.
3. Capture a small set of Scala replay traces from explicit deals using the pinned source. Compare after every action and at round boundaries. Include oriented chains, ordered hands and boneyard in the test trace, complete and canonical move lists, seat to act, raw board total, awarded points, and round or match result. Use plain structured fixture data and keep the Scala fixture generator or regeneration instructions alongside it. Exercise a full match, including a redeal or round transition, through the same public engine entry points the terminal will call.
4. Configure a debug build with `cmake -S cpp -B cpp/build/debug -DCMAKE_BUILD_TYPE=Debug`, build with `cmake --build cpp/build/debug`, and run `ctest --test-dir cpp/build/debug --output-on-failure`. Run the test binary under GDB and break on first-spinner placement. Step through the left-end attachment, inspect the chain before and after reversal, and use `bt` to trace the call from the fixture. Repeat with a breakpoint at round settlement and watch which seat gets the bonus. Verify an optimized build as well before starting the terminal stage.

Make focused commits for the build and tile model, board transitions, match lifecycle, and Scala parity fixtures. Stage 1 is complete when the debug and optimized builds pass, scripted matches agree with Scala at the recorded reference, and the GDB exercises have been run on the engine tests.

### Run and regenerate the Scala replays

From the repository root, run `ctest --test-dir cpp/build/debug -R scala_replay_tests --output-on-failure` after building. The C++ test reads `cpp/tests/scala_replay.txt` and reports the fixture line and both values on a mismatch.

Regenerate the trace with `sbt 'Test/runMain game.EngineReplayFixture cpp/tests/scala_replay.txt'`. The test-side generator records each deal and action, followed by the expected event and position. `S` lines include the phase, seat, winner, scores, board total, oriented chains, ordered hands and boneyard, and full and canonical action lists. `E` lines include the acting seat, placement, play points, round bonus, and public draw evidence.

The generator supplies the next ordered deal after a no-double opening. At a locked round boundary the comparison omits remaining hands because Scala clears them during settlement and C++ retains them until the next deal. The trace still checks both hands immediately before the lock and checks the bonus and final scores after it.
