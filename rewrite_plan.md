# C++ rewrite plan

Port the Scala dominoes bot in small, playable steps. Use the project to refresh C++ and learn GDB, then measure whether the port can search faster and whether that extra search improves play. Keep the Scala bot runnable as a reference throughout.

## Repository and Git workflow

Start in this repository on a `rewrite/cpp` branch and put the C++ project under `cpp/`, alongside the Scala project. This gives both humans and coding agents one checkout containing the rules, tests, and decision logs. It also makes it easy to run both versions against the same fixtures. Keep changes to Scala limited to reference tooling or independently reviewed fixes. Revisit repository layout once the C++ bot can stand on its own.

- At kickoff, record the Scala reference commit SHA in the detailed engine plan. Use `git show <scala-sha>:src/main/scala/game.scala` to inspect the reference if Scala changes during the port. Record the SHA with benchmark results as well.
- Create the branch with `git switch -c rewrite/cpp` from the intended baseline. If you want your current Scala checkout available for other work, use `git worktree add -b rewrite/cpp ../dominoes_bot-cpp HEAD` instead. Both versions will still be present in the rewrite worktree.
- Make focused commits for each stage and keep fixture changes, behavior changes, and performance changes easy to review independently. Bring Scala fixes into the rewrite branch as needed and explicitly update the reference SHA when a fix changes expected behavior.
- If the C++ version eventually moves to its own repository, clone the Scala repository beside it and pin that checkout to the recorded SHA. Example layout: `~/code/dominoes_bot_cpp` and `~/code/dominoes_bot_scala`. Open both directories in the same agent workspace where supported, or grant the agent read access to the Scala checkout and give it the absolute path and SHA. For a remote agent or CI job, configure an explicit second checkout at that SHA with read access to the Scala repository. A Git remote or URL alone does not put the reference files in the agent's workspace. Keep the shared fixtures and the instructions for regenerating them available in the C++ repository.

## 1. Port the game engine

Follow the [game engine rewrite plan](game_engine_rewrite_plan.md). Cover tile identity and orientation, the spinner and chain ends, legal actions and move equivalence, drawing and passing, Fives scoring, and round and match transitions. A match ends after round scoring when a player has at least 150 points and leads the opponent. In Scala, `Game` also handles terminal input and output. Give the C++ rules a headless API so fixtures, the terminal, and the AI can all drive the same engine.

Use C++20 and a small CMake project with CTest as a starting point. Begin with value types and explicit state transitions. Make deals and draws controllable in tests through a scripted boneyard. Use existing Scala tests and a few replay fixtures to compare legal actions, resulting board state, scores, and turn progression. Scripted tile order lets both languages replay exactly the same game. Capture any README and Scala discrepancies in the engine plan before choosing expected behavior.

Use a debug build with symbols for the first GDB exercise. Break in a move or scoring function, step through a spinner fixture, inspect the board and hands, and get a backtrace if a check fails. The stage is complete when representative rounds can run headlessly and the rules fixtures match the Scala reference.

## 2. Build the terminal game

Use `app.scala` and the input and display code in `game.scala` as references. Render the board, spinner, scores, and active player. Preserve hidden opponent tiles and boneyard contents in normal play. Support the existing help, move, draw, and pass commands, including input errors and round transitions. Add a simple opponent based on the Scala greedy policy so a human can finish a match before the search is ready. Make the same policy available to the headless runner for later comparisons.

## 3. Port the AI search

Use `robot.scala`, `heuristic.scala`, and the max, min, and average nodes to port the expectiminimax approximation. Start with prescribed hands and draws, then put opponent-hand sampling behind an interface and supply a simple sampler over feasible hands. Use fixed depth and simulation counts to compare per-move scores, tie-breaking, and search statistics on small positions. Compare sampled decisions with controlled samples or explicit tolerances. Once the search agrees on those cases, wire in the progressive search budgets and decision logging. In GDB, step through one shallow tree and inspect how a draw outcome contributes to its parent score.

## 4. Port opponent-hand inference

Follow `Perspective`, `EliminationHand`, `PossibleHand`, and `HandMask`. Preserve the distribution over complete hands, using the existing 28-bit tile mapping as the reference. First port candidate generation, evidence from public actions and known tiles, normalization, and weighted sampling. Then add Bayesian reweighting based on how likely the opponent was to choose an observed move, reusing the search from stage 3 for the likelihood estimates. Check small distributions against Scala for opening doubles, plays, draws, and passes. Compare feasible hand masks, probability mass, tile marginals, and sampling behavior. Connect the model to the match loop and check that the bot receives only its own tiles and public observations.

## 5. Measure speed and playing strength

Capture a Scala baseline before tuning the C++ implementation. Use matched positions, rules, heuristics, and search budgets. Benchmark optimized C++ builds and give the JVM a warm-up before recording steady-state results. Report decision latency, search work per second, and peak memory. Measure inference updates separately, since large candidate-hand sets can dominate a turn. Use GDB for correctness and a profiler to identify where the optimized build spends its time.

Run paired matches from the same deals with seats swapped. Compare both bots against the greedy baseline and against each other once they can share a match harness. Report win rates, number of games, and uncertainty. Use fixed work budgets to check the port and fixed wall-clock budgets to see whether C++ can buy more search and stronger play. Tune depth, sampling, or representations after establishing the baseline, measuring each change separately.
