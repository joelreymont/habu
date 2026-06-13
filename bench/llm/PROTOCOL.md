# habu LLM benchmark

habu's thesis is empirically testable: a checked stack-effect language should let
a model generate compact Forth, run the checker, and repair — reaching working
code in fewer iterations than plain Forth (where stack drift fails silently).
This suite is the harness for measuring that.

## Layout
- `tasks.md` — the task specs (the prompts given to a model): a name, a declared
  `( in -- out )`, a one-line spec, and the functional tests.
- `solutions.f` — the reference solutions (the answer key): one checked
  `: NAME ( sig ) … ;` per task. `run.sh` proves every reference typechecks
  (`CHECK!` ⇒ certified) and passes its tests, so the suite is self-consistent.
- `run.sh` — validate the reference solutions: typecheck (verify body-vs-sig) +
  run the functional tests. Reports `N/N` typechecked, `N/N` tests passed.

## Running an LLM evaluation
For each task in `tasks.md`: give the model the spec (name, sig, one-liner) WITHOUT
the reference. Take the model's `: NAME ( sig ) … ;`, then:
1. **First-pass checker** — feed it to the checker (`CHECK!`). Record certified /
   rejected / uncheckable.
2. On rejection, return the diagnostic (use `JSON-DIAGS ON` for the structured
   form) and let the model repair. Record the repair iteration count.
3. **Test** — run the task's functional tests against the (certified) word.

## Metrics to record per task
- first-pass checker success (certified on attempt 1?)
- first-pass test success
- repair iterations to certified + passing
- tokens used
- final code size (chars / compiled bytes)
- number of `TRUST` uses (should be 0 for these tasks)
- **did the model weaken the signature instead of fixing the body?** (the
  anti-pattern — see `LLM.md` §4) — inspect each repair

## Comparison baselines
Run the same task specs against: plain gforth (no checker), Factor, Zig, Rust, C,
a bare typed stack DSL. habu's edge is the tight `check → repair` loop on
stack-effect errors; the metric that should move is **repair iterations** and
**signature-weakening rate**.

## Categories covered (15 tasks)
arithmetic · control flow · locals · polymorphic stack words · counted loops ·
return stack · quotations & combinators · recursion · a small parser · a
test-first underspecified task.
