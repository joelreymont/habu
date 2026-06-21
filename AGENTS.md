# habu — Checked Forth

A checked stack-effect system for Forth, self-hosted by `bin/hb`. LLM-generated
Forth is verified by a checker (typed, row-polymorphic stack effects) rather
than trusting the model to track the stack by hand. See `PLAN.md` for design and
roadmap.

## Conventions

- **Project-defined Forth words are UPPER-CASE; built-ins stay as-is/lower-case.**
  Define and call our words in upper case (`SQUARE`, `CHECKED:`); keep core words
  lower-case (`dup`, `drop`, `if`, `?do`). Lower-case only for `\` comments and
  prose outside code.
- No-binary recovery runs the checked seed tool with a trusted native `hb` seed:
  see `docs/seed.md`. The seed immediately rebuilds current source through
  `tools/build-fixpoint-main.f -- install`.
  There is no supported build-from-gforth recovery path in this checkout.

## Forth style (BLOCKING)

See **`docs/forth.md`** for the full Forth standards. In short: new public/library
Forth defaults to checked typed definitions; unchecked code must be an explicit
tested boundary; our words UPPER-CASE (built-ins as-is); hyphens not underscores;
small factored words with a `( in -- out )` comment each; shallow stacks (factor /
`{: :}` locals, no deep juggling); `throw` named codes, never silent; named
constants; and a `T{ … -> … }T` test for every word.

- **One concern per file** — never bundle unrelated responsibilities (parser vs
  renderer vs DB vs data table). Split at responsibility seams; it aids review
  and lets files be built in parallel. See `docs/forth.md` § Files.

## Habu Only (BLOCKING)

- New repo automation, tests, benchmarks, report reducers, parsers, code
  generators, build logic, repair tools, and LLM tooling must be checked/typed
  Habu Forth run by `bin/hb`.
- Do not add new Python, JavaScript, Node, shell, awk, sed, or Perl logic. A
  shell file is allowed only as a compatibility launcher whose behavior is
  `exec bin/hb tool.f "$@"`, and the remaining launcher must be tracked as host
  glue to retire.
- Existing host glue is legacy debt. Do not extend it; replace it. When Habu
  lacks a needed primitive or typed abstraction, add a detailed dot for that
  capability and build it in Habu instead of adding host code.
- Unchecked Habu (`0 set-check`, `TRUSTED:`) is allowed only as a named, tested
  boundary for behavior the checker cannot yet express. The missing typed
  capability must be tracked by dot and removed from the unchecked boundary when
  implemented.

## Correct Fixes (BLOCKING)

- No hacks, no patches, no workarounds. Always do the long-term correct fix:
  root-cause the failure, repair the violated invariant or missing capability,
  add/keep tests that prove the fix, and record any remaining substantive work
  as detailed dots.

## Lessons

- **Read `docs/forth.md` and `LESSONS.md` at the start of every session, and
  update `LESSONS.md` after any finding, mistake, or insight.** The former is the
  coding standard; the latter is the project's concise running memory.
- `LESSONS.md` holds lessons only — what worked, what didn't, and why. It is NOT
  for API reference, code snippets, Forth standards, or language patterns; those
  go in `docs/` (especially `docs/forth.md`).

## Workflow

- VCS is `jj` (Jujutsu). One change per commit; 50-char imperative subject; no emoji.
- Commit after each significant change or feature; include new files.
- RCA is blocking for generated checked fixtures: if a generated checked fixture
  stalls, times out, or exits without diagnostics, immediately isolate the
  checker/harness phase and root cause. Do not call the fixture "too expensive",
  shrink it, bypass it, or replace it until the failing mechanism is proven.
- Native crash/memory RCA must use debugger evidence first: set breakpoints,
  step, and inspect stack/data/watch cells. If the debugger cannot expose the
  needed state, extend the debugger/stepper before falling back to print-marker
  probes.
- Parallel dot execution follows `docs/parallel-agents.md`: read-only scouts do
  not edit the current tree; workers edit isolated jj workspaces unless their
  file ownership is disjoint.
- Gate: `bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f` — Habu-native, no gforth. If `bin/hb`
  is missing, recover from a trusted native seed with the checked command in
  `docs/seed.md`.
