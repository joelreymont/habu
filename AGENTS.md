# habu — Checked Forth

A checked stack-effect system for Forth, self-hosted by `bin/hb`. LLM-generated
Forth is verified by a checker (typed, row-polymorphic stack effects) rather
than trusting the model to track the stack by hand. Active work lives in dots;
current verification state lives in `STATUS.md`.

## First Response Triggers (BLOCKING)

- First action on every turn is a silent latest-message trigger scan. Do it
  before progress updates, tool calls, implementation plans, status text, or
  apologies. Scan for “why didn’t the checker catch this?”, “why didn’t the
  compiler catch this?”, or equivalent wording. Quoted examples, meta-process
  questions, and variants such as “why did the checker/compiler miss it?” are
  live triggers. Questions about improving or enforcing this rule are not
  exempt; they must still start with the invariant line.
- If that trigger is present, the first visible line must be `Static invariant:`
  followed by the pre-runtime fact that should have been impossible and the
  checker/compiler boundary that should enforce it. No runtime symptom,
  workaround, library edit, or test-harness edit comes first.
- If the invariant is not known yet, the first visible line is still
  `Static invariant:` and states the unknown precisely. Then reduce the case
  until the invariant, owner, reproducer, compiler fix, and regression are known.
- If the trigger was missed, the next visible message must restart with
  `Static invariant:` before continuing. Do not explain the process first.
- Continue with `docs/forth.md` § Checker-Miss RCA: prove the exact path is
  fail-closed, classify the miss, add a minimal negative checked regression, fix
  the checker/compiler/primitive model or dot the missing capability, then repair
  downstream code. Documentation or process edits do not discharge the trigger;
  they only support the compiler/checker fix or capability dot.

## Conventions

- **Project-defined Forth words are UPPER-CASE; built-ins stay as-is/lower-case.**
  Define and call our words in upper case (`SQUARE`, `CHECKED:`); keep core words
  lower-case (`dup`, `drop`, `if`, `?do`). Lower-case only for `\` comments and
  prose outside code.
- No-binary recovery uses the explicit Gforth bootstrap path in
  `docs/bootstrap.md`. Gforth is only a recovery host: it creates private
  `HB_TMP` artifacts, installs `bin/hb`, and the native checked fixpoint refresh
  takes over immediately. A trusted native seed remains documented in
  `docs/seed.md` for seed-maintenance work.

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
  Habu Forth run by `bin/hb`, except the audited no-binary recovery launcher
  `tools/bootstrap.sh`.
- Do not add new Python, JavaScript, Node, shell, awk, sed, or Perl logic. A
  shell file is allowed only as a compatibility launcher whose behavior is
  `exec bin/hb tool.f "$@"`, and the remaining launcher must be tracked as host
  glue to retire.
- Existing host glue is legacy debt. Do not extend it outside the audited
  no-binary recovery launcher; replace it. When Habu lacks a needed primitive or
  typed abstraction, add a detailed dot for that capability and build it in Habu
  instead of adding host code.
- Unchecked Habu (`0 set-check`, `TRUSTED:`) is allowed only as a named, tested
  boundary for behavior the checker cannot yet express. The missing typed
  capability must be tracked by dot and removed from the unchecked boundary when
  implemented.

## Correct Fixes (BLOCKING)

- No hacks, no patches, no workarounds. Always do the long-term correct fix:
  root-cause the failure, repair the violated invariant or missing capability,
  add/keep tests that prove the fix, and record any remaining substantive work
  as detailed dots.
- Checker-first RCA is mandatory. Treat the phrase “why didn’t the checker catch
  this?” and equivalent wording as an immediate trigger, even in meta-discussion
  about process or rule-maintenance. The first question is always: **What
  static invariant should have made this impossible before runtime, and where
  should the compiler/checker enforce it?** The first visible line of the
  response, progress update, note, dot, or investigation must be
  `Static invariant:` with that answer. No runtime symptom, guard, workaround,
  documentation edit, or library edit may come first. Use the RCA template in
  `docs/forth.md` § Checker-Miss RCA. Do not edit runtime/library code until the
  investigation also records: fail-closed proof for the exact command path, miss
  class, minimal checked reproducer, and the checker/compiler change or explicit
  dot that will close the soundness gap.
  Prove fail-closed status with a minimal bad definition on the same load path,
  command exit status, and stderr/stdout diagnostics. If the path was not
  fail-closed, fix that harness/tooling gap before runtime repair. If it was
  fail-closed, reduce the checker miss to a minimal checked fixture, then fix
  the checker/compiler and add a negative regression before fixing or accepting
  downstream code.
- A checker miss is compiler/checker work by default. Only classify it as an
  application/library bug after proving the checked path was not supposed to
  express that invariant; otherwise add the missing checker/compiler capability,
  primitive model, or boundary effect and a failing negative regression first.
- If the checker cannot yet express the invariant, do not normalize the gap with
  local runtime guards. Add the missing checker/type/compiler capability as a
  detailed dot, keep only a named tested boundary while it exists, and remove
  that boundary when the capability lands.

## Lessons

- **Read `docs/forth.md` and `LESSONS.md` at the start of every session, and
  update `LESSONS.md` after any finding, mistake, or insight.** The former is the
  coding standard; the latter is the project's concise running memory.
- `LESSONS.md` holds lessons only — what worked, what didn't, and why. It is NOT
  for API reference, code snippets, Forth standards, or language patterns; those
  go in `docs/` (especially `docs/forth.md`).

## Workflow

- VCS is `jj` (Jujutsu). One change per commit; 50-char imperative subject; no emoji.
- Commit rule (BLOCKING): `jj commit` is a proof checkpoint, not a stash. Before
  creating it, inspect `jj diff`, identify the touched source classes, run the
  focused validation for those paths through native `bin/hb`, and record any
  unresolved substantive work as dots instead of committing known failures. If
  the gate cannot be run, leave the change uncommitted or dot the blocker; do
  not commit speculative, skipped, or "fix later" code.
- Forth commit gate (BLOCKING): before `jj commit`, any change
  touching `.f` or `.fs` source must prove checked Habu was used wherever
  possible. Scan the diff for new/changed `:`, `+:`, `CHECKED:`, `TRUSTED:`,
  `0 set-check`, and `TRUST` sites; every new or changed definition needs a real
  typed stack effect unless it is an explicitly documented boundary. Every
  changed Forth file must be checked through its exact owning `bin/hb --load ...`
  path or documented as an explicit uncheckable boundary, and every unchecked
  boundary needs a focused test plus a dot for the missing typed capability.
  Treat a failed or skipped commit gate as unfinished work, not as something to
  commit first and clean up later.
- Commit after each significant change or feature; include new files.
- RCA is blocking for generated checked fixtures: if a generated checked fixture
  stalls, times out, or exits without diagnostics, immediately isolate the
  checker/harness phase and root cause. Do not call the fixture "too expensive",
  shrink it, bypass it, or replace it until the failing mechanism is proven.
- For checker misses where the command path is fail-closed, classify the miss
  before editing runtime code: wrong primitive/boundary effect, checker
  semantics, codegen/runtime mismatch, or same-type semantic-role gap. Each class
  needs a minimal checked fixture, a negative regression, and either an immediate
  compiler/tooling fix or a detailed dot for the missing checker capability. The
  runtime fix is not complete until the checker path rejects the bad program.
- Native crash/memory RCA must use debugger evidence first: set breakpoints,
  step, and inspect stack/data/watch cells. If the debugger cannot expose the
  needed state, extend the debugger/stepper before falling back to print-marker
  probes. See `docs/debugging.md` for the baked Forth stepper, breakpoint/watch
  tools, JIT/image dumpers, and gdb/lldb fallback boundary.
- Parallel dot execution follows `docs/parallel-agents.md`: read-only scouts do
  not edit the current tree; workers edit isolated jj workspaces unless their
  file ownership is disjoint.
- Gate: use the target/checker/env prelude and native gate command from
  `docs/bootstrap.md` — Habu-native, no gforth. If `bin/hb`
  is missing, recover with `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` as
  documented in `docs/bootstrap.md`.
