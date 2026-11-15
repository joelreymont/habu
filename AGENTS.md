# Habu — checked Forth

Application-specific policy and model workflows stay out of the language core.

## Read first

- Before writing Habu: [docs/forth-card.md](docs/forth-card.md); look things
  up in [docs/forth.md](docs/forth.md) at the heading the card names.
- Before writing or changing Habu's tools, modules or boundaries: [docs/design.md](docs/design.md#writing-and-changing-habu) "Writing and changing Habu".
- Before extending any existing machinery: [docs/design.md](docs/design.md#machinery-being-replaced) "Machinery being replaced".
- Before writing or running a test: [test/AGENTS.md](test/AGENTS.md).
- Before building a candidate or installing: [docs/bootstrap.md](docs/bootstrap.md#stable-local-command) "Stable local command".
- When `bin/hb` is missing or stale: [docs/bootstrap.md](docs/bootstrap.md) "Recovery".
- Before diagnosing a native failure: [docs/debugging.md](docs/debugging.md).
- Before diagnosing checked code that misbehaves: [docs/debugging.md](docs/debugging.md#debugging-the-self-hosted-standalone) "Debugging the self-hosted standalone".

## Repository

- The root checkout stays on `master`: its working copy is one change on
  `master`'s tip. Commit here only what lands on `master` at once, and run
  `jj new master` here whenever `master` moves.
- `spec/` holds TLA+ specs of Habu's state machines;
  [spec/README.md](spec/README.md) says how to check them.

## Changes

- Incremental builds ship in the release: a warm build never fails or differs
  where a cold build would not.
- The installed tree is checked by being byte-identical to the candidate,
  never by building again.
- The test suite, the convergence check and a feature's review run once,
  before the install, never per change; a rebase over changes to `.dots/` alone
  reruns nothing.
- There are no runtime checks: check things statically as much as possible.
  Code instrumented with bounds and other checks may be generated on demand,
  never by default.

## Where findings go

A finding goes where it is checked: a test or a code comment when either can
hold it; [docs/forth.md](docs/forth.md) "Rules learned by refusal" and the card
for a language rule; [docs/bootstrap.md](docs/bootstrap.md),
[test/AGENTS.md](test/AGENTS.md) or [docs/debugging.md](docs/debugging.md) for
a build, test or diagnosis rule. Other reference material belongs in `docs/`.
State the rule and the fact that proves it, without dates or narrative, and
search and merge before adding. A doc passage that must track code is deleted;
the fact goes into the code's comment. This file holds only rules: a procedure
is a step that names its doc.
