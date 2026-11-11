# Habu — checked Forth

Application-specific policy and model workflows stay out of the language core.

Agents write, build and debug everything in Habu and on it; no person uses
Habu directly.

Zen, Tender and Maki build, test and run on Habu without issues. A dot that
removes an issue one of them meets goes before any other work.

Until the three are released, dot `habu-until-zen-tender-31e8d2a1` says how a
Habu change is measured and lands.

Incremental builds ship in the release, rock-solid: a warm build never fails or
differs where a cold build would not.

## Read first

- Before writing or changing Habu's tools, modules or boundaries: [docs/design.md](docs/design.md#writing-and-changing-habu) "Writing and changing Habu".
- Before extending any existing machinery: [docs/design.md](docs/design.md#machinery-being-replaced) "Machinery being replaced".
- Before choosing or running a check: [docs/gate.md](docs/gate.md#native-test-suite) "Native test suite".
- Before writing a test: [docs/gate.md](docs/gate.md#writing-tests) "Writing tests".
- Before building a candidate or installing: [docs/bootstrap.md](docs/bootstrap.md#stable-local-command) "Stable local command".
- Before diagnosing checked code that misbehaves: [docs/debugging.md](docs/debugging.md#debugging-the-self-hosted-standalone) "Debugging the self-hosted standalone".

## Repository

- `spec/` holds TLA+ specs of Habu's state machines;
  [spec/README.md](spec/README.md) says how to check them.

## Writing Habu

- Read [docs/forth-card.md](docs/forth-card.md) before writing Habu, and look
  things up in [docs/forth.md](docs/forth.md) at the heading the card names.
- Diagnose a native failure by [docs/debugging.md](docs/debugging.md); recover
  a missing or stale `bin/hb` by [docs/bootstrap.md](docs/bootstrap.md)
  "Recovery".

## Changes

- Never compress code or data without the user's explicit approval.
  Compression shrinks code or data through a codec; dropping zero-filled,
  unused or unreachable storage, packing allocations tightly and choosing a
  compact table encoding are not compression.

## Where findings go

A finding goes where it is checked: a test or a code comment when either can
hold it; [docs/forth.md](docs/forth.md) "Rules learned by refusal" and the card
for a language rule; [docs/bootstrap.md](docs/bootstrap.md),
[docs/gate.md](docs/gate.md) or [docs/debugging.md](docs/debugging.md) for a
build, test or diagnosis rule. Other reference material belongs in `docs/`.
State the rule and the fact that proves it, without dates or narrative, and
search and merge before adding. A doc passage that must track code is deleted;
the fact goes into the code's comment. This file holds only rules: a procedure
is a step that names its doc.
