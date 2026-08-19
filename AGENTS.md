# habu — Checked Forth

Habu is a Forth with a type checker. Every word (function) declares what it
takes and leaves on the stack, and the checker proves the body matches the
declaration — so Forth written by an LLM is verified instead of trusted. The
system builds itself through `bin/hb`. Open work is tracked in dots.

## Orchestrator Role (BLOCKING)

- The main session plans, dispatches, reviews, and merges. It never writes
  implementation code itself; all coding is done by worker subagents.
- Review every worker diff hunk by hunk before merging. No hacks, no shaved
  scope; only long-term-correct code merges. Substandard work goes back to
  the worker or to a fresh attempt.

## Human English (BLOCKING)

- Use human English. Always!

## Simplify Relentlessly (BLOCKING)

- Before accepting any new mechanism, flag, campaign, or leaf, probe what the
  tree already provides: read the owning source and its notes, run the exit-70
  probe, and prefer the existing shape or a deletion. Consensus between
  orchestrators is never evidence; the tree is.
- New machinery requires a failing probe through the real gate AND a named
  first consumer. If the consumer is "delete ceremony X", prove X can't be
  deleted with existing mechanisms first.
- When a design survives the probe, build the smallest sufficient form — one
  generator or compiler change beats a record/parse/enforce campaign.
  (Precedent: LESSONS.md, the 2026-08-04 owner-construction stop.)

## Forth Style (BLOCKING)

- `docs/forth.md` is the standard — naming, packages, factoring, stack
  signatures, testing, the commit gate, and the Checker-Miss RCA template.
  Read it at session start. The heart of it: small typed words, factored
  aggressively, composed into readable checked mini-languages. One concern
  per file.
- Every new module opens a real `package NAME`. Run the package lint as soon
  as the first definition exists; if it rejects a caller, fix the caller — no
  forwarding shims.
- If `bin/hb` is ever lost or broken, recover with
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` (`docs/bootstrap.md`).

## Test Integrity (BLOCKING)

- A test must run the real thing: the production entry point and load path.
  A test that re-implements the logic it checks proves nothing.
- A test for a parser or lint tool must check the structure of the input,
  not just search its text; include fixtures built to fool it. "The file
  contains this substring" is not proof.

## Proof Integrity (BLOCKING)

- `docs/proofs.md` governs published results: mutation-falsifiable only,
  and unprovable is a result, never a weakened statement. Read it before
  touching models, manifests, or parity gates.

## Habu Only (BLOCKING)

- Prefer Habu to write new tooling. Unchecked Habu (`0 set-check`,
  `TRUSTED:`) only as a named, tested boundary around something the checker
  cannot yet express — with a dot for the missing capability, and the
  boundary removed when it lands.

## Correct Fixes (BLOCKING)

- When a bug makes you ask "why didn't the checker catch this?", answer that
  first: proof the failure fails loudly on its load path, a minimal
  reproducer, and the checker fix — or a dot for the missing capability —
  plus a regression test showing the bad program is now rejected. Template:
  `docs/forth.md` § Checker-Miss RCA.
- Never paper over a checker gap with runtime guards everywhere; one named,
  tested boundary until the capability lands.

## Lessons

- Read `docs/forth.md` and `LESSONS.md` at the start of every session; add to
  `LESSONS.md` whenever you learn something. Lessons only — reference
  material goes in `docs/`.

## Workflow

- Version control is `jj`; mechanics in the jj skill, claim choreography in
  the dots skill, dispatch doctrine in the parallel-agents skill. Read-only
  scouts don't edit the tree; workers edit in their own `.jj-ws/` workspaces.
- **Master is always green (BLOCKING).** Fast-forward `master` only after,
  on the exact tree being merged, the maki suite
  (`bin/hb --load maki/test.f`), the ptx-stdlib slice, and any native slices
  you touched are all green. Red, skipped, or unrun means no merge; a red
  master is a stop-everything incident; nobody commits to `master` directly.
  Commit gate: `docs/forth.md` § Commit gate.
- Dots (BLOCKING): claim per the dots skill and pass the dot lint
  (`HB_TMP=<private-root> bin/hb --load tools/dot-dep-lint.f` — exit 0,
  `0 finding(s)`) before publishing the claim. The dot stays active until its
  reviewed commit is merged and verified. Never rerun `dot on` on an active
  dot; never take a blocked or claimed dot. Dot metadata publishes like code.
- If a generated fixture hangs, times out, or dies silently, find out why
  before shrinking or bypassing it. Debug native crashes with the debugger
  first (`docs/debugging.md`); on any opaque failure, build a small reusable
  tool (kept in `tools/` or `docs/debugging.md`) instead of print-and-retry
  guessing.

## Blackboard

- `.blackboard/` is the inter-orchestrator channel (bb skill) — it exists only
  while more than one orchestrator is live. If the directory exists, check it
  before and after every launch, review, merge, or push, and show the user
  every message you post. Ask the user in the conversation, never through it.
- Don't leave watchers or workers running after the session ends.
