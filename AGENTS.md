# habu — Checked Forth

Habu is a Forth with a type checker. Every word (function) declares what it
takes and leaves on the stack, and the checker proves the body matches the
declaration — so Forth written by an LLM is verified instead of trusted. The
system builds itself through `bin/hb`. Open work is tracked in dots.

## Orchestrator Role (BLOCKING)

- The main session plans, dispatches, reviews, and merges. It never writes
  implementation code itself.
- All coding is done by worker subagents (Opus for Claude).
- Review every worker diff hunk by hunk before merging. No hacks, no shaved
  scope; only long-term-correct code merges. Substandard work goes back to
  the worker or to a fresh attempt.

## Human English (BLOCKING)

- Use human English. Always!

## Conventions

- If `bin/hb` is ever lost or broken, recover with
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` (see `docs/bootstrap.md`).
  Gforth is only a recovery host: it produces a working `bin/hb`, and the
  native build takes over from there.

## Forth Style (BLOCKING)

- `docs/forth.md` is the standard — naming, packages, factoring, stack
  signatures, testing, the commit gate, and the Checker-Miss RCA template
  all live there. Read it at session start. The heart of it: small typed
  words, factored aggressively, composed into readable checked
  mini-languages.
- One concern per file; split where responsibilities split.

## Packages (BLOCKING)

- Every new module opens a real `package NAME`; see `docs/forth.md`
  § Packages. Run the package lint as soon as the first definition exists;
  if it rejects a caller, fix the caller — no exceptions, no forwarding
  shims.

## Test Integrity (BLOCKING)

- A test must run the real thing: the same entry point and load path that
  production uses. A test that re-implements the logic it is checking — a
  copied validator, a hand-built state machine, a synthetic stand-in —
  proves nothing about the implementation.
- A test for a parser or lint tool must check the structure of the input,
  not just search its text. Include fixtures built to fool it: the expected
  text hidden in a comment or a string, duplicated, reordered, or in the
  wrong role. "The file contains this substring" is not proof.

## Habu Only (BLOCKING)

- Prefer Habu to write new tooling.
- Unchecked Habu (`0 set-check`, `TRUSTED:`) is allowed only as a named,
  tested boundary around something the checker cannot yet express — with a
  dot for the missing capability, and the boundary removed when it lands.

## Correct Fixes (BLOCKING)

- No hacks, no patches, no workarounds. Find the root cause, repair the
  broken invariant, keep a test that proves the repair, and file dots for
  whatever real work remains.
- When a bug makes you ask "why didn't the checker catch this?", answer that
  first: what rule should have made the bug impossible, and where should the
  checker enforce it? Before touching runtime or library code, establish:
  proof that the failure fails loudly on its load path (a minimal bad
  program, its exit code, its error message), a minimal reproducer, and the
  checker fix — or a dot for the missing capability — plus a regression test
  showing the bad program is now rejected. Template: `docs/forth.md`
  § Checker-Miss RCA.
- If the checker can't express the needed rule yet, don't paper over the gap
  with runtime guards everywhere. File a dot for the checker capability and
  keep one named, tested boundary until it lands.

## Lessons

- Read `docs/forth.md` and `LESSONS.md` at the start of every session, and
  add to `LESSONS.md` whenever you learn something.
- `LESSONS.md` is for lessons only — what worked, what didn't, why.
  Reference material goes in `docs/`.

## Workflow

- Version control is `jj`. Mechanics and safety rules live in the global
  `jj` skill; dispatch and claim choreography in the global `dots` and
  `parallel-agents` skills. Read-only scouts don't edit the tree; workers
  edit in their own jj workspaces under `.jj-ws/`.
- **Master is always green (BLOCKING).** Never point `master` at a commit
  whose gates haven't passed. Work on your own branch and push there freely;
  fast-forward `master` only after, on the exact tree being merged, all of
  these are green: the maki suite (`bin/hb --load maki/test.f`), the
  ptx-stdlib slice plus any native slices you touched.
  Red, skipped, or unrun means no merge. A red master is a stop-everything
  incident, and nobody commits to `master` directly.
- Forth commit gate (BLOCKING): `docs/forth.md` § Commit gate.
- Dots (BLOCKING): claim by writing
  `Claim: agent=<name> workspace=.jj-ws/<dot-id>` into the leaf, run
  `dot on <exact-id>`, and pass the dot lint
  (`HB_TMP=<private-root> bin/hb --load tools/dot-dep-lint.f` — exit 0,
  `0 finding(s)`) before publishing the claim. The dot stays active until
  its reviewed commit is merged and verified; only then
  `dot off <exact-id> -r "..."`. Never rerun `dot on` on an active dot — it
  rewrites metadata. Never take a dot that is blocked or claimed by someone
  else. Dot metadata changes publish like code: gates, green `master`,
  verified remote, before anything that depends on them merges.
- If a generated test fixture hangs, times out, or dies silently, find out
  why before shrinking or bypassing it. "Too expensive" is not a diagnosis.
- Debug native crashes with the debugger first — breakpoints, stepping,
  memory inspection (`docs/debugging.md`). If the debugger can't show the
  state you need, extend the debugger before falling back to print probes.
- **Build the tool, don't guess (BLOCKING).** On a crash, hang, or opaque
  exit code, get evidence with a tool — and if no tool shows it, build a
  small reusable one (a catch-and-report word, a dumper, an inspector)
  instead of print-and-retry guessing. Keep it in `tools/` or
  `docs/debugging.md` so the next crash starts with a tool. (An opaque exit
  183 was solved in minutes by catching it in gdb and reading a register:
  error -3401.)

## Blackboard

- `.blackboard/` is the chat channel between orchestrators. Check for unread
  messages at least every 60 seconds while working, and before and after
  every launch, review, commit, merge, or push.
- Post when you take, pause, block, redesign, review, commit, land, or
  release work. Ask design questions early instead of discovering divergent
  implementations later.
- Blackboard messages don't replace review or the dot contract. Ask the user
  in the conversation, not through the blackboard.
- Show the user every message you post, with its channel and ID. Ideas,
  honest feelings, and humor are welcome. Don't leave watchers or workers
  running after the SSH session ends.

## Fix Review Gate (BLOCKING)

- Before a fix merges, the reviewer independently answers: is this the
  correct long-term fix, or a patch dressed up as one? Re-derive the
  invariant from the code; never take the implementer's own label
  ("minimal", "documented pattern") as evidence.
- A fix that leans on a lucky value — a magic range, a sentinel, timing —
  where a structural check is possible (existence, capability probe, single
  writer) is a patch. Send it back.
- Workers answer the same question in their final reports.
