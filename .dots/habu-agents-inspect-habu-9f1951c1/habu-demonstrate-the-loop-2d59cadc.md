---
title: "Demonstrate the loop and measure inspection's cost"
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T19:42:25.734800+02:00"
blocks:
  - habu-observe-the-typed-ced829e1
  - habu-compare-supplied-candidates-265e1b5a
---

Problem: the three words are useful only if a fresh agent finds them and ordinary compilation did not slow down; neither is shown by the words existing.
Acceptance: one end-to-end run on SQUARE in one live REPL: a fresh agent finds the words through AGENTS.md and the card; describes SQUARE; observes the state before `*`; submits `*`, `+` and `DUP`; sees `DUP` refused and `+` fail the 3 gives 9 fixture; keeps the right definition and the case as ordinary source. AGENTS.md gets one short rule: use the words when an unfamiliar contract, binding or checker failure would otherwise be guessed, not for every edit. Measured with the same source, target, engine and cache, a few paired runs, wall and CPU time and result size, recorded here: baseline against changed compiler with inspection off; inspection off against one observation; one candidate against a small batch; one ordinary incremental build with inspection off. A repeatable regression with inspection off is explained and removed before the campaign closes. No percentage target, no claim of zero overhead, no productivity multiplier claimed.
Depends: Observe the typed state at one boundary in a check; Compare supplied candidates against a fixed case.
Claim: unassigned.
