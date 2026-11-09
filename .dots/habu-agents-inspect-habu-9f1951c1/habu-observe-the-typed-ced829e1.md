---
title: Observe the typed state at one boundary in a check
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T19:42:25.712495+02:00"
blocks:
  - habu-describe-a-published-501824c2
  - habu-make-incremental-builds-41fd3214
---

Problem: the checker knows the typed data and return rows and the typed locals at every token of a definition it checks, but exposes at a cursor only the spellings that bind there (src/core/checker.f:10369 CHECKER-CURSOR!, used by src/habu/verify-source.f CSR-ARM); an agent that wants the stack before one token has to reason it out by hand.
Acceptance: one public word takes a complete ordinary definition's source and a zero-based byte offset immediately before a token, runs the ordinary check, and returns: the source identity and offset; whether the point was reached; the typed rows and visible typed locals the checker can reliably expose there, with unresolved row and type variables kept; the enclosing declared effect; an explicit mark that these are provisional facts of the final attempt; and, separately, the whole-definition verdict (refused, deferred, uncheckable, incomplete all preserved). A point checking never reaches returns not reached, not an empty stack. Capture happens inside the armed path of the check that owns the cursor; prior attempts' observations are dropped and only the final attempt's is published, cleanup on every exit; no tape observer competing with T0 (src/habu/t0.f), no reentry into checking from the capture, no generic hook registry. Results are copied before the checker's temporary storage resets; no address into a growable capture block or checker arena is kept. Unfinished control or quotation scopes are out of scope. With no observation armed, the ordinary check adds no copying, rendering or output.
Files: src/core/checker.f (cursor owner, PUBLISH-EXIT path), the inspection module under src/habu/, docs/forth-card.md.
Verify: through the real load path in one program: `: SQUARE ( i64 -- i64 ) DUP * ;` before `*` shows two i64; a typed-locals case; an existing retry or failure case; each ordinary verdict unchanged; a following query sees no stale state; an offset past where checking stops returns not reached.
Depends: Describe a published word in the live REPL; habu-make-incremental-builds-41fd3214 (DECISIONS.md 2026-11-04 parks check-tool work until then).
Ownership: the cursor owner's capture path in src/core/checker.f, the inspection module. Claim: unassigned.
