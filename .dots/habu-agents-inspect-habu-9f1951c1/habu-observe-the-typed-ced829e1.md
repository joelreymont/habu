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

Problem: the checker knows the typed data and return rows and the typed locals at every token of a definition it checks, but exposes at a cursor only the spellings that bind there (src/core/checker.f:10382 CHECKER-CURSOR!, whose visitor is the defer CHECKER-ON-CAND, 10362; its only production caller is src/habu/verify-source.f CSR-ARM, 1291, which habu-check-src-through-1198f4ed deletes); an agent that wants the stack before one token has to reason it out by hand.
Acceptance: one public word takes a complete ordinary definition's source and a zero-based byte offset immediately before a token, runs the ordinary check, and returns: the source identity and offset; whether the point was reached; the typed rows and visible typed locals the checker can reliably expose there, with unresolved row and type variables kept; the enclosing declared effect; an explicit mark that these are provisional facts of the final attempt; and, separately, the whole-definition verdict (refused, deferred, uncheckable, incomplete all preserved). A point checking never reaches returns not reached, not an empty stack. Capture happens inside the armed path of the check that owns the cursor; prior attempts' observations are dropped and only the final attempt's is published, cleanup on every exit; no tape observer competing with T0 (src/habu/t0.f), no reentry into checking from the capture, no generic hook registry. Results are copied before the checker's temporary storage resets; no address into a growable capture block or checker arena is kept. Unfinished control or quotation scopes are out of scope. With no observation armed, the ordinary check adds no copying, rendering or output.
Files: src/core/checker.f (cursor owner, PUBLISH-EXIT path), the inspection module under src/habu/, docs/forth-card.md.
Verify: through the real load path in one program: `: SQUARE ( i64 -- i64 ) DUP * ;` before `*` shows two i64; a typed-locals case; an existing retry or failure case; each ordinary verdict unchanged; a following query sees no stale state; an offset past where checking stops returns not reached.
Held (Joel, 2026-11-10): the moment it serves measured 148 stack-mismatch episodes in 988 refusal episodes, each reaching a clean run in a median 2-3 calls.
Depends: Describe a published word in the live REPL; habu-make-incremental-builds-41fd3214 (check-tool work is parked until then, Joel 2026-11-04).
Ownership: the cursor owner's capture path in src/core/checker.f, the inspection module. Claim: unassigned.

Joel, 2026-11-10 evening, asked which stages start now: "i have no hold on showing the checker's typed state before a token. i don't know what that is so you have to explain and give me context! what is best for long term here?" The 'Held' line above is not his ruling: it came from an option he picked without the thing being explained. Not yet settled when this stage is built; explain it to him with an example first (what an agent sees today, what it would see).
