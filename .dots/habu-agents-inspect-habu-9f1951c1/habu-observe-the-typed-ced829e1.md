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

Problem: the checker knows the typed data and return rows and the typed locals at every token of a definition it checks, but exposes at a cursor only the spellings that bind there (src/core/checker.f:10545 CHECKER-CURSOR!, whose visitor is the defer CHECKER-ON-CAND, :10525; its only production caller is src/habu/verify-source.f:1288 CSR-ARM, which habu-check-src-through-1198f4ed deletes); an agent that wants the stack before one token has to reason it out by hand.
Acceptance: one public word takes a complete ordinary definition's source and a zero-based byte offset immediately before a token, runs the ordinary check, and returns: the source identity and offset; whether the point was reached; the typed rows and visible typed locals the checker can reliably expose there, with unresolved row and type variables kept; the enclosing declared effect; an explicit mark that these are provisional facts of the final attempt; and, separately, the whole-definition verdict (refused, deferred, uncheckable, incomplete all preserved). A point checking never reaches returns not reached, not an empty stack. Capture happens inside the armed path of the check that owns the cursor (src/core/checker.f:10598 PUBLISH-EXIT); prior attempts' observations are dropped and only the final attempt's is published, cleanup on every exit; no tape observer competing with T0 (src/habu/t0.f), no reentry into checking from the capture, no generic hook registry. Results are copied before the checker's temporary storage resets; no address into a growable capture block or checker arena is kept. Unfinished control or quotation scopes are out of scope. With no observation armed, the ordinary check adds no copying, rendering or output.
Joel, 2026-11-10, asking which stages start now: "i have no hold on showing the checker's typed state before a token. i don't know what that is so you have to explain and give me context! what is best for long term here?" Then, after the stage was explained to him with an example (the stack at one failing word today; the stack, return stack and locals before any word with the stage built) and told it is built only if a measure says stack mismatches cost enough: "measure then". The measure is habu-measure-what-a-7edae949, closed with its ranked list, which went to Joel; the closed dot records refusals as 0.94% of agent calls; this stage is built only if that measure says stack mismatches cost enough.
Depends: Describe a published word in the live REPL; habu-make-incremental-builds-41fd3214 (check-tool work is parked until then, Joel 2026-11-04).
Claim: unassigned.
