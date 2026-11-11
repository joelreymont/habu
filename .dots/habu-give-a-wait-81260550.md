---
title: Give a wait with no deadline a type without timeout
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:49:39.480171+02:00"
---

Problem (tim, Maki lead, 2026-11-11; /tmp/tim-wait-ending.md): PROCESS:WAIT-ENDING ( pid -- ending ) (src/lib/process.f:265) is WAIT-STATUS STATUS>ENDING, and STATUS>ENDING (process.f:221) answers only exited or signaled; a failed wait throws E-PROC-WAIT. Its result type ending (process.f:39-43) also has the variant timeout, which only a deadline wait answers (WAIT-BOUNDED, process.f:376-387, and the capture loop). So every exhaustive MATCH on what WAIT-ENDING answers must name timeout (E-MATCH-NONEXHAUSTIVE) and carries an arm only a bug reaches. Maki meets it: deploy/rehearsal/rehearse.f KILL-MAKI, the arm at :4302 (Maki master 1b851601); Maki's AGENTS.md forbids such an arm. Callers counted 2026-11-11 (files naming WAIT-ENDING / WAIT-BOUNDED): Habu src 7/6, test 4/1, tools 1/0; Zen 2/2; Tender 3/2; Maki 1/4. Acceptance: a wait with no deadline answers a type whose every variant it can answer, so Maki deletes its arm and the image builds; no MATCH in Habu, Zen, Tender or Maki on a no-deadline wait names timeout. First step: a census of every word that answers or takes ending in the four repositories (which can answer timeout, which cannot), then the type split settled from it: either ending loses timeout and the deadline waits answer option<ending>, or the no-deadline waits get a two-variant type of their own. It changes a public type callers follow, so the consumers' edits ride with it. Files: src/lib/process.f and the callers the census names. Verify: process tests rc 0; Maki, Zen, Tender build cold and warm. Depends: none. Ownership: src/lib/process.f. Claim: unassigned.
