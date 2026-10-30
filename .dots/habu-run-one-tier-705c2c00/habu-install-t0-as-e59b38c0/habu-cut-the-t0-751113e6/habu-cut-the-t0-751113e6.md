---
title: "Cut the T0 stack's comments to facts"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T20:50:10.129133+02:00"
closed-at: "2026-10-31T12:12:25.275847+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510 on the final candidate"
---

Problem: the T0 release stack (ulzktzrp..yuxsxwrk on master 6dccbb6a) adds about 500 comment lines, many written before the comment rules (now docs/forth-card.md '9 Comments') landed on master (the 'Cut ... comments to facts' commits of 2026-10-30): multi-line paragraphs that restate the code, cite file:line, or argue the design (e.g. definition.f SETTLE-UNIT's header, elaborate.f UNDEFINED-REFUSE and DO-TICK, t0.f DO-TICK, definers.f DEF-IMMEDIATE, habu2.f CAPTURE-IMMEDIATE, test/runtime-regression-test.f's 76 added lines). The lead's conflict merges kept that length. Acceptance: every comment the stack adds or changes states, in one or two lines, only what the code cannot state, cites words not lines, and carries no history; comments that restate code go. One commit on top of the stack after the ctrl fold-in. Files: the stack's changed files (jj diff --from master@origin --to <tip>). Verify: comment-only diff (no code line changes); error-code-lint and the build unchanged. Depends: habu-ctrl-refuse-malformed-65c26004. Ownership: comments in the stack's files. Claim: unassigned.

Round 1 reviewed: otnulolo ACCEPT; ca, cb, cd, ce fold-ins applied by the lead with the reviewer's exact text (strip.py: code identical); the t0.f DO-TICK hunk left otnulolo so kqzvlqvl's text stands. Round-2 item added: layout.f above package NCOMP-DISPATCH says "TIER-CELL selects tier 1. It sits at $358" ($358 is XT-CELL) and repeats "the two declaration-owner cells above it.".
