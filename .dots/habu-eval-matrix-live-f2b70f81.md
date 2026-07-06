---
title: "Eval matrix: live model generator + sampled pass@k"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T12:11:42.947866+02:00"
---

Initial stochastic pass@k data was collected on 2026-06-27 using independent Claude subagents as the generator and is recorded in docs/eval-triton.md. This dot is now the durability follow-up, not a blocker for that recorded snapshot: commit a checked Habu grader/generation harness so future pass@k runs do not depend on /tmp scripts or ad hoc subagent transcripts. Coordinate with habu-commit-checked-habu-a8ab5f56 and habu-re-run-habu-20318fcf.

## Audit refresh (2026-07-06, head 1eb3b5d3)

The grader half is committed: maki/eval-author.f GRADE-AUTHOR grades candidate
kernel source against its task from the committed tree (maki/eval-test.f rc 0),
retiring the /tmp grade scripts. Remaining scope: only the GENERATION arm — a
live model generator stays an external documented boundary per Habu Only; track
wiring sampled pass@k runs to the committed grader (with habu-re-run-habu-20318fcf
for the corrected ROW-STORE re-run).
