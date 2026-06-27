---
title: "Eval matrix: live model generator + sampled pass@k"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T12:11:42.947866+02:00"
---

Initial stochastic pass@k data was collected on 2026-06-27 using independent Claude subagents as the generator and is recorded in docs/eval-triton.md. This dot is now the durability follow-up, not a blocker for that recorded snapshot: commit a checked Habu grader/generation harness so future pass@k runs do not depend on /tmp scripts or ad hoc subagent transcripts. Coordinate with habu-commit-checked-habu-a8ab5f56 and habu-re-run-habu-20318fcf.
