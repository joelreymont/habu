---
title: Give the insn proof gate headroom
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T14:33:37.001488+02:00"
---

compiler-insn-proof runs 108-117s standalone on this host against SUITE-TIMEOUT-MS=120s — a knife-edge that TIMEOUT-UNDER-LOADs whenever anything shares the machine (observed three times on 2026-08-04; I adopted --pool-slots 3 as a standing workaround instead of fixing it, which is a shortcut). Same defect class as habu-stabilize-two-pool-763a7ec9: a wall-clock budget standing in for a logical property. Fix properly: either the suite declares its own measured budget (member-level timeout derived from its standalone cost times a stated load factor), or the pool runs proof-class members with a longer per-member budget, or the proof is split into parallel halves that each fit comfortably. No magic bump of the global constant — derive the number and write the derivation next to it. Acceptance: full gate green at default pool slots on a loaded host, three consecutive runs; the --pool-slots 3 workaround retired from the merge choreography.
