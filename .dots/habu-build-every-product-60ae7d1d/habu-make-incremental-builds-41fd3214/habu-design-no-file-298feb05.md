---
title: "Design: no file object carries process state"
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T12:43:40.325040+02:00"
---

Problem: a file object carries the plain cells its file's top level stored, while the hook that cleans them or refuses the build (IMAGE-LIFECYCLE:REGISTER, run by PREPARE from app-end.f) exists only in the process that ran the top level. Shown: the hook count (c1efdd1a), the engine-path cache (a program printed the builder's bin/hb as its own path; c1c72c21), task.f's main park record (program dies -3606, rc 67), and a top-level TASK:NEW that a cold build refuses (rc 74, task at capture) while a warm build passes and its program dies signal 11. Per-owner scratch marks (DECISIONS 2026-11-09 'values a linked object replays') cover cleaning, not the refusal, and nothing checks a new owner. Joel 2026-11-10 on the hook-count fix: 'what is this fixing? is it fixing the cause or the symptoms?' Acceptance: a design, put to Joel one question per item the long-term rule cannot settle (a shared scratch declaration's name is one), under which no file object carries process state, a warm build refuses where a cold one does and a later owner cannot silently get it wrong; then its lanes filed as dots. Files: read-only. Verify: the design cites path:line for what exists and for the program path reaching each public item. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-owned (read-only design; brief .jj-ws.noindex/jerry-wasm/.out/briefs/design-process-state.md).

Review of c1c72c21 (2026-11-10), for the shared scratch declaration: EID-KEY (64), EID-MAPPED (1272) and EID-OPENED (176) in src/lib/engine-id.f are marked by sizes that are whole cells only by value; the one shared word refuses a byte count that is not a whole number of cells, so a mark cannot reach the next definition.

Review of "Set HOOKED after its registration" (2026-11-10): an owner that sets its registered flag after IMAGE-LIFECYCLE:REGISTER returns (the rule at PREPARE's comment; context.f HOOK-CAPTURE, task.f TASK-ARM-SWEEP) is left with the flag 1 and no hook if PREPARE runs the hook between the two on another thread; PREPARE runs hooks without the lock. No path in the tree does this (compiling on a task's thread exits 79). The design removes the flag-and-hook pair or says why the race cannot occur.
