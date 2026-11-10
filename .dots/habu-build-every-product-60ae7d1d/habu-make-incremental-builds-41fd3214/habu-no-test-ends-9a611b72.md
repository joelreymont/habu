---
title: No test ends a file with a callback in flight
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:52:05.884554+02:00"
---

Problem (2026-11-10): the task capture sweep refused a build made while a thread was inside a callback slot ('callback slot in flight at capture', tested by the child case 'captures' of src/lib/ffi-callback-test.f). The sweep and that case are deleted; design .jj-ws.noindex/jerry-wasm/.out/process-state-design.md 'Refusals after the change' says the refusal stays, moved to the file's end through the live-task count (src/habu/linker.f PIECE-END, LINKER:E-LIVE-TASK), and marks as inferred that the count covers a callback in flight on a foreign thread. No test shows it. Acceptance: a case of test/file-objects.f (or src/lib/ffi-callback-test.f through hb-build) builds a file whose top level leaves a foreign thread parked inside a callback: record what the build does, cold and again in the same out root; if it is not refused, the callback rows being scratch must be shown to leave the program sound, or the file's end refuses it. Files: test/file-objects.f, test/fixtures/file-objects/, src/lib/task.f. Verify: the case. Depends: the process-state feature landing. Ownership: unassigned. Claim: unassigned.
