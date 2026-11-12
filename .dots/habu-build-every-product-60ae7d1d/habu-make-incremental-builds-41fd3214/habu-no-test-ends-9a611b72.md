---
title: No test ends a file with a callback in flight
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:52:05.884554+02:00"
---

Problem: a build made while a thread is inside a callback slot is refused, if at all, at the file's end through the live-task count (src/habu/linker.f END-CK :4897, LINKER:E-LIVE-TASK :49); design .jj-ws.noindex/jerry-wasm/.out/process-state-design.md 'Refusals after the change' marks as inferred that the count covers a callback in flight on a foreign thread. No test shows it, and no test shows that a task left EXPOSED (not activated) at a file's end is refused. Acceptance: a case of test/file-objects.f (or src/lib/ffi-callback-test.f through hb-build) builds a file whose top level leaves a foreign thread parked inside a callback: record what the build does, cold and again in the same out root; if it is not refused, the callback rows being scratch must be shown to leave the program sound, or the file's end refuses it. A fixture whose top level exposes a defined task and does not activate it is refused at that file's end by the live-task count, cold and again in the same out root. Depends: none. Claim: unassigned.
