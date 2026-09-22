---
title: "Run IMAGE-LIFECYCLE:PREPARE before the stripped link captures data"
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T16:48:07.897443+03:00"
---

Measured by aspen at 75c797be: the Tender server `build --server --stripped` refuses with first offender word=MUNMAP-XT data-off=11909600 value=281473074573184 - lib/task.f:193, a variable holding the linking process dlsym address of libc munmap stored by LOAD-SYMBOLS (task.f:224). RESET-SYMBOLS (task.f:213) zeroes it and its seven siblings (PTHREAD-CREATE/JOIN/EXIT-XT, SCHED-YIELD-XT, MUTEX-INIT/LOCK/UNLOCK-XT) but is armed only through IMAGE-LIFECYCLE:REGISTER, and IMAGE-LIFECYCLE:PREPARE (lib/image-lifecycle.f:70) runs on the snapshot path (src/habu/snap.f:51) and not on the stripped AOT path (no PREPARE in tools/hb-build.f, aot-lib.f, aot-closure.f). With PREPARE appended to the generated loader the server links rc 0 (73 s, 2,031,808 B) with no refusal: every offender is a lifecycle registrant process-local state. Acceptance: the stripped link runs the lifecycle PREPARE before it captures persistent data, exactly where snap.f does, so no loader needs the lever; a fixture whose program caches a foreign address in a persistent cell through a lifecycle registrant links clean and the restored image reloads it; the Tender server stripped link is clean. Ownership: hazel.
