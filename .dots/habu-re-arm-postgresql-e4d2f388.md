---
title: Re-arm PostgreSQL image cleanup after each capture
status: active
priority: 1
issue-type: task
created-at: "2026-09-20T02:02:45.051781+03:00"
---

lib/db/pq.f FORGET-HANDLES retires connection/result slots but does not reset REGISTERED; IMAGE-LIFECYCLE:PREPARE consumes the hook, so a later CONNECT skips registration. Source finding from the FFI lifecycle reduction (9c283543). Claim: alder, .jj-ws/alder-lifecycle-followups; Hazel says fix after the current stripped-lifecycle/FFI/deadline stack. Measure repeated connect/prepare cycles, reset the registration at its owner, and prove subsequent captures retire slots and release arenas without stale process pointers. Keep scope to this lifecycle defect and its fixture.

Claim moved to .jj-ws/alder-pq-recapture, based on the reviewed lifecycle/FFI/
deadline stack 27219507. A public CONNECT/prepare/CONNECT/prepare probe gives
hook counts 2,1,1,1 on its private gen3 (the one retained hook is FFI's), proving
the DB hook is not rearmed. Probe: /tmp/alder-capture-persistent/pq-hook-probe.f.
Extend the existing live IMAGE-CASES to run twice, pinning both stale connection
and stale result refusals after each preparation.

The repeated live case fails assertions 67 and 68 before the fix: both the stale
connection and stale result remain usable (0 instead of E-HANDLE -9256 and
E-CLEARED -9254). FORGET-HANDLES now clears REGISTERED after successful retirement
of every slot, letting a later CONNECT arm its one-shot cleanup again.

Validation: the full pq-test passes against a private ephemeral PostgreSQL
server started by test/db/pg-fixture.sh; the ordinary db-pq registry row also
passes with its documented no-server skip. Private gen3 host 31be3fb0, isolated
tree and HOME/HB_TMP at /tmp/alder-pq-recapture; before.log and after.log record
the live-server regression. Astra review clear. No full gate run; Hazel chains
this after the persistent lifecycle/FFI prerequisite.
