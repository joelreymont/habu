---
title: Fix explicit FILE profile refusals
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.158153+03:00"
closed-at: "2026-10-20T18:19:57.417551+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Final fixed-point a5f93a78 native gate fails native-file-unit, native-file-ffi and native-file-explicit-reclaim with E-NUNIT-PROFILE(-8597); explicit reclaim has19 assertions. Reproduce on final source24e30532 with matching engine, identify the first responsible export/import profile mismatch, preserve semantic/context/compiler/root/pointer refusal invariants, and correct the boundary or proven stale fixture. Existing real-load rows must pass without weakened assertions or raised deadlines; independent Astra review is required. Main captured log: /Users/joel/.cache/tmp/carl-incremental-native.brg87d/native.stdout.log.

Reclaim's 19 failures reproduce on 6d542249 and a5f93a78. Correcting its stale top-level controls, lowercase serialized checker-name expectations and newline number output makes the existing row pass; the reused address is cleared after comparison so stripped-image raw pointer refusal stays intact. All original origin counts remain. Corrected repeatable artifacts: /Users/joel/.cache/tmp/carl-explicit-cache/native-file-explicit-reclaim-63902268769500-0. Unit/FFI reject a stale early-origin scratch journal left by the compiler bundle's temporary early-install probe: two fresh whitebox processes retain the same 203-byte journal at 4621811712, and traversing it faults. The fixture now drains its own target journal before restoring the full hook and forgetting its temporary definitions. Product code and FILE refusals stay unchanged; replacement bundle qualification and independent review remain.
