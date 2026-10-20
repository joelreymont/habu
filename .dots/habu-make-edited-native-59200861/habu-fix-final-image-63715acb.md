---
title: Fix final image boundary checks
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.225520+03:00"
closed-at: "2026-10-20T18:19:57.511239+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Final a5f93a78 registry fails whitebox-engine-key: closure lacks boot prefix and edited prefix leaves same artifact key; internal-word-gate18 assertions; capture-tree timeout-subject readiness/overflow timeout under load. Preserve real external key invalidation, internal checked-call refusal and meaningful process timeout behavior. Reproduce on final source24e30532, distinguish related mechanisms and genuine load interruption; do not raise deadlines or keep obsolete name/count checks. Independent review and existing affected E2Es before acceptance. Captured log under carl-incremental-native.brg87d.
