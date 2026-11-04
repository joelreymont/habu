---
title: Honor capture deadlines under output
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:22:47.597215+03:00"
closed-at: "2026-10-20T18:19:58.746738+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Independent review of two-growing-stream capture found that remaining time0 still calls poll0, which reports ready while output is continuous; the reader can keep allocating past its deadline. The existing one-growing-stream loop has the same control flow. Reproduce with a busy writer, enforce absolute expiry at the shared capture polling boundary, preserve retained bytes and child reaping, and rerun fixed/one-growing/two-growing timeout consumers. No raised limits or separate watchdog framework.
