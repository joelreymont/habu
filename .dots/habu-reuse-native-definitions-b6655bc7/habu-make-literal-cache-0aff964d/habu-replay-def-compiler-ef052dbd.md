---
title: Replay definition compiler histories
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:37:44.266853+03:00"
closed-at: "2026-10-20T18:19:58.387717+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Automatic definition import skips the native compile path that advances module identities, context generation/release epoch and builder generation. Warm literal E2E differs by8 module serials and3 context generations while FILE imports already replay their histories. Record the four child-context increments at NCOMP after SESSION-READY, append them to definition metadata, and replay through one ordinary lease before literal/publication/checker install. Preserve nonreuse authority and literal order; old producers without both capabilities are ineligible. Keep actual full saved-image equality and stale-handle twin E2Es, with matching native/convergence and independent review.
