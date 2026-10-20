---
title: Make literal cache images byte-identical
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T15:58:33.155614+03:00"
closed-at: "2026-10-20T18:19:58.705817+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

On exact native bd295aba (engine84e65bac), native-file-auto-e2e -- literal passes actual literal output/loop folding but fails F18/F32 cold-vs-warm native image byte equality and F24 expected one definition hit. Real logs/artifacts retained at ~/.cache/tmp/habu-provisional-behavior.iZ2NXU. Reduce first differing bytes to emitted code/DATA/signature and responsible literal/cache/capture state; fix that layer without normalizing away differences or weakening equality/hit assertions. Existing real E2E is red; preserve raw pointer admission and actual literal sharing, then cold/warm runnable artifacts must be byte-identical. No broad new framework or unrelated engine rebuild.

The matching local-state engine d4da16ef (source ef013d26, map dc82fb7c) passes all seven unchanged native-file-auto selectors: diag, literal, segment, body-edit, body-reader, body-delimiter and body-incomplete. Each exits0 with test:ok and empty stderr, retaining exact cache counters, runtime results and cold/warm image equality. This resolves the focused scratch-history failures; final combined native and generation gates remain required.
