---
title: Skip classified zero DATA windows
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:08:21.338715+03:00"
closed-at: "2026-10-20T22:36:53.530291+03:00"
close-reason: Two bounded zero cells permit eight scan starts to be skipped only when current pointer predicates admit zero. Existing raw and declared pointer E2Es plus all-alignment, short-tail and mapped-zero probes pass; independent Astra review clear. Combined stripped tests pass. Single matched root and leaf user CPU fell 9.1 and 3.8 percent beyond bounded lookup.
---

Measured Dots final AOT scan has5918896 zero-valued8bytewindows outof5928321 (99.841percent), including57 zero runs at least16bytes and a5.1MB run. Classify zero once with the existing text/mapped predicates, then skip only overlapping windows proven entirely zero by bounded reads when zero is admissible. Every nonzero/edge window keeps bytewide checking, declaration-cursor semantics and first-refusal order. No value-range heuristic, assumed null-map policy, changed admission or format. Extend existing real pointer-boundary E2E onlyfor distinct skipped-edge/alignment gaps beforeimplementation; measure actualCLI edit CPU and outputequivalence plus independentreview.
