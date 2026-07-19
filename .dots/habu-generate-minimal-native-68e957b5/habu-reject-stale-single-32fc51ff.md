---
title: Reject stale waiver replacement
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T23:08:04.372573+02:00"
---

tools/kernel-perf-lint-core.f WV-STANDING-GE counts same-kernel+emitter waivers in the post-change registry, and STALE-CHECK rejects only when that count is greater than 1. That threshold deliberately discounts the newly added row itself, but the post-change registry has already lost a deleted older row. A replacement diff can therefore remove v2, add v1, leave only the added v1 in perf-rows.tsv, produce count=1, and pass. Changing the predicate to greater than 0 would instead reject every valid first v1 and v1-to-v2 increment because their added row is also present. Compare against authenticated pre-change state: either load a baseline registry or reconstruct deleted registry rows from the diff, exclude additions, and require every added version to exceed every pre-change same-kernel+emitter sibling. Use the same baseline semantics in the command path and test seam; keep duplicate identity and cross-emitter policy distinct. Add exact first-ever +v1, -v1/+v2, -v1/+v1, -v2/+v1, append-history, deletion/addition, different kernel/emitter, max-version and malformed-baseline tests; first and increment pass, same-version and downgrade fail. Verify perf registry/lint, PTX gate, host/filemap/dot/full gates. Files: tools/kernel-perf-lint-core.f and test; generic measurement ownership remains habu-bind-performance-evidence-e454f629.
