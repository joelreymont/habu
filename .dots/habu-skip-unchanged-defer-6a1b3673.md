---
title: Skip unchanged defer state without consuming rollback history
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T22:32:28.053356+03:00"
---

Tracked B2 `hb-B2-origin-tracked` (SHA256
`cd88273b9c27dd424c934cddee5bf010ede646d6965eab344b19e933fd9132c3`)
exits 76 with `checker: defer table full` while loading
`tools/compile-floor.f`. Its cold history has 3,918 valid 16-byte rows in the
65,536-byte table: 3,823 are false-to-false stores, and only 95 change state.
An ordinary define/undefine cycle appends another false row. This is redundant
history from teardown and checker-owner transfer, not a damaged restore pointer.

`DFER-ADD-SYM` now asks the existing rollback-aware lookup before appending.
Unchanged boolean state consumes no row. Real transitions, terminators, cache
updates, capacity and saved rollback offsets are unchanged. Existing images are
not compacted. All callers produce canonical typed booleans; both stored and
cached reads also canonicalize their boolean result.

`test/defer-history.f` loads current checker source through the actual fresh-owner
handoff and public dictionary library. The child checks bulk ordinary signature
transfer, every history row's state change, 5,000 false and true no-ops each,
primed-cache scope/candidate/prefix-marker rollback, 5,000 public
define/call/undefine cycles, and real defer bind/call/signature behavior with two
rejected candidates. The registered driver passes on tracked B2 (`test: ok`);
the same child against the original checker exits 76 with
`defer history contains unchanged state`.

Evidence: `/home/joel/.cache/cedar-defer-owner-p9qcd055/`. Run the focused driver
from this source tree with `HABU_UNDER_TEST` set to the engine being checked.
Independent review and rebuilt-engine compiler-floor/full-suite checks belong to
the integration owner. This does not change the separate snapshot address-table
capacity issue.
