---
title: Reject a declaration with no library in scope
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:22:45.358297+01:00"
---

Problem: a FUNCTION: whose scope selected no library prints only 'hb: uncaught throw code -3503' and exits 67, under --load and tools/check.f alike: src/lib/ffi-abi.f CHECK-SCOPE throws E-FFI-LIBRARY bare. Acceptance: both cases (no selection, a selection in another package or section) print a placed record with a help naming the selection words and exit 70, the second as a test in src/lib/ffi-test.f. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-libscope.
