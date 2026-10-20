---
title: Drop redundant reader cache edit
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:39:35.742801+03:00"
closed-at: "2026-10-19T13:48:06.692349+03:00"
close-reason: Removed one redundant ordinary-callee edit/build and its invalid zero-skip assertion. The actual parsing-binding invalidation, runtime result, fresh-cache miss and byte equality pass in focused body-reader E2E; independent Astra review confirms remaining coverage.
---

Existing BODY-READER-CASE wrongly requires zero body skips after STEP changes from empty to 1 drop, although both compile to the identical nop/ret and ordinary callers depend on the binding contract rather than producer implementation. The same case already tests the distinct parsing-flag change with compile-step output, current runtime result and cold/warm equality. Remove the redundant intermediate ordinary edit/build and its change-detector assertion; preserve the real parsing invalidation and artifact checks. Run the focused body-reader E2E and independent review.
