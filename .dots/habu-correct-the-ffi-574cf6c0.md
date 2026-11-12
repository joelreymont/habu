---
title: "Correct the FFI capability rows' comment in prims.f"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T23:39:31.865126+02:00"
---

Problem: src/habu/prims.f:1099-1104 says the global ETRUSTED-ONLY! row is kept for 'the TRUSTED: bodies outside FFI that still call it (src/lib/fs-identity.f, src/lib/f64-text.f)'. Neither file has a TRUSTED: body: fs-identity.f declares its foreign calls with FUNCTION:, and f64-text.f has no foreign call once habu-format-a-binary64-477e6a35 lands. Whether any TRUSTED: body outside package FFI still calls ffi-call-bounded and its three siblings is not traced (src/lib/task.f, error.f and ffi-callback.f name ffi-call). Acceptance: the comment names the callers that exist, or drops the clause when there are none; if no caller outside FFI is left, say in the report whether the global row is still needed, as a finding for a new dot, without changing it here. Files: src/habu/prims.f. Verify: a search of src for each of the four primitives' callers, listed in the report; a comment-only edit needs no build. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: none reported it; found by the review of 477e6a35.
