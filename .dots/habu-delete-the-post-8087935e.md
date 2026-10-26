---
title: Delete the post-allot zero loops
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T15:04:37.985863+02:00"
---

allot now zeroes reused dictionary space (5d046858), so the zero loops after allot are redundant: LBUF-ALLOT (src/core/layout-buffer.f:218), DBUF-ALLOT (:688), include.f:962, task.f:863, ffi-abi.f:318, ffi-callback.f:120, aot-window-latch.f:137. LBUF-ZERO stays for the DYNAMIC-BUFFER mapped-region callers. Workspace .jj-ws.noindex/jerry-zero, lead inline. Claim: jerry.
