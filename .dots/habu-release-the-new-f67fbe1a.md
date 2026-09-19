---
title: Release the new span when a grow refuses
status: open
priority: 3
issue-type: task
created-at: "2026-09-19T19:38:06.929874+03:00"
---

Problem: lib/byte-buffer.f:120-135 RESIZE-RAW allocates the new storage (cap STORAGE-ALLOC) and INSTALL-RESIZE then throws E-SPAN-RANGE from 'old buf LEN-RAW@ SPAN:TAKE' when the header's len exceeds cap, after the new span exists and before it is installed or released, so the refused grow leaks the mapping; the header comment says a failed grow never reaches INSTALL-RESIZE, which the span move c621b61f made false. Reachable only through a header whose len exceeds cap (SET-LEN forbids it; BUFT-GROW-OVER-READ writes it raw). Acceptance: the length check precedes the allocation, or the refusal releases the new span, and the comment states the order; BUFT-GROW-OVER-READ also asserts no mapping is left behind. Files: lib/byte-buffer.f, lib/byte-buffer-test.f. Verify: bin/hb --load lib/byte-buffer-test.f. Depends: none. Ownership: lib. Claim: unassigned.
