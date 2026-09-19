---
title: "Take the work span's reach from its producer"
status: open
priority: 3
issue-type: task
created-at: "2026-09-19T19:38:06.931236+03:00"
---

Problem: lib/fs-identity.f:67-71 COMPARE-IDENTITIES drops the NUM:alloc-byte-len it is handed and mints 'buffer WORK-BYTES SPAN:MAKE' from the constant; the reach is equal by construction to what SAMEFILE allocates (WORK-BYTES BYTES-ALLOC-LEN ... WITH-BYTES) but asserted rather than taken from the producer, because the private SPAN-ALLOC-LEN>N cast is MEM-only. Acceptance: FS obtains the span (or its reach) from the allocation it was handed, with no hand-stated constant at the crossing and no new cast; the fs-identity suite unchanged in what it asserts. Files: lib/fs-identity.f, lib/memory.f (only if the producer must publish the span). Verify: bin/hb --load lib/fs-identity-test.f. Depends: none. Ownership: lib. Claim: unassigned.
