---
title: Pass the last written buffers as span, not ^u8 n
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:37:49.179441+01:00"
---

Problem: four words still take a buffer they write, or a byte range, as ^u8 n, which Joel ruled out (2026-11-19: str or vector): src/lib/process-tree.f LOAD-INTO ( ^u8 ^u8 n ^n -- load ), src/lib/span.f LEAD ( ^u8 n -- n ), src/core/arena-pages.f REG-DATA-SPAN? ( ^u8 n -- bool ) and src/habu/intern.f OWNER-INIT ( ^u8 ^u8 n -- ); the checker does not reject the shape, so they passed the split-str conversion. Acceptance: each takes a SPAN:span<u8> where it writes or a str where it reads (or a count that is not a byte range is typed as such), callers follow, and the engine builds. Depends: none. Claim: unassigned.
