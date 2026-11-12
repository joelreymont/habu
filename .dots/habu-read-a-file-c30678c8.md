---
title: "Read a file's bytes at an offset"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T10:49:31.123889+02:00"
---

Problem (Maki, tim 2026-11-12): FS has no read at an offset (src/lib/fs.f:535-587: READ-ALL, OPEN-SIZED, READ-SIZED), so Maki's planned ranges tool would read a whole mesh or scene file to answer part of it. Acceptance: FS:READ-AT ( ptr u8 n n ptr u8 n -- ): path, offset, destination, length; exactly that many bytes or E-FS-IO; its focused test reads a range and rejects one past the end. First caller: Maki PROJECT:RANGE (design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/design-view.md section 3). Depends: none. Claim: unassigned.
