---
title: "Close an object's descriptor when OBJECT cannot hold it"
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T03:06:15.663043+02:00"
---

Problem (L4 Fable review F3, 2026-11-07, by reading): src/habu/linker.f READ-AT opens the object with FS:OPEN-SIZED, whose descriptor READ-SIZED closes; if OBJECT's BUF:GROW throws first (E-BUF-BOUNDS for a size past the buffer's limit, e.g. a planted multi-GB file under the key's name), READ-OBJECT rethrows, the descriptor is never closed and the build fails E-FAILED. Acceptance: an object larger than OBJECT can hold counts as stale (the file compiles and its write replaces the object) and no descriptor stays open; FS has no public word to close an OPEN-SIZED descriptor today, so the fix either checks the size before growing or gives FS that word. Files: src/habu/linker.f (src/lib/fs.f if needed). Depends: L4 landed. Ownership: carl (lead). Claim: unassigned.
