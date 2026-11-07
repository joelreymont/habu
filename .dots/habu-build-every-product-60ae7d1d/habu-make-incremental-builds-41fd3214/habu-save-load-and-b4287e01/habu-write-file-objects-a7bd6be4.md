---
title: Write file objects without a full sync
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.278486+02:00"
closed-at: "2026-11-09T21:04:03.069226+02:00"
close-reason: "landed with the arm64 linker build: hb built by the tree rebuilds itself byte for byte (gen 1 = gen 2, maps equal), full suite 269 of 269 green with pg"
---

Problem: L2a writes each object through FS:ATOMIC-WRITE-FILE, which syncs with F_FULLFSYNC (about 14 ms per object, about 2 s of a 157-file cold build), yet a missing, short or torn object already counts as stale and is rebuilt (design-fobj.md section 2 item 10), so the sync buys nothing. Acceptance: objects are written to a sibling and renamed into place without a sync; measured cold-build time before and after. A new FS word, if needed, goes to Joel first. Files: src/habu/linker.f, src/lib/fs.f only if a word is needed. Verify: test/file-objects.f; cold build time of its fixture and of hb-build at step 5a. Depends: L2a. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-c5a2

Open: without a sync, a crash can leave an object of the right size whose later blocks are zeros. The lane must show that such an object reads as stale (its magic, key or INPUT fail), or add a hash of the object's bytes that the reader checks; a new object field is a saved-format change and goes to Joel first.

Done in carl-c5a2 @ mtmtpxkv (uncommitted): WRITE-OBJECT writes through FS:REPLACE-STAGED (a RESERVE-SIBLING sibling FS:WRITE-ALL fills, renamed over the object, no sync, removed on a throw); no new FS word. The open question is settled by the object's own SHA-256, checked before anything else (dot habu-check-an-obj-bc7a0eca): an object a crash cut short or zero-filled fails it and its file compiles. Timing on idle spark (census of hb's startup rows at LIMIT 69, hbe, /tmp/carl-spark/time3.sh, two alternating runs): cold wall 4.40/4.40 -> 3.74/3.82 s with user 3.74/3.75 -> 3.66/3.73 s (the wait outside CPU, 0.58 s for 63 objects, is gone); warm unchanged 1.96/1.95 -> 1.95/1.92 s; cold and warm out-equal on both sides. Mac census obj-equal and out-equal with no sibling left in obj/; test/file-objects.f ok.
