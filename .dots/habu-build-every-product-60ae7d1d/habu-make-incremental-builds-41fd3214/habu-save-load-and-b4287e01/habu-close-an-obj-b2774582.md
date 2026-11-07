---
title: "Close an object's descriptor when OBJECT cannot hold it"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-07T03:06:15.663043+02:00"
closed-at: "2026-11-07T05:10:11.116103+02:00"
close-reason: "READ-WHOLE also counts E-MEM-MAP as unreadable, so an object, load list or listed source too large to map counts as unreadable; READ-INTO closes the descriptor in a finally cleanup (READ-DONE) until FS:READ-SIZED owns it. test/file-objects.f A5 (macOS) grows count.f's object to 2^48 bytes and the warm build compiles all seven."
---

Problem (L4 Fable review F3, 2026-11-07, by reading): src/habu/linker.f READ-AT opens the object with FS:OPEN-SIZED, whose descriptor READ-SIZED closes; if OBJECT's BUF:GROW throws first (E-BUF-BOUNDS for a size past the buffer's limit, e.g. a planted multi-GB file under the key's name), READ-OBJECT rethrows, the descriptor is never closed and the build fails E-FAILED. Acceptance: an object larger than OBJECT can hold counts as stale (the file compiles and its write replaces the object) and no descriptor stays open; FS has no public word to close an OPEN-SIZED descriptor today, so the fix either checks the size before growing or gives FS that word. Extended (L5 Fable feature review, 2026-11-07, by reading): READ-AT became READ-INTO (src/habu/linker.f :1436 at L5's landing), now shared by the object (READ-OBJECT), the load list (WALK) and each listed source (WALK-ROW); READ-WHOLE catches only E-FS-OPEN, E-FS-STAT, E-FS-IO, E-FS-PATH and E-FFI-NUL, so a buffer or memory throw from BUF:GROW on any of the three also leaves the descriptor open and fails every later build until the file is removed. src/lib/build.f :105 and src/lib/codesign.f :312 already close an OPEN-SIZED descriptor themselves (`FD>N close` in a `finally` cleanup), so FS needs no new word; tools/native-definition-cache.f :503 leaks the same way but is machinery being replaced. Acceptance also: an object, list or listed source too large to hold counts as unreadable, with no descriptor left open. Files: src/habu/linker.f (src/lib/fs.f if needed). Depends: L4 landed. Ownership: carl (lead). Claim: agent=worker workspace=.jj-ws.noindex/carl-fclose.
