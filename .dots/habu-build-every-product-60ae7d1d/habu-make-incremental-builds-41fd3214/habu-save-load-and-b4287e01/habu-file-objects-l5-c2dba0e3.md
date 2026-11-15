---
title: "File objects L5: a warm build after an edit"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.556348+02:00"
closed-at: "2026-11-07T04:41:40.167111+02:00"
close-reason: "Landed in 'Rebuild every file after an edit': a warm build links every file or compiles every file, decided at its first file from the load list; objects carry a prefix SHA-256 and a checksum. Four suites rc 0; Fable feature review accepted after its fold."
---

Problem: after L4 a warm build links only when nothing changed before a file: an edit to any file but the first is LINKER:E-STALE on every later build until the out root is deleted; an earlier file that recompiles can make a later linked file's object refuse mid-link (78, then E-FAILED, every later build; L4 review probe /tmp/carl-rev-l4w/probe-c.f); a flipped or cut byte in an object can link silently.

Design (lead's decision 2026-11-07 under Joel's night words; .out/carl-file/design-l5.md "Lead's decision"): until a file can compile after a linked one (L6), a warm build decides before its first file whether it links every file or compiles every file.
- L5-I1 "Make an object's freshness cover the files before it and its own bytes": INPUT gains a prefix SHA-256 over the key and source SHA-256 of every file whose load started before this one in its build (INPUT-BYTES 40 -> 72; RUN-FILE keeps the running list); the header carries a SHA-256 over the object's bytes after it, checked first in HEADER-CK, a mismatch makes the file stale (dot habu-check-an-obj-bc7a0eca folds in).
- L5-I2 "Decide a warm build's mode from its load list": RUN writes `<out>/loads` (magic HABULOAD; per row key, path, root) in its finally cleanup; PROGRESS step `listed` at OPEN walks it, checking each listed object with FRESH?; modes `open compiling linking`; a file stale after a linked one (only a file the list does not name) throws E-STALE with the text "a file of this build has no fresh object after a linked file; the next build compiles every file".

Acceptance: test/file-objects.f passes whole with L5-T's cases (commit "Test a warm build after an edit", lane carl-l5t): `stale: compiled 2`, `tail: compiled 2 2 0`, `share: compiled 2 2 0`, A3 (a changed string in an object) and A4 (a cut object, dot habu-test-that-an-1d571003) compile 7 and write the cold program byte for byte, B2 `compiled 8`, D's objects byte-identical to A's. Files: src/habu/linker.f, test/fixtures/file-objects/build.f, test/file-objects.f. Verify: four suites (linker arm64-link-data arm64-link-run file-objects) rc 0. Depends: L4 (landed). Moved out: nested-load pieces and request rows (L2e); dependencies by request, reference and spelled name, laid strings, wordlist counter and loader state, which replace the prefix (L6); the restart (rejected). Ownership: carl (lead), worker lanes. Claim: agent=worker workspace=.jj-ws.noindex/carl-l5t (L5-T), then L5-I1 and L5-I2 lanes.
