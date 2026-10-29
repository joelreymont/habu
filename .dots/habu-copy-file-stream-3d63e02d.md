---
title: COPY-FILE-STREAM leaks its source descriptor when the destination is refused
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:34:31.913509+02:00"
---

Problem: src/lib/fs-mutate.f COPY-FILE-STREAM (~:485) opens the source descriptor, then COPY-OPEN-DST (~:447-450) runs EXISTS? and >PATHZ2 on the destination with nothing that closes the source if they throw; a destination holding a NUL (E-FFI-NUL after the C-string lanes, E-FS-PATH-UNSAFE before) or longer than PATH-CAP (E-FS-PATH) leaves one descriptor open per call (probe: stream copy to a NUL destination, fds-leaked 1; found by the L3 review, present on the parent). Acceptance: no descriptor outlives a refused copy on any throw path (judge the destination before opening the source, or close the source on the throw path); a regression row in fs-mutate-test.f that counts open descriptors across a refused stream copy, failing on the current code. Files: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f. Verify: bin/hb --load src/lib/fs-mutate-test.f; tools/check.f on fs-mutate.f. Depends: habu-c-str-l3-0c9f0875 (lands first; the fix is its own commit). Ownership: FS. Claim: unassigned.
