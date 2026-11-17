---
title: "Set FS:ERRNO when REMOVE-DIR throws"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T23:41:37.279577+01:00"
---

Problem: FS:REMOVE-DIR (src/lib/fs-mutate.f:199-201) says 'A throw sets FS:ERRNO' but calls the engine's rmdir primitive, which collapses every failure to a bare -1 (as :183-184 says of mkdir), and throws E-FS-IO without ERRNO-FAIL, so a caller reading FS:ERRNO after its throw reads a stale errno. MAKE-DIR binds mkdir through libc for this reason (:185-193). Found from Maki's review of its design directory sync, 2026-11-18. Acceptance: a REMOVE-DIR throw leaves rmdir's errno in FS:ERRNO (ENOENT for a missing directory, ENOTEMPTY for a full one), as MAKE-DIR's does, checked by fs-mutate-test.f. Depends: none. Claim: unassigned.
