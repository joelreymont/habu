---
title: Move MKDIR-MODE and MKDIR-ONE onto the mkdir primitive
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:08:45.287115+02:00"
---

Problem: src/lib/fs-mutate.f binds libc mkdir as MKDIR-CALL (:201) and its comment says the engine primitive loses errno, which is false: the engine mkdir primitive saves errno (BMKDIR src/habu/habu1.f:3193-3200, kernel-x64.f:1430 end in FS-SYS-PUSH). Found by the ERRNO review. Acceptance: MKDIR-MODE (:206) and MKDIR-ONE (:328) call the primitive, MKDIR-CALL and its comment go, FS:ERRNO after a refused mkdir is unchanged. Files: src/lib/fs-mutate.f. Verify: fs-mutate-test, fs-cause-test on macOS and spark. Depends: ERRNO$ (carl-errno voqvlkmo) lands first. Ownership: unassigned. Claim: unassigned.
