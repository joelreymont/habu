---
title: "An atomic write's failure hides whether it replaced the target"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.071638+01:00"
---

Problem: src/lib/fs-mutate.f's atomic writes (FS:ATOMIC-WRITE-FILE :734, FS:ATOMIC-WRITE-PRIVATE :741) throw E-FS-IO both before the rename and after it, when the directory sync fails (:632, :694), so a caller cannot tell whether the target holds the new bytes; kiba's install marker reads the file back to find out. Reproducer: an LD_PRELOAD shim failing fsync with EIO on the target's directory. Acceptance: a failure after the rename is told apart from one before it, by a distinct code; a test with the injected directory sync failure holds it. Depends: none. Claim: unassigned.
