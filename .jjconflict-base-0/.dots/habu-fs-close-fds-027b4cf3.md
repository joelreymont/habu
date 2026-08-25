---
title: "FS: close fds across throwing walk callbacks; NUL-safe paths"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:05:23.122376+02:00"
---

Problem: lib/fs.f:460-478 FS-WALK-PATH executes the user quotation bare — a throw from q unwinds past up to 32 open directory fds and FS-FDS-RESET (:126-130) stamps slots -1 without closing: permanent fd leak (FS-THROW-WALK proves the close-on-error invariant is intended). lib/fs.f:480-484 WALK-FILES is not reentrant: nested walk clobbers global FS-DEPTH/FS-FDS/path state. lib/fs.f:309-314 FS-WRITE-BY-FLAGS throws E-FS-OPEN after open without closing FS-IO-FD (pre-check/open race). lib/fs.f:170-178 FS-PATHZ silently truncates at interior NUL — EXISTS?/WRITE-ALL operate on the truncated path (demonstrated: '/tmp\0XX' EXISTS? -> true); E-FS-PATH-UNSAFE exists (lib/errors.f:22) but is enforced only in lib/source.f:104. Expected fix: run q under catch routing nonzero through FS-THROW-WALK; active-walk latch with named throw (or stacked state); close fd before the re-check throw; FS-PATHZ scans for interior NUL and throws E-FS-PATH-UNSAFE. Acceptance: T{ }T: throwing callback -> fds all closed (probe via reopen count or /proc/self/fd delta), outer throw preserved; nested walk -> named throw; NUL path -> E-FS-PATH-UNSAFE for EXISTS?/WRITE-ALL/APPEND-FILE; race-path fd close covered by code inspection test where feasible. Files: lib/fs.f, lib/fs-test.f, lib/fs-mutate-test.f. Verify: bin/hb --load lib/fs-test.f lib/fs-mutate-test.f; walk consumers (lints) stay green. Depends: none. Ownership: lib/fs.f. Claim: unassigned.
