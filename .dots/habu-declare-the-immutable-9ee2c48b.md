---
title: Declare the immutable data extents an AOT image may carry
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T14:05:06.874116+03:00"
---

Problem: checked Forth converts a pointer to a number only by subtracting NULL-PTR, and any engine-resident helper that reads engine static DATA compiles an absolute address the stripped AOT linker refuses as data outside the restored span (aot-closure.f DATA-ADDRESS!). The null cell was moved to a base-relative header offset (dab2c17e) which fixes that one caller for good, but the next helper that reads engine static data hits the same refusal, and the linker has no way to carry such a datum because a bare DATA address carries no extent. Acceptance: an engine-declared registry of immutable data extents (address, length, owner) an AOT image may carry, DATA-TARGET relocating a declared extent into the image the way NSTR rows are re-interned, refusal by name for an undeclared engine-DATA address as today, a fixture that declares one extent and links a stripped program reading it, and the refusal fixture kept. Files: src/habu/aot-closure.f, src/habu/aot-lib.f, the declaring side in src/core (a small registry), test/gate-aot-positive-lib.f. Verify: gate-aot-positive and stripped-* on a rebuilt engine. Depends: none (design, after release). Ownership: hazel line. Claim: unassigned.
