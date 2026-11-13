---
title: Delete the internal, trusted-only and owned marks
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.771402+02:00"
blocks:
  - habu-move-engine-internal-fc2ba551
---

Problem: three marks say "not for everyone" and overlap: DNAME-INT (hidden engine word), the trusted-only flag (src/habu/prims.f ETRUSTED-ONLY!; src/core/checker.f PRIM-TRUSTED-ONLY!) and DNAME-OWNED (owner-only). With internal words private to the system package and stripped from the product, none of them carries anything privacy does not.
Ruling (Joel, 2026-10-08): none survive; privacy plus the build's strip only.
Acceptance: DNAME-INT, DNAME-OWNED and the trusted-only flag, with every reader and writer (src/core/internal-mark.f's marking, the refusals "internal engine word" and "trusted-only tick" in src/habu/interpreter.f, the AOT capture's mark field), are deleted; a hidden word is simply absent from the product.
Files: src/habu/layout.f (DNAME-* bits), src/core/internal-mark.f, src/habu/prims.f, src/habu/primitive-registry.f, src/core/checker.f, src/habu/interpreter.f, src/compiler/native/dict.f, tests naming the marks (test/c2-memory-entry.f expects "internal engine word").
Verify: full suite on the built hb; product probes for formerly internal names answer E-UNDEFINED.
Depends: the dot moving internal primitives into the system package. Ownership: lead. Claim: unassigned.

Acceptance case: inside a build (LINKER:WITH), after the product's src/core/internal-mark.f has run, the startup driver's top-level REQUIRE-BOOT-FREEZE (src/core/include.f; tools/hb-build-core.f queues it) is refused "internal engine word" (rc 70), so a build cannot finish hb's startup list. With the marks deleted, the build's driver completes the list's freeze step.

Acceptance case: a linked hb carries no DNAME-INT mark, so the strip has to make an engine variable absent instead: source cannot store into the reserved view family `-1 C2-READ-FAM !` (src/core/checker.f:1725, a global variable read by checker.f, type-family.f and sumtype.f; the suite once asserted "source cannot replace the reserved view family identity"). C2-READ-FAM needs a package's private section before the strip can remove it.

Acceptance case: a cold build of hb's startup files inside LINKER:WITH ends at src/core/internal-mark.f with LINKER:E-STORE (-9691): its marking changes the flags of records earlier files made, which the linker refuses for any file. With the marks deleted, the file changes no earlier record.

Finding: DNAME-INT's tick refusal is load-bearing, so the marks cannot go before the privacy move (habu-move-engine-internal-fc2ba551): a hidden word's tick dies "internal engine word" only because of DNAME-INT. A word defined with the check off runs when called at top level and when ticked and executed: on the installed hb (2026-11-13) both `check@ 0 set-check : W ( -- n ) 5 ; set-check  W . cr` and the same with `' W execute . cr` print 5, rc 0.
