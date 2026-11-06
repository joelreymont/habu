---
title: Delete the internal, trusted-only and owned marks
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.771402+02:00"
blocks:
  - habu-move-engine-internal-fc2ba551
---

Problem: three marks say "not for everyone" and overlap: DNAME-INT (hidden engine word), the trusted-only flag (src/habu/prims.f ETRUSTED-ONLY!, 19 rows; src/core/checker.f PRIM-TRUSTED-ONLY!) and DNAME-OWNED (owner-only). With internal words private to the system package and stripped from the product, none of them carries anything privacy does not.
Ruling (Joel, 2026-10-08): none survive; privacy plus the build's strip only.
Acceptance: DNAME-INT, DNAME-OWNED and the trusted-only flag, with every reader and writer (internal-mark.f's marking, the prompt/tick refusals "internal engine word" and "trusted-only tick", the AOT capture's mark field), are deleted; a hidden word is simply absent from the product.
Files: src/habu/layout.f (DNAME-* bits), src/core/internal-mark.f, src/habu/prims.f, src/core/checker.f, src/habu/aot-capture.f, src/habu/habu2.f (prompt/tick refusals), tests naming the marks.
Verify: full suite on the built hb; product probes for formerly internal names answer E-UNDEFINED.
Depends: the dot moving internal primitives into the system package. Ownership: lead. Claim: unassigned.

Acceptance case (S5c census 4, 2026-11-06, /tmp/jerry-ckrows/census4.md G): inside a build (LINKER:WITH), after the product's src/core/internal-mark.f has run, the startup driver's top-level REQUIRE-BOOT-FREEZE is refused "hb: internal engine word" (rc 70), so a build cannot finish hb's startup list. With the marks deleted, the build's driver completes the list's freeze step.
