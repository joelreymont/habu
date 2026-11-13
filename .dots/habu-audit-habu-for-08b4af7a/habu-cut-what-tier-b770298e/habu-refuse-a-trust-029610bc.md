---
title: Refuse a trust row over a primitive its caller may not reach
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T18:11:08.447797+01:00"
---

Problem: a user's `trust` row grants checked code outside a primitive's owner what the rows withhold: after `s" code-publish" s" ptr u8 n n --" trust` a checked caller outside NPUB compiles code-publish, and xref-retarget, does-record and run-rc likewise (any primitive with only a package-private row, or none); PRIM-TRUSTED-SYM? (src/core/checker.f) refuses only a global trusted-only row. wide-mark's global row (src/habu/prims.f, `EPRIM: wide-mark EPRIM;`) is not trusted-only though src/habu/link-band.f lists it as unsafe, so `: QA ( -- ) wide-mark ;` certifies anywhere. Acceptance: no `trust` row admits a checked caller to a primitive its rows do not give that caller; wide-mark refuses a checked caller outside trusted code; Zen, Tender and Maki still build. Depends: none. Claim: unassigned.
