---
title: Delete the global rows of private primitives
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.881520+01:00"
blocks:
  - habu-give-the-trusted-2e0a8538
  - habu-make-the-plain-6bd9c77b
---

Problem: a primitive with private rows (src/habu/prims.f EPPRIM:) keeps its global EPRIM: row, refused to checked code only by ETRUSTED-ONLY! and the marks: on the installed hb `' trust-sig!` answers "internal engine word", and `package OUTER public : ZZ ( -- n ) s" ZZQ" true namespace-record ; ;package` is refused E-CAP-TRUSTED. Acceptance: no engine-internal primitive has a global row or record; on the product `' trust-sig!` and the ZZ program answer E-UNDEFINED; ETRUSTED-ONLY! has no row left to mark; the engine builds itself and the full suite passes. Depends: habu-give-the-trusted-2e0a8538, habu-make-the-plain-6bd9c77b (each caller in a package). Claim: unassigned.
