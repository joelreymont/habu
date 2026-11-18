---
title: Delete the global rows of private primitives
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.881520+01:00"
---

Problem: a primitive with private rows (src/habu/prims.f EPPRIM:) keeps its global EPRIM: row, refused to checked code only by ETRUSTED-ONLY! and the marks: on the installed hb `' trust-sig!` answers "internal engine word", and `package OUTER public : ZZ ( -- n ) s" ZZQ" true namespace-record ; ;package` is refused E-CAP-TRUSTED, while a checked file's top-level line runs such primitives unrefused: `0 0 patch32` faults (E-MEMORY-FAULT), `3 callmap-set`, `99999999 ndict!`, `999999 prot-wid-add`, `0 99999999 record-facts!` and `4096 map-anon drop ptr-cell-mark` reach kernel stops; `def-open` and `' def-open drop` pass `tools/check.f -- --verify-only` (rc 0) and only the interpreter's run-time gate rejects them under --load. Also: the stripped-link check does not cover CHECK:RECORD-LINK-OCC and MARK-REP. Acceptance: no engine-internal primitive has a global row or record; on the product `' trust-sig!`, the ZZ program and each of those top-level lines answer E-UNDEFINED; ETRUSTED-ONLY! has no row left to mark; the engine builds itself and the full suite passes. Cases: /tmp/carl-spark/rf/pc-plan/keep/int-call.f (--load E-UNSAFE, --verify-only E-UNDEFINED), int-tick.f, int-body-tick.f, int-postpone.f, int-trusted-call.f, int-trusted-call2.f and prot-wid.f (--load rc 70, --verify-only rc 0) agree once fixed. Depends: habu-give-the-trusted-2e0a8538. Claim: unassigned.
