---
title: Mirror cp!/ndict! PROT-GUARD onto Gforth stage0 + re-pin seal-absence
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:53:37.491259+02:00"
---

TFAM 2b-v follow-up to habu-range-reject-cp-e2eed7e4. The native engine now PROT-GUARDs the FORGET code-emit sinks cp!/ndict! (src/habu/habu1.f BCPSET/BNDSET): each guards the address the sink redirects a write to, so a post-seal cp!/ndict! into either sealed band fails closed (E-SEAL-VIOLATION). The Gforth stage0 mirror (bootstrap/cg/forth.fs BCPSET/BNDSET, ~lines 248-253) still lowers cp!/ndict! with NO PROT-GUARD, and test/seal-absence.f neither pins them as guarded sinks nor as absent surfaces, so the mirror could gain an unguarded emit-redirect silently. Risk is low (Gforth is a bootstrap-recovery host running trusted source, not untrusted LLM Forth; native bin/hb takes over immediately), which is why this is split from the native fix rather than bundled. Do: (1) mirror the PROT-GUARD emission onto forth.fs BCPSET (guard the CP value) and BNDSET (guard DBASE+n*DREC), matching the native latch semantics; (2) add the two guarded-sink presence pins to test/seal-absence.f (bump SAB-GUARD-PINS accordingly) so the mirror guards cannot be silently deleted; (3) prove parity via test/seal.f-style trap where the recovery path supports it, or document why the mirror cannot be forge-tested. DEPENDS: native fix landed in habu-range-reject-cp-e2eed7e4.
