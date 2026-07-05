---
title: Mirror cp!/ndict! PROT-GUARD onto Gforth stage0 + re-pin seal-absence
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:53:37.491259+02:00"
---

TFAM 2b-v follow-up to habu-range-reject-cp-e2eed7e4. The native engine now PROT-GUARDs the FORGET code-emit sinks cp!/ndict! (src/habu/habu1.f BCPSET/BNDSET): each guards the address the sink redirects a write to, so a post-seal cp!/ndict! into either sealed band fails closed (E-SEAL-VIOLATION). The Gforth stage0 mirror (bootstrap/cg/forth.fs BCPSET/BNDSET, ~lines 248-253) still lowers cp!/ndict! with NO PROT-GUARD, and test/seal-absence.f neither pins them as guarded sinks nor as absent surfaces, so the mirror could gain an unguarded emit-redirect silently. Risk is low (Gforth is a bootstrap-recovery host running trusted source, not untrusted LLM Forth; native bin/hb takes over immediately), which is why this is split from the native fix rather than bundled. Do: (1) mirror the PROT-GUARD emission onto forth.fs BCPSET (guard the CP value) and BNDSET (guard DBASE+n*DREC), matching the native latch semantics; (2) add the two guarded-sink presence pins to test/seal-absence.f (bump SAB-GUARD-PINS accordingly) so the mirror guards cannot be silently deleted; (3) prove parity via test/seal.f-style trap where the recovery path supports it, or document why the mirror cannot be forge-tested. DEPENDS: native fix landed in habu-range-reject-cp-e2eed7e4.

## Implemented (2026-07-05, seal-hardening worker)

- bootstrap/cg/forth.fs: PROT-GUARD widened from the single-band latch-length
  trick to the native two-band form (latch CBZ gate; band 1 crown jewels, band 2
  the protected-WID registry ADDRESS band via new PROT-REG-OFF/PROT-REG-LEN
  mirror constants), and moved above the primitive bodies so the earliest sinks
  can reach it. BCPSET guards the popped CP; BNDSET guards DBASE+n*DREC --
  identical wiring to native habu1.f.
- MIRROR BOUNDARY (census discrepancy 5): stage0 has no package system and no
  protected-WID registry, so ONLY the address-band range checks mirror. The
  WID-membership guards (publish guard C-STORE-DEF-NAME, LPROTWIDQ, LAOTWIDGATE
  AOT boot gates, EM-AOT-REGISTER-PROT-WIDS restore) cannot exist in stage0 and
  stay pinned ABSENT by test/seal-absence.f SAB-ADD-PROTWID; the band-2 cells
  are guarded dead storage in the seed engine.
- test/seal-absence.f: SAB-GUARD-PINS 8 -> 10 (additive pin only; red-first:
  "expected 10 got 8" before the mirror edit).
- Parity proven by forging the CHECK_ONLY-built seed engine directly (gforth
  bootstrap, HABU_BOOTSTRAP_CHECK_ONLY=1): cp! into band 1/band 2 -> rc 83,
  raw store band-2 count/last-byte -> rc 83, store $3D00/$1A0 -> rc 0, legit
  ndict! and cp!/ndict! FORGET round-trip -> rc 0.
