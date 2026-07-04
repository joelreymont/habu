---
title: "TFAM 2b-v(f): boot integration test for protected WIDs"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:34:10.910432+02:00"
---

Split from habu-tfam-2b-v-0a0e48a9 per the 2b-v design report (2026-07-04): candidate-boot assertion that a protected record with wid>255 survives AOT seed capture/restore with full u32 WID, WIDN advanced past registry entries, registry persisted, and a user forge cannot publish into it. Needs a live friend producer: either item 8's generated-ctor-package creation calling PROT-WID-ADD (habu2.f C-PACKAGE-ALLOC-WIDS ~:2878) or a metabuild-only hook in src/habu/stdin.f CAPTURE-REPL (:84) that PROT-WID-ADDs a synthetic wid before ACAP-CAPTURE; assert via the gate-aot harness (test/gate-aot-positive-lib.f), suite test/aot-wid-suite.f wired per GE-TYPE-DECL-SUITE pattern. DEPENDS: 2b-v slices a-e (registry+persistence+guards), item 8 (or the stdin.f hook).

## RCA + executable spec (2026-07-05, seal-hardening worker)

Investigated while landing habu-range-reject-cp-e2eed7e4. Findings that make this
dot precise and executable, plus why it was NOT done in that session (own focused
effort with production-build blast radius).

- BLOCKER (producer): no protected-WID producer exists. `C-PACKAGE-ALLOC-WIDS`
  (habu2.f:3051) allocates two package WIDs but never registers one; the only
  registry writes are init-empty (habu2.f:2919, `PROT-WID-N-CELL := 0`) and AOT
  restore (habu2.f:2572, `:= N`). No `PROT-WID-ADD` is defined anywhere. So the
  registry is always empty in production and `LPROTWIDQ` + the publish/AOT-gate
  guards are inert plumbing. The real generated-ctor producer is TFAM 8
  (habu-tfam-8-generated-f89a7ae9, itself deps TFAM 7 + TFAM 12), unbuilt.

- UNBLOCK (guard test needs NO real package / no TFAM 8): the publish guard
  `C-STORE-DEF-NAME` (habu2.f:1676-1683) rejects `<wid> set-current : FOO ;` with
  rc 84 (E-SEAL-PACKAGE) whenever the definition WID (redirected via `set-current`)
  is a registered protected wid and the friend latch is sealed. So a SYNTHETIC
  protected wid baked into the registry can be tamper-tested directly via
  `set-current` — the "protected -> rc 84, user wid -> rc 0" assertion does not
  require a real sealed package. TFAM 8 is only needed if the test additionally
  asserts real generated-ctor package reopen (rc 84 via C-PACKAGE-SEAL-GUARD).

- EXECUTABLE PLAN:
  1. Producer: env-gated metabuild hook in stdin.f CAPTURE-REPL (:84), BEFORE the
     ACAP-CAPTURE call (line 89). If `HABU_AOT_PWID_TEST` is set to a wid (>255),
     write `data-base PROT-WID-OFF + wid u32!` and `data-base PROT-WID-N-CELL + 1 !`
     (metabuild host, latch 0 -> raw writes allowed; GETENV [src/os/env-base.f] and
     PROT-WID-OFF/PROT-WID-N-CELL [layout.f] are both in scope before stdin.f).
     Ungated production builds stay empty (regression: rebuild without the env var,
     assert registry count 0 + all gates green).
  2. Harness: rebuild a bin/hb VARIANT with HABU_AOT_PWID_TEST=<wid> via the
     build-fixpoint path into a temp path, then boot it. NOTE: a bare
     `bin/hb --load <srclist stdin>` dies rc 74 — the metabuild needs the
     BF-STDIN-SOURCE concat + stage host + env setup, so drive build-fixpoint (or a
     minimal extract of BF-STDIN-*), not a raw --load.
  3. Assertions (spawn child forges, capture EXIT rc like test/seal.f SLV-RUN-LOAD):
     (i) `<wid> set-current : FOO 1 ;` -> rc 84; (ii) `<user-wid> set-current : FOO 1 ;`
     -> rc 0; (iii) probe PROT-WID? membership + PROT-WID-N-CELL == 1 + WIDN advanced
     past wid (full u32; wid>255 round-trips). Exercises EM-AOT-REGISTER-PROT-WIDS
     restore + WIDN advance + the publish guard end-to-end.
  4. Wire test/aot-wid-suite.f into test/gate-stdlib-cases.f as its own TEST:SUITE
     (mirror `friend-arena-seal` / test/seal.f), OR the gate-aot suite.

- COST/PLACEMENT: the variant engine rebuild is heavy (a full metabuild) — place it
  in a dedicated heavier gate slice, not the fast main suite. This plus the stdin.f
  production-driver touch is high blast radius on the always-green build, so it
  warrants its own focused session + review rather than riding alongside an
  unrelated security fix.

- DEPENDS corrected: 2b-v a-e (landed). NOT TFAM 8 for the set-current guard test.
