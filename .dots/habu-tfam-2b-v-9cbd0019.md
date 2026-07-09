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

## Harness built + engine blocker found (2026-07-08, on b4b519ef)

The integration test was prototyped end-to-end and immediately caught a real
engine-side integration gap. STOPPED before committing the suite: the fix is
in src/habu/habu2.f (engine lane, locked) - see
habu-aot-protected-wid-08716547 for the fix spec. Everything below is proven
against a real variant build; the suite goes green the moment that dot lands.

PRODUCER RESOLVED (no stdin.f edit, no env gate): the coordinator's
first-choice real-SUMTYPE producer is impossible in the metabuild host (the
stdin-run source is prelude + COMMON(engine emitters) + include + driver-io +
aot-capture.f + driver; no checker/sumtype is loaded there), and the two
in-host channels both fail for capture:
- `prot-wid-add` (engine FPRIM) writes the MAKER's own live registry at
  data-base; ACAP-PWID-CAPTURE reads AOT-DBASE = dbase@ = the emit-target
  region -> captures 0 (verified in-maker: data-base count 2, dbase@ count 0).
- raw `dbase@ PROT-WID-OFF + !` writes crash SIGBUS at driver time (the
  emit-target region is not writable then; reads work).
The sanctioned seam is the CAPTURE BUFFER itself - ACAP-PWID-PUT is documented
as "the exact serialize the capture uses" - injected BETWEEN ACAP-CAPTURE and
the image emit. Since GO is atomic, the variant driver = src/habu/stdin.f
bytes with the trailing top-level `GO` call stripped (fail-closed: the builder
dies "stdin.f no longer ends with a top-level GO call" if the tail drifts),
plus a PWID-GO that mirrors GO with the injection after CAPTURE-REPL:
  : PWID-GO ( -- )
     CAPTURE-REPL
     300 0 ACAP-PWID-PUT   70000 1 ACAP-PWID-PUT   2 AOT-PWID-N !
     0 0= STDIN? !
     HB@ 0 EMIT-FORTH
     s" hb" STDIN-OUT DRV-EMIT-IMAGE
     DRV-EXIT-OK ;
  PWID-GO

HARNESS (proven): builder loads tools/build-fixpoint.f in a child with a
private HB_TMP and runs: BF-PREFLIGHT, BF-STAGE2-SOURCE, BF-CERTIFY-STAGE2,
BF-BOOTSTRAP-STAGE, write pwid-driver.f (producer above), `s" stage2-src"
<driver> BF-EMIT-STDIN-RUN-SOURCE`, BF-CERTIFY-GENERATED (the variant driver
certifies clean), BF-RUN-STAGE, rename stage2-got -> hb-pwid-mk, run it,
rename hb-stdin-got -> hb-pwid, chmod + codesign-verify. Cost measured:
38s cold (17s bootstrap stage + certify + stage run + maker run); the
stdin-only step (hb-stage reused) ~15s. Confirms the dedicated-heavy-slice
placement (GSI-TAIL-PROCESS fork, 600s timeout, pool-parallel).

SERIALIZE PROVEN: the built image contains the baked registry block - count=2
(8B LE) followed by u32 300 and u32 70000, located at the LAOTNPWID/LAOTPWID
labels (byte-verified) - the full u32 wid>255 (and >65535) round-trip through
ACAP-PWID-PUT -> EMIT-AOT-SEED works.

RESTORE BLOCKED (the engine bug): probes against the booted hb-pwid in BATCH
sessions (piped stdin and --load - exactly the dot's spawn-child assertion
paths) read PROT-WID-N-CELL 0, `wordlist` returns 1 (no WIDN advance), and
`300 set-current : FOO ( -- n ) 1 ;` exits 0, not 84. Root cause:
EM-AOT-REGISTER-PROT-WIDS rides EM-SEED-AOT at LEXIT, and batch input is
consumed by the pre-LEXIT interpret loop - discriminator: `BP.` (AOT-seeded
debugger word) is E-UNDEFINED in the same batch session. Baked protected WIDs
currently guard only post-seed interactive sessions; the guard machinery
itself is fine (live-registered wids rc-84 in batch today per seal-package.f).
Fix spec + evidence: habu-aot-protected-wid-08716547 (engine lane).

REMAINING once that lands (this dot's close-out): commit test/aot-wid-build.f
(builder above) + test/aot-wid-suite.f (spawn builder child with private
HB_TMP; probe forges against hb-pwid: count==2 exact, entry u32s 300+70000
byte-composed from PROT-WID-OFF, `300|70000 set-current : FOO ( -- n ) 1 ;`
rc 84 on both --load and stdin paths, user `wordlist set-current` define rc 0,
`wordlist` > 70000 for the WIDN advance, and a control run of the count probe
against plain bin/hb expecting 0), wire as a GSI-TAIL-PROCESS fork
(gate-stdlib-inline-lib.f) next to test/seal-package.f, and quote the phase
cost. The probes are ALREADY red against today's engine, so the wiring commit
doubles as the red-first proof for the engine fix.
