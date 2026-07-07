---
title: Bake boot-prefix pin into image
status: closed
priority: 2
issue-type: task
created-at: "2026-07-04T11:00:00.000000+02:00"
closed-at: "2026-07-07T09:30:00+02:00"
close-reason: "rescoped with evidence: reload TOCTOU real, but bin/hb bakes only primitives so an emitted digest constant is itself re-read source and cannot pin the prefix it rides in; fail-closed per-boot verify deadlocks self-hosting. Landed tools/boot-pin.f print/verify (build->deploy boundary, fail-closed rc 70) + test/boot-pin-test.f + gate suite boot-pin-fixtures. Remaining baked-data capability tracked by dot habu-baked-boot-data."
---

Build-time half landed earlier: BF-PIN in tools/build-fixpoint.f records each emitted
boot-prefix source file's content digest on first read and re-verifies on every
reload across the stage2/stdin/snap emissions; a mid-build source edit throws
E-BUILD-BOOT-DRIFT. Regression: tools/build-fixpoint-test.f `boot pin mismatch`.

Boot-time half rescoped 2026-07-07 — evidence and design notes:

- Reload verdict: REAL. Boot path (non-snapshot) is EM-STARTUP-RUNTIME-STATE ->
  EMIT-SOURCE -> C-SOURCE-STDIN -> LCOLDPFX -> EMIT-COLD-PREFIX ->
  PFX-LOAD-BASE-FILES (habu2.f), which re-reads ~232KB of checker/core source
  from the checkout via LSRCRD and re-parses it. Verified live: appending a
  garbage token to src/core/enums.f makes bin/hb print E-UNDEFINED for that
  token at boot. An engine certified against revision A executes revision B.
- Infeasibility of "emit a constant into the stage source and verify at
  startup": bin/hb bakes ONLY primitives (the #PL registry; stdin.f: EMIT-DICT
  bakes the emit-builder #PL list, NOT the host dictionary). Every colon
  word/constant in the generated stage source — including any emitted digest
  constant and its verifier — is itself re-read checkout source at boot, so it
  cannot tamper-proof the very files it rides in. Verified by construction: an
  injected VERIFY-BOOT-PIN boot token died E-UNDEFINED on every boot (word
  never baked) and, being pre-prefix, bricked the self-rebuild loop until the
  engine was restored from a sibling checkout.
- Policy finding for any future per-boot verify: fail-closed-by-default
  deadlocks self-hosting (the rebuild command boots bin/hb re-reading the
  drifted prefix before it can rebuild). Default must be off/warn with strict
  opt-in (HABU_BOOT_PIN=strict for CI/production). Per-boot hash cost for the
  19-file prefix measured ~1-2ms.
- Landed instead (build->deploy boundary, the feasible enforcement point):
  tools/boot-pin.f (BP-EACH canonical path list, BP-HASH manifest digest =
  SHA-256 over ordered per-file SHA-256s in exact PFX-LOAD-BASE-FILES +
  script-argv order, print/verify CLI with --root sandbox support, drift =
  named diagnostic + rc 70), test/boot-pin-test.f (determinism, sandboxed
  drift fixture, CLI rc/diagnostic, path-list cross-check vs habu2.f incl.
  PFX-LOAD-ROW count tripwire; 32 assertions), gate-stdlib suite
  boot-pin-fixtures.
