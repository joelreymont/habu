---
title: DDC cross-check of the fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.836873+02:00"
---

Diverse Double-Compiling: build bin/hb via the native fixpoint AND via the independent Gforth bootstrap chain (tools/bootstrap.sh HABU_BOOTSTRAP_CHECK_ONLY=1 path, docs/bootstrap.md), require byte-identical output; a seed backdoor must then be mirrored in Gforth to survive - reduces seed trust to 'no coordinated cross-host backdoor'. Deliverable: Habu-native comparison tool (tools/ddc-verify.f) running both chains and diffing sha256 of the artifacts, documented in docs/bootstrap.md, runnable as an explicit (not per-commit) audit gate. Blockers: requires working Gforth recovery host; keep it optional-but-audited (HABU_ALLOW_BOOTSTRAP=1).

## Tool landed + first-run FINDING + fix spec (2026-07-07, head ce13d7d0)

DELIVERABLE DONE: tools/ddc-verify.f (Habu-native, checked) + tools/ddc-verify-test.f
+ tools/ddc-drive.f, documented in docs/bootstrap.md (DDC Audit section). It runs
the Gforth CHECK_ONLY chain via the audited launcher, sha256s both artifacts,
and reports byte-identity or the first differing offset; gated on
HABU_ALLOW_BOOTSTRAP=1, explicit (not per-commit). Test green (byte-identical,
one-byte-diff@18, length-diff cases).

FIRST AUDIT RESULT - the chains are NOT byte-identical yet (real finding, this is
exactly what DDC surfaces). Evidence:
- native bin/hb sha 15e051d0... (= merge-gate copy AND a fresh install --force -
  native chain is deterministic); gforth hb-stdin sha 0bd60294...; both 132343 B.
- Divergence: 875 differing bytes, first at offset 84676. ~678 are in __text
  (0x14000-0x18000), the rest is the downstream code signature (content hash, so
  it follows). The __text diffs sit at a 28-byte stride in immediate positions;
  disassembly at the diff (0x14b50): `d29ab7c9 f2a01029 f2c08809 f2e00009` =
  a movz/movk x9 64-bit ADDRESS-immediate chain.
- BENIGN, not a backdoor or code difference: both binaries are FUNCTIONALLY
  IDENTICAL (`7 SQ .` -> 49 on both), and each chain is INTERNALLY deterministic
  (gforth built twice = byte-identical; native = byte-identical). Only
  native-vs-gforth differs.
- ROOT CAUSE: the AOT-REPL blob is captured in hb-stdin-mk, whose __text/region
  addresses differ between the native-built and gforth-built host; the baked
  DATA/CODE-literal + call immediates therefore carry the CAPTURE HOST's
  addresses. EM-SEED-AOT re-relocates them at boot (Approach 3 name/DATA/CODE
  relocation, per the closed habu-decide-unbake-repl dot), so the running
  addresses are correct on both - the baked pre-relocation bytes are DEAD but
  host-dependent, defeating byte-identity.

FIX SPEC (spec-and-stop - AOT capture/emit is engine-family): canonicalize the
baked AOT immediates at CAPTURE. The movz/movk chains EM-SEED-AOT relocates at
boot (call sites via the name-reloc table; DATA/CODE literals via their reloc
tables) must be written with a FIXED placeholder (0 or a canonical base) in the
baked blob, since boot overwrites them regardless of baked value. This mirrors
the snapshot canonical-base determinism already done for `-- snap` images
(that work made the snapshot WRITER deterministic; the AOT capture path was
never canonicalized across HOSTS). Files: src/habu/aot-capture.f (ACAP-CAPTURE,
where the blob is copied to scratch - zero the reloc-site immediates there using
the SAME reloc tables EM-SEED-AOT consumes) and/or the emit in src/habu/habu2.f
(EMIT-AOT-SEED). Regression: `tools/ddc-verify.f` DDC-VERIFY must report
`byte-identical`; add the two-build byte-compare to the periodic no-binary check.
ROUTE to the AOT/engine lane (item-8 owns habu2.f; aot-capture.f is src/habu).

STATUS: the audit tool is complete and MINTS its own finding; full DDC
byte-identity stays OPEN pending the engine canonicalization fix above.
