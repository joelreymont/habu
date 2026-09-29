---
title: Re-enter evaluate from Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.689073+03:00"
blocks:
  - habu-close-definitions-with-8ace78d8
  - habu-move-pkgs-using-22f18b81
---

Problem: `evaluate` is assembly. First of I9a-c.
Acceptance: a re-entrant `evaluate` in `src/habu/outer.f`: saved input state, the `EVALD` depth, nested exits (`LEX0`/`EM-EVAL-CLEAN-EXIT` semantics).
Files: `src/habu/outer.f`, cases beside `test/outer-interpret.f`.
Verify: spark: nested `evaluate` cases through the Habu loop under the feature cell; gate.
Depends: habu-close-definitions-with-8ace78d8 (I5e), habu-move-pkgs-using-22f18b81 (I8).
Route: Alder (shared: src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.
Design correction, rev 2 (absorbs I1 `habu-mark-blob-provided-21605a53`, closed; these override the lines above where they differ). No row moves before this leaf: I2 and I3 build beside `search-wl`/`num-parse` with differential tests, and K7 carries an `evaluate` stub until this leaf; the first body that leaves assembly is `ELAB: evaluate` (`src/habu/prims.f:621`, body `src/habu/habu1.f:3437`). A standalone mechanism leaf had no failing check.
- Problem: `evaluate` is assembly (`habu1.f:3437`, `ELAB:` row `prims.f:621`); `prims.f` has no spelling for a row whose body is a checked definition in the captured runtime, so the seeded product would carry two `evaluate`s.
- Representation: a flag, not a kind (`evaluate` is `ELAB:`; the replay exits early for ELAB, `checker.f:8350`). `2 constant FL-PREFIX-PROVIDED` beside `FL-TRUSTED-ONLY` (`prims.f:104`); marker `EPREFIX-PROVIDED!` (the shape of `ETRUSTED-ONLY!`, `prims.f:225-227`) after `ELAB: evaluate`; reader `PRIM-SPEC:PREFIX-PROVIDED? ( n -- bool )` beside `TRUSTED-ONLY?` (`prims.f:252-253`), with a `PPRIM:` row at `checker.f:8850`. No file span: a captured record proves its file was in `native-runtime.f`, and no build-time table can check a name against the manifest.
- Seam: `ENGINE-PRIMS` gains `variable SEEDED` and `SEEDED! ( bool -- )` (an int cell like `SHAKE?`, `src/habu/treeshake.f:9,53`), armed in `EMIT-RESET-BUILDER` (`habu2.f:10582-10584`) as `SEEDED-RUNTIME? ENGINE-PRIMS:SEEDED!` beside `ENGINE-PRIMS:RESET`. `SEEDED-RUNTIME?` (`habu2.f:1259`) is already read during emission (`habu2.f:10900,11074`).
- Body skip: `ENGINE-PRIMS:KEEP-BODY? ( ptr u8 n -- bool )` is `KEEP?` and not (`PREFIX-PROVIDED?` and seeded). `FP-KEEP?` (`habu1.f:85-86`) calls it, so `FPRIM`/`FPRIM-L`/`FPRIM-WID`/`GDEREF-L/F` (`habu1.f:91-115,143-146`; `prof.f:1290-1294`) skip the body in seeded builds and keep it cold. One predicate, no per-site conditions.
- Gate: `ENGINE-PRIMS:COMPLETE ( [ ptr u8 n -- bool ] -- )` takes the provider. `CHECK-ROW` (`primitive-registry.f:131-135`), for a prefix-provided row when seeded: `BODY?` dies "assembly body for a prefix-provided row in a seeded build"; provider false dies "prefix-provided row not in the captured runtime"; otherwise today's rule. Call site `habu2.f:11114`: `[: 0 AOT-PAYLOAD-INDEX 0 >= ;] ENGINE-PRIMS:COMPLETE` (`AOT-PAYLOAD-INDEX` `habu2.f:10820` is top-level between `;package` 10750 and `package` 10942; captured globals carry wid 0, `src/habu/aot-capture.f:455-457,1371,1389`).
- Row: `outer.f` defines global `evaluate` (saved input state, `EVALD`, `LEX0`/`EM-EVAL-CLEAN-EXIT`); `s" src/habu/outer.f" required` in `native-runtime.f` plus an `ML-ENTRY+` with its reason (`tools/manifest-lint-core.f:169-173`), unless an earlier leaf added them (`rg outer.f src/habu/native-runtime.f tools/manifest-lint-core.f`).
- Measure first, on spark: whether the build host admits a global `: evaluate` beside the seed primitive (rc 78 duplicate rule, card section 1; `undefine` exists, `src/habu/verify-source.f:697`, `src/habu/xref.f:426`). The gate's wid-0 lookup needs the captured record to be a global `evaluate`.
- Docs: `docs/x86-64.md:472-491` (row forms, readers); `docs/porting.md:83-85`.
- Files: `src/habu/outer.f`, `test/outer-interpret.f`, `src/habu/prims.f`, `src/habu/primitive-registry.f`, `src/habu/habu1.f`, `src/habu/habu2.f`, `src/core/checker.f`, `src/habu/native-runtime.f`, `tools/manifest-lint-core.f`, `docs/x86-64.md`, `docs/porting.md`.
- Verify (spark): rebuild; the nested cases on the rebuilt engine through plain `evaluate` (the only `evaluate`); gate (every suite's `evaluate` now runs the captured one); `bin/hb --load test/prim-parity.f` and `test/primitive-registry.f` unchanged; chain to convergence (a builder change: gen2 == gen3); once by hand, build with the `outer.f` manifest row removed and record the rc-76 prefix-provided refusal; the periodic no-binary check (`docs/bootstrap.md:209-220`): the cold `hb-stdin` keeps the assembly body.
- Depends: I5e, I8, K4 (`habu-share-the-primitive-58c235e5`). Base on X7's bookmark until K4/X7 land. Serialise `habu2.f` (K4, X7, I6, I10c), `prims.f` (I6), `native-runtime.f` (I10c).
- Route: Alder (shared: every file above except `outer.f` and the test).
- From I4 design: retire `SOURCE-ROOT:INCLUDE-INTERPRET`; `INCLUDE-EVALUATE` calls the one `evaluate`.
