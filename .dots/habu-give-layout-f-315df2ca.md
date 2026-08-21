---
title: Give layout.f and snap-lib.f package owners
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-29T21:34:38.637859+02:00\""
blocks:
  - habu-add-using-to-d815f0ab
---

Full context: the snapshot relocation step-2 work (preserved as WIP commit e50fb3ec in .jj-ws/habu-relocate-snapshot-region-752042fe) is blocked because tools/package-diff-lint.f rejects its edits to src/habu/layout.f and src/habu/snap-lib.f: those files define global constants with no package owner. The long-term-correct fix is to give each file a real package (short name, package-local tails, cross-package calls qualified), NOT an exact-path exemption like the one habu2.f received in commit a943eb40 - habu2.f is the engine trunk with hundreds of pre-package globals, while layout.f and snap-lib.f are small constant tables that CAN be packaged in under 30 minutes. Acceptance: package-diff-lint passes on a representative diff touching each file with no new exemption added; existing callers updated; engine rebuilds to fixpoint; gate-stdlib red-phase set unchanged. If packaging genuinely breaks the bootstrap ordering (constants consumed before the package machinery loads), record that proof in this dot and only then fall back to an exemption with the proof attached.

Claim: agent=pkglayout workspace=.jj-ws/habu-give-layout-f-315df2ca (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED (agent pkglayout, workspace .jj-ws/habu-give-layout-f-315df2ca)

What I did. src/habu/snap-lib.f is now owned by a real `package SNAP`. Everything
the file defines is package-private except two public words: `SNAPGO`, the entry
the snapshot driver calls, and `INSTALL-HOOK`, the audited trusted entry that
freezes the verify-on-definition hook into an emitted image. The old global tails
lost their `SNAP-` prefix inside the package, because the package linter rejects a
tail that repeats the package name or the file stem: `SNAP-SIZE!` became `SIZE!`,
`SNAP-HDR` became `HDR`, `SNAP-WRITE` became `WRITE-IMAGE`, `SNAP-CANON-DATA`
became `CANON-DATA`, `SNAP-CHECK-HOOK` became `CHECK-HOOK`, and so on. The
existing inner `package SNAP-CLOSE-SEAM` block is untouched; `package SNAP` closes
before it and reopens after it, since packages cannot nest.

Callers updated: src/habu/snap.f now imports the writer with `using SNAP` right
after its `require`, tools/hook-sites.f row 2 and the audited hook allowlist in
tools/checked-boundary-lint-core.f now name `CHECK-HOOK`, and the three TRUSTED.md
rows for the renamed trusted words follow. No exemption was added to
tools/package-diff-lint-core.f.

Why the public entry kept the name `SNAPGO`. The package linter treats any edited
definition in an unpackaged file as a finding, so renaming the call site inside
src/habu/snap.f would have made that file's `SNAP-RETIRE-GO` a changed global and
pulled snap.f into this change; packaging snap.f in turn forces renaming
`SNAP-RETIRE-GO` (the file stem `snap` is a forbidden tail prefix), which changes
the emitted-source string that tools/build-fixpoint-test.f pins, which pulls that
1473-line unpackaged test file in as well. `using SNAP` is the documented
consumer-side import for exactly this case and keeps snap.f's definitions
byte-identical, so the change stops at the file this dot owns. Recommended
follow-up dot: give src/habu/snap.f and tools/build-fixpoint-test.f package
owners, and shorten `SNAPGO` to a plain tail at that point.

src/habu/layout.f was NOT packaged. Findings below; this half needs a design
decision before any implementation, and no exemption was added.

Gate outputs, all run in this workspace with a bin/hb rebuilt to the fixpoint from
this tree.

  Premise check, before any edit. A comment-only edit to src/habu/layout.f passes:
  `bin/hb --load tools/package-diff-lint.f -- /tmp/p.patch` exits 0. Adding one
  constant (`99 constant PROBE-ONLY`) fails:
  `E-PACKAGE-OWNERSHIP src/habu/layout.f:4:13: 'PROBE-ONLY' defines a changed
  module word outside a package` / `package-diff-lint: threw 1`, exit 1. So the
  gate rejects changed definitions, not every diff that touches the file.

  Final diff artifact (`jj diff --git`):
    tools/package-diff-lint.f      exit 0
    tools/typed-local-diff-lint.f  exit 0

  Engine and snapshot builds:
    bin/hb --load tools/build-fixpoint-refresh.f -- snap
      "self-check census (macos-arm64): 0 uncheckable, 0 rejected, certified = 4232"
      "bin/hb refresh OK: compiler fixpoint" / "snapshot image OK: candidate validated"
    bin/hb --load tools/build-fixpoint-refresh.f -- install --force
      same census, "bin/hb ready (small checked engine, tty REPL + stdin)"

  Owning gates for every changed file:
    tools/trust-lint.f            "937 TRUST site(s), 970 manifest row(s), 0 finding(s)"
    tools/trusted-inventory-test.f  "test: ok"
    tools/build-fixpoint-test.f   12 failures, byte-identical log to the same
                                  command on the unmodified tree (the failures are
                                  the pre-existing "hb: cannot map fixed code
                                  region" snapshot-execution fixtures)
    test/gate-stdlib.f            "red phases: 6" - engine-error-package,
                                  pre-trust-defer, aot-wid-restore,
                                  stdlib-process-fixtures, owner-wid-internal,
                                  build-fixpoint-fixtures. Same six as the recorded
                                  baseline. host-lint, shadow-lint, trust-lint,
                                  trusted-inventory, tool-boundary-lints and
                                  owner-wid-snapshot all PASS.

Why src/habu/layout.f cannot be packaged inside this leaf.

  1. Scale. layout.f defines 240 constants. Searching the tree for those names word
     by word matches 277 files, with 1129 hits in bootstrap/cg/forth.fs, 1100 in
     src/habu/habu2.f, 272 in src/habu/habu1.f, 116 each in src/habu/jit.f and
     bootstrap/cg/jit.fs, and the rest spread across the checker, the standard
     library, the tools, the tests and maki.

  2. The import that would avoid renaming those references does not exist in the
     no-binary recovery chain. `using NAME` makes a package's public words visible
     to bare lookup, which is exactly what these callers need. The keyword table
     the Gforth recovery host emits into its stage0 engine
     (bootstrap/cg/forth.fs lines 2475-2478) declares `package`, `public`,
     `private` and `;package` but no `using` and no `;using`; the strings
     `LKWUSING`, `s" using"` and `C-USING` appear only in src/habu/habu2.f, the
     native engine. tools/bootstrap.sh concatenates src/habu/layout.f together with
     habu1.f, habu2.f, jit.f, prof.f, regalloc.f and xref.f into one source file
     that the stage0 engine interprets, so a `using` line anywhere in that
     concatenation breaks recovery at the first hop.

  3. That leaves fully qualified references. The stage0 engine does understand
     qualified `NAME:WORD` tokens (bootstrap/cg/forth.fs lines 1379-1380 and 3043),
     so packaging layout.f is possible - but only by rewriting every one of those
     several thousand bare references to `LAY:DREC`, `LAY:DATA-START` and so on,
     across the engine assembler sources, the checker, the standard library, the
     tools and the tests, plus requalifying the ARGC-CELL / ARGV-CELL / ENVP-CELL
     TRUST rows in layout.f itself and in TRUSTED.md. That is an engine-wide
     namespace migration, not a sub-30-minute leaf, and it would make the assembler
     sources markedly noisier.

  Recommended follow-up dots, in order: (a) add `using` and `;using` to the Gforth
  recovery host's stage0 keyword table and compile path so the import exists on
  every build lineage; (b) package src/habu/layout.f behind that capability and
  migrate its consumers file by file; (c) package src/habu/snap.f and
  tools/build-fixpoint-test.f. Until (a) lands, any diff that adds or changes a
  constant in layout.f still fails the package gate, so the snapshot relocation
  step-2 work stays blocked on this decision.

ORCHESTRATOR REVIEW 2026-07-30: the snap-lib.f half is reviewed hunk-by-hunk
and landed on the proofs branch (commit dbf52aaa, rebased from the lane).
Independent re-run of package-diff-lint, typed-local-diff-lint, and
error-code-lint on the rebased tree: all exit 0. The layout.f half is BLOCKED
on a proven capability gap, now tracked as the prerequisite in this dot's
blocks list: the stage0 recovery compiler lacks the using keyword
(habu-add-using-to-d815f0ab), and requalifying ~2500 bare references instead
is an engine-wide migration, not this leaf. Three more findings from the lane
were minted as their own dots: habu-pkg-snap-f-5f43d7af (snap.f and
build-fixpoint-test.f packaging plus the SNAPGO respelling),
habu-verify-emitted-images-cf0fbf79 (INSTALL-HOOK has no caller anywhere -
possible missing verify hook in restored images), and
habu-path-qualify-the-823d7e8e (name-only hook allowlist, pre-existing).
This dot stays active until the layout.f half lands behind the stage0 work.
