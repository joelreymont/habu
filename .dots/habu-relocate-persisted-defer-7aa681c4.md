---
title: Relocate persisted defer cells across snapshots
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-29T21:34:38.660676+02:00\""
---

Full context: lldb evidence in the round-3 review of .jj-ws/habu-relocate-snapshot-region-752042fe shows defer/is cells in the DP heap (including HOOK-CELL) hold execution tokens that are absolute writer-run region addresses; after the region is relocated to text+REGION-OFF these cells still point into the old writer mapping and the restored image crashes on first defer call. This is the hard blocker habu-canonicalise-data-region-72628eaa describes for DATA pointers generally. Required design per review: record a declared relocation kind at the is store site (the store word tags the cell as an xt cell in a relocation table), and rebase tagged cells during EM-SNAPSHOT-REBASE-DATA-XT at write and restore. A value-band scan over the heap guessing which cells look like xts is FORBIDDEN - that is a value heuristic where a structural invariant (declared at store time) is possible. Acceptance: a snapshot written after an is store boots and the deferred word executes correctly at the relocated address; negative regression proving an untagged forged value is not rebased; part of the 200-clean-boot campaign acceptance.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-07-29 (agent=snapreloc, workspace .jj-ws/habu-relocate-snapshot-region-752042fe).
Partly delivered. The declared-kind mechanism is built and proven; the end-to-end
acceptance is not reached, for a reason outside this dot. Details below, including
what is missing.

The failure this dot describes, reproduced first. A snapshot image built from the
step-2 tree booted, printed for `1 . cr`, and then died the moment it had to
compile a definition. Crash register dump plus lldb: SIGSEGV, program counter
0x105a1dd30 taken from `ldr x16,[x9]` / `blr x16` -- the compiled shape of a
deferred call -- with x9 a DP-heap cell at DATA offset 0x4dfe08 and the live
region at 0x103550000. The cell held the writing run's address for the target.
Twenty runs out of twenty.

The design that was built. A second table in the engine-reserved DATA band holds
the DATA offset of every cell that was DECLARED to hold a region address: a count
cell plus 4096 rows (SNAP-RELOC:XTCELL-* in src/habu/layout.f). Three sites
declare, and each is the point where the cell's kind is decided rather than
guessed:
  - C-DEFER-CELL, when `defer` allocates a dispatch cell;
  - J-IS, when `is` resolves the cell it is about to store an execution token
    into (this is the store site the dot asks for);
  - EM-STARTUP-RUNTIME-STATE's cold branch, by name, for HOOK-CELL,
    COMPILE-PREFLIGHT-CELL and TOP-HOOK-CELL. TOP-HOOK-CELL was missing from the
    step-2 WIP's hardcoded pair and is covered now.
The registration routine (SNAP-RELOC:EMIT-MARK) refuses duplicates, so a cell
that arrives from both `defer` and `is` is listed once and relocated once, and it
stops with a named fd-2 diagnostic and exit status SNAP-RELOC:XTCELL-RC (96) if
the table is full, because silently dropping a row would put the original bug
back. The snapshot writer canonicalises every listed cell against the RBASE-VA
sentinel in the scratch DATA copy (SNAP-RELOC:CANON-CELLS in src/habu/snap-lib.f,
checked Habu), and the loader adds the live region delta back
(SNAP-RELOC:EMIT-XT, called from EM-SNAPSHOT-RESTORE). No pass anywhere looks at
what a cell contains to decide whether to move it. The step-2 WIP's two hardcoded
named cells and EM-SNAPSHOT-REBASE-DATA-XT were removed in favour of this.

Measured result, through the real write-and-restore path. A snapshot image built
with `bin/hb --load tools/build-fixpoint-refresh.f -- snap` and inspected under
lldb after restore: the table carries 124 rows, beginning 0x38, 0x27e8, 0x27f0 --
exactly HOOK-CELL, COMPILE-PREFLIGHT-CELL and TOP-HOOK-CELL -- followed by 121
dispatch cells. Of those 124 cells, 123 hold an address inside the live JIT
region [x26, x26+REGION) and one holds zero (an uninstalled hook); none is
outside. Before the change the first of them was a writing-run address and the
image died on it. The deferred-call crash is gone.

What is NOT delivered, honestly.
1. The regressions are written but not scheduled. test/snapshot-xt-cell-decl.f
   drives the live `defer` and `is` handlers and asserts: `defer` adds exactly one
   row; `is` on an already-declared cell adds none; a second `is` adds none;
   storing dbase@+16 -- a value indistinguishable from a live execution token --
   into an ordinary heap cell adds no row, leaves that cell out of the table, and
   leaves its value untouched (this is the negative regression the dot demands);
   and the deferred word still dispatches. It is NOT routed in
   test/gate-stdlib-cases.f, because it does not pass yet: see item 2.
2. Unexplained, and the reason item 1 is red. In the freshly built small engine
   (HB_TMP/hb-stdin) the table count reads 0 after boot and stays 0 across a
   user-level `defer` and `is`, even though the very same engine, running the
   snapshot build, produced the populated 124-row table that the restored image
   carries and relocates correctly. Registration therefore works on the path that
   matters for the acceptance but appears not to run on the cold small-engine
   `--load` path. This must be root-caused before the regression can be scheduled
   and before this dot can close.
3. The 200-consecutive-clean-boot campaign acceptance is not reached, for two
   further causes that are not this dot's:
   a. Region mapping now fails outright most of the time. Fifty consecutive bare
      runs of the current image: 47 exit 78 ("hb: cannot map fixed code region"),
      2 exit 70, 1 exit 134. The exit 78s are a real mmap failure reported from
      the kernel's own carry-flag error signal, not the old "address is not the
      one I asked for" check. Accepting whatever base the kernel returns does not
      help when the kernel returns an error instead of a base. The fix belongs to
      the parent dot habu-relocate-snapshot-region-752042fe: retry the mapping at
      successive candidate bases inside BL's reach of __text and fail closed with
      a named exit only when the whole window is exhausted.
   b. A second class of persisted region pointer remains, outside this dot's
      declared-kind rule. On a run that did map, the image still died compiling a
      definition, at an `execute` on a value from DP-heap cell DATA offset
      0x81d948 which is not in the table and was never declared -- lldb watchpoint
      shows it arrives with the verbatim DATA copy, so it is persisted, and its
      value changes when the image is rebuilt. That is the dangling-pointer class
      SND-QUARANTINE zeroes by hardcoded, content-dependent offsets, and those
      offsets drift whenever the snapshot builder's own source changes. Two
      successive builds of the image in one HB_TMP differ in 9608 bytes, so the
      image is not reproducible on this tree at all. Owners: dot
      habu-persist-dangling-owners and dot habu-canonicalise-data-region-72628eaa.

Is this the best long-term solution or a patch? The mechanism is long-term. Its
invariant, re-derived from the code rather than from any label: a cell's kind is
recorded by the code that creates or writes it, at the moment it does so, so the
relocation passes never have to recognise an execution token by its value -- and
the negative regression exists precisely to kill any future version that tries.
The alternatives were worse in kind, not in degree: a value-band scan over DATA
would corrupt an ordinary integer that happens to look like an address, and
walking the dictionary for the defer record's magic would still be a value test
and would not cover the three hook cells. Two details are worth a reviewer's
independent check. The declaration in J-IS is made while `is` is being compiled,
not each time the store executes, which is why re-pointing a defer a thousand
times still costs one row. And the duplicate check is a linear walk of the table,
which is correct because the table is short and only compile-time code touches
it; if that ever stops being true it needs an index, not a weakened rule.

CORRECTION 2026-07-29 (agent=snapreloc). Two claims in the report above were
measured against a STALE artifact and are withdrawn. Re-measured on an image and
an engine built from the exact current tree:

1. "47 of 50 runs exit 78" is WITHDRAWN. Fifty consecutive bare runs of the
   freshly built image: 50 exit 0. There is no mmap failure and no retry design
   is needed. The orchestrator's reasoning was right -- an anonymous mapping with
   a hint and without MAP_FIXED does not fail when the hint is occupied, the
   kernel simply picks another base. The exit 78s came from an image left over
   from an earlier build, not from the code under test. The carry-flag error test
   in EM-MMAP-CODE-REGION is correct and stays: SYS, leaves the same convention
   on both targets, and habu1.f SYS-PUSH already reads the same flag the same way
   (`9 C-CS CSET,` = carry set means error).
2. "the table count stays 0 on the cold small-engine path" is WITHDRAWN, same
   cause. The freshly built small engine reports 60 declared cells after boot,
   and the restored image reports 124. Declaration works on both paths.

With those corrected, test/snapshot-xt-cell-decl.f is GREEN on the engine built
from this tree ("test: ok / snapshot-xt-cell-decl-test: ok"), and it is now
scheduled: SUITE snapshot-xt-cell-decl in test/gate-stdlib-cases.f and a
GSI-FORK-INCLUDE row in test/gate-stdlib-inline-lib.f.
tools/suite-coverage-lint.f: 165 suites, 0 findings, exit 0.
tools/trust-lint.f: 939 trust sites, 972 manifest rows, 0 findings, exit 0 -- the
two new TRUSTED: scratch-copy accessors carry TRUSTED.md rows.

One more finding, from the rebase onto the packaged snap-lib.f. The writer's half
of this pass was first written as a `package SNAP-RELOC` block reopened INSIDE
`package SNAP`. That nesting builds fine in the small engine but breaks the
snapshot build outright: the child died with E-BUILD-STATUS after printing a bare
"package", exit code -2802. The four writer words now live in SNAP itself with
plain SND-XT-* tails, which is also the better ownership line -- the engine owns
declaring and restoring, the writer owns the one pass over its own scratch copy --
and the snapshot build is green again. Anyone tempted to reopen a package inside
another package in a file the snapshot builder loads should read this first.

Remaining gap, unchanged: a restored image still crashes when it compiles a
definition, on a persisted DP-heap cell that nothing declared (DATA offset
0x81d948 on the image measured; lldb watchpoint shows it arrives with the
verbatim DATA copy). That is the dangling-pointer class owned by dot
habu-persist-dangling-owners, and the build-to-build image drift that makes its
hardcoded offsets rot is dot habu-make-snapshot-image-afd2b7b1. Nothing in this
dot's RCA explains that drift, so nothing is added to it from here.
