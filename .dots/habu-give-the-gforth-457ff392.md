---
title: Give the Gforth mirror a package-gate category
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:04:43.317697+02:00\""
---

Full context: measured by agent stage0using 2026-07-30. tools/package-diff-lint.f has no category for bootstrap/cg/forth.fs, so EVERY edit that touches an existing definition in the Gforth mirror fails the gate - adding one trailing comment to the existing global BCOUNT reports E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3. The mirror runs under gforth, which has no habu package word, so packaging its definitions is IMPOSSIBLE in that file - a structural impossibility, not a debt like habu2.f. Decide and implement the principled category: the mirror path admits changed and new definitions (mirror discipline is owned by the parity gates in tools/bootstrap-codegen-test.f and bootstrap-mirror-lint.f, not by package scope), with the same one-comparison-site row pattern the engine-trunk category uses, pinned both ways in tools/package-diff-lint-test.f (a mirror edit passes; the same edit at a non-mirror .fs path still fails). Depends on the admission-key rework in flight in .jj-ws/habu-relocate-snapshot-region-752042fe (dot habu-admit-layout-f-7e317a72) - build on top of it, not beside it. This unblocks landing the stage0 using commit b9d5fca5 (44 findings today, 40 of them this pre-existing gap).

Claim: agent=mirrorcat workspace=.jj-ws/habu-give-the-gforth-457ff392

MEASURED by agent mirrorcat 2026-07-30, in .jj-ws/habu-give-the-gforth-457ff392
on parent 0b28ce76. Every number below comes from running the real
`bin/hb --load tools/package-diff-lint.f` on a `jj diff --git` artifact.

Baseline reproduced exactly as reported. Adding one trailing comment to the
existing global BCOUNT reported
`E-PACKAGE-OWNERSHIP bootstrap/cg/forth.fs:811:3` and exit 1.

The gate does scan the whole Gforth corpus. All 64 `.fs` files in the tree
(29 in bootstrap/cg, 27 in bootstrap/src, 6 bootstrap load drivers, and the two
Gforth test harnesses test/nf.fs and test/bootstrap-wide-memory.fs) were probed
one at a time by appending a single global definition to each. Every one of the
64 reported E-PACKAGE-OWNERSHIP, so nothing is skipped and nothing was already
admitted by accident. Not one of the 64 files opens a habu `package`; the only
occurrence of the bytes `;package` anywhere in the corpus is a string literal in
bootstrap/cg/forth.fs line 2476, which is the keyword table this emitter WRITES
for the engine it builds.

Decision on the row set: one row, the exact path bootstrap/cg/forth.fs. The
argument for the category has two halves - packaging is impossible under Gforth,
AND named parity gates own the file's correctness instead - and only forth.fs has
the second half today. tools/bootstrap-codegen-test.f loads that exact path and
asserts over its text at eighteen sites, tools/bootstrap-mirror-lint.f names it
as the file whose absent width-aware pass makes the src/ declaration boundary a
red gate, and the tools/bootstrap.sh recovery fixtures run it end to end. There
is no comparable authority over bootstrap/cg/jit.fs or any other sibling, and no
sibling has a measured blocked change, so they are left reporting. A sibling gets
a row when a real change to it is blocked and its own compensating authority can
be named, which keeps the exact-path rule in docs/forth.md § Packages intact. The
`.fs` extension is deliberately NOT the key: it would admit any future file that
happened to be named that way, and the `.f` twin at the same stem is pinned as
still failing.

Decision on a rename or copy arriving AT the mirror path: reported, the same as
the engine trunk. The mirror is one committed file whose content the parity gates
know; a wholesale replacement arriving at its path is exactly where that
authority has not looked yet, and a rename marks no line as added, so without the
WHOLE-CHANGED guard the entire arriving file would ride in unread.

Both admitted directions now measured green. A comment-only change to the
existing BCOUNT body: exit 0. A new global BUSING added to forth.fs: exit 0. Both
together in one artifact: exit 0.

Consumer class reproduced. One artifact holding a changed forth.fs definition
plus a new global in forth.fs plus a new global in a `.f` test fixture reported
exactly one finding, and it was the test fixture
(`E-PACKAGE-OWNERSHIP test/zz-using-probe.f:2:3`). The mirror half contributes
zero. So of the 44 findings on the stage0 `using` commit b9d5fca5, the 40 mirror
findings are closed by this change and the remaining 4 are the using-fixture
globals in `.f` files, which are a separate matter to be judged at that commit's
own integration - this row does not and should not admit them.

Pins added to tools/package-diff-lint-test.f, registered in TEST-MAIN as
TEST-MIRROR-EXEMPTION: two positives (changed body, new definition) and eight
negatives (stem-prefix sibling bootstrap/cg/forth-extra.fs, basename collision
bootstrap/forth.fs, whole-path suffix test/bootstrap/cg/forth.fs, the habu twin
bootstrap/cg/forth.f, the case variant bootstrap/cg/FORTH.fs, the sibling mirror
bootstrap/cg/jit.fs, a rename arriving at the mirror path, and a deleted package
boundary inside the mirror). Every finding is read back by name, not by count.

Mutation proof, four mutations, each killing exactly the fixtures it should and
nothing else, with the file restored byte-identical and the suite green after
each. Loosening the single path comparison to a suffix match killed the
test/bootstrap/cg/forth.fs fixture (3 assertions). Loosening it to a
case-insensitive match killed the bootstrap/cg/FORTH.fs fixture (3 assertions).
Dropping the WHOLE-CHANGED guard killed the rename-arrival fixture (3
assertions). Removing the whole arm from GLOBAL-SURFACE? killed both positives (4
assertions), so neither positive is vacuous.
