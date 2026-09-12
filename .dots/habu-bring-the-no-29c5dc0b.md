---
title: Bring the no-binary recovery chain back to green
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T17:14:16.186050+03:00"
---

Problem: tools/bootstrap.sh (docs/bootstrap.md No-Binary Recovery) was red on the tip: the stage0 generator lacked seven engine primitives the boot prefix uses (ptr-cell-mark, byte-view, cell-view, seal-captured?, map-anon, align, realpath), its prefix tables had drifted 12 rows from habu2.f's, its C-BTICK and C-BCHAR lacked the body-capture append, its undefined-word leg inside evaluate was fail-open (SIGSEGV far from the token), and test/nf.fs shared four /tmp names between lanes; all fixed and landed 2026-09-12 (commits on the root after d4308140). The chain now builds hb-stage0 and hb-stage but hb-stage exits 82 'AOT metadata corrupt' because EM-SEED-AOT refuses LAOTNREC = 0 (habu-let-a-stage2-6744d545). Remaining seed divergences recorded in bootstrap/cg/forth.fs: no provide rows and only three stdlib LOAD rows (provided needs realpath, which the static seed cannot reach), internal-mark.f and top-row.f unloaded. Acceptance: HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh exits 0 end to end on the root; the periodic check is run after every engine or prefix-table change and its result posted with the tip; a seed realpath decision (lexical canonicalizer or loader-slot) recorded in docs/bootstrap.md. Files: bootstrap/cg/forth.fs, tools/bootstrap.sh, src/habu/habu2.f (EM-SEED-AOT), docs/bootstrap.md. Verify: the periodic check. Depends: habu-let-a-stage2-6744d545. Ownership: hazel. Claim: unassigned.
