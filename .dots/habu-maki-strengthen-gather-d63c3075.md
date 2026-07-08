---
title: "Maki: strengthen gather golden with varied synthetic indices"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:50:42.053492+02:00"
---

SLICE 4 GATHER copy kernel (maki/lower-move.f LMV-BODY-GATHER) rounds an f32 index input exactly like the executor EX-BUILD-IDX (add.f32 +0.5 then cvt.rzi.s32.f32) and device==host V-PASS on the Orin. But maki/golden-artifact.f GA-FILL-VAL fills any gather INDEX slot with 0.0 (GA-INDEX-SLOT?), so every gathered row is source row 0: the golden proves index-load + rounding + row addressing (a positional identity kernel would MISMATCH since src rows differ) but does NOT exercise distinct index->row mapping. Fix: give GA-FILL-VAL a deterministic in-range varied index per gather index slot (e.g. (elem*small) mod src_rows) so the golden covers a permutation of rows. golden-artifact.f is the shared reference-material file (coordinate; not slice-4 lower surface). Then the device gather golden (maki/lower-mv-device-test.f) exercises real row selection.

LANDED 2026-07-08 (host leg):
- maki/golden-artifact.f: GA-FILL-VAL now fills a gather INDEX slot with the REVERSAL src_rows-1 - (e mod src_rows) - exact small integers (unambiguous under +0.5/cvt.rzi rounding), in-range for every src_rows, non-constant and non-identity for src_rows>1, so the golden exercises a real row permutation AND still discriminates an index-ignoring positional copy (identity would not). The suggested (elem*small) mod src_rows was rejected: any fixed multiplier degenerates to constant 0 whenever it divides src_rows (e.g. small=3 with 3 source rows). src_rows is threaded from the IR, not hardcoded: GA-SRC-ROWS reads the gather node's data-operand (operand 0) row count via slot/node shape accessors; GA-IDX-ROWS takes the MIN over all gather nodes indexing via the slot (replaces boolean GA-INDEX-SLOT?, its only caller), so a slot shared by several gathers stays in range for every consumer. Non-index slots keep the existing pseudo-random fill; change confined to the synthesis section, section comment updated in place.
- maki/golden-artifact-test.f (the file's existing test home, per-file focused load + maki/test.f suite): new sections assert (a) the fill for MODEL: GA-GAT ( x:4x2 idx:3x1 -- y ) GATHER is exactly {3,2,1}; (b) the executed host gather output under that fill discriminates - y[0,0] = x[3,0] = 1.42, provably NOT the row-0 value x[0,0] = 0.40 (the old all-0.0 fill and a positional-copy kernel would both produce 0.40 there), plus y[2,0] = x[1,0] = 0.74; (c) a gather artifact save -> check round-trips V-PASS under the varied fill.

Gates 2026-07-08 (macOS host): bin/hb --load maki/golden-artifact-test.f -> test: ok; bin/hb --load maki/test.f -> 73 PASS, 0 FAIL, test: ok (all GA-BIND-SYNTH consumers - golden self-consistency, lower-golden host legs, demo-ffn - green under the new fill); typed-local-diff-lint clean; dot-dep-lint 0 findings.

PENDING-ZED:
- Re-run the device gather golden on the Orin under the varied fill: scp to zed:Work/habu, `bin/hb --load maki/lower-mv-device-test.f` - the GATHER 8x8-by-4x1 leg now selects rows {7,6,5,4} instead of row 0 four times, so device==host V-PASS there proves real distinct index->row mapping on device (index load + rounding + row addressing per selected row). Close the dot only after that run is green.
