---
title: "Maki: strengthen gather golden with varied synthetic indices"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:50:42.053492+02:00"
---

SLICE 4 GATHER copy kernel (maki/lower-move.f LMV-BODY-GATHER) rounds an f32 index input exactly like the executor EX-BUILD-IDX (add.f32 +0.5 then cvt.rzi.s32.f32) and device==host V-PASS on the Orin. But maki/golden-artifact.f GA-FILL-VAL fills any gather INDEX slot with 0.0 (GA-INDEX-SLOT?), so every gathered row is source row 0: the golden proves index-load + rounding + row addressing (a positional identity kernel would MISMATCH since src rows differ) but does NOT exercise distinct index->row mapping. Fix: give GA-FILL-VAL a deterministic in-range varied index per gather index slot (e.g. (elem*small) mod src_rows) so the golden covers a permutation of rows. golden-artifact.f is the shared reference-material file (coordinate; not slice-4 lower surface). Then the device gather golden (maki/lower-mv-device-test.f) exercises real row selection.
