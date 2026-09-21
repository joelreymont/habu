---
title: Rename the dstand variant entry to entry-base
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T15:18:57.017956+03:00"
---

Problem: src/compiler/native/dialect.f lines 80-82 declare SUMTYPE dstand with variants survey and entry; entry means the pointer stands at the BASE the entry transfer leaves it at (offset 0 in VDSTAND-AT's measure, d2786d19), which reads as 'at the entry position' and collides with the vocab FIELD entry (IR-ID:ir-symbol-id, line 102) two declarations below. Acceptance: the variant is entry-base; every reader uses the new name - regalloc-verify.f VDPLACE-CK and BND-STAND, select-x64.f's VOCABULARY and its comment at lines 48-58, dialect.f's comment at 72-79, test/compiler/x64-regalloc.f - with no behavior change; dialect.f is baked, so the change goes through a byte-fixpoint chain. Files: src/compiler/native/dialect.f, src/compiler/native/regalloc-verify.f, src/compiler/native/select-x64.f, src/compiler/native/select.f (comment), test/compiler/x64-regalloc.f. Verify: bin/hb --load test/compiler/x64-regalloc.f; test/compiler/native-regalloc.f; test/compiler/x64-select.f; native-build fixpoint; test/run.f. Depends: none. Ownership: native dialect vocabulary (hazel). Claim: unassigned.
