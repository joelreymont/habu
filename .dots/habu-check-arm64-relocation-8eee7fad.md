---
title: Check ARM64 relocation reach
status: open
priority: 1
issue-type: task
created-at: "2026-07-14T04:34:16.794053+02:00"
---

Full context: src/arch/arm64/icode.f D19/D26/ENC-ADRD mask deltas without signed-range validation, so forward and backward BCOND/CBZ/CBNZ/ADR and sufficiently large B/BL silently wrap; the 2 MiB code window exceeds REL19/ADR reach while bootstrap/cg/asm.fs already rejects overflow. Root fix: shared typed signed-reach validators for REL19 word deltas, REL26 word deltas, and ADR byte deltas on both immediate and deferred paths before emit/patch mutation. Acceptance: exact positive/negative boundary encodings pass; one-beyond each boundary exits 72 with named diagnostics; failed deferred patch leaves code/fixup state inspectably unchanged; recovery/native behavior agrees; fixpoint/full gates pass. Files: src/arch/arm64/icode.f, bootstrap/cg/asm.fs only if parity changes, focused boundary tests, FILEMAP/LESSONS. Depends on fixup-chain reclamation.
