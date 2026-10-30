---
title: Land the ARENA definer
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:50.776972+02:00"
---

Problem: phil's hand-in tkqllxpl a48228ab 'Add the ARENA definer' + vyuwwrlw 4b504a69 'Check ARENA declarations' (ARENA definer, E-ARENA-CALL/E-IN-PLACE checker refusals, reserved field-project on both walls, wasm FIELD-PROJECT -> PROJECT-FIELD, TDPLAN-FP-ARM -> TYPE-DECL:FIELD-ARM; src/core/arena.f added to the boot lists). Acceptance: duplicates on master@origin (carl-arena nqxklktr, znyvkltx) pass a Fable feature review and its fold-ins, the candidate's aot-wid-build prefix boot, the full suite (watch stdlib-runner-fixtures GTT-TEST-PROGRESS-SIGNAL F40/F41) and the Gforth no-binary recovery check; land, install, tell phil. Files: the lane's 38. Verify: carl-arena/.out/gate/gate.sh. Depends: none. Ownership: the lane's files. Claim: agent=carl workspace=.jj-ws.noindex/carl-arena

Review (Fable, 2026-10-30): REVISE. Raw routes (byte-view, ptr-field, atomic@/atomic!) copy or forge a head; DYNAMIC-BUFFER/ARENA admit an in-place element type; LOWER, duplicates CORE-FOLD-C; cosmetic string.f reflows. Fold-ins: worker in .jj-ws.noindex/carl-arenaf (brief carl-dots/.out/carl-file/brief-arena-fold.md), then an Opus re-review and the gate rerun there.

Gate on the pre-fold lane (carl-arena/.out/gate/rc.txt): cand rc 0 (127 s), Gforth recovery rc 0 (228 s), prefix rc 0 (36 s), suite rc 0, 499 of 499 (1217 s). After the fold-ins: cand, prefix and suite rerun in carl-arenaf; recovery stands (fold-ins touch no seed, launcher or boot list).
