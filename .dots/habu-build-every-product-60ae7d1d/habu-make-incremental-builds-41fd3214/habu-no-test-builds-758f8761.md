---
title: No test builds a file that leaves an arena live
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:47:49.041679+02:00"
---

Problem (2026-11-10): the 'live arena' refusal (DYNAMIC-STORAGE:ARENA-GUARD, run by IMAGE-LIFECYCLE:PREPARE) is deleted with the package (design .jj-ws.noindex/jerry-wasm/.out/process-state-design.md 'Refusals after the change': it goes because nothing of the arena reaches the program, and the refused reference at a file's end is the only guard left). No test shows what a build does with a file whose top level leaves an ARENA or a HEAP record live: whether the owner's head cell is refused at the file's end, or the program starts with an empty arena, and that a cold and a warm build agree. After the guard goes, ARENAS (src/core/dynamic-storage.f:66) and each first chunk's name cells (:253-254, :309-310) are written and unlinked but read by nothing else. docs/forth-card.md:338 and :340 tell the writer to release 'before an image save'. Acceptance: cases in test/file-objects.f (a file whose top level takes an arena record and keeps it; one that takes and drops the pointer; built cold, then again in one out root after an unrelated program): record rc and the program's output for each, equal cold and warm; then the card rows say what a build does; then ARENAS and the name cells are deleted if nothing reads them. Files: test/file-objects.f, src/core/dynamic-storage.f, docs/forth-card.md, docs/forth.md. Verify: test/file-objects.f; suite arena; engine built twice equal when dynamic-storage.f changes. Depends: habu-delete-image-lifecycle-96ec25ea landing. Ownership: unassigned. Claim: unassigned.
