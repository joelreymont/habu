---
title: No test builds a file that leaves an arena live
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T14:47:49.041679+02:00"
closed-at: "2026-11-14T10:51:13.551570+01:00"
---

Problem: the 'live arena' refusal went with image-lifecycle, and the refused reference at a file's end is the only guard left (design .jj-ws.noindex/jerry-wasm/.out/process-state-design.md 'Refusals after the change'). No test shows what a build does with a file whose top level leaves an ARENA record or a HEAP record live: whether the owner's head cell is refused at the file's end, or the program starts with an empty arena, and that a cold and a warm build agree. docs/forth-card.md:369 and :370 tell the writer to release 'before an image save'. Acceptance: cases in test/file-objects.f (a file whose top level takes an arena record and keeps it; one that takes and drops the pointer; built cold, then again in one out root after an unrelated program): record rc and the program's output for each, equal cold and warm; then the card rows say what a build does. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-arena.
