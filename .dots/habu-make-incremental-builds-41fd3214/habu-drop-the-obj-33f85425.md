---
title: Drop the object sections only the replay read
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:48:15.315088+02:00"
---

Problem: after the replay path goes (see the delete-the-whole-file-replay-path dot), the file object still records sections nothing reads on load: segment images and data rows (SEGMENT-IMAGE / IMAGE-TAKE, FILE-ENCODE's cut), slot rows, LOAD event rows with absolute source offsets (LOAD-CK refuses a resume offset past the end, so an edit that shortens a file before a require is a silent miss), pins, FFI, task, user and hist rows; FILE-COUNTS and PAYLOAD-CK still frame them. Under the fasl model the top level re-runs, so these are dead weight written on every store. Acceptance (design by the lead after the replay deletion lands in the spike): each section whose last reader was deleted is no longer written, framed, counted or checked; the object format shrinks under hard cutover (no version); what the spike still needs is kept and named (the file's input rows, origin rows, index rows and payloads, the checker slices, the ambient key); warm == cold byte for byte and the P4/P8 proofs of lane 9 hold; the object's byte size per file is reported before and after. Files: tools/native-unit-object.f, tools/native-unit-file.f, tools/native-definition-cache.f, src/habu/habu2.f (LOAD rows). Verify: eng.sh cold+warm, lane 9's P4 and P8, check.f. Depends: the replay-path deletion dot. Ownership: carl-fasl. Claim: unassigned.

Lead notes (carl, 2026-10-30, after lane 10): see the spike dot's lane 10 continuation for what this dot inherits (DEF-OPEN?, the HIST rows' lease column and HISTORY-CK/HIST-TOTAL-CK, OPEN-REM and the event row's remainder, POOL-PLAN's validate-only planning, the pre-existing dead words). Section census at lane 10's end: 20 sections (native-unit-file.f SECTION-COUNT); the spike reads 0, 1, 2, 3, 10, 11, 13, 15, DEPS-SLOT on its load path and SEGMENTS-CK (native-unit-object.f ~:3722) reads 4, 5, 6, 7, 9, 12, 17, 18 only to check them. Order: habu-store-no-carried-e9ccf2ef first, since its P9 tools parse the current format.
