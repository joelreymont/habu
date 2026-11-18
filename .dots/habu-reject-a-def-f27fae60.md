---
title: "Reject a full code space in one place"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T15:19:11.943785+01:00"
---

Problem: three places find the code space full, with three outcomes. DEF-CODE-ROOM (src/habu/interpreter.f) rejects a definer at the ceiling E-CODE-SPACE-FULL with its place and help, exit FULL (77). A definition that starts below CODE-SPACE:CEILING but whose code would end past it makes src/compiler/native/publish.f ROOM-CK or its siblings (:53, :187, :215) throw a bare E-NPUB-ROOM (-8561): uncaught, no place or help, exit 67. A warm build's src/habu/linker.f MAKE-DEF (:4797) and MAKE-OWNED (:5698) call INTERPRETER:DEF-RECORD without DEF-CODE-ROOM, so with CP at the ceiling they reach def-open's kernel stop (src/habu/habu2.f, exit 83): a later part linked after an earlier file grew can get there where the cold build of the same source is rejected (AGENTS.md: a warm build never fails or differs where a cold build would not). Acceptance: whether a definition starts at the ceiling or would pass it, compiled or linked, it is rejected E-CODE-SPACE-FULL with the definition's place and help and exits 77, caught as EXIT-STATUS:FULL by TEST-EVAL:RC; a warm build that crosses the ceiling ends as its cold build does; test/fixtures/protection-span-full.f's fill (which halves the body on E-NPUB-ROOM) follows the new code; E-NPUB-ROOM and its src/lib/error.f row go if nothing else throws it. Depends: none. Claim: unassigned.
