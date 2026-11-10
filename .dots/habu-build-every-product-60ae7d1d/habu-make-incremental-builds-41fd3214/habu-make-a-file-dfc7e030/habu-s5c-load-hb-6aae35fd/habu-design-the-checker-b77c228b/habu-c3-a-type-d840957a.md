---
title: "C3: a type is an entry"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:02.478114+02:00"
closed-at: "2026-11-10T21:50:20.587230+02:00"
close-reason: "done: src/habu/linker.f has KIND-TYPE entries, type-family.f has no TFAM-FIND-PUBLIC, task.f:1885 qualifies TASK:sem (b5ed59f4, dcd00f50)"
blocks:
  - habu-c2d-write-declarations-f94670f2
---

Problem: types live in the checker's family registry (src/core/type-family.f), filled only by a compiled declaration, so a linked file's types never reach a warm build (the lazy warm test stops at string.f on `( -- option<a> )`) and the cross-built hb lacks them at first start. DECISIONS 'where the checker's facts live' (a type is an entry whose text is its declaration) and 'type names' (types in a wordlist of their own, found by the word rules, no renames, no twins refused; the public fallback TFAM-FIND-PUBLIC goes). Acceptance: in a build the linker writes, at each compiled file's end, an entry in the types wordlist (header cell BOOT:TYPES-WL-CELL $2D78) for each family the file declared, its facts text the family's rows with every family named by package; the object carries it (KIND-TYPE) and a link remakes it; a lookup that misses the registry reads the entry's text (reentrant, write-again compare refuses a text that does not say the rows); run as a word an entry refuses by name; TFAM-FIND-PUBLIC goes and task.f, queue.f and aio.f qualify their does> rows. The lazy warm test stops 0 and its objects cmp equal to a cold build's of the edited tree. Files: src/core/type-family.f, src/core/checker.f, src/habu/linker.f, src/habu/boot-cells.f, src/habu/layout.f, src/lib/task.f, queue.f, aio.f. Verify: .out/a2a census scripts (lazy warm, rf, r, z). Depends: C2d. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-a2a.

Released 2026-11-09 (jerry, off the arm64 path; parked heads listed in habu-design-the-checker-b77c228b). Claim: unassigned.
