---
title: Walk sealed and type entries in the file-objects fixture
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T15:25:34.900908+02:00"
closed-at: "2026-11-10T23:38:05.243856+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: test/fixtures/file-objects/build.f ENTRY-WALK mirrors linker.f RECORD+ wrongly for three cases the crafted object does not hold today: a kind 5 (KIND-SEALED, SEAL+) entry is kind and one identity reference (32 bytes, no name, no record) but is walked as a named record; a wordlist role 5 (types, WORDLIST+) carries no package reference but 'OBJ-U64 3 >=' skips three fields for it; a kind 6 (KIND-TYPE) entry has no fields after its role but one is skipped. Found by the worker of habu-save-no-emission-70f83ca3. Acceptance: ENTRY-WALK skips kind 5 like kind 4 without storing LATE-AT, skips a package reference only for roles 3 and 4, and skips nothing for kind 6. Files: test/fixtures/file-objects/build.f. Verify: the file-objects test passes. Depends: none. Ownership: carl. Claim: in the stack as nmmvkpzs 1fc094b5, lands with the feature. The file-objects test passes; the crafted object holds no kind 5 or 6 entry and no role 5, so the new branches are read against RECORD+, SEAL+ and WORDLIST+, not run.
