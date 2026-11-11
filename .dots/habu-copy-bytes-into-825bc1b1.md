---
title: Copy bytes into an arena with one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:17:27.942173+02:00"
---

Problem (tim 2026-11-11, Maki dot maki-copy-a-name-977fa167): Maki has six private copies of 'ARENA:ALLOC then BYTE-COPY' for names kept in records (maki src/weld.f KEEP-NAME, src/weld-solid.f:86, src/sheet.f KEEP$, src/trim.f KEEP$ :1516-1522, src/sketch-constraints.f:198). Asked: a word that copies n bytes into an arena and answers the copy, e.g. ARENA:COPY ( ptr u8 n ptr arena -- ptr u8 n ); name and order are Joel's (a word a program calls); n = 0 refused as ALLOC or an empty span. Acceptance: the word, a test, tim told its name when installed. Files: src/lib/arena.f, src/lib/arena-test.f. Verify: the test. Depends: the storage shape under discussion with Joel (dot habu-give-an-arena-25a7d3cc). Not urgent, nothing of Maki's waits. Ownership: carl. Claim: unassigned.
