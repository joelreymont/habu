---
title: Habu quirk fixes (ergonomics)
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T13:15:15.527085+02:00"
---

Parent for the language/stdlib quirk fixes surfaced porting Odin to Habu. Three buckets by leverage/effort: A trivial stdlib words, B checker diagnostics, C language design. CONSTRAINT: every engine change must self-rebuild byte-for-byte to fixpoint + pass the gate; sequence A->B->C; B/C touch the checker/compiler (rebuild-agent lane) so prototype via BF-BUILD-ALL temp engine before proposing.
