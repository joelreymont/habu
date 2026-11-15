---
title: "Parse a linked word's facts on first use"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T09:08:12.448727+02:00"
closed-at: "2026-11-10T21:50:20.639308+02:00"
close-reason: "done: src/habu/linker.f:3604-3618 LINK-FACTS stores texts and parses none; checker.f:33168 READ-BOUND parses on the first bind"
---

Problem: the lead's choice of 2026-11-06 settles facts text parsed on first use, but LINKER:LINK-FACTS (src/habu/linker.f) calls CHECK:READ-FACTS (src/core/facts-read.f) for every record it links, so a warm link parses every word's text. On carl's scale probe (.jj-ws.noindex/carl-dots/.out/kept/c5a2/scale, 863 KB, 30,000 words; build.f and prof.f there, hb /tmp/carl-c5a2c/hb, run from .jj-ws.noindex/carl-c5a2) READ-FACTS/READ-BODY is 150-200 ms of the 621 ms link. Acceptance: a link lays and stores each record's text (CHECK:FACTS!) but parses none; the checker parses a record's text the first time a compile resolves that word (the lookup that finds no effect for a linked record reads its text); test/file-objects.f facts mode still prints after: 0, misuse and dead refusals; the probe's warm link drops by READ-FACTS' share. Files: src/core/facts-read.f, src/habu/linker.f LINK-FACTS, the checker's word lookup in src/core/checker.f. Depends: C3 (habu-c3-a-type-d840957a: the registry as a cache filled from text on first use) shares the lookup; land after jerry's C-series touches it. Ownership: carl (linker, facts-read.f); checker lookup coordinated with jerry. Claim: unassigned.

Checked (carl, 2026-11-08): jerry's twzrnlzk (jerry-a2a) implements this. LINK-FACTS stores each record's text (CHECK:FACTS!) and reads only TRUST rows eagerly; checker.f READ-BOUND parses a record's text the first time a lookup binds it (LIVE-BIND through FACTS-READ-XT) unless the record has a link row, and READ-ALIAS reads an export's source first. Left once that stack runs: the facts-mode fixture and the scale probe's warm link drop (CHECK:READ-BODY is 10.8% of warm census-Gt).
