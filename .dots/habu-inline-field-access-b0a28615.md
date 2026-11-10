---
title: Inline field access and the store guard in compiled code
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:10:16.952407+02:00"
---

Problem: measured by the JSON:VALUE= hunt (profile ~/.cache/tmp/carl-json/prof.txt, 46,782 samples, installed hb from master c128ff07, tier not determined): a record field access compiles to a call of field-project (src/core/sumtype.f:1968), 14.5% of samples plus about 9% in the reader's CURSOR-* field words, and every '!' calls the store guard (PROT-SPAN) (src/habu/habu1.f:2131), 7.4%. Both sit under every JSON read, so they bound what any library fix can reach: one plain read of a 290-byte row costs 3.9 us, 13.4 ns a byte. Acceptance: a field access with a constant offset and a store compile without a call on the tier that runs library code; the reproducer's per-row time falls by the measured shares. Files: the engine's code generation (T0; not carl's lane). Verify: ~/.cache/tmp/carl-json/prof.f before and after. Depends: none. Ownership: the engine's code generation. Claim: unassigned.
