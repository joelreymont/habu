---
title: Inline constant-offset field access in compiled code
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:10:16.952407+02:00"
---

Problem: measured by the JSON:VALUE= hunt (profile ~/.cache/tmp/carl-json/prof.txt, 46,782 samples, installed hb from master c128ff07, tier not determined): a record field access compiles to a call of field-project, 14.5% of samples plus about 9% in the reader's CURSOR-* field words. field-project is `+` marked intrinsic (src/core/structure-make.f:156); the generated field word applies it with its byte offset (src/core/sumtype.f:1967-1968). It sits under every JSON read, so it bounds what any library fix can reach: one plain read of a 290-byte row costs 3.9 us, 13.4 ns a byte. Which tier compiled the profiled code, and so which tier does not inline the intrinsic, is not known. The store guard's 7.4% under the same reads is habu-cut-the-store-50159b7b.
Acceptance: the tier that runs library code is named, and on it a field access with a constant offset compiles to an add of the offset with no call; the reproducer's per-row time falls by the measured share.
Files: that tier's code generation (named once the tier is known; not carl's lane), src/core/sumtype.f if the generated word changes.
Verify: first name the tier that compiled ~/.cache/tmp/carl-json/prof.f's reader; then profile prof.f before and after.
Depends: none. Ownership: the engine's code generation. Claim: unassigned.
