---
title: Cut and quote a shown name in one REASON word
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T09:34:33.863531+01:00"
---

Problem: five libraries cut a shown name at REASON:QUOTE-MOST on a character's first byte and quote it by hand: src/lib/json/value.f QUOTE-CUT, src/lib/json/read.f :313-314 and :336-347, src/lib/xml/reject.f :74-78, src/lib/regex.f :169 KEPT, src/lib/odata/metadata.f :429-434. Acceptance: one REASON word cuts and quotes; every site calls it and no copy remains. Depends: none. Claim: unassigned.
