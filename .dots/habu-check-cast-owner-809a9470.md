---
title: "Keep CAST: from making another package's OPAQUE structure"
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T12:31:54.196358+02:00"
---

Finding (census tc1, 2026-10-31): in package XML-C2, CAST: ( ptr u8 -- ptr XML:cursor ), a pointer to another package's STRUCTURE, is admitted, while CAST: ( ptr n -- ptr NUM:alloc-byte-len ) is refused E-CAST-OWNER. Either the owner check misses the first or wrongly refuses the second. Acceptance: one stated rule for a cast whose result names another package's type, both cases following it, with rows in the cast tests. Files: src/core/checker.f cast owner check. Evidence: /tmp/jerry-big/trusted-census-tc1.md, /tmp/jerry-big/tc1-c2-mem-probe.diff. Ownership: unassigned.

Lead reading (2026-10-31, public hb 203201d3): both cases already follow the stated rule. CAST-OWNER? guards cell families; a layout is walked, its fields, payloads and pointees being introduction positions (test/cast-negative-suite.f CNW1-CNW14). XML:cursor's fields are ptr u8 and n, so ( ptr u8 -- ptr XML:cursor ) introduces no family; ( ptr n -- ptr NUM:alloc-byte-len ) introduces NUM's cell family at the pointee. The real defect is that walk's view of an OPAQUE structure: docs/forth.md:507-512 says another package constructs an OPAQUE box only through the words its package defines, yet /tmp/jerry-big/cown-opaque-probe.f (package OPB: CAST: FORGE ( n -- OPA:opaq ) and CAST: FORGE-P ( ptr u8 -- ptr OPA:opaq ), OPA's STRUCTURE opaq OPAQUE FIELD v n) passes tools/check.f rc 0. Sites outside the owner that cast into a foreign OPAQUE structure today: src/lib/xml/c2.f >CURSOR (XML-C2) and src/lib/xml-source-test.f STORE>CURSOR (XML-SOURCE-TEST); src/lib/process-chunks.f >SPANS names SPAN:span, which is not OPAQUE. Rows: cast-negative, xml-byte-edits, xml-c2-consumer, c2-xml-source.
