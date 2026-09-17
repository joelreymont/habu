---
title: Bound pointer arithmetic from NULL-PTR and data-base
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:29:19.248115+03:00"
---

Problem: ': PEEK3 ( n -- n ) NULL-PTR + @ ;' and ': POKE3 ( n n -- ) NULL-PTR + ! ;' certify and run (scout probe 2026-09-17 on d5e871c0, report ~/.cache/hazel/scout-pun/report.md: read 4660, wrote $C0DE), and 'data-base + @' does the same. NULL-PTR (src/core/cell-effects.f:9) is declared '-- ptr a' with an ordinary TVK-ANY pointee and + is pointee-polymorphic (src/core/checker.f:6900), so the caller also picks the pointee: 'NEWTYPE thing 0  : F ( n -- thing ) NULL-PTR + @ ;' certifies and forges a nominal value. The axiom 'ptr a n + -- ptr a' with an unconstrained n reaches any address from any base, so the raw-storage rule (habu-refuse-a-ptr-5ad2734e) does not close this; it needs extents or provenance on pointers. Acceptance: first the nominal forgery: a pointer derived from NULL-PTR or data-base by arithmetic never yields a nominal or pointer pointee (the F fixture above is rejected by name while 'ptr thing NULL-PTR =' and 'NULL-PTR -' keep certifying), with rejected-program fixtures; then the reach: a design under the campaign for bounded pointers (a span type carrying an extent, or provenance on +) with PEEK3 and POKE3 rejected and the cost on the 'BUF k +' idiom measured across src lib tools test and the five consumers before the rule lands. Files: src/core/cell-effects.f, src/core/checker.f, docs/effects.md, docs/type-system.md. Verify: the fixtures; test/run.f. Depends: habu-refuse-a-ptr-5ad2734e. Ownership: checker. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
