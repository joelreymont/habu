---
title: Refuse a dead parsing word at tier 1 without E-UNDEFINED
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T10:53:26.856990+02:00"
---

Finding (lane t0fe brief, 2026-10-31): tier 1 refuses TRUSTED: LRD-DEAD2 ( -- n ) 1 exit s" ab" drop ; at the payload, not the opener: 'ncomp: cannot compile LRD-DEAD2 at ab"' then a false 'E-UNDEFINED: ab"', rc 70 (probe /tmp/jerry-big/rv/t1dead2.f, 1 set-tier). A dead DUP is refused cleanly (t1dead.f). Acceptance: tier 1 names the dead s" and prints no E-UNDEFINED line, as T0 does after t0fe. Files: src/compiler/native/elaborate.f SK-STEP / SK-AFTER-END-CK. Verify: t1dead2.f; row load-reject-diag. Not a T0 release prerequisite (tier 1 stays for experiments). Ownership: unassigned.

Also (lane t0fe fold-in): tier 1 refuses `TRUSTED: B7 ( -- [ -- n ] ) [: 1 exit [: 2 ;] ;] ;` at `[:` but adds a false `E-UNDEFINED: [:` line (/tmp/jerry-rv-t0fe/p/b7t-t1.f).
