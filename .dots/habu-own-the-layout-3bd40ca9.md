---
title: "Own the layout walk's term lifetime"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T09:54:42.228495+02:00"
---

PRIORITY 1 - found by the layout-guard lane (50f434cd), deliberately NOT fixed there: the sibling QUEUE-PRODUCT reads the declared field slot (wrong like QUEUE-SUM was) but its obvious arithmetic fix DOES NOT WORK because two deeper defects stack: (1) ENV-TERM! calls ENV! with (arg,i) where it wants (idx,term) - every parameter substitution stores the loop counter at the argument's term id, so SCHEMA-TERM reads ENV@[0]=0 and EVERY nested walk in layout-valid.f has been silently dead (VARIANT-OFF included, masked because its loop only runs for a variant's second payload field); (2) SCHEMA-TERM mints terms ON TOP of live ones - instrumented: MK-PARAM allocated AT INDEX 7 while arg0=7 was live, clobbering it (the term arena's high-water is rewound before the certificate producer runs). Fixing (1) alone UNMASKS (2): nested walks go live and emit checks against clobbered terms - new aborts on valid programs. The fix is a term-lifetime ownership decision (mark/rewind around the producer, a private arena, or derive widths without minting), designed not improvised. Reproducer in the lane's report (product with param field before nested sum, store-then-load aborts 85). Also inherits the assumption: TFAM-FIELD-PROJ-DO (unreachable today - nothing calls FIELD-PROJ!). Files: src/core/layout-valid.f. Depends: none.
