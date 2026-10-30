---
title: Restore Gforth recovery under the top-level row rule
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T11:07:36.906782+02:00"
---

tools/bootstrap.sh stops at test/type-ctor-suite.f:47 on the recovery engine hb-stdin: E-UNDEFINED CHECKER-PACKAGE-PRIVATE, rc 70 (spark 2026-10-30, logs /tmp/carl-wx-spark/mboot.log; the same tree's parent 6d78ea54 boots rc 0). e4dfba8d 'Refuse a top-level step without a row' (checker.f TOP-ROW?, TOP-ANSWER) refuses a top-level word step whose word has no row; on hb-stdin's cold from-source prefix a constant checker.f creates without a PRIM: row has none (the suite's own comment at lines 40-46 and docs/forth.md 'A sealed from-source prefix needs external rows for pre-hook words'), so the suite's read of CHECKER-PACKAGE-PRIVATE and E-CTOR-PROTECTED into file-owned constants, which worked around the earlier unknown-cell refusal, is now itself refused. Find the responsible layer (the prefix's rows, the rule's treatment of the cold prefix, or the suite) and fix it there; proof: HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh rc 0 on the Mac, the suite rows e4dfba8d touched on the normal engine. Blocks the W^X spark proofs (habu-w-x) and any release recovery audit.
