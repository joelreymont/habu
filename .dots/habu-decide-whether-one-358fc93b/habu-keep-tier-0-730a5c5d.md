---
title: Keep tier 0 values in registers across joins
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.261483+02:00"
blocks:
  - habu-widen-the-tier-8c29a00c
---

Joel 2026-10-26: T0-04 minus copy-on-write aliases (those are habu-copy-on-write-a885df73). Today tier 0 spills everything at if/else/then (CF-ENTRY habu2.f ~6501-6510, CFB-ENTRY ~6544-6546) and on an LVRECON mismatch (jit.f:879-882). Single pass: the register layout at if is the join layout; else/then reconcile to it with a parallel copy (one scratch register breaks a cycle); spill only values that cannot be matched. Extends LVSNAP/LVRECON (jit.f:727-882) from BEGIN loops; no liveness, interference graph or IR. Pool widening rides with it (habu-widen-the-tier-8c29a00c). zz arithmetic: planned steps cover ~86% of the gap, joins ~7%, bar needs ~98%.
