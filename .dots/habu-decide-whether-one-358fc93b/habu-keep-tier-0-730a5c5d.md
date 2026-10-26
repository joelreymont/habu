---
title: Keep tier 0 values in registers across joins
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.261483+02:00"
blocks:
  - habu-widen-the-tier-8c29a00c
---

Joel 2026-10-26: T0-04 minus copy-on-write aliases (those are habu-copy-on-write-a885df73). Today tier 0 spills everything at if/else/then (CF-ENTRY habu2.f ~6501-6510, CFB-ENTRY ~6544-6546) and on an LVRECON mismatch (jit.f:879-882). Single pass: the register layout at if is the join layout; else/then reconcile to it with a parallel copy (one scratch register breaks a cycle); spill only values that cannot be matched. Extends LVSNAP/LVRECON (jit.f:727-882) from BEGIN loops; no liveness, interference graph or IR. Pool widening rides with it (habu-widen-the-tier-8c29a00c). zz arithmetic: planned steps cover ~86% of the gap, joins ~7%, bar needs ~98%.

Design: ~/.cache/tmp/jerry-join-design.md (Plan/Fable), adopted with the lead cuts in its section 7. Claim: jerry. x27 holds NDICT (layout.f:6), so it is not poolable.

Lanes (master 36e54552, host ~/.cache/tmp/jerry-host/hb built from master):
- 0, size the JIT-SNAP frames by CFSTK-DEPTH-MAX and delete the nest check: workspace .jj-ws.noindex/jerry-snapframes, handoff ~/.cache/tmp/jerry-snapframes-handoff.md. Committed 4c0ebbb5; in review and gate (logs ~/.cache/tmp/jerry-gate0/).
- 1a, LVRECON parallel copy at loop back edges: workspace .jj-ws.noindex/jerry-recon, handoff ~/.cache/tmp/jerry-recon-handoff.md. First worker handed off after the tests (R-SWAP and R-PULL fail pre-change); a fresh worker continues from the handoff. Found: tier 1 refuses swap loops (habu-tier-1-refuses-31528d7c).
- 1b, joins at if/else/then and while/repeat (after 0 and 1a): not started.
- 3, measurement after the widening lanes (habu-widen-the-tier-8c29a00c).
