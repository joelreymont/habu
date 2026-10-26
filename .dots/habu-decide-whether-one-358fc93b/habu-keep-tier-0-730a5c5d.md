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
- 0, size the JIT-SNAP frames by CFSTK-DEPTH-MAX and delete the nest check: landed 82993e84.
- 1a, LVRECON parallel copy at loop back edges: landed d0ebe4f8 (Fable ACCEPT). Merged 0+1a gate: build and two-gen rc 0; full suite 497/502, reds lsp-boundary, check-cli-boundary, check-verify, aot-seed-metadata pass alone; catch-stale times out the same way on base master standalone. Rebased onto master 9987b871: tier0-join, tier, runtime-regression, tier0-compare-branch rc 0. Found: tier 1 refuses swap loops (habu-tier-1-refuses-31528d7c).
- 1b, joins at if/else/then and while/repeat: commit 7b13c835 in .jj-ws.noindex/jerry-join, Fable ACCEPT. Added a forward-target fence (FWDTGT-CELL $36B8, written by LPAT, read by LVFUSE): `then` emits nothing when the arm already holds the join layout, so an unfenced fuse could rewind a patched branch target (row GEJFZ printed -3 for 97). Landed 68e6d6be. Gate (~/.cache/tmp/jerry-gate1b/): build and two-gen rc 0, suite 479/502 at load average up to 54; every red passes alone or fails the same way on master's engine. check-verify-test failed cold (two-in-turn 168 s) and passed warm (463 ms); check-test's file-load-context hit the 54 s child guard alone, and passed side by side with master's engine at equal load (77 s vs 76 s, pair4/). check-verify-batch fails standalone on master too. Rebased onto master 1969acdd: two-gen converged; tier0-join, tier0-compare-branch, tier, runtime-regression rc 0.
- 3, measurement after the widening lanes (habu-widen-the-tier-8c29a00c).
