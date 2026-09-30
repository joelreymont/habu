---
title: Report certified returns through (RETURNED)
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T10:58:43.417835+02:00"
blocks:
  - habu-pool-data-addresses-d65bdc94
  - habu-divide-through-div-1118f223
  - habu-count-the-arm64-e13ae0a3
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.8). Line references are at master 8c9b75af; re-verify before editing.

Problem: after a call certified no-return, DEAD-END (src/compiler/native/elaborate.f:3100-3105) stages message, length and status (TRAP-ARGS, :2932-2936), publishes all three and traps into die (PUT-TRAP, emit.f:1441-1445): about 40 bytes per site. Terminal lowering (b62ae1d2) removed the throw/die sites; user no-return words keep the fallback.

Acceptance: engine helper (RETURNED) in habu1.f, registered like (DIV-ZERO), taking x0 = message, x1 = length, x2 = status and falling into BDIE; the trap site is carrier + two movz + bl, 24 bytes, through fixed argument places and NDICT:HELPER-TARGET. Fixture in native-trap.f: a tier-1 caller of a user word certified no-return that returns exits ENGINE-ERROR:CODE-CERT with `hb: <name> returned`; span 24 bytes. Artifact: suite output; no-return-fallback census row (−16 bytes per site); tools/engine-size.f before and after; gen 2 == gen 3.

Break-even: dispatch only if the habu-count-the-arm64-e13ae0a3 census counts more than 100 sites engine-wide.

Files: src/habu/habu1.f, src/compiler/native/trap.f, select.f, emit.f, a64ir.f, test/compiler/native-trap.f. Engine text: yes; two-stage host landing (the host must hold the record the compiler resolves); seed mirror: no.

Verify: stage-1 build (helper only); stage-2 build; bin/hb --load test/compiler/native-trap.f; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f.

Depends: habu-pool-data-addresses-d65bdc94 (shared select.f/emit.f); habu-divide-through-div-1118f223 (NDICT:HELPER-TARGET); habu-count-the-arm64-e13ae0a3's count.

Ownership: the files above.

Census (tools/codegen-census.f, product of 2f165004, SHA-256 82148a2d…8c25): no-return-fallback 342 sites (11,912 B, est. saving 3,704 B): clears the 100-site break-even.

Claim: unassigned.
