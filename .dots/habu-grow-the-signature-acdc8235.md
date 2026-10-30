---
title: "Grow the signature's binder-domain stack with its nesting"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T07:48:36.977405+02:00"
---

Problem: src/core/checker.f SIG-FORALL-DOMAIN refuses a signature whose forall binders nest 32 deep (BIND-DEPTH @ 32 >=) because BIND-DOMAINS is a fixed 32-cell stack (create BIND-DOMAINS 32 cells allot, near NMAP; written at the push in SIG-FORALL-DOMAIN, read by the slot lookup near line 5989), and it refuses as SGBAD-SYNTAX, so a capacity shows as a syntax error. A fixed limit on checked input, the same class as the forall worklist (tim's child upxyuppz). Found by tim 2026-10-30 reading SIG-FORALL-DOMAIN. The one-letter binder spelling (one lowercase letter other than n, f and r, slot NMAP by letter, 23 usable) is the signature grammar, judged a language rule and left as it is. Acceptance: the domain stack grows with the nesting in the checker's own storage (no 32 cells, no depth refusal), NMAP's depth encoding (BIND-DEPTH negate 2 -) unchanged; a test with a signature nesting binders past 32 checks and runs; existing forall refusals unchanged. Files: src/core/checker.f (BIND-DOMAINS, BIND-DEPTH, SIG-FORALL-DOMAIN, the slot lookup), a test under test/. Verify: the new test, then the full suite (shared checker behaviour). Depends: none. Ownership: the words named. Claim: agent=tim workspace=.jj-ws.noindex/tim-binddepth (one commit "Grow the signature binder stack" on 3c85f516, handed in after tim's review).
