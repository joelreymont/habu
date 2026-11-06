---
title: "C2c-e: find why forall inside a bracket pair loses its type"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T01:33:35.678403+02:00"
closed-at: "2026-11-07T04:02:42.506928+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: the C2c candidate (/tmp/jc2.dP/hb) renders `forall<j inside [p,state<p>], ...>` (test/c2-forall-input.f:23 START, src/lib/xml/c2.f:156 WITH-READER) as `forall<e inside a,...>` and refuses it (`facts text differs`; suites c2-memory-source, c2-xml-source, c2-diagnostics in /tmp/jt.Ry/suite.log). Acceptance: a reported cause with file:line and the rule the fix follows; the fix is a second brief, a commit under nkwlmvos. Files: read-only. Verify: `/tmp/jc2.dP/hb --load test/c2-forall-input.f`. Depends: none. Brief: /tmp/jerry-big/brief-rca-in.md. Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rca-in.

Finding (worker, 2026-11-07, /tmp/rca-in.Mw): for `inside [p,state<p>]` the parser (src/core/checker.f:6733-6741 SIG-INSIDE-ROOTS) stores the bracket's second member in F>SECOND (:853); render.f's T-FORALL arm (:509-510) writes only F>PARENT, so the text builds second = 0. The parser's facts mode reads the bracket form back exactly; only the renderer is wrong (diagnostics lose the bracket the same way). Fix (lane, brief /tmp/jerry-big/brief-fix-in.md, workspace .jj-ws.noindex/jerry-rca-in on nkwlmvos, moved below it at landing): write ` inside [PARENT,SECOND]` when F>SECOND is nonzero.
