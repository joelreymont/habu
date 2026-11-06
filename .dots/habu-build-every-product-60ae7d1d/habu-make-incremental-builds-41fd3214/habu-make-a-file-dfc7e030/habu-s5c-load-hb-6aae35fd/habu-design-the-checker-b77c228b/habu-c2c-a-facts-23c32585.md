---
title: "C2c-a: facts text of a nested part's type does not read back"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T00:13:25.137921+02:00"
closed-at: "2026-11-07T04:02:42.405596+02:00"
close-reason: "implemented, reviewed, landed with C2c (master after b05b406c): gate chain /tmp/jc.7p fixpoint at gen2 rc 0 (real 445 s), full suite 293 of 293 PASS rc 0 (/tmp/jt.yi/suite.log)"
---

Problem: C2c's check (jerry-c2c nkwlmvos, render.f FACTS-SAME?, check-hook.f CHECK-FACTS) refuses 298 definitions in a candidate build (/tmp/jc2.zz/diffs.txt, bad=1 rows): in packages comp:arm64, comp:x86-64 and comp:label, facts mode refuses (SGBAD) a type named with a package part, comp:label:head, comp:label:branch, or a parent's type named from inside a part, comp:vreg, comp:cc, comp:shifter. Example: mark-xt: ( n -- ptr [ A -- A comp:label:head | B -- B ] ), pkg comp:label. Unknown: whether REND-FACTS spells the type wrongly or the facts-mode parser cannot resolve it from SIGSCOPE! of a part's path. Acceptance: (1) a root-cause report from a reproducer (.jj-ws.noindex/jerry-rca-part, brief /tmp/jerry-big/brief-rca-part.md); (2) a fix brief and lane; C2c reruns its proof a-d after the fix. Files: src/core/render.f, src/core/checker.f (by the RCA). Verify: the reproducer reads every row back same; C2c's build passes with the throw. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rca-part
