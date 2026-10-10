---
title: Name a refused does> clause at tier 0
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T10:26:26.247170+03:00"
---

Problem: at tier 0 under --load a definer whose does> clause the checker refuses prints only `does> at <file>:<line>`, rc 70, with no diagnostic, whatever the reason; master the same. ~/.cache/tmp/carl-rsin/does-diag/S14.f (`: DEF ( n -- ) create , does> ( -- | n ) >r ;`, a declared return-stack cell) and S42.f (`does> ( -- bogus ) @`, an unknown type). Its JSON packet is correct (fix_return_stack for S14). A refused colon body prints the checker's diagnostic (word, code, token, place). Tier 1 refuses the same clauses -8572 rc 67 with no diagnostic (t1-S14.f, t1-S42.f): habu-report-a-refused-de5d1404 owns that path.
Acceptance: at tier 0 a refused does> clause prints the diagnostic a refused colon body prints, naming the fix where its class has one (S14: fix_return_stack), rc 70; S14 and S42 join test/outer-interpret.f.
Files: src/habu/habu2.f (LCOMPILEDIE's refusal tail, LATMSG) or the clause path that reaches it, test/outer-interpret.f.
Verify: rebuild bin/hb per docs/gate.md; the two reproducers under `bin/hb --load`; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/run.f`.
Depends: none.
Worker: worker.
