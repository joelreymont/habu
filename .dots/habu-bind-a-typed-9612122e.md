---
title: Bind a typed local whose width a sibling local widens
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T03:59:53.731892+02:00"
---

Problem: in a typed {: :}, one local's unify can set a width another local of the same list shares after LOC-BUNDLE-ROW (src/core/checker.f) has walked past it; that cell stays logical and XG-READ-GROUP refuses it as a layout that never transports. '{: x c:option<n> :}' (WL-TWO-NONE) and '{: c:option<n> x :}' are refused at ':}' at master 81f791cc and with c9743c5a (stuart, found in the hbind fix, 2026-11-07). Fails closed. Smallest fix stuart's reviewer saw: run LOC-AS-BUNDLE?'s trial unifies before descending and test LOGICAL-KNOWN? again after the recursion (this changes the order of trial unifies). Acceptance: both orders bind every cell; a case in test/wide-typed-local-probe.f. Files: src/core/checker.f, test/wide-typed-local-probe.f. Verify: bin/hb --load test/wide-typed-local-probe.f. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-chk.
