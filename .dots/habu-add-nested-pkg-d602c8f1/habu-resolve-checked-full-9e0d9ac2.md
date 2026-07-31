---
title: Resolve checked full namespace paths
status: active
priority: 1
issue-type: task
created-at: "2026-07-31T06:35:00.353571+02:00"
---

Source dependency: exact reviewed E1 namespace rows and the frozen native last-separator contract; this stop-the-world branch keeps upstream dots active until M17, so exact code ancestry enforces ordering. Owner: checker, renderer, and live xref. Change the existing qualified-name scan to record only the last non-edge colon while independently rejecting leading, trailing, or doubled separators. Resolve the exact full prefix; keep checker identity as full-prefix plus public role plus tail. Xref lookup accepts package and type rows and uses named none/bad results instead of raw sentinels. Checker and xref must share the same semantics but not copied state machines. No namespace kind table, parent/path side table, compatibility spelling, version, ancestor lookup, using change, or native emitter edit. Write set: src/core/checker.f, src/core/render.f, src/habu/xref.f, test/engine-suite.f, tools/xref-test.f, and only necessary current TRUSTED.md row deletion. Pre-M17 proof is source census and hunk review only. M17 acceptance: deep sibling identities remain distinct, valid deep names resolve through production checker/xref paths, malformed names differ from undefined names, and exact full-prefix diagnostics are retained. Claim: agent=e2a_checked_impl workspace=.jj-ws/habu-resolve-checked-full-9e0d9ac2.
