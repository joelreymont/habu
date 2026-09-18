---
title: Name an input underflow at the token that consumes
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T08:09:44.145103+03:00"
---

Problem: a word called with fewer cells than its declared inputs inside a definition is refused at the NEXT token with no reason: ': CONSUME ( n -- ) drop ;  : G ( -- n ) CONSUME 0 ;' answers 'habu: in g: at '0'' / 'hook: non-certified definition: g at '0'' and nothing else (measured 2026-09-18 on the release engine e7b1e2dc and on the pre-rule engine 57b804c1, so pre-existing; birch/Tender hit it through a probe whose RUN called DOC:CLOSE ( document -- ) after binding the document into a local and read the refusal as a checker regression). The same with a locals binding before the call reports the same. Acceptance: the refusal is attributed to the consuming token ('at CONSUME') with a reason naming the shortfall (expected n, stack empty: the E-UNDERFLOW family or the value-position diagnostic with its expected/actual line), rendered through the same MDIAG path as other value-position refusals; regressions for the bare case, the locals case and a two-deep shortfall in test/compiler/ (or test/checker-*), and the render class documented. Files: src/core/checker.f (CHECKER-STEP's FAILSET and the pin the token owns; see habu-latch-value-pos-373cad6b, the same latch seam), src/core/render.f, test/. Verify: the regressions; test/run.f. Depends: none (habu-latch-value-pos-373cad6b is the natural place). Ownership: checker diagnostics. Claim: agent=hazel-diag-latch workspace=.jj-ws/hazel-diag-latch.
