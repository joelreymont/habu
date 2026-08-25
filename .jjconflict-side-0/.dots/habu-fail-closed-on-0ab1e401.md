---
title: Fail closed on negative extra-pad in construct/MATCH xpad record
status: closed
priority: 2
issue-type: task
created-at: "2026-07-22T14:04:11.627759+02:00"
close-reason: "Landed as 932507edad14."
---

Problem: src/core/type-family.f:1978 TFC-CON-XPAD-RECORD and :1987 TFAM-MATCH-XPAD-RECORD guard 'extra 0 >' and silently drop negative extra-pad facts. extra<0 occurs when the widest-declared variant is not the widest-instantiated one; pass-1 then emits declared pads with no pass-2 correction, so a construct certifies at width W while the native bundle is W+1 (proven live: xneg<xw2> certifies 5 cells, runtime produces 6; positive-extra control reconciles). Every checked caller runs with the stack shifted: silent corruption in fully-checked code. Expected fix: fail closed at the construct/of site for extra<0 (CONSTRUCT-WIDE-STAGED-REJECT precedent) OR carry signed xpad corrections through pass-2; choose whichever preserves the certified-width invariant structurally, not by value heuristic. Acceptance: negative checked regression with the two-variant asymmetric-growth fixture (declared-widest != instantiated-widest) -> reject or correct width, plus runtime depth probe parity for both signs of extra. Reproducers: scratchpad hb-audit-checker/xpad-neg.f, xpad-rt.f (session 94a7f15f). Files: src/core/type-family.f, test suite for construct width. Verify: new fixtures + bin/hb --load test/type-match-suite.f + maki/test.f. Depends: none (disjoint functions from the linearity-reader dot; same file, coordinate merge). Ownership: TFC-CON-XPAD-RECORD/TFAM-MATCH-XPAD-RECORD and pass-2 xpad consumers. Note (2026-07-22): master 703e0f8b moved the recorder sites to type-family.f:2124/2133 (still skip-guarded, hole re-proven live on a fresh master probe); re-application runs in the -m workspace on base 703e0f8b.
