---
title: "V2 typestate: stage kind families"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-13T16:16:55.874793+02:00\""
blocks:
  - habu-v2-typestate-evidence-f124dc85
---

Implement sub-dot 1 of the R7 typestate addendum: MODEL-CAD-V2-PLAN.md:1644-1656 (full spec; design at 1280-1643). Stage families for MODEL/TIR/RIR/PLAN/KIR/CAND/ART as staged PRODUCTs pairing CAD-KIND ids with package-sealed proof tokens. Review foldings (BLOCKING): (a) Files must include homes for CAND and ART stage families - the design's maki/ir/*/stage.f dirs do not exist yet; follow existing flat maki/ layout or deliberately introduce the dirs with FILEMAP rows; (b) create the RESULT-DROP helper the fixture sketches use (plan:1571-1583) with tests; (c) acceptance must force ALL 13 transition words incl. MODEL:ELABORATE, TIR:SOLVE, CAND:EMIT, ART:BUILD (plan:1534-1541), and add the missing negative for 'unverified KIR cannot enter target emission' (plan:456). Acceptance: wrong-order fixture TS-BAD-ORDER verdict 0 (plan:1560-1570) plus per-transition negatives. Verify: new typestate-test suite green, maki/test.f, typed-local-diff-lint. Depends: none (first in chain). Ownership: new stage-family files + typestate tests. Claim: unassigned.

## Parked 2026-07-13 (session limit)
Worker (tsstage) terminated by API session limit with NO committed work (empty
workspace tip). Not started. Claim released. Resume per the R7 sub-dot 1 spec
above; it is the head of the typestate chain (blocks evidence-f124dc85).
