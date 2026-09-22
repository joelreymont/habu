---
title: "Name the site in icode's adr-out-of-reach refusal"
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T07:15:31.647421+03:00"
---

Problem (aspen's re-probe on cc455508, engine ec37691e): the standalone tender stripped build (src/main.f, 'python3 scripts/habu.py build --stripped' in ~/Work/Tender at d945db63) dies 'icode: adr out of reach', exit 72 after 50 s (log ~/.cache/tender/habu-gaps/stripped-life-hook/strip-standalone.log). src/arch/arm64/icode.f:323 ?ADR names nothing - not the site being assembled, the target label, the delta or the section it reaches into - so the only way to find the ADR is to bisect a 50-second build; aot-lib.f MAP-TARGET! names site= and target= for the same class of failure. Acceptance: the refusal names the emitting site (the record or label under assembly), the target and the delta in bytes, keeps exit 72, and a test pins the message on a synthetic out-of-reach ADR; then the tender build's refusal names its site. Files: src/arch/arm64/icode.f, its suite (test/icode-fixup-test.f or the icode suite that owns ?ADR). Verify: the icode suites; test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
