---
title: "Read .( as the engine does in the check's pre-pass"
status: open
priority: 2
issue-type: task
created-at: "2026-10-06T21:48:27.938104+02:00"
---

Problem: the native engine defines no `.(` (`.( hello )` under --load: E-UNDEFINED: .(, rc 70), but the check's pre-pass skips its text as a print opener (src/habu/verify-source.f PRINT-OPENER? ~211, NEXT ~345). So tools/check.f answers `.( hello )` with a raw 'E-UNDEFINED: .(' line and no packet in plain and --all-errors (rc 70) and accepts it in --verify-only (rc 0); and `.( s\" A\yB" )` then `require missing-dep.f` makes discovery (which reads `.(` as a word) stop at the bad escape while the pre-pass never sees it, so plain and --verify-only end 'check.f: the verifier did not complete: exit 74' rc 69 and --all-errors rc 74 with no packet. Measured on the escape fix's engine (rev-l4esc review F2, probes r03 and q02 in ~/.cache/tmp/heron-arm64/evidence/rev-l4esc/probes/). Acceptance: the pre-pass reads `.(` as an ordinary token, as the engine and discovery do; each file above is one refusal of `.(` as an undefined word at its position, rc 70, in plain, --all-errors and --verify-only, the load stopping there; tools/lint/source-lex.f keeps its `.(` rule (Gforth .fs sources and lint fixtures use it; no .f under src lib tools test calls `.(`). Files: src/habu/verify-source.f; the check suite that holds the cases (test/diag-position-test.f). Verify: the new cases red then green on the current engine; rows diag-position, check-verify, check-cli-boundary, source-discovery. Depends: the escape fix ("Decode escaped literals the check reads") on master. Ownership: verify-source.f's opener set and the new cases. Claim: unassigned.
