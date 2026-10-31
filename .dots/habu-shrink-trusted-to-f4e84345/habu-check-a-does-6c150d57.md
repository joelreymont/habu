---
title: "Check a DOES> body's declared fresh inputs as its callers see them"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T15:26:29.176777+02:00"
closed-at: "2026-10-31T19:00:07.402503+02:00"
close-reason: "Landed 520f4db5 'Check a does> clause's fresh inputs': CHECK-DOES-RUN takes the signature through SIG-INPUT-FRESH and SIG-BODY-FRESH and checks the output with CHECK-FRESH-OUTPUT, as colon bodies do. Proof: on a22cfbb5 (base efb69612) full suite ran 512 of 512, rc 0, generation 5 matches 4 byte for byte; rebased onto 4853f4a1 (t0tr, cown checker.f changes, different words): engine build rc 0, rows rigid-region and cast-negative rc 0. The accepted caller row for RR-DFP lands with habu-let-a-caller-9951b427 (a call binding a callee's fresh input)."
---

Problem: src/core/checker.f CHECK-DOES-RUN builds a DOES> body's rows straight from its signature, so a declared fresh-* input stays a template atom (negative key) in the body, and REGION-ATOM? / ATOM-OK? refuse it, the same defect habu-pass-a-fresh-bb992e31 fixes for colon bodies (lane frgn worker found it; it removed an untested call it had added there). Acceptance: a DOES> body whose signature declares a fresh-region input passes it to a region-generic word and returns it, as an accepted row failing on the master engine; the refusal rows of rigid-region keep their reasons. Files: src/core/checker.f, test/rigid-region-suite.f. Verify: engine build; rigid-region row. Depends: none (habu-pass-a-fresh-bb992e31 landed; reuse its SIG-INPUT-FRESH, SIG-BODY-FRESH and FRESH-CLAIM for the DOES> body rather than a second path). Ownership: checker.f CHECK-DOES-RUN. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-dfrsh.
