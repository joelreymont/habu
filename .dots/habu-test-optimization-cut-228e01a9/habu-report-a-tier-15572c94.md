---
title: Report a tier-1 compile refusal in prop-test
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T10:40:34.540501+02:00"
---

Problem (review 267 of keeparity b4977a65): test/prop-test-core.f:335 CONFIRM-FR? assumes `0 set-check` compiles a checker-rejected body; that holds only at tier 0 (the JIT skips the empty hook cell). At tier 1 KEEP-ARITY refuses the body (throw 70 since b4977a65, -8579 before), the throw escapes evaluate, and neither CONFIRM-FR? nor RUN-CORE catches it, so a tier-1 run dies instead of reporting; the false-reject oracle is tier-0-only and does not say so. Probes: $HOME/.cache/tmp/kestrel-r4-rev267/p-fr0.f, p-fr2.f. Acceptance: CONFIRM-FR? catches the compile refusal and reports it as unconfirmed rather than a false reject (or the test pins `0 set-tier` with that reason); a `1 set-tier` run of test/prop-test.f with a seed that yields a non-perturbed reject ends with its summary lines and exit 0; the tier-0 certified/false-reject counts for a fixed seed are unchanged; failing case first. Files: test/prop-test-core.f. Base: after keeparity b4977a65 lands.
