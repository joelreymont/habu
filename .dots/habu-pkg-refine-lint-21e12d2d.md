---
title: Package refine lint core and test
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T12:53:02.530993+02:00"
closed-at: "2026-08-02T15:17:47.130815+02:00"
close-reason: "Superseded by the a8c716c5 hard cut: refine-lint and its administrative seed registry were deleted; no lint core remains to package."
---

Same wall, same fix as the landed habu-pkg-stale-status-9d6a69b2 precedent: tools/refine-lint-core.f is an unpackaged legacy lint, so the S6b1 lane's required seed rows (the two GPT2TX mint registrations) trip E-PACKAGE-OWNERSHIP on RFL-SEED#, RFL-SEED-NAME$, RFL-SEED-OWNER$ - and per the anti-exception rule the allowlist is not a fix. Behavior: open package RFL across tools/refine-lint-core.f (about 220 references), tools/refine-lint-test.f (69), tools/refine-lint.f (2), following the STALE-STATUS-LINT seam design - short package-local tails, a capability-shaped public surface sized to what the CLI and the gate slice actually call, NO raw variable exports, callers migrated in the same commit. Behavior byte-identical, proven with the stale-status discipline: identical stdout/stderr/rc across the lint's CLI paths before and after, plus a mechanical rename-map audit isolating the structural changes. Acceptance: the exact probe that fails today (a one-line body edit) passes package-diff-lint after; refine-lint and refine-lint-test green; the lint-tools slice green through test/run.f; typed-local and package diff lints on the full diff; maki/test.f green. Owner: new package RFL. Dependencies: none. Claim: agent=s6b1 workspace=.jj-ws/habu-s6b1-prepare (precedes the seed-row commit in the same lane).
