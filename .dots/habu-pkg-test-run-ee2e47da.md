---
title: Package test run-lib harness
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T19:33:02.428845+02:00"
---

Third instance of the unpackaged-legacy wall (stale-status and refine-lint precedents landed; same fix): test/run-lib.f has no package owner, so package-diff-lint rejects ANY body change to any of its 241 definitions (proven pre-existing by counterfactual: parent file plus one space in TR-USAGE flags identically) - and the perf-verdict retirement, which is now the LANDING BLOCKER for the whole repository (master deterministically red on the aggregate gate), must change five of them. Behavior: open a real package over test/run-lib.f per the landed packaging discipline (short tails; public surface sized to the real callers - measure the 14 caller files under test/ first and report the surface at checkpoint; no raw variable exports - state-shaped access becomes capability-shaped words per the STALE-STATUS-LINT precedent); callers migrated in the same commit; behavior byte-identical proven by the before/after run discipline (test/run.f full output comparison on identical trees) plus a mechanical rename-map audit isolating structural changes. If the caller cascade or the public-surface design exceeds a reviewable single commit, STOP and report the split. Acceptance: the exact probe that fails today (one-space body edit) passes package-diff-lint after; test/run.f and the stdlib gate green; both diff lints on the full artifact; maki/test.f green. Owner: the new package. Claim: agent=perfcal workspace=.jj-ws/habu-perf-recal (inserted between commit A and the retirement commit in the same lane).
