---
title: Name the engine under test without HABU_UNDER_TEST
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.479643+02:00"
---

Problem: a gate names the engine under test to its children through the environment variable HABU_UNDER_TEST. src/lib/engine-candidate.f:25 reads it first, tools/launch-context.f:28 keeps a second resolver copy, test/gate-stdlib-lib.f:44, test/catch-stale-suite.f:189 and tools/chain-run.f:53 set it for children, and tools/check-core.f:1492, :1974 and several tests document it. An environment census classed it replaceable. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. What replaces it (an argument, a flag or program text) is a choice of public interface nobody has made. Acceptance: a design choosing the carrier for the engine under test, approved by Joel and recorded here; then HABU_UNDER_TEST is read and set nowhere. Files: src/lib/engine-candidate.f, tools/launch-context.f, tools/chain-run.f, tools/check-core.f, test/gate-stdlib-lib.f, test/catch-stale-suite.f and the tests that name it. Verify: rg -n HABU_UNDER_TEST prints nothing outside .dots. Depends: none. Ownership: unassigned. Claim: unassigned.
