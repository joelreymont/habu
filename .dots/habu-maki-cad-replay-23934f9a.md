---
title: maki cad replay child must run parent engine
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T18:46:35.382328+02:00\""
---

Surfaced while landing habu-gate-must-test-cd70ef4e: maki/cad-test.f:88 (RPL-CHILD-TILE$) hardcodes s" bin/hb" for its fresh-process replay child. The durable cad-store key includes the SHA of the RUNNING engine binary (lib/engine-id.f via _NSGetExecutablePath / /proc/self/exe), so when the gate runs maki against a candidate that differs from bin/hb, the replay child computes a different store key, misses the cross-engine store, and F100/F101 go red - the gate would blame the candidate for a harness artifact. Today it stays green only because a clean master's candidate is byte-identical to bin/hb. Fix: the replay child must run the SAME engine as its parent - resolve HABU_UNDER_TEST (the gate now exports it to the maki child) or the parent's own executable path via lib/engine-id.f, never a hardcoded bin/hb. Regression: run cad-test.f with HABU_UNDER_TEST pointing at a copy of bin/hb at a different path and assert F100/F101 stay green. Territory: maki/cad-test.f. Distinct from habu-per-process-cad-cde27fd1 (shared tmp/cad-store isolation), which does not cause this miss.

Claim: agent=cad-replay workspace=.jj-ws/habu-maki-cad-replay-23934f9a
