---
title: Lower native typed locals
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.027236+02:00"
closed-at: "2026-08-14T11:20:33.513142+02:00"
close-reason: "Closed as SATISFIED (acceptance audit 2026-08-14): immutable locals are SSA values staging nothing (LERP/TWO-GROUPS/BRANCHG structural cases + a 24-byte no-traffic probe - local-using and bare bodies emit identical bytes); the reachable lifetime case (live across a bare call) gets a typed carrier slot proven LIVE by the engine-callee fixture; nine registered scope negatives + the adversarial DUPLICATE triple; type fidelity through the real load path. Rebinding and address-taking are genuinely unimplemented but refused fail-closed with ZERO tree uses (2026-08-06 whole-tree probe) - ordered after the cut, owned by b2a3e369 and 18a38b4f; the shadow refusal owned by ca3fdb26."
---

Full context: design Wave 4 lowers typed locals into SSA aliases or explicit homes only when address/lifetime requires storage. Acceptance: immutable locals emit no traffic, mutable/escaping locals have typed homes, scope/type/stale-home mutations reject, and differential fixtures pass.
