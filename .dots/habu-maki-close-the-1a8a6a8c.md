---
title: "maki: close the durable schedule-replay loop (PROMOTE->TILE rehydration)"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-13T10:29:58.036550+02:00\\\"\""
---

Flow-tracer gap (2026-07-13, goal e93371de): the durable replay loop never closes on any production path. PROMOTE-EVIDENCE (maki/cad.f:1032) writes SCHED-PUT durable-only; TILE-REPLAY-NOTE (cad.f:927) reads SK-GET memory-only; the bridge SK-PUT-DURABLE (store-replay.f:46) and rehydrator STORE-REPLAY-LOAD (:51) have ZERO callers outside maki/store-replay-test.f. Live evidence: after a passing PROMOTE, SCHED-GET hits but TILE still renders 'schedule: unmeasured shape class -> using defaults', same-process AND fresh-process; manual SK-TAB-RESET STORE-REPLAY-LOAD fixes it - the capability works, nothing invokes it. Contradicts maki/store.f:43-44 ('written by PROMOTE') and store-replay.f:9-10 ('fresh process can rehydrate') and the docs/model-cad.md replay north-star. FIX: PROMOTE-EVIDENCE records through SK-PUT-DURABLE (memory + file); the TILE/TUNE entry (or a session-init word) does SK-TAB-RESET STORE-REPLAY-LOAD before replay lookup; regression asserting TILE reports a replay HIT after PROMOTE in same-process and fresh-process (child-spawn idiom) runs. If wiring is deliberately deferred to a cad-6 slice, instead correct the store.f/store-replay.f claims and re-dot - but prefer the wiring (small, capability proven). ALSO: cad.f:957-959 stale comment says gradcheck stays not-run for matmul models; live row is gradcheck=pass - fix the comment. Files: maki/cad.f, maki/store-replay.f, maki/store.f, maki/cad-test.f or store-replay-test.f.
