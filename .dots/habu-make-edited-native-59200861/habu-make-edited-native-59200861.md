---
title: Make edited native rebuilds incremental
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:09:07.007563+02:00"
---

Incremental compilation and linking serve Habu itself, REPL applications and stripped applications; bootstrap recovery remains a separate entry. Keep definition reuse at the existing native compiler boundary while ordinary source initialization executes in order. Consumed input, binding, checked contract, folded value, source origin and captured storage facts must remain correct. Do not restore obsolete whole-FILE replay or copy the DATA prefix per definition.

Public correctness milestone: a69b39012b2fa2ff209016f19001dc2bc075be17 was qualified, with engine948295deb3455501f9acc50b43d5f16d6deeeaa243abb3d35154eb44ebd0e4f9 and map479fa0dd16b3bb5292468c79a796665437b3030024a04989b3d92e4575cbb58c. Independent reviews cleared the stale-family miss, capture scratch, verifier identity and exec census fixes. Full native gate486/486 exits0; five native engines/maps are byte-identical. Early/middle/late engine edits match cold executable/map and preserve runtime/origins and invalid-input refusal. Unchanged repeat takes1.10s; actual edits13.55/14.22/10.02s under load. These edits are not subsecond.

Real Maki d5c2d976 cold/warm/fresh images are byte-identical and restore successfully; warm8715definition hits/5861body skips/369compiled takes3.73wall/3.27user/.44sys. Nested transitive-family edits now cause a bounded cache miss after cleanup; malformed encountered entries still refuse. Existing runnable family-edit E2E and installed vocabulary/cache checks pass. The original e34 containers have different producer keys; importing them was not claimed. Kevin's retained Tender shared-cache76 requires its own unchanged-caller replay before attributing it to this reduction.

The reviewed scratch-clearing optimization is qualified: converged engine6ec91554/map0a4ca90e passes486/486 native suites and five identical native/map generations. Unchanged Habu repeat takes1.225s; early edit18.018s under load matches its cold executable/map. Frozen Maki cold/warm/fresh images match, with three warm runs3.27–3.28s wall/2.80s user,8715hits/369compiled. The prior warm observation was3.75s/3.28s user; bracketed candidate comparisons showed5–13 percent CPU reductions with load variation. Remaining priority: reduce whole-invocation edits further; these measurements do not meet subsecond edits. Profiled costs include protected stores, exact binding-name comparisons and safely refused checker-busy cache stores. Existing profiler names after native rewind/reuse are not uniformly trustworthy; habu-profile-native-replay-9fe4b920 tracks that attribution boundary. Keep measurements representative and separate whole-process time from sampled portions.

Acceptance for further changes: runnable real-load cold/warm/root/leaf/early edits, exact cached/cold executable and map equality, source/effect/ownership rejection behavior, matching changed-engine execution, independent review, required native gate and multi-generation convergence. No blanket cache clearing, caller workaround or permissive import. Retain this parent while its substantive children remain unfinished. Jerry owns test-suite reduction and has delivered a299.76s warm-cache gate; do not make further test-speed work a prerequisite for incremental build fixes.
