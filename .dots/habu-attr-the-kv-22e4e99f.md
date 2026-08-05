---
title: Attribute the kv-cache device-close red
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T11:01:50.431965+02:00"
---

Claim: agent=kv-attr workspace=.jj-ws/habu-attr-the-kv-22e4e99f

maki/test.f red at maki/infer/kv-cache-test.f:1282 'device close failure with host release'. Two independent lanes reproduced it on master's own tree (e04bd6fa) with its own fixpoint engine on the Mac — it is NOT from the codegen or measurement diffs (proven by lib/errors.f revert control and by require-closure disjointness). Master's merge gates were green when cd7bf8eb landed, so either host state changed (reboot on 2026-08-05, device/Metal state) or the test is order/environment sensitive. Diagnose with the debugger per docs/debugging.md, attribute properly, fix the root cause; this blocks the maki-green requirement for every merge until resolved.

Evidence update: reproduced RED on e04bd6fa in .jj-ws/merge-gate with master's engine (TFAIL assert 40, 'device close failure with host release'). The external review ran the full maki suite GREEN on the same reviewed tree on its own host, so the failure is Mac-host-specific — prime suspect is device/Metal state after the 2026-08-05 reboot, not code. Diagnose host state before touching the test.
