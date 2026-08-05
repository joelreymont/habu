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

Attribution (2026-08-05, agent=kv-attr). NOT host state and NOT the reboot.
maki/infer/kv-cache-test.f:38 KVT-MUST-SESSION calls GPU:OPEN for every case;
maki/gpu-session.f:162 OPEN -> GS-START -> GS-OPEN-DRIVER -> MKD:OPEN reaches
lib/ptx/cuda-driver.f:63 CUDA:OPEN?, which is dlopen("libcuda.so.1"). macOS has
no libcuda, so GPU:OPEN returns E-CUDA (-5002) and every case that needs a
session fails; the last of the 37 is the one the title named. Proven three ways:
a direct GPU:OPEN probe returns -5002 on this host; installing only the nine
session-open fakes that maki/gpu-session-test.f already uses makes GPU:OPEN and
GPU:CLOSE succeed here unchanged; and deleting the first failing case leaves the
other 36, so it is not leaked state from an earlier member. The suite began
requiring a device at 11a9ffbf "Own device KV storage", which introduced the
GPU:OPEN call (the preceding version e7677541 has none) - so no macOS host has
run it green since, and the reviewer's green run was on a host with a driver.

Also found: maki/test.f is not host-independent. It aborts at the FIRST device
suite, which is why only kv-cache's failures were visible; with kv-cache removed
the same -5002 stops the run at maki/gpu-buffer-test.f, and maki/lower/model-test.f,
maki/onnx/deploy-test.f, maki/infer/gpt2-model-test.f, maki/eval/device-fault-test.f
and maki/device-smoke.f all reference the driver without installing the MKD fakes.
"maki green on the Mac" therefore cannot be reached by fixing this one file.

Landed here: the precondition is now checked once and named (exit 74,
"no CUDA driver (dlopen libcuda.so.1 failed)"). Suite membership deliberately
left alone - see the report for the two options and why the choice belongs to
whoever owns the maki gate policy.
