---
title: "RCA: Xid 31 GPU MMU fault from hb device path"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T06:41:55.050533+02:00\""
---

Joel-reported 2026-07-21 (live /proc watcher evidence from the machine-health session): every gate run that exercises Habu's GB10 device path raises an NVIDIA Xid 31 - a GPU MMU page fault: always the GR engine, always a texture/L1 read, always ONE deterministic VA per boot (0x6_5dc00000 this boot, 0x4_1dc00000 previous boot), fresh PID each time because each fault is a short-lived hb invocation (bin/hb and workspace copies, spawned by lane gates). CPU-only invocations (dot-dep-lint, checker probes) never fault; the device suites do - yet the gates are GREEN (goldens + gradchecks pass), so the invalid read is either (a) an out-of-bounds read whose value is discarded - prime suspects: vectorized v4 loads with tail handling (saxpy-v4-tail), reduction tails, TMA/box descriptors reading past N - or (b) a DELIBERATE fail-closed negative test that proves an error path by committing a real illegal access and catching E-CUDA. Find it: enumerate every kernel launch in a gate run (device-gated suites + TC-GATE matrix), search for negatives that catch launch/sync errors, then bisect by running each device suite solo and correlating (the fault oracle without dmesg access: a caught CUDA_ERROR_ILLEGAL_ADDRESS in-process, or coordinate timestamps with the health session's watcher via Joel). Fix at the root: an accidental OOB read gets a bounds/tail fix with a red-first regression test; a deliberate illegal-access negative gets redesigned to prove fail-closed WITHOUT executing an invalid access (validate at the contract layer before launch) - machine health telemetry must stay clean; Joel burned a day chasing this signature. Single fixed per-boot VA hints at one specific allocation site read at one specific offset - the offset pattern (VA ends in dc00000 both boots) is a clue worth decoding against the allocator.

Claim: agent=xid31 workspace=.jj-ws/fable-xid31 machine=spark (owns the RCA + fix for the deterministic device-path GPU MMU fault; correctness-only GPU bursts, solo-suite bisection)
