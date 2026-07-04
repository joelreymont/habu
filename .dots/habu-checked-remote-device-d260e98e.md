---
title: Checked remote device-run harness (ssh zed)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:09:43.066705+02:00"
---

Capability: run CUDA device tests on the Orin box from this Mac. ssh zed works (BatchMode, host GTW-ONX1-E19Z46SH, aarch64, /usr/local/cuda/bin/ptxas, /usr/local/bin/nvcc, GPU Orin). Build a checked Habu harness (Habu-only rule; NOT a shell script): tools/zed-run-lib.f + tools/zed-run.f using lib/process to spawn ssh with argv (no shell interpolation of test input), rsync-or-scp the needed artifacts (PTX text, cubin, driver source) to a private remote scratch dir, execute the remote command, capture stdout/stderr/rc, and map them into the local suite (T{ }T / gate patterns). Fail closed: unreachable host, nonzero remote rc, missing remote toolchain each produce named throw codes (E-ZED-* block in lib/errors.f), never silent skips; a suite that needs the device SKIPS EXPLICITLY with a printed reason only when HABU_ZED=0/unset (policy decision recorded in the harness header). First consumers: habu-fix-ptx-collective-997cfcce device goldens, habu-make-ptx-device-c0eb12a3 remainder, then gradcheck harness habu-ptx-ad-device-2b511851.
