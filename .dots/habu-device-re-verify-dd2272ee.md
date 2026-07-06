---
title: Device re-verify cuLaunchKernel migration on Orin
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T00:14:06.516659+02:00"
---

Follow-up to habu-rca-culaunchkernel-ee5babba (RCA done, launch migrated to cuLaunchKernel + FFI-KPARAM via lib/cuda-launch.f CU-LAUNCH-1D in tools/ptx/cuda-launch.f + maki/gpu.f G-LAUNCH). The cuLaunchKernel MECHANISM is already device-golden on the Orin via tools/ptx/culk-probe.f (rc=0, cuCtxSynchronize rc=0, read-back 0x40C00000=6.0); the migrated launchers use the identical helper and compile clean, and all local gates (host-lint, filemap-lint, trusted-inventory, typed-local-diff-lint, ffi + maki suites) are green. What remains is a direct on-device golden run of the MIGRATED files, which could not be done in-session because the Orin (ssh alias zed) went unreachable partway through (was up earlier for the full RCA experiment set). When zed is back: (1) emit SAXPY PTX locally (bin/hb --load lib/errors.f lib/string.f lib/float.f lib/fmt.f src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/tile.f tools/ptx/saxpy-cg.f), scp to /tmp/saxpy.ptx, ptxas -arch=sm_87 -> /tmp/saxpy.cubin; (2) scp the migrated tools/ptx/cuda-launch.f + lib/cuda-launch.f into ~/Work/habu on zed and run cd ~/Work/habu && ./bin/hb --load lib/errors.f lib/string.f lib/ffi.f lib/cuda-launch.f tools/ptx/cuda-launch.f -> expect PASS 0x40C00000; (3) HABU_ZED=1 run maki/gpu-train-test.f (SGD epochs) golden. If any mismatch, RCA before merge. No code change expected -- this is the device-golden sign-off gate for the migration.

## Outage + parked chain (2026-07-07)

The zed Orin box is unreachable for ~a week; expected back ~2026-07-14. Until
then this device sign-off cannot run. State: the standalone-load commit was
extracted and merged independently (head 7fce161077f8, its dot closed) since it
never depended on the migration. The remaining pair is PARKED in
.jj-ws/seal-hardening: cd144b09 "Migrate launch to cuLaunchKernel; RCA the hang"
+ 4d51b403 "Add stdlib manifest row for lib/cuda-launch.f" (ids float across
rebases; identify by description). Risk is low: the cuLaunchKernel MECHANISM the
migration uses was ALREADY device-golden-proven on the Orin via
tools/ptx/culk-probe.f (rc 0 / cuCtxSynchronize rc 0 / read-back 0x40C00000 = 6.0)
before the box went down, and the migrated launchers use the identical
CU-LAUNCH-1D helper and pass all local gates -- only the direct on-device run of
the migrated files themselves is outstanding. When zed returns (~2026-07-14):
rebase the pair onto the then-current head, run steps (1)-(3) above, then merge.
