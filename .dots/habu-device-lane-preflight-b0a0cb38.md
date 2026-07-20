---
title: Device-lane preflight must run the combined macOS gate image
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T14:07:13.176502+02:00"
---

Three device-side landings today broke the macOS certification path in ways the linux device lanes never see: the aot-data-span forge test pinned linux literals, the match-factor-pin fixture pinned linux die-block syscall bytes, and the autotune-sweep zero-side-effect test asserted absolute arena state that only fails in the combined gate-stdlib spawned image (the mma-exact member allocates first). Root process gap: device-lane preflight runs device slices and per-file tests but not the combined ptx-toolchain image or the full macOS-shaped standalone gate, so cross-member state leaks and per-target byte pins land green on linux and red on the Mac. Fix in the device-lane gate script(s): before any master push from a device lane, run the full test/gate-stdlib.f standalone image (host-only members; device legs already skip without hardware) so combined-image state leaks are caught host-side, and require per-target gating or per-target pins for any fixture that hardcodes target bytes or paths. Owner: the device-machine executor's preflight; coordinate via its AGENTS.md or gate wrapper rather than Mac-side files.
