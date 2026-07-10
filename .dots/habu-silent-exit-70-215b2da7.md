---
title: Silent exit 70 on --load reject of tool file
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T22:09:00.902375+02:00"
---

bin/hb --load tools/public-signatures-core.f (or its test) exiting 70 printed NOTHING on stdout or stderr when a definition inside the required file rejected (E-UNDEFINED: TFAM-DERIVE-EQ? before the PRIM row landed). The same file through bin/hb --load tools/check.f -- --json-errors prints the full packet. The --load/require chain must emit the reject diagnostic before exiting 70; a silent fail-closed exit violates the build-the-tool/no-guessing workflow (found during derive S1, 2026-07-10). Repro: remove a PRIM row a tool uses, rebuild, bin/hb --load that tool -> silent rc=70.
