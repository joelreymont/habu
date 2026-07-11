---
title: Silent exit 70 on --load reject of tool file
status: closed
priority: 2
issue-type: task
created-at: "2026-07-10T22:09:00.902375+02:00"
---

bin/hb --load tools/public-signatures-core.f (or its test) exiting 70 printed NOTHING on stdout or stderr when a definition inside the required file rejected (E-UNDEFINED: TFAM-DERIVE-EQ? before the PRIM row landed). The same file through bin/hb --load tools/check.f -- --json-errors prints the full packet. The --load/require chain must emit the reject diagnostic before exiting 70; a silent fail-closed exit violates the build-the-tool/no-guessing workflow (found during derive S1, 2026-07-10). Repro: remove a PRIM row a tool uses, rebuild, bin/hb --load that tool -> silent rc=70.

## CLOSED — not reproducible on any committed engine; invariant pinned (2026-07-11)

Static invariant: a rejecting load must identify itself on stderr on EVERY load
leg; the diagnostic surface lives in src/core/render.f (UNDEFERR/E-UNDEFINED
rendering, `habu: in <word>: at '<tok>'` body rejects) reached from the load
paths in src/habu.

AUDIT (exact repro simulated, current master engine): copied
tools/public-signatures-core.f with TFAM-DERIVE-EQ? renamed to an undefined
word — `bin/hb --load` prints `E-UNDEFINED: <name>` to stderr and exits 70. SIX
legs all report correctly: direct --load, multi-file --load (failing file
second), require chain, `include` chain, stdin, package-wrapped definition, and
the test-file pair. Also plain body rejects (`habu: in <w>: at 'drop' …`). No
swallow exists anywhere on the committed load surface.

Explanation: the silent run happened on the derive worker's LOCALLY REBUILT
mid-S1 engine (the PRIM row removed from their in-progress tree) — a broken
build of the diagnostic surface itself, not a committed-engine gap; the
diagnostic-attribution chain (00ed4616 RBUF fail-closed, c41afe48 attribution,
0005a325 repair packets, 44efc694 de-masked top-level throws) had also just
landed in that window. No committed commit needed a fix.

PINNED: test/load-reject-diag-test.f (gate suite load-reject-diag) spawns the
engine (HABU_UNDER_TEST else bin/hb) on generated rejecting fixtures and
asserts, per leg: exit-kind EXIT, rc 70, stdout EMPTY, stderr NON-EMPTY and
NAMING the failing token (E-UNDEFINED + word for the direct and require-chain
legs; `in lrd-y` + `at 'drop'` for the checked-body leg). Any future silent
exit-70 regression goes RED here. If it ever fires, the fix belongs in
src/core/render.f / the src/habu load surface (src/core currently owned by
derive S2).
