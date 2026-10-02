---
title: "End check.f's child when check.f is ended"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T05:11:06.490698+02:00"
---

Problem: tools/check.f runs the checked program in a child (bin/hb --load <HB_TMP>/habu-check-*/run.f) and leaves it running when check.f itself is ended: 'timeout 20 bin/hb --load tools/check.f -- spin.f' (spin.f: ': SPIN ( -- ) begin again ; SPIN') rc 124, and the child still ran afterwards (pgrep found it; engine rb3/g1 on 53e02ad8). The r4-semiquot lane hit it: an orphan spun for about 34 minutes. GNU timeout's group kill does not reach it, so the child runs in its own group. The gate root already answers SIGTERM/SIGINT/SIGHUP by killing its rows and dying of the same signal (docs/gate.md, test/gate-signal-test.f). Acceptance: check.f ended by SIGTERM, SIGINT or SIGHUP kills its child and removes its temporary directory, then dies of the same signal, through the gate root's mechanism rather than a second one; a case through the real load path, seen failing first. Files: tools/check.f (or the shared run helper it uses) and its test.
