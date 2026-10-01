---
title: "Name the mirror's BEGIN nest limit"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T05:25:37.874991+02:00\""
closed-at: "2026-10-01T12:50:20.170981+02:00"
close-reason: Landed as f1dcae76 (r4-mirror 8e3fb757); Fable review ACCEPT
---

Problem: the Gforth mirror's nest-limit check (bootstrap/cg/jit.fs EMIT-SNAP-NEST-CHECK) makes a compiled engine exit 75 with nothing on stderr when a definition nests BEGIN frames past SNAP-FRAMES (28); native's check names the limit and the depth. Measured by the r4-mirror lane: depth 29 on the fixed mirror exits 75 silently. Acceptance: the mirror's refusal states the limit and the depth reached on stderr with the same exit status native uses for it, the test written first through the bootstrap path sees the message; recovery output for sources within the limit unchanged (cmp hb-stage, hb-stdin-mk, hb-stdin, stage2-src). Files: bootstrap/cg/jit.fs, test/bootstrap-begin-nest*.f/.fs, tools/bootstrap.sh if the step changes. Verify: Gforth check-only recovery. Depends: habu-keep-the-mirror-3e10d406. Ownership: mirror nest-limit diagnostic.
