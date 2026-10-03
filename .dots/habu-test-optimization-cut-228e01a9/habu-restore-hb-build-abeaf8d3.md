---
title: "Restore hb-build-aot's pre-window refusal case"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T08:52:24.867911+02:00"
---

Problem: round-4 regression. tools/hb-build-aot-test.f PASSES in gate A (d40cc36d, $HOME/.cache/tmp/kestrel-r4/gate-a/gate.log:185) and fails on the round-4 tip 53e02ad8: 'bin/hb --load tools/hb-build-aot-test.f' (engine rb3/g1) rc 1, F108 'expected 74 got 0' and F109, in KEYED-PRE-WINDOW-REFUSED (:200-204), called for HBT-SPAN-SRC$, HBT-UNREQUIRED-SRC$ (42 FMT:.INT) and HBT-EXBUILD-SRC$ (:228-230): the keyed maker builds a program whose closure should reach a word defined before the capture window opened, instead of refusing it (74 'aot: closure reaches a word defined before the capture window opened'). Found by the r4-hbbinst lane (0d2dc1d6); log $HOME/.cache/tmp/kestrel-r4-hbbinst/ and the lead's rerun. FMT:.INT is not in rb3/g1.names. Acceptance: name the round-4 commit that changed this (bisect the line master@origin..53e02ad8 by source; the maker loads from source) and whether the refusal or the test's premise is what changed; fix the responsible layer so the three programs are refused by name again, or, if the premise changed legitimately, give the case a program that still reaches a pre-window word and say why; tools/hb-build-aot-test.f rc 0. Base: 53e02ad8.
