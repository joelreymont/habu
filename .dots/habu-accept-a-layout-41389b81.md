---
title: Accept a layout-typed value in the interpret-mode REPL
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T20:23:43.601712+03:00"
---

Problem: a native Maki image built from Maki e357dbb6 with the maki-pin engine (Habu 10d66991) refuses, at its TTY REPL, the line `2 QTY:MM 3 QTY:MM PCB:AT PCB:POINT-X QTY:NM@` with `hb: interpret-mode layout value: PCB:AT`: a word answering a checked layout value (PCB:point) cannot be interpreted at the REPL, so Maki's warm-recapture acceptance (test/native_image.py, awaiting MAKI-POINT-OK) times out. Piped circuit and import and both CLI DRC checks pass first. Measured by alder 2026-09-13; log /home/joel/Work/maki/.jj-ws/alder-bindings-design/build/remove-swig-native-image.log with the image build log beside it. Acceptance: the interpret mode of a native image holds a layout-typed value on the stack between REPL words exactly as the compiled path does (or docs/forth.md states the rule that forbids it and the accepted form, and Maki's harness is changed to that form by its owner); a Habu test with a checked layout value read back at the REPL; Maki's test/native_image.py reaches MAKI-POINT-OK on a fresh image. Files: the engine's interpret-mode value handling, docs/forth.md, a test. Verify: the Habu test; Maki test/native_image.py on a fresh image from the pin. Depends: none. Ownership: Habu maintainer (cedar). Claim: independent Astra reduction in progress; Cedar integration.
