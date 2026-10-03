---
title: Build the span AOT row on the image
status: open
priority: 4
issue-type: task
created-at: "2026-10-02T09:14:00.392042+02:00"
---

Problem: tools/hb-build-aot-test.f:167-168, BUILD-AOT-SPAN, gives 'lib/span.f is in the linker's lib closure' as the reason the row builds on the engine (HBT-HBB-PREPARE-AOT-SOURCE, :173). False since master 017ccec7 built lib/span.f and ALLOC-SPAN/FREE-SPAN into the engine: the r4-aotpre lane (fe32a191, dot abeaf8d3) measured the span program linking on the linker image rc 0 and its image printing the expected text. Acceptance: the row builds through HBT-HBB-PREPARE-AOT (the image), or the comment states the true reason it must stay on the engine; the false sentence goes; hb-build-aot-test rc 0; wall time before and after. Base: after fe32a191 lands.
