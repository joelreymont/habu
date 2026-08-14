---
title: "Chase the window's d0 alignment rounding"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:35:40.625651+02:00"
---

Found by the prewindow landing (3443a30d): the capture window's DATA span is 8-alignment-sensitive to d0 - any pre-window byte change moves the baked span by up to 7 bytes and thus the aot-seed size row (proven: a bare 27-byte pad reproduces the -4 on unchanged source). No align appears in the window sources; something inside the window rounds DP up. Find it, pin it, and either fix the sensitivity or document the noise band on the ratchet row - every lane touching the metabuild prefix pays this today. Files: src/habu/stdin.f / aot-capture.f (the window open path). Depends: none.
