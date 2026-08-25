---
title: "Chase the window's d0 alignment rounding"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:35:40.625651+02:00"
---

Found by the prewindow landing (3443a30d): the capture window's DATA span is 8-alignment-sensitive to d0 - any pre-window byte change moves the baked span by up to 7 bytes and thus the aot-seed size row (proven: a bare 27-byte pad reproduces the -4 on unchanged source). No align appears in the window sources; something inside the window rounds DP up. Find it, pin it, and either fix the sensitivity or document the noise band on the ratchet row - every lane touching the metabuild prefix pays this today. Files: src/habu/stdin.f / aot-capture.f (the window open path). Depends: none.

BROADER FINDING (cap-hash lane 2026-08-14): beyond the 8-alignment
sensitivity, AOT-DATA-D0 is baked as an ABSOLUTE (habu2.f
LAOTDATAD0) and the blob's DATA literals hold host absolutes - so
bin/hb's bytes depend on the metabuild host's DP layout wholesale:
ANY host-only allocation before the window moves the image
(measured: 524,368 new host bytes shifted the sha; a padding
control proved the capture's decisions identical). Deterministic
per tree, but this dot's fix should consider d0-relative baking
(the bake lane's artifact already stores b0-relative code and
records d0 - the image side could follow).
