---
title: Reset clobber vector headers if a snapshot ever loads the chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T22:36:59.148824+02:00"
---

src/compiler/native/clobber.f's three row columns are now lib/vector.f vectors whose headers hold mmap pointers in DATA space. No snapshot builder loads src/compiler/native/* today (checked 2026-08-09), so the hazard is not live - but a snapshot that ever captures the chain would resurrect dead mmap pointers on restore. When the seed closure for the cut puts the chain into the image (cut leaf a5aa3f1f, seed step), these headers must be reset on image entry the same way other process-owned state is; wire it into whatever init seam the seed closure creates and delete this dot if that seam handles all vectors generically. Files: src/compiler/native/clobber.f, the seed-closure init. Depends: the cut's seed-closure step.
