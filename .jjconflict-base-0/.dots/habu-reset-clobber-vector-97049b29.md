---
title: Reset clobber vector headers if a snapshot ever loads the chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T22:36:59.148824+02:00"
---

src/compiler/native/clobber.f's three row columns are now lib/vector.f vectors whose headers hold mmap pointers in DATA space. No snapshot builder loads src/compiler/native/* today (checked 2026-08-09), so the hazard is not live - but a snapshot that ever captures the chain would resurrect dead mmap pointers on restore. When the seed closure for the cut puts the chain into the image (cut leaf a5aa3f1f, seed step), these headers must be reset on image entry the same way other process-owned state is; wire it into whatever init seam the seed closure creates and delete this dot if that seam handles all vectors generically. Files: src/compiler/native/clobber.f, the seed-closure init. Depends: the cut's seed-closure step.

AMENDED 2026-08-11 (seed-closure lane): TEN headers, not three - publish.f's
replacement log holds seven more (LOG-NAMES/LENS/WIDS/OLD-START/OLD-LEN/
NEW-START/NEW-LEN, publish.f:297-314), and BOTH tables mmap at LOAD
(TABLE-INIT and LOG-INIT are top-level calls), not first use - verified: one
require of migrate.f moves DATA 13 bytes before any compile. STRIKE the
'delete this dot if the seam handles all vectors generically' clause: NSTR's
arena and NTRAP's table are never-reset BY DESIGN (their addresses/ordinals
are compiled into published routines) - a generic reset corrupts every
string and trap site. The seam resets exactly the ten named headers.
Restore failure mode is a dead-pointer dereference (VEC-CHECK-LIVE reads
nonzero as live), not a throw - the negative test must refuse a stale
header. ALSO: Class 2 (code addresses INSIDE the mmap payloads -
R-ENTRY, LOG-OLD/NEW-START) is unrelocatable by construction
(XTCELL-OFF-MAX requires cells inside DATA) and is NOT this dot's scope -
see habu-relocate-mmap-payload (minted 2026-08-11).
