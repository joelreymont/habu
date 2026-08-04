---
title: "Seal: relocate TRUSTED/DOESB/CHECKER-PACKAGE-MODE into arena"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:30:09.229129+02:00"
---

2b-i deferral + review finding. TRUSTED-CELL ($27B8) and DOESB-CELL ($27B0) stayed outside the friend arena; per-definition C-CLEAR-TRUSTED-STATE (habu2.f:1641, called on all 6 open paths) neutralizes top-level forging, but an immediate word running during compilation can set TRUSTED-CELL between open and close and the close path (habu2.f:3076) then registers the def as trusted. CHECKER-PACKAGE-MODE relocation was also deferred to go with the checker-registry migration (2b-iii). Fix: grow FRIEND-ARENA-LEN ( -> , band $20..$C0 still < BODYLEN-CELL $1B8), move the three cells, mirror layout in habu1/habu2/forth.fs + snapshot restore writers, negative forge fixtures per cell. Depends: 2b-i merged; coordinate with habu-tfam-2b-iii-d8af2634.

GROOMED 2026-08-04 (dot-groom). Dangling blocker dropped. habu-tfam-2b-iii-d8af2634 ("TFAM
2b-iii: guard walls cats 1/2/4/5/6"), named above as the dot to coordinate with, no longer
exists: it was dissolved, not delivered, by commit 150be3a2f "Archive 2b-iii umbrella
guard-walls dot", which archived the umbrella in favour of the individual seal leaves. There
is nothing left to coordinate with; the 2b-i dependency is unaffected.
