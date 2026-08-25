---
title: Move the control-flow stack out of the protected region
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T10:37:58.707394+02:00"
---

Option E from the lprot lane's priced design (dot 9637c873 checkpoint): the compile-time control-flow stack (DBASE + CFSTK-OFF, 4KB, zeroed per colon and written per if/begin/do) is one of the three bands every code bracket must declare writable. Moving it into DATA deletes the whole band: worth ~16ms/boot beyond design C (~5s more off the battery's 323 boots, total ~15s with C). A layout move: CFSTK-OFF, DICT-SIZE, CFSTK-DEPTH-MAX, a GUARD-BAND row. Land AFTER design C (the declaration ABI) so the band's deletion is measured against C's baseline. Files: src/habu/layout.f, habu1.f. Depends: 9637c873 (design C landing).
