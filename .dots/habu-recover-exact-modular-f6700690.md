---
title: Recover exact modular AOT composition lane
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T11:33:20.670366+02:00"
---

Forensic sweep 2026-07-19: stranded lane habu-build-exact-modular-44f4c2dc holds the exact modular AOT source-composition work (9 own commits, ~530 behind master; tip commit: Consume frozen build source in memory). Preserved by bookmarks sol-exact-modular-aot and recover-exact-modular (pushed to origin). Assess against the current AOT build path (which has since gained the closure/relocation work) and either re-derive or record superseded-with-evidence.
