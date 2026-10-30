---
title: Store no carried reference rows
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T13:34:25.233100+02:00"
---

Problem: on the fasl spike (habu-spike-the-fasl-16b76e28, lane 9b, P9) the memory.f object stored again after a live head carries 9 reference rows a fresh cold store lacks (kind 4 DATA, owner 1, items 1-9: imports of src/core/type-family.f tfam data) between rows 85 and 86; no check ref names them and every later ref shifts +9 to the same target, so they are dead storage that survives every re-store. Acceptance: the source of the 9 rows is traced (the export path FILE-ADD-INSTALLED takes after SPIKE-INSTALL, or the viewed object's table) and the re-stored object's reference table equals a fresh cold store's: P9 counts 0 slices and 0 reference rows differing for memory.f and verify.f (tools /tmp/carl-fasl/p9tool/p9.py and s13.py). Files: tools/native-unit-object.f, tools/native-definition-cache.f, src/core/checker.f if the export carries them. Verify: lane 9b's P4 A then P9 against a fresh cold store. Depends: habu-delete-the-whole-5d7d3a31. Ownership: carl-fasl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fasl (brief carl-dots/.out/carl-file/brief-fasl-carry-rows.md, a new change on lane 10's xlmmrwto).
