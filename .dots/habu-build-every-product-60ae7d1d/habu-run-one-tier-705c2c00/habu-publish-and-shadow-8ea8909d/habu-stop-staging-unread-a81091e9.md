---
title: Stop staging unread host facts on import
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T00:59:14.021578+02:00"
closed-at: "2026-11-04T11:48:48.300125+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the file linker (docs/design.md \"Machinery being replaced\"). Unread IMPL rows written on definition replay (tools/native-unit-object.f ~4028); replay is the per-definition cache."
---

C5 review (qyrnnlqs): NHOST:PREPARE-IMPORT (src/compiler/native/host-publish.f IMPORT-BODY/IMPORT-ONE/IMPORT-TYPES, called from tools/native-unit-object.f ~4028 on definition replay) writes IMPL rows through PREPARE that nothing reads: NPUB:IMPORT-PENDING and IMPORT-PENDING-DOES (publish.f ~353/~372) publish without the observer, the only reader of IMPL rows (DEF-CAPTURE-HOST via TAKEN-FACTS, native-unit-object.f ~1801). END-TAKE then discards them. Decide whether import keeps only CHECK-IMPORT-TABLE validation (and TAKEN-FUNS if publication needs it) or drops NHOST from replay. Workspace: none yet.
