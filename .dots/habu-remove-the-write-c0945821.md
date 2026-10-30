---
title: Remove the write-only EDGE-KIND field
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:10:52.725791+02:00"
---

Problem: src/compiler/native/host.f EDGE-KIND (declared :64, reserved :126, stored :536 by host-publish.f IMPORT-EDGES from a row's kind field and by the cold path from NEMIT:CALL-KIND@ at host-publish.f:49) has no reader anywhere in the tree; read-back now passes the unit-object format's kind to it untranslated (tools/native-unit-object.f ~:4111-4113 to NHOST:PREPARE-IMPORT), which no reader can notice. Found by the nunit-kinds review (habu-own-the-unit-0965c0ab). Acceptance: the field, its reserve and both stores are gone, or a reader exists and the stored value is the window's kind translated from the format; no reverse translation added for a write-only field. Files: src/compiler/native/host.f, host-publish.f, tools/native-unit-object.f. Verify: tools/check.f on the three files; test/native-file-types-e2e.f; the full suite if the host layout changes. Depends: none. Ownership: src/compiler/native/host.f. Claim: unassigned.
