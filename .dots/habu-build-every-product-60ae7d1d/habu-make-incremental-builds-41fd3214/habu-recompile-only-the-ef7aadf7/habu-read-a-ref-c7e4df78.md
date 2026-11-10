---
title: Read a reference as a tag and four fields
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:19:56.232997+02:00"
---

Problem: a quotation reference by its owner's identity needs four fields (file, name sum, count, function); src/habu/linker.f RD-REF answers a tag and three, and thirteen readers pass three along. Acceptance: RD-REF answers four fields, the fourth 0 for every tag, and CODE-REF-CK, DATA-REF-CK, CELL-REF-CK, DISPATCH-CK, CALL-CK, ADDR-CK, SLOT-CK, STORE-CK, RESOLVE, NEMIT-FILL, MAKE-DEF and LINK-DATA pass it; nothing saved changes; ev.sh as before. Files: src/habu/linker.f. Verify: ev.sh. Depends: habu-save-no-emission-70f83ca3, habu-name-a-facts-857fd379 (same words; launched on their merge). Ownership: carl. Brief: carl-layout/.out/briefs/lane-ref-fourth-field.md. Claim: merged into the stack as zyqnpryo 2e172052 (workspace retired), lands with the feature.
