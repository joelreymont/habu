---
title: "ENGINE-INTERNAL:SEAL-PACKAGES has no caller once wasm-build.f is gone"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T20:22:09.801734+02:00"
---

Problem: src/core/internal-mark.f SEAL-PACKAGES (:18) was called only by tools/wasm-build.f :78; the file's header and the word's comment describe sealing the packages a capture ships. The file is a start-up row (src/habu/boot-files.f :116), required by src/core/checker-surface.f :8 and described in src/habu/habu2.f :1191-1210, src/core/checker.f :23395 and src/core/type-schema.f :174. Acceptance: the word is gone and the file's header says what the file still does (its seal-package line), or the file and its row go if nothing is left. Files: src/core/internal-mark.f and the mentions above. Verify: engines converge. Depends: habu-wasm-3-remove-5698c955. Ownership: none. Claim: unassigned.
