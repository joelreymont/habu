---
title: "The capture's shadow file keeps the Wasm reader's words"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T20:22:09.789039+02:00"
closed-at: "2026-11-13T16:01:14.459603+01:00"
close-reason: Superseded by habu-delete-the-aot-fb486263, which deletes the whole capture chain
---

Problem: once src/arch/wasm/capture.f is gone, src/habu/aot-shadow.f SH-STRIPPED (:80), SH-OWNER? (:451) and SH-STRIP (:462) have no caller, and the comments at aot-shadow.f :78, :284, :456 and src/habu/aot-decl.f :466 still describe the Wasm reader. No arm64 host test loads aot-shadow.f (test/x86-64-link-records.f is an x86-64 row), so the deletion is shown on the x86-64 host. Acceptance: the three words and the Wasm reader's mentions are gone, or the files go whole when x86-64 builds through the linker. Files: src/habu/aot-shadow.f, src/habu/aot-decl.f. Verify: test/x86-64-link-records.f on the x86-64 host. Depends: the x86-64 step (habu-re-enable-the-959502b5). Ownership: none. Claim: unassigned.
