---
title: Move the SHA-256 trio callers to owned contexts
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T20:46:03.559283+03:00"
---

SHA256-RESET / SHA256-UPDATE / SHA256-FINAL, SHA256, SHA256-FILE and SHA256-FILE-HEX still run over the static SHA-CTX0 and SHA-FCTX0 (src/core/sha256.f), so one digest at a time in one thread; the Tender reproducer /home/joel/.cache/tender/habu-gaps/sha-call-state/repro.f exits 1 on them. Move every caller of the trio and the file wrappers - src/habu/aot-file.f, aot-ident.f, aot-owned-cells.f, tools/diff-side-content.f, diff-side-content-read.f, diff-side-content-test.f, test/aot-artifact-rows.f, aot-owned.f, aot-owned-capture.f, lib/crypto/evp.f, lib/engine-id.f, lib/content-key.f, src/compiler/digest.f and the rest rg finds - to a context of its own through SHA256-BEGIN/FEED/END and SHA256-FILE-IN/-HEX-IN, then delete the trio, the wrappers, SHA-CTX0 and SHA-FCTX0 and their aot-owned-cells claims; SHA-DIGEST stays for TF-SHA16 or moves into it. Acceptance: no static digest context in src/core/sha256.f; the reproducer exits 0; tools/sha256-file-test.f, test/aot-owned.f, test/aot-chain-capture-suite.f and tools/hb-build-test.f green; consumers loom, radar and maki call only SHA256-FILE-HEX today, so they get the -IN word or a kept wrapper (announce before landing). Ownership: hazel. Claim: unassigned.
