---
title: Root the saved verifier builder at its entry
status: closed
priority: 2
issue-type: task
created-at: "2026-10-21T12:05:13.384561+03:00"
closed-at: "2026-10-21T12:32:22.167873+03:00"
close-reason: Set the saved verifier builder engine root to its entry before loading support. Independent Astra review clear; cold foreign-root stdio E2E and image root/leak cases pass; converged matching engine full native gate 486/486 exits 0. Installed startup check remains part of promotion.
---

Installed a69 hb from an older Habu checkout loads absolute tools/lsp.f and answers initialize, then a didOpen/documentHighlight request exits 78: duplicate E-A-FIRST in installed src/lib/errors.f and check-verify: image build failed. check-verify-save.f roots required paths at its entry tree but leaves the engine root at the caller's checkout, so engine-provided files acquire a second identity. Root the builder at its entry for both child and saver loads; the saved image must still restore the caller's root at startup. Verify through the existing stdio LSP entry harness from a foreign Habu CWD, including a cold image build and a real document check.
