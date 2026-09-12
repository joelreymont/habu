---
title: Switch layout-buffer to the align primitive
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T10:40:08.142274+03:00"
---

Problem: src/core/layout-buffer.f computes its own cell pad (LBUF-ALIGN-PAD / LBUF-ALIGN) because the core prefix is recompiled by whatever engine launches in the checkout, and the standard align primitive landed only in 04701ef9; the cold-build seed /tmp/cedar-crossing-realpath/hb-stdin and any engine older than that tip cannot load a core that calls align. Acceptance: layout-buffer calls align at its seven sites and the helper is deleted, once the cold-build seed is refreshed to an engine that carries align (document the refreshed seed in docs/bootstrap.md and docs/maintainer-handoff.md); engine-suite typed-storage cases keep passing; a cold build from the refreshed seed succeeds. Files: src/core/layout-buffer.f, docs/bootstrap.md, docs/maintainer-handoff.md. Verify: cold build from the refreshed seed, test/engine-suite.f, lib/json-read-test.f, test/native-resource-image.f. Depends: none. Ownership: hazel. Claim: unassigned.
