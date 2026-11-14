---
title: Give every --json-errors rejection one shape
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T19:58:43.270630+01:00"
---

Problem: nuynnwzp 'Place a declaration's rejection' (lane carl-decl) writes a declaration's rejection as a REJECTION record (src/core/rejection.f OPEN, JSON mode), while every other rejection under --json-errors keeps its schema_version-1 record; the 'declaration' shape and tools/gate-json-assert-core.f GJA-DIAG-DECL (:634, :726) are now unreachable, and the repair-packet and LSP readers (tools/lsp-*.f) are not yet reconciled with the new record. Acceptance: --json-errors writes one record shape for every rejection (Joel 2026-11-14: diagnostics are Rust-style: place, source line, expected/found, fix); no shape or assert nothing reaches; the LSP and repair readers read it. Depends: nuynnwzp landing. Claim: unassigned.
