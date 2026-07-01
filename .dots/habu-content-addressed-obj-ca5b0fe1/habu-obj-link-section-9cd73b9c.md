---
title: Object link section layout
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:15:24.187501+02:00\\\"\""
closed-at: "2026-07-01T21:18:08.323192+02:00"
close-reason: "completed: OBJLINK now tracks object/text/data totals, scans text/data hex byte sizes, validates relocation offsets against current object text bytes, and exposes OBJECT-COUNT/TEXT-SIZE/DATA-SIZE. Proof: object-link test; typed-local-diff-lint; stdlib-manifest-test; filemap/host/trust/stale lints; lint-artifacts-fast; full native suite 17888ms <=40000ms."
---

Problem: OBJLINK validates symbols but does not yet compute section sizes or fail bad relocation offsets, so a future linker would still need ad hoc text/data scans before merging objects. Fix: extend lib/object-link.f to scan each current OBJ record for text/data hex byte sizes, track object/text/data totals, validate reloc offsets against the current object's text bytes, and expose object/text/data counts. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: ADD accumulates text/data byte sizes across objects; OBJECT-COUNT/TEXT-SIZE/DATA-SIZE report totals; reloc offset inside text passes; reloc offset outside current text throws E-OBJ-SCHEMA; existing symbol validations still pass. Verify object-link test, typed-local-diff-lint, manifest/filemap/host/trust/stale lints, lint-artifacts-fast, full suite before master.
