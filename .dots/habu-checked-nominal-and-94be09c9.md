---
title: Checked nominal and linear handle minting
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:44:49.846693+02:00"
---

Capability dot from the checked-PTY recovery (slices landed under habu-recover-checked-pty-04fcb611): the checker cannot mint a use-once linear handle or a distinct role nominal (supervisor pid vs group pid vs target pid; three watch-descriptor roles) from a raw cell in an empty checked body, so lib/process-pty-handle.f carries 18 paired TRUSTED: mint/erase coercions classified stdlib-boundary (rows in TRUSTED.md dated 2026-07-19). Design and implement the checker capability that expresses refined-nominal and linear-handle minting - candidate shapes: a checked mint form tied to the owning package's constructor discipline, or a typed-buffer-backed provenance rule - then retire the 18 casts and their manifest rows. Related precedent boundaries that would shrink with the same capability: the role coercions in lib/roles.f cited by task.f, ffi-abi.f, memory.f.
