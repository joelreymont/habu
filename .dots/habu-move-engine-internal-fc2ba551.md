---
title: Move engine-internal primitives into the system package
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.751596+02:00"
---

Problem: engine-internal primitives (trust-sig!, body-append, def-open, namespace-record, int-mark, ndict-append, ffi-call, set-check, ...) sit in the global dictionary. The product keeps them and refuses them only by marks (measured on master's bin/hb: `' trust-sig!` -> "hb: internal engine word: trust-sig!", `' ffi-call` -> "hb: trusted-only tick: ffi-call"). A per-primitive owner table bolts caller checks on top: src/habu/prims.f EPPRIM: (52 rows over 20 packages; trust-sig!'s effect is written twice, :789 EPRIM: and :791 EPPRIM:; xref-search-wl has 7 owner rows). It still leaks: on master `package OUTER public : EVIL ( ptr u8 n -- ) body-append ; ;package` certifies. Package privacy already gives the restriction: the product build strips private names (XREF's private N>REC is named 0 in bin/hb.names and E-UNDEFINED on the product; public XREF:REC resolves, and XREF's public words compiled before the strip still run).
Ruling (Joel, 2026-10-08): system words go in the system package's private section, never the global dictionary; no owner rows, no caller checks. Restricting a word = putting it in a private section.
Acceptance: every engine-internal primitive is a private word of the system package; EPPRIM:/ECLOSE-PRIVATE and the K-PKG-PRIVATE row kind are deleted; a primitive several packages used gets one home and the others call its public word (pattern: XREF:WL-RECORD over xref-search-wl); on the product `' trust-sig!` and the OUTER squat above answer E-UNDEFINED; the engine's own callers are compiled before the strip and keep working.
Measure first: how the engine's packages name a system-private word while the engine is built (the strip itself is existing machinery).
Supersedes the owner-row design of habu-honour-owner-private-0a19f45d.
Files: src/habu/prims.f, src/core/checker.f (PE-SPEC-ROW K-PKG-PRIVATE), src/habu/primitive-registry.f, the callers in src/habu/*.f and src/core/*.f, tests that name the rows (test/owner-access.f, test/prim-owner-scope.f, test/baked-owner.f).
Verify: product probe of the refused names above; full suite on the built hb.
Depends: none. Ownership: lead. Claim: unassigned.
