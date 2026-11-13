---
title: Make engine-internal primitives private words
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.751596+02:00"
---

Problem: engine-internal primitives (trust-sig!, set-check, namespace-record, package-scope!, code-publish, ...) have a global record, kept from checked code only by marks and by per-package owner rows (src/habu/prims.f EPRIM: with ETRUSTED-ONLY!, EPPRIM:), and several stay reachable: the probes are in the children.
Ruling (Joel, 2026-10-08): system words go in the system package's private section, never the global dictionary; no owner rows, no caller checks. Restricting a word = putting it in a private section.
Design (settled by the lead, 2026-11-13; each slice is a child dot):
- An internal primitive is a private word of each engine package that calls it: prims.f keeps one private row per primitive and calling package (the form patch32 has in XREF and DEBUG) and no global row. No package named SYSTEM, no language change. A caller in no package, or behind a TRUSTED: wrapper, moves into a package with its own private row.
- A build lays each private row's record from the host kernel's seed table by name.
- A primitive with a public form safe for programs keeps one home: xref-search-wl in CHECKER-RESOLVE (src/core/checker.f loads first; the others call CHECKER-RESOLVE:WL-PROBE), run-rc in PROCESS.
- `postponed` becomes an ordinary global primitive beside define-word, body-append and literal; postpone finds no engine private by name.
- ffi-call and top-check@, dead on arm64, go with the x86-64 kernel's tests at the x86-64 step.
- The marks (DNAME-INT, DNAME-OWNED, the trusted-only flag) go after this, in habu-delete-the-internal-3a818b8a; the strip, in habu-re-enable-hb-a2404c26.
Acceptance: every child is closed: no engine-internal primitive has a global record, and the engine builds itself and passes the full suite.
Depends: the children. Claim: unassigned.
