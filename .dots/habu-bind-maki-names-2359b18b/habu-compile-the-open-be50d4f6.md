---
title: "Compile the open package's name with [package]"
status: active
priority: 2
issue-type: task
created-at: "2026-11-01T15:36:36.150212+02:00"
---

Joel-approved (via tim 2026-11-01): [package] ( -- ptr u8 n ), compile-only in a body: compiles the open package's name as a string literal in the checker's folded lower-case spelling; at top level pushes the open package's name; outside any package the empty string. Takes no operand. Mechanism: carl (engine compiler habu2.f keyword tables beside [char]/['], checker, verify-source, native compiler). Acceptance: body and top-level use inside and outside a package, a reopened package, a part of a nested package; checker effect ( -- ptr u8 n ); rows in the keyword suites. Verify: focused rows, rebuilt bin/hb, full suite. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pkg. Lane P1 (body, brief .out/carl-file/brief-pkg-body.md) running; P2 (top level, docs) follows.

P1 landed 2026-11-01 in caad7923 (body form, review ACCEPT); P2 (top level and docs) remains.
