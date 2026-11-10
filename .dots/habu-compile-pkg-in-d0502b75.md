---
title: Compile [package] in a body outside any package
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T10:05:54.405572+02:00"
---

Problem (reported by tim from Maki's scope-names lane, 2026-11-03): a colon body that uses [package] outside any package fails to compile. /tmp/carl-pkgtop/a.f (copy of maki tim/.out/r2/pkg-top/a.f), two lines ': X ( -- ) [package] type ;' and 'X cr', on the installed hb 92594f9e prints 'ncomp: cannot compile X' then 'hb: uncaught throw code 7122' (E-LAYOUT-BOUNDS), rc 67. The same body inside 'package PQ ... ;package' (b.f) prints 'pq', rc 0; top-level '[package] nip .' outside a package prints 0, and an empty literal in a body (': Y ( -- ) s" " type ;') works. docs/forth.md:178 states the contract: '[package] ( -- ptr u8 n )', the open package's name, folded, 'empty outside one; a string literal in a body'. src/habu/t0-emit.f:502 says an EMIT operation before any EMIT-SELECT throws 7122, which is a lead for the root cause, not a conclusion. Acceptance: root cause named at its layer (the body path for [package] with no open package: habu2.f C-BPACKAGE :6167, C-PKG-NAME, and T0's lowering of it); X compiles and pushes the empty string, rc 0; a failing case first beside the existing [package] cases (test/policy/package-literal.f or test/compiler/native-string-forms.f). No public word changes. Files: per the root cause. Verify: the reproducer and the chosen test file. Depends: none. Ownership: per the root cause. Claim: unassigned. Tell tim when it lands.

Consumers: no Maki word body uses [package] today.
