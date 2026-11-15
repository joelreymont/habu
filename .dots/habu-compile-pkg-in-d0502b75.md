---
title: Compile [package] in a body outside any package
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T10:05:54.405572+02:00"
---

Problem: a colon body that uses [package] outside any package, ': X ( -- ) [package] type ;' then 'X cr', compiles and prints an empty line, rc 0, on the installed hb (Maki's report of 2026-11-03 saw it fail with E-LAYOUT-BOUNDS); the body's [package] now goes through src/core/checker.f PKG-LIT-TOK :20846, and no test covers a body outside a package: test/policy/package-literal.f has only the case inside one. Acceptance: a case beside it (or in test/compiler/native-string-forms.f) builds X outside any package and shows it pushes the empty string. No public word changes. Files: the chosen test file. Verify: that test. Depends: none. Claim: unassigned. Tell tim when it lands.

Consumers: no Maki word body uses [package] today.
