---
title: "Checker: replay package DEFTYPE casts"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T01:47:47.864282+02:00\""
---

Static invariant: tools/check.f preverification must recognize every generated converter that native checked evaluation recognizes. Minimal reproducer /tmp/deftype-package.f defines package P, deftype foo, then uses >foo and foo>N inside the package: native bin/hb load succeeds, tools/check.f rejects >foo as E-UNDEFINED. Fix verify-source/check support replay so package-local DEFTYPE generated converter words are scoped and resolved exactly like runtime, without leaking them globally or weakening nominal distinctions. Add positive package-private/public converter/local/signature tests, negative cross-package and role-swap tests, and all-errors parity. Verify reproducer, owning checker suites, typed-local/host/filemap lints. Dependency: owner persistence artifact proof uses package-local owner-row-idx and prot-row-idx.
