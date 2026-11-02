---
title: "Key a file's object by the open package"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T18:56:52.286988+02:00"
closed-at: "2026-11-02T14:23:30.816668+02:00"
close-reason: "Not a defect: a warm cached build recompiles a file whose requirer's scope changed. Reproduced both ways (package AAA then CCC; using UPA then UPB): lib.f recompiles with 'native-file: dependency miss: scope changed' and prints the right value. tools/native-file-scope.f LIVE-INPUT records package, parent, current wordlist, use package and using stack as a SCOPE dependency row, checked by tools/native-unit-dependency.f CHECK-ROW. FILE-KEY is the same under both scopes, so alternating scopes overwrite one object slot and recompile on each switch; correct, not a reuse."
---

A file compiled under a package its requirer opened reads that package: a [package] body folds its name into code, and its definitions land in its wordlist. tools/native-definition-cache.f FILE-KEY hashes the engine, target, AMBIENT-HASH (tier, checker state, borrow, dispatch role) and the instance key (parent, role, path, kind, occurrence), not the open package or compilation wordlist, so a file whose requirer changes its package from AAA to CCC can reuse an object compiled under AAA (P1 review of [package], 2026-11-01; its app probe printed the right name but may not have reused the file). First a reproducer through hb-build with a warm cache: lib.f with no package of its own defines : WHO ( -- ptr u8 n ) [package] ; main.f requires it inside package AAA, then CCC. If it reuses, add the open package and the compilation wordlist to AMBIENT-HASH. Owner: carl.

Claim: agent=carl worker workspace=.jj-ws.noindex/carl-pkgkey (2026-11-02), brief .out/carl-file/brief-pkgkey.md: reproduce through a warm cached build, then key the file by the open package, its section and the open usings if it reproduces.
