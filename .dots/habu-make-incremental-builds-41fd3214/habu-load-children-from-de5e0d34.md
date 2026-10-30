---
title: Load children from words the top level runs
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:08.046842+02:00"
---

Problem: warm-diag.md R2: a loader token inside a word the top level runs (compiler.f ' LOAD-PASSES execute) is not at a loader cut, so dc LOAD-BEFORE refuses load-boundary and no LOAD-ACTION starts; compiler.f's segment spans the passes subtree and its 194 definitions compile live every warm build (also loader.f in test/native-file-dynamic-load-e2e.f). Acceptance: such a load starts a load action at the top-level position; compiler.f reused; dynamic-load ENVIRONMENT counts updated. Files: tools/native-definition-cache.f, tools/native-unit-object.f, test/native-file-dynamic-load-e2e.f. Verify: P0 on the lane; dynamic-load e2e. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-r2.md. Depends: none. Ownership: LOAD-BEFORE, LOAD-ACTION, LOAD-FINISH, FILE-BEGIN. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fr2
