---
title: Lay out CWORK from its declarations
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.582462+02:00"
---

Problem: CWORK:DECLARE (src/compiler/workspace.f ~446-500) mutates central USED/ALLOC/EVENT/DEFAULT tables at load time; a reused file replays only its own records and DATA, so test/native-file-cwork-e2e.f F9 reads 0 for a default-7 variable. Joel 2026-10-26 chose definitions-own-layout over carrying the state through the cache: each descriptor holds extent, alignment and default; CWORK:OPEN lays out in declaration order from a frontier found by walking the dictionary (structural identification, no central head); a fresh mapping lays out everything. Delete the file-capture API (FILE-IDENTITY-SET..FILE-CLOSE, ALLOC/EVENT/PREP/PROJECT tables) and the public DEFAULT, DECLARED-BYTES, TAKE, LAYOUT-MARK (approved; only the cwork e2e used them). Abandon the paused carry WIP in .jj-ws.noindex/carl-cwork (tools/native-unit-{capture,file,object}.f). Acceptance: cwork e2e incl. F9 and CHECK-MOVED-PREFIX, frame/auto/state/types e2e, engine cold==warm byte-equal; accessor+OPEN cost measured on a compiler-heavy workload. Files: src/compiler/workspace.f, test/native-file-cwork-e2e.f. Depends: none. Workspace: .jj-ws.noindex/carl-cwork. Claim: carl.
