---
title: Hard-cut HB package names
status: open
priority: 1
issue-type: task
created-at: "2026-07-31T07:07:42.876877+02:00"
---

Joel's accepted package names exist only in control metadata; live source still defines ENGINE-EMIT, ENGINE-SIZE, ENGINE-ERROR, and ENGINE-BUILD. This is a hard cut, not compatibility work. Atomically rename the native and Gforth recovery owners and every live caller: HB-EMIT publishes FORTH, BUILD, and BUILDING?; HB-SIZE keeps the current size API under the new owner; HB-ERROR keeps the current error constants under the new owner. Delete ENGINE-BUILD by moving its two lifecycle words into HB-EMIT. Keep tools/hb-build-report.f's unrelated HB-BUILD package as the sole HB-BUILD owner. Rename vocabulary scopes, package scopes, qualified references, source-facing fixture expectations, current TRUSTED rows, and current non-archived documentation. Do not rename files merely because their subject is engine errors or size. Historical archive prose may retain history; all live .f/.fs source and current docs must have zero ENGINE-EMIT, ENGINE-SIZE, ENGINE-ERROR, or ENGINE-BUILD spellings.

Owner and exact scope: src, bootstrap/cg/forth.fs, lib, maki, test, tools, TRUSTED.md, LESSONS.md only where a live source/API statement changes, and current docs outside docs/archive. Dependency: completed emitter ownership and Gforth vocabulary ownership recognition. This prerequisite lands before E1/E2 namespace candidates integrate; those candidates rebase and resolve only spelling conflicts. No behavior change, alias, forwarding word, duplicate package, ABI/version, translation table, fallback, new lint, or package-gate exception. First checkpoint: exact source census shows the four old owners and proves existing tools HB-BUILD is a separate collision. Pre-M17 proof: every changed definition remains under its renamed owner, all qualified callers resolve by exact source census, old live spellings are absent, tools HB-BUILD definitions are untouched, and independent hunk review. M17 acceptance: the terminal native suite, recovery bootstrap, ptx/native slices, host/filemap gates, and source package gates all run on the exact hard-cut tree.
