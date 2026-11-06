---
title: "File objects L4: link a fresh object"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T12:21:51.537461+02:00"
blocks:
  - habu-file-objects-l3-7d856048
---

Problem: a warm build compiles every file again. Acceptance: when a file's object exists and is fresh, the runner links it instead of running its text: records through new LINKER-private rows of def-open, namespace-record, namespace-private, alias-record and package-scope! (prims.f:840-885 pattern), emissions filed by new NSHADOW:IMPORT with host copies placed by ARM64:PLACE (src/arch/arm64/link.f:49), DATA copied at DP with each address cell stored again through xt! or ptr-cell!, strings back in the table through new INTERN:ADOPT ( ptr -- ), STORES applied, each request performed through the loader. In this lane fresh means only: the object exists, opens, matches its key and the file's bytes hash as recorded (L5 adds the rest). A malformed object counts as stale; a valid one that fails to link fails the build. Design: .out/carl-file/design-fobj.md section 2 items 2, 10. Files: src/habu/linker.f (or its object file), src/habu/prims.f, src/compiler/native/shadow.f, src/habu/intern.f, test/file-objects.f. Verify: a cold build, then a warm build in a fresh process (the boundary under test) writes a program cmp-identical to the cold one with LINKER:COMPILED 0 and no file text run; test/linker.f, arm64-link-run.f, arm64-link-data.f. Depends: L3. Ownership: carl (lead), worker lane. Claim: unassigned.

Acceptance case (error names, dot habu-name-every-err-581ec405): ERROR: rows are chained by a self-relative distance to the previous row, across files, and ERROR's HEAD (an earlier file's cell) moves with each declaration. A reused file whose rows follow an earlier declaring file that recompiled must still answer ERROR:NAME$ and ERROR:TEXT$ for every code, its own and the earlier file's, without making every later declaring file depend on every earlier one. The lead's leaning: the object records each declaration (code, name, text) and the link replays it through ERROR:DECLARE instead of copying the row bytes; a new object section is a saved-format change and goes to Joel with L4's design.
