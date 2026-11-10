---
title: Audit host state a built word changes through a header cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T23:10:54.285536+02:00"
---

Problem: in a program build the built copy of hb's boot files reads and writes the running hb's own cells through header cells (src/core/include.f LOADER-CELL via SRCLOC:SOURCE-CELL; src/habu/habu2.f ROUTE-HEADER). A top-level run of such a word changes host state no object carries, so a part that links on the warm build leaves it unchanged: SOURCE-ROOT:ROOT! was one case (dot habu-a-warm-build-1b7d258c, fixed by counting its runs). Read but not audited: the input cursor (INP-CELL, INE-CELL), the loader's TOP frame and SCOPES, any other RESERVED-*-CELL a public word stores through. Acceptance: a list of every public word that stores to the running hb through a header cell, each with either the linker rule that makes its part compile (path:word) or a fixture in test/file-objects.f that fails warm; no new mechanism where an existing ACTS row or UNLINKABLE rule covers it. Files: src/core/include.f, src/habu/habu2.f, src/habu/linker.f, test/file-objects.f. Verify: bin/hb --load test/file-objects.f. Depends: none. Ownership: carl. Claim: unassigned.
