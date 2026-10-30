---
title: Fork plainly in an image without a region alias
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T02:05:37.873455+02:00"
---

Problem: tools/hb-build-stripped-test.f HBT-STRIPPED-FORK (asserts F7, F8) fails on the W^X stack (W3 ulpwprur 20b12154): the stripped image built from test/stripped-fork-subject.f prints 'hb: fork: cannot map the child's region alias' and child=failed. FORK's child (src/habu/habu1.f, macOS path about :2985-3000 with REGION-REMAP; Linux path about :2955-2975 plus the parent's memfd fill, ALIAS-COPY and munmap) re-establishes an alias at DBASE+delta in an image whose startup never mapped the two views; its delta cell reads zero (worker inference, to be observed first). Rule: a forked child ends with exactly what its parent process has; an image with no alias forks plainly; the guard is the existence fact the startup records (delta 0 if an alias can never sit at DBASE), never a new cell. Acceptance: the hb-build-stripped row rc 0 on the stack's rebuilt engine, src/lib/process-fork-test.f and test/code-window.f rc 0, the subject's child exits 0 with empty stderr; the Linux path proven on spark before W4. Files: src/habu/habu1.f (FORK, both target paths). Verify: tools/hb-build-stripped-test.f, src/lib/process-fork-test.f, test/code-window.f with the rebuilt engine; spark for Linux. Depends: habu-map-the-linux-25501359. Ownership: src/habu/habu1.f FORK. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-wx-fix.
