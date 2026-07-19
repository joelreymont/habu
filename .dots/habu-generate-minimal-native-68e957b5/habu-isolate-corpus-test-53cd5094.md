---
title: Isolate corpus test fixtures
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T22:50:17.959475+02:00"
---

maki/data-loader-test.f:18-21,36-39,72-80 writes fixed process-global /tmp/habu-shakespeare-{fixture,empty}.txt, assumes /tmp/habu-shakespeare-absent.txt does not exist, and never cleans any path. WRITE-ALL in lib/fs.f:309-325 uses create/truncate and follows symlinks, so parallel runs race, stale files falsify the missing-file assertion, and a same-user precreated symlink can truncate an arbitrary writable target; current tests leave files behind. Allocate every fixture below a unique HB_TMP-owned test directory using the repository's safe filesystem primitives, create the missing-case name inside that private root, reject symlink substitution, and perform exception-safe cleanup without masking the original error. Coordinate missing safe-path capability with habu-recover-safe-filesystem-6f1de404 instead of adding raw host glue. Add concurrent two-run isolation, stale/symlink collision, throw-path cleanup, and no-artifact assertions. Verify focused loader tests, Maki, safe-filesystem, host/filemap/dot gates. Files: maki/data-loader-test.f and existing safe test helpers only.
