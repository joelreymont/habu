---
title: Rename src/habu to src/hb (builds bin/hb)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:10:11.393902+02:00"
---

User decision 2026-07-04: the directory that builds the hb binary is named after the binary. Rename src/habu/ -> src/hb/ (~602 references in 59 files): PFX path rows in src/habu/habu2.f itself + bootstrap/cg/forth.fs mirror, tools/bootstrap.sh, tools/build-fixpoint.f, tools/srclist.f, test/run-files.f, tools/hb-build-lib.f key lists, tools/diagnose-hb-core.f, FILEMAP.md, TRUSTED.md source columns, docs/. Consider renaming habu1.f/habu2.f -> hb1.f/hb2.f in the same sweep (ask user if unclear). Whole-tree path sweep: MUST run solo, no parallel workers; engine rebuild via fixpoint + full native gate + bootstrap check (HABU_BOOTSTRAP_CHECK_ONLY=1) + host-lint/filemap-lint after. SEQUENCE: after in-flight TFAM merges; pairs naturally with the lib-> src/lib move and end-package->;package sweeps (adjacent solo slots, separate commits).
