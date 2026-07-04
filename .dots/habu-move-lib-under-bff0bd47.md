---
title: Move lib/ under src/ (src/lib)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:10:11.398145+02:00"
---

User decision 2026-07-04: single source root; lib/ looks strange beside src/. Move lib/ -> src/lib/ (~2038 references across .f/.fs/.sh/.md): every 'require lib/...' and '--load lib/...' site, lib/std.manifest (and its row paths), tools/bootstrap.sh, build-fixpoint prelude lists, hb-build key lists, test/run-files.f, FILEMAP.md, TRUSTED.md evidence columns, docs/bootstrap.md gate prelude, docs/stdlib.md. NOTE the load-time boundary this blurs: src/core+src/habu are baked engine prefix, lib/ is runtime-loaded checked stdlib — keep that distinction documented in FILEMAP.md/docs/forth.md section Files after the move (src/lib = runtime stdlib, NOT engine prefix; build-fixpoint must not start baking it). Whole-tree path sweep: MUST run solo; full native gate + maki suite + bootstrap check + lints after. SEQUENCE: after src/habu->src/hb rename (do renames in dependency-free order, separate commits, one solo slot).
