---
title: Give each package-less module a package
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T00:50:57.389420+02:00"
---

CLAUDE.md: every module has a real package. 98 non-test .f files under src/ and tools/ have none: rg --files-without-match --glob '*.f' -e '^\s*package\s' src tools | rg -v -- '-test(-lib)?\.f$' (2026-10-31: tools 32, src/core 12, src/os/* 22, src/habu 7, tools/lint 6, src/lib 5, src/lib/test 4, others 10). Among them src/habu/snap-lib.f (global HEAP-START) and tools/diag-origin-core.f (DO- prefixed globals). Sort each: a file loaded before packages exist or a pure require list states why beside its header; every other gets a package and its callers qualify. After the T0 release.
