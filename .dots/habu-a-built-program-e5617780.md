---
title: A built program that requires json.f does not compile
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:34:16.703497+02:00"
---

Problem: building a program whose source is 'require src/lib/json.f' and a one-line MAIN through tools/hb-build.f fails with 'ncomp: cannot compile JW-BUF@', on the engine before and after 78d0aacb; loading the same file with --load works. Source: /Users/joel/Work/reviews/habu-10x-2026-11-10/cells-review/probes/p5.f. Found by the review of habu-a-process-cell-9ddedca3. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names why the native compiler refuses JW-BUF@ in a build and not in a load (declaration, checker, compiler or linker, as path:line); then p5.f builds and its product prints ok, or the refusal names its reason. Files: src/lib/json.f, src/compiler/native/definition.f. Verify: p5.f built and run on bin/hb; a case where built products are tested. Depends: none. Ownership: the native compiler's build path. Claim: agent=bob workspace=.jj-ws.noindex/bob-jsonbuild
