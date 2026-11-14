---
title: Make the source loader one package
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T18:10:42.420493+02:00"
---

Problem: src/core/include.f splits one subject, loading source files, into six packages: REQUIRE-REG, INCLUDE-EVAL-BIND, SOURCE-INPUT, SOURCE-ROOT (canonical paths, cwd, engine root, confinement, the include frame stack), SOURCE-UNIT and SOURCE-EVENT, beside src/lib/source.f's SOURCE. Each seam turns an internal call into a public word: REQUIRE-REG:APPEND (include.f:876) and PERSIST (:961) are public only so SOURCE-ROOT and the snapshot step can call them. Joel asked 2026-11-02: 'why does this smell? why do we have both source-file and source-root? why do we need source-root?' His 2026-10-31 compiler ruling applies: "compiler words should live in the comp package, called internally without the package prefix. why would we split the compiler package into several packages?", choosing "One compiler package": a subject's files reopen one package and call shared words bare in its private section. Joel 2026-11-02: "review the public api of the source package to make sure all those words REALLY need to be public". On the loader's install cells Joel asked "why do you need hooks? does that need smell to you?": 12 of the 25 installs only record reads, so SOURCE-INPUT and SOURCE-UNIT are replaced, not kept as hooks.

Acceptance: one package SOURCE (Joel 2026-11-02: "shouldn't all these be in the SOURCE package?", then "either SOURCE or LOADER"; SOURCE was chosen, which src/lib/source.f already is), holding the loader's six packages, the origin table when the origin lane adds it (INTERN and PERSIST private) and source.f, its parts only where clashes demand them; the words only the loader calls go private; the public list (what programs and tools outside include.f call) goes to Joel with each word's callers. LINKER:WITH moves onto SOURCE:LOAD-WITH: its body runs inside LOAD-WITH, and ROUTE (src/habu/linker.f:5135), PRIOR-RUNNER (:63) and its close's `routed` arm go; test/file-objects.f checks the restore (a host load after the build leaves LINKER:COMPILED at 3). Fold in an include.f defect: INCLUDE-RANGE-RC (include.f:19) and E-FS-TIMEOUT (src/lib/error.f:32) are both -2110. First step: a census of every public word of the six packages and source.f with its callers outside include.f and its name clashes.

Depends: none. Claim: unassigned, owner jerry.
