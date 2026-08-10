---
title: One bundling rule, two owners
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T20:40:58.262072+02:00"
---

tools/bundle-lib-core.f and tools/examples-test.f now both apply the assume-or-carry rule (engine-provided files stated in the header + verified at load, others carried inline) - the second copy exists because examples-test's fixtures interleave example scripts the bundler cannot express (seeda lane 2026-08-11). Factor the rule to one owner or teach the bundler the interleaving. Files: tools/{bundle-lib-core,examples-test}.f. Depends: none.
