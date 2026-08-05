---
title: Rename the nzcv result member
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:29:50.008243+02:00"
---

CG-04, tip red. src/compiler/a64-effect.f:194 declares ENUM nzcv member 'result', colliding with the generic result<T,E> family from lib/adt/result.f: 'bad enum declaration nzcv: name is reserved or already taken at result', exit 67. Reproduce: bin/hb --load lib/adt/result.f test/compiler/a64-effect.f </dev/null. This kills 13 of 71 scheduled codegen tests before their first assertion, including codegen-compare-test.f (262 assertions) and codegen-workload-test.f (227). Fix: give the member a non-conflicting semantic name, and make the resident schedule assert scheduled-vs-ran counts so a member that fails to load names itself instead of going dark.
