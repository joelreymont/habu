---
title: Drop the duplicated comment line in combine.f
status: open
priority: 2
issue-type: task
created-at: "2026-08-08T23:28:38.314238+02:00"
---

src/compiler/native/combine.f:398-399 carries one comment line twice (verbatim duplicate on master, predates the float lanes). Delete one copy. Verify: bin/hb --load test/compiler/native-combine.f stays green; the two lines at 398-399 differ afterwards. Files: src/compiler/native/combine.f. Depends: none.
