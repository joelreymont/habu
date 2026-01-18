---
title: Fix stdlib.habu load failure
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T14:36:12.062333+02:00"
---

stdlib.habu fails to load with 'do' macro TypeMismatch error at loop-generate-code:3114. Error: macro_vm.run TypeMismatch when expanding do macro. Root cause: either do macro implementation bug or macro expansion order issue. Fix: debug macro expansion, check do macro closure execution.
