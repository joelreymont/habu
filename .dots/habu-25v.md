---
title: Complete gen-gc macho.lisp integration
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-03T13:36:06.815043+02:00"
closed-at: "2025-12-03T19:21:27.526949+02:00"
close-reason: ""
---

Infrastructure in place (gen-gc.lisp, gc-trigger-code, gen-write-barrier-code). Remaining work: (1) Create wrap-bytecode-with-heap-for-gen-gc in macho.lisp using gen-heap-init-code, (2) Modify deliver to use gen-gc-runtime-code when *use-generational-gc* is true, (3) Add tests for generational GC.
