---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

The current goal is a polished Habu that other agents can use and that builds standalone binaries retaining the application, checker, native compiler and REPL. Keep PRIM confined to genuine engine/foreign boundaries; asserting ordinary Forth implementations does not satisfy the checked-Forth requirement. The child tasks retain the authorized compiler, library, source-loading, documentation, target and debugger work. No product-specific policy belongs in Habu.

Combined native fixes through d8b044e7 are integrated and cold-built. The public --repl builder produces a standalone executable that runs MAIN and compiles a new checked word from /tmp. Typed stored quotation relocation is complete. Remaining work includes warm checked source replay, final TRUST removal, scoped source roots, consumer libraries and process lifetimes, package/core binding, queued tools and final combined gates. The previous full suite had 108 passes and 70 failures on an older revision; focused passes do not make the final suite green. Each open child states its remaining acceptance.
