---
title: Start forked child at global scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T11:42:28.998012+02:00"
---

Engine seam found during internal-word-gate packaging (445aa258, two-sided probe in that file's header): a SUBJECT fork inherits the parent's open package scope, so a forked child that opens its own package is treated as opening a NESTED package and dies rc=75 with the bare diagnostic 'package'. A fork intended as a fresh top-level interpreter should start at global scope - or, if inheritance is deliberate, the nested-package refusal in a forked child needs a diagnostic naming the inherited open package and the fork boundary, because the current bare token is undebuggable from the child's exit alone. Owned result: decide the semantic (fresh-global-scope fork is the expected shape; state the reason if inheritance wins), implement it in the engine fork path, and convert the internal-word-gate suite's forced runner-after-;package arrangement into the natural runner-in-block shape as the production regression proving the fix - that suite going green with MAIN private is the acceptance. Reproducer: the two-sided probe recorded in test/internal-word-gate.f's header (fork with package open = rc75, fork after ;package = rc0). Not on the critical path; schedule after the cutover.
