---
title: Locate origins across opaque top-level calls
status: open
priority: 2
issue-type: task
created-at: "2026-10-19T17:43:47.472873+03:00"
---

Problem: LSP origin navigation (go to the source of an image symbol, tools/lsp-source-index.f TARGET-IN, rule stated at :158-170) has no source-hash bypass, so its conservative standalone index defers a whole file at any top-level dictionary call (a loader, `undefine`, a replaceable library definer), whose execution could change any binding, and then gives no Location even for unchanged declarations. Fixtures pin this as deferred or missing locations: tools/lsp-test-lib.f:3191 (turning the colon of `: SCRIPT-ARGC` into a space hides the unchanged SCRIPT-ARG-START), :3218 (src/core/sumtype.f) and :3288 (the top-level allocation calls in src/core/layout-buffer.f). A trusted does> definition defers the same way, because the scanner skips its clause body; that case ends with habu-delete-the-trusted-42b30edd. Lifting the limit needs a design first: safe source bindings established at the verifier boundary responsible for them, without restoring hashes or guessing through opaque calls, keeping the callback, collector and index contracts and the 72-byte FILE record; modelling native calls stays outside the incremental qualification adapter. No user need is recorded.

Acceptance: a decision recorded in this dot: either a design naming where the verifier establishes the bindings and how each fixture above becomes located, or a ruling that the deferral is the intended behaviour, after which the dot closes and the fixtures stay as they are.

Files: tools/lsp-source-index.f, src/habu/verify-source.f, tools/lsp-test-lib.f.

Verify: the decision is in the dot; for a design, the three fixtures answer located in the real prefix and open-origin LSP conversations.

Depends: none.

Ownership: unassigned. Claim: unassigned.
