---
title: Give the quotation mark its own refusal code
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T18:45:01.496052+02:00"
---

Found by the return-stack lowering (8db52e9e): E-NELAB-BUNDLE is shared between a-cell-of-a-multi-cell-value (a real bundle) and a-cell-carrying-a-quotation-mark (not a bundle) - the parked-value refusal reuses it for both and the source documents the mismatch in place. Next time elaborate.f's error vocabulary is opened, mint a distinct code for the quotation-mark case and split the tests; error-code-lint owns the registry. Files: src/compiler/native/elaborate.f. Depends: none.
