---
title: Share the label-triple helpers of the two reach lints
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T09:02:57.341917+03:00"
---

Problem (found reviewing the reach lane, landed 1d46de1f): tools/aot-startup-reach-lint.f copies tools/aot-section-reach-lint.f's label table (NAMES/NOFF/NLEN, ADD-LABEL, LABEL?, CAP-FAIL), TAIL, WORD?, LEX-WORD= and TRIPLE? verbatim, about fifty lines, so a fix to the triple shape or the qualified-name tail has to be made twice. Acceptance: one module under tools/lint/ (for example tools/lint/label-triple.f) that both lints require, holding the label table and the three-token shape; both lints keep their own model (section membership vs same-definition binding) and report lines; tools/aot-section-reach-lint-test.f and tools/aot-startup-reach-lint-test.f green with no fixture changed; no behaviour change. Files: tools/lint/, tools/aot-section-reach-lint.f, tools/aot-startup-reach-lint.f. Verify: the two lint tests; test/run.f. Depends: none. Ownership: hazel (tools load path). Claim: unassigned.
