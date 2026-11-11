---
title: "lsp-test-lib expects an engine word's definition to be found"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:12:26.569758+02:00"
---

Problem: tools/lsp-test-lib.f (the case near :3246) expects go-to-definition on an engine word to locate target.f; no engine word has a source location (ENGINE-ORIGIN:TARGET answers not found), so the case fails on engines before and after the ENGINE-ORIGIN deletion. It is hidden because the lsp-boundary test is switched off. Acceptance: when records carry a source location (dot habu-report-a-primitive-dc40eac9's source cell) the case passes; until then the lsp-boundary test stays off and this dot names why. Files: tools/lsp-test-lib.f. Verify: the lsp-boundary test once it is on. Consumers: none meets it. Depends: habu-report-a-primitive-dc40eac9. Ownership: carl. Claim: unassigned.
