---
title: "Find whether check's reserved-name lint restates the language"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:13:48.323939+02:00"
---

Problem (lead's sweep): plain tools/check.f on test/engine-suite.f reports E-RESERVED-DEFINITION for ': char ( n -- n ) 1+ ;' and ": ' ( n -- n ) 1+ ;" inside package ES-CHAR-N (550-551, 'reserved by parser/control dispatch; rename after prefix stripping'), which bin/hb --load admits and the test relies on (a body's char calls the word of that spelling). tools/reserved-name-lint-core.f is a reader of its own over source, run by tools/check-core.f CHK-RUN-RESERVED-NAMES and by tools/reserved-name-lint.f, beside the definers. Acceptance: a hunt states each rule the lint applies, whether the checker or the definers apply it too, and where they disagree; the fix (delete the lint if it only restates or overreaches the language, as chk-1 deletes the declaration scan; else move the rule into the layer that owns it) is a second brief. Files: tools/reserved-name-lint-core.f, tools/reserved-name-lint.f, tools/check-core.f. Verify: plain check of test/engine-suite.f has no E-RESERVED-DEFINITION. Depends: none. Ownership: check's lints.

Waiting (no consumer meets it). The census is done and its findings are dots 06192e22 (;match) and 50d84a9b (UNSAFE-TOK?), with deleting the lint blocked by them. Workspace carl-hrsv was never used and is retired. Claim: none.
