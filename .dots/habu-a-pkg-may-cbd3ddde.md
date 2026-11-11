---
title: A package may publish a word named ;
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.557659+02:00"
---

Problem: policy-admit admits a package whose public wordlist holds a word spelled `;`, on the installed Habu ee4b1361 and on the row-walk candidate (row-walk review, scratch program). The interpreter's keyword tables (src/habu/interpreter.f DEF-DESIGN-KEYWORDS, DEF-INTERPRET-KEYWORDS) and its own TOKEN-IS? lists (PACKAGE? :1287-1298, DEFINE? :2216-2227) are kept separately and nothing ties them together, which is also how seal-package was missed. Acceptance: POLICY-KEYWORD's answer is derived from the same lists the interpreter dispatches on, so a spelling the interpreter reads before lookup cannot be published; `;` is refused or shown harmless with a test. Files: src/habu/interpreter.f. Verify: refusal cases in src/lib/vocabulary-test.f for `;` and one spelling from each dispatch list. Depends: the row walk (habu-delete-the-jit-d9fdaf67) landing first. Ownership: src/habu/interpreter.f keyword tables. Claim: unassigned.
