---
title: ROOT! says a root does not exist when it could not tell
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:00:52.895334+02:00"
---

Problem: SOURCE-ROOT:ROOT! (src/core/include.f) refuses with 'source root: does not exist: <path>' whenever the canon of the path fails (CANON-N below zero), whatever the reason: an existing directory behind a parent with no search permission gets the same line (probed 2026-11-10 on the engine at 'Refuse a word's load in a build': mkdir -p locked/inner, chmod 000 locked, ROOT! of locked/inner prints 'source root: does not exist: .../locked/inner', exit 74). The comment over ROOT-DIE in the same file says the canon does not say why a path fails, so a refusal names what a missing, unsearchable or long directory all are not; SOURCE-ROOT:WITH (ROOT-CANON) follows it and ROOT! does not. Found by the review of Tender's 'Load Zen's sources at each file's top level'. Acceptance: ROOT!'s refusal of a path whose canon fails claims no reason the code did not establish, and names the absolute spelling tried. Files: src/core/include.f, test/source-root-test.f. Verify: a case with an unsearchable parent and one with a missing directory in test/source-root-test.f. Depends: none. Ownership: none. Claim: unassigned.
