---
title: "List a part's public words under the part"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:32:02.084867+02:00"
---

Problem (from tim via Maki, 2026-11-03): tools/public-signatures-core.f:653-669 PS-PACKAGE-NAME! keeps one package name, so a nested 'package' inside a package (a part) replaces it and the part's public words are printed as the outer package's exported words. Acceptance: a part's public words are listed with the part's path, and the outer package's own public words keep its name after the part's ';package'; the public-signatures test gets a part case that fails before. Files: tools/public-signatures-core.f, its test. Verify: the public-signatures test. Depends: none. Ownership: unassigned. Claim: unassigned.
