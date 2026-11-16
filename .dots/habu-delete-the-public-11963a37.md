---
title: Delete the public wordlist word
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:49:36.907347+01:00"
---

Problem: once set-current is deleted (habu-verifier-rejects-export-e3912170) a program's `wordlist` makes a wordlist nothing can fill; only src/habu/linker.f:5124 uses it, and a build rejection exists only for it (test/fixtures/file-objects/build.f:106, src/habu/link-map.f:46-50). Zen, Tender and Maki do not call it. Acceptance: `wordlist` is absent from the product (the linker's use private), the rejection it alone needed is deleted. Depends: habu-verifier-rejects-export-e3912170. Claim: unassigned.
