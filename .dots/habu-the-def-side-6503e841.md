---
title: The definition-side qualifier lookup is still linear
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T10:47:48.116234+02:00"
---

Full context: two places resolve a NAME:tail qualifier to a wordlist. The REFERENCE side is LFIND's FIND-QTAILOK in src/habu/habu1.f, which now probes the dictionary hash index for a record carrying the wordlist marker -1 and cost 10.0 us per qualified token before that and 1.05 us after (dot habu-compile-shaped-cost-4e74a181). The DEFINITION side is C-QUALIFY in src/habu/habu2.f, reached when a definition NAME itself is qualified, and it still walks every record from index 0 looking for the same marker. It is far colder than the reference side - definitions inside a package block are published bare, so a qualified definition name is the exception - which is why it was left out of that lane rather than because it is correct as it stands. The conversion is the one already written: set the wid to -1, hash the head of the token, walk the chain, and keep the scan for no-table and chain-exhausted. It shares FIND-QTAILOK's semantic precondition, so the same fixture covers both: a wordlist name is unique, so the scan's first match and the probe's chain match are the same record. Depends: src/habu/habu2.f C-QUALIFY; src/habu/habu1.f FIND-QTAILOK, C-HIDX-HASH. Ownership: qualified definition names. Claim: unassigned.
