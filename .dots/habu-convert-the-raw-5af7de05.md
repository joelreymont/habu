---
title: Convert the raw pointer cells in test
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T18:44:23.883474+03:00"
---

Problem: the raw-storage census (/home/joel/.cache/hazel/scout-pun/census.md) lists the test/ sites the rule habu-refuse-a-ptr-5ad2734e refuses (fixtures and harness words that take ptr-field of a raw cell or store a pointer in one). Acceptance: every mechanical-shape site under test/ converted to the declared form the census names, a fixture that exists to exercise raw storage rewritten to say so through the declared forms or moved into the rule's rejected-program fixtures, every converted suite green on the release engine and loading on ~/.cache/hazel/engines/raw-rule-gen1. Files: test/ per the census. Verify: the converted suites; test/run.f. Depends: habu-convert-the-raw lib and tools lane (shared harness words). Ownership: test harness. Parent: habu-refuse-a-ptr-5ad2734e. Claim: unassigned.
