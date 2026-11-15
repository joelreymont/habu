---
title: Accept a reading quotation passed to RECURSE when the word never keeps it
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:43:10.460262+02:00"
---

Problem: a RECURSE call shares the signature's own block variables (src/core/checker.f SIG-EFF-CACHE!, CF-RECURSE-EFF), so a quotation that reads a local is refused E-LOCAL-ESCAPE at every RECURSE, also where the word only runs it: a tree walk that passes a new reading quotation down (~/.cache/tmp/quot-hunt-a/p/p01-tree-walk-safe.f) is refused though it is safe. The body's use of a block part is known only after the body, and a RECURSE inside it is judged before. Acceptance: a recursive word that passes a new reading quotation to RECURSE and never keeps, stores or returns it certifies and runs; every program refused today for keeping one stays refused (p06, r2-quot, lim2-quot). Needs a fixpoint over the signature's block parts (check the body, then the RECURSE calls against the finished block parts, again until nothing changes): a design before a brief. Accepting more programs breaks none, so it waits for a consumer that meets the refusal. Depends: none. Claim: unassigned.
