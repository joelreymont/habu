---
title: TF-RBF-DEPTH is the unprotected sibling
status: closed
priority: 2
issue-type: task
created-at: "2026-08-20T21:03:58.851907+02:00"
closed-at: "2026-08-21T09:19:12.936722+02:00"
close-reason: "Landed with the type-family seal (dot habu-tfam-2b-sealed-1b77662c): src/core/type-family.f variable TF-RBF-DEPTH carries REG-PROTECT, and the same sweep found a second unprotected cell, SVX-HI, which now carries one too. Both are pinned in test/internal-word-gate.f TFAM-SEAL-CASES: 5 TFAM:TF-RBF-DEPTH ! answers rc 70 internal engine word, and the SVX-HI program that turned a catchable reject into exit 76 answers E-UNDEFINED."
---

From seal-3 (2026-08-20): src/core/type-family.f:2167 variable TF-RBF-DEPTH has no REG-PROTECT - the exact sibling of the SCH-RBF-P hole the schema seal closed (writable rollback-frame control from user source). Belongs to the type-family seal; protect it there with the same idiom (PF-TX-DEPTH is the file's own precedent) and a mutation-pinned case. Blocked-by the type-family cascade's dispatch.
