---
title: TF-RBF-DEPTH is the unprotected sibling
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T21:03:58.851907+02:00"
---

From seal-3 (2026-08-20): src/core/type-family.f:2167 variable TF-RBF-DEPTH has no REG-PROTECT - the exact sibling of the SCH-RBF-P hole the schema seal closed (writable rollback-frame control from user source). Belongs to the type-family seal; protect it there with the same idiom (PF-TX-DEPTH is the file's own precedent) and a mutation-pinned case. Blocked-by the type-family cascade's dispatch.
