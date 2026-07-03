---
title: Builder TRUST rows to CHECKED
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.827175+02:00"
---

Convert the ~307 TRUST rows asserting builder emit-word effects (91 in src/habu/habu2.f, habu1.f, jit.f - e.g. habu1.f:7,67,75,251, habu2.f:145,1074,2294,2467) into real checked definitions: the builder is ordinary host Forth over the asm DSL, and reg/label/asm roles already exist (src/core/roles.f). Work file-by-file (habu1 -> habu2 -> jit), keeping the byte-for-byte fixpoint green after each batch; any word the checker cannot yet express gets classified (see habu-trusted-inventory-classifier output) and a named capability dot instead of staying silently trusted. Metric: TRUST row count monotonically down, tracked by the inventory ratchet. Conflicts: src/habu/habu2.f owned by engine worker until CP-rollback lands - start after merge.
