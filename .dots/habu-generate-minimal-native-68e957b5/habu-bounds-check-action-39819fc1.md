---
title: Bounds-check action enumeration
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T20:55:58.402015+02:00"
---

maki/db/action.f:552-553 exposes public ENUM-AT (n -- CAD-KIND:action-id), builds the canonical order, then evaluates k cells ORD + @ RAW>ACTION-ID with no k check. Negative k reads before ORD; k>=ACT-N reads stale/zero/unrelated cells within or beyond the allocation; the private raw refinement then mints that value as a nominal action-id without validation. Current tests only enumerate 0..COUNT-1. Guard k<0 or k>=ACT-N with E-ACTION-ID before pointer arithmetic and before RAW>ACTION-ID; leave ORD-BUILD side-effect-free for rejected inputs. Add exact negative tests for -1, COUNT, COUNT+1, ACT-CAP, and a large wrapping index, with canaries around ORD proving no out-of-bounds read/write and no nominal value escapes; add property coverage that only 0<=k<COUNT returns and every returned id validates. Audit every public *-AT/*-ORDER@/*-ENUM accessor for the same raw-index-then-refine shape; async-dag ADAG-ORDER@ is the proven guarded precedent. Files: maki/db/action.f, action-test.f, any sibling accessor proven defective. Depends: none. Ownership: public enumeration bounds/nominal mint safety only; no registry factoring or action semantics.
