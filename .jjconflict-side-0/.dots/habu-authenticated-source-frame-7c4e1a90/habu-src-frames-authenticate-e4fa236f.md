---
title: "Source frames: authenticate owned bytes"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:25.243060+02:00"
---

Problem: parser input currently has no package-owned immutable identity; accepting a caller pointer plus claimed digest would let bytes change after validation. Acceptance: add the bootstrap-safe private source-frame owner that copies exact source bytes, verifies the required content digest, binds canonical logical source identity and immutable extent, assigns a generation-checked internal frame id, and releases every partially acquired resource on open failure. It creates a frame but does not activate parser state or own nesting. Address, allocation order, checkout root, and diagnostic coordinates never affect identity. No legacy type declarer or public raw handle is added; pre-checker storage uses the explicit asserted implementation-layout exception from MODEL-CAD-V2-PLAN.md. Files: the narrow source-frame owner, native and Gforth layout mirrors, and focused authentication tests. Verify: digest mismatch, mutation-after-open, empty and no-final-newline sources, allocation/copy/digest failure injection, generation exhaustion, native/recovery parity. Depends: none. Ownership: immutable frame bytes, canonical identity, generation, and open/release only. Claim: unassigned.
