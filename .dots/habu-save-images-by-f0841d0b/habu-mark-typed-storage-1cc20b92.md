---
title: Mark typed storage pointer fields as DATA rows
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.977329+02:00"
---

T1b. Typed storage (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, CWORK) marks each pointer field as a DATA row in STORAGE-MARK-TERM at STORAGE-ALLOT and STORAGE-CAPTURE-WALK (src/core/layout-buffer.f:185-252), as it marks quotation fields as xt rows; TFAM gets a storage-has-an-address predicate beside TFAM-STORAGE-HAS-QUOT? (src/core/type-family.f); a sum payload's kind follows its tag, so ADDRESS-CELLS:REMOVE-XT-SPAN (address-cells.f:250) removes both row kinds. Verify: typed-storage capture tests, rebuild, full suite. Depends: T1a. Claim: unassigned.
