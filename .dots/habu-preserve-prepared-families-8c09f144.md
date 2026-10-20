---
title: Preserve prepared families through capture
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T15:11:13.675368+03:00"
---

REG-EXT-PERSIST invokes UNIT-FAM-END, freeing UFP storage still referenced by an ordinary prepared checker plan through UP.TFAM-STATE; later INSTALL dereferences it. Extend the existing checker-unit-capture E2E with a portable unit referencing a nominal family already present before MARK. Use the real NFILE reference providers and encoded reference section, close their resources after preparation, then run CAPTURE-PREPARE and install the checker-owned plan. Preserve the plan until explicit PREPARE-END, while pending plans still discard. Require an accepted typed downstream client, rejection of a numeric substitute, numeric runtime results and later small-plan reuse. Existing FILE type E2Es cover new nominal declarations. EXPLICIT-IMPORT installs, discards and ends preparation before image save, so no new save-with-open-plan contract, copy callback, owner ABI field or snapshot wrapper is required. Independent Astra review and matching native execution remain necessary; performance gain is not claimed.
