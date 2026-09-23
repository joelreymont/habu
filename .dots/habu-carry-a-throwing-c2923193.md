---
title: "Carry a throwing callee's intact-input evidence into catch"
status: open
priority: 2
issue-type: task
created-at: "2026-09-23T10:49:11.992852+03:00"
---

The catch-stale rule (89902bde) counts a callee that throws as overwriting its declared inputs, so a caught stack-preserving word that throws makes its inputs stale and about 30 tree fixtures had to keep their values in locals before the catch. Record each word's own intact mask over its declared inputs where its signature is persisted (self-describing, fail closed: the retained-checker transfer has no version discriminator) and apply it at the call site inside a caught quotation instead of the conservative all-stale row. Evidence: catch-stale-handoff.md in the lane; the migrated sites are the measure of what this buys back.
