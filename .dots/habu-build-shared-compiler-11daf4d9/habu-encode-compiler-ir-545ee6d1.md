---
title: Encode compiler IR canonically
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.449418+02:00"
blocks:
  - habu-render-and-diff-3d249719
---

Full context: design section 6.6 requires a versioned canonical wire format and digest. Define magic, major/minor version, little-endian widths, ordered tables, counts/lengths, decoder limits, full-input consumption, reference remap, and SHA-256. Acceptance: encode/decode/re-encode is byte-identical; malformed/noncanonical/oversized/trailing data rejects before unsafe allocation; one semantic field changes digest. Dependency: canonical tables and renderer fixtures.
