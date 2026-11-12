---
title: Delete the host facts nothing reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:59:37.356733+02:00"
---

Problem: src/compiler/native/host.f keeps buffers that are written and never read: IMPL-CONTRACT, IMPL-FIRST, IMPL-EDGES and the edge rows EDGE-TO, EDGE-SITE, EDGE-KIND, EDGE-LOC with EDGE-N, filled by SEED-ROW, PREPARE and EDGE+ (callers in src/compiler/native/host-publish.f TAKE-EDGES); and IMPL-TYPE-FIRST and IMPL-TYPES with IMPL-TYPE-N, filled by SEED-ROW and SOURCE-CONTRACT-COPY and saved in host-publish.f TAKEN-TYPES. Acceptance: each buffer no word reads is deleted with its stores, and an argument or word that then feeds nothing is deleted with it; the engine builds twice and the suite passes. IMPL-REASON, IMPL-LOC and the reason classes belong to habu-remove-the-observers-5a486a1d. Depends: none. Claim: unassigned.
