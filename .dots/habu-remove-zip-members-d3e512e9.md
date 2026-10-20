---
title: Remove ZIP members
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T14:26:18.610220+03:00"
---

Stuart/Tender needs a bounded public ZIP member-removal operation for cleaned blank/erase DOCX outputs. Existing EDIT/REPLACE/COMMIT preserves answer-bearing thumbnail payloads. Add public archive/entry removal at the ZIP library boundary; commit must physically omit deleted local payload and central entry while preserving retained member bytes, method and order. Keep ordinary WRITE preservation policy and OPC/thumbnail selection in Tender. Pre-code focused real source/native E2E: two members, remove preview, commit/reopen retained member only and deleted payload absent; retain lifecycle/partial-failure safety and compressed/native STORE backends. Independent Astra review and qualified immutable public source release required. Context inventory is /Users/joel/Work/tender/private/cleaned-corpus/inventory/blank-holds-review.json; minimal preview repro is being retained. Does not depend on core incremental implementation; assign a separate library owner.
