---
title: "Libraries: migrate object index record"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T13:08:32.796309+02:00"
closed-at: "2026-07-24T17:43:03.137221+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/object-index.f still declares OBJIDX:rec with legacy PRODUCT, blocking total declaration-event release. Owner: lib/object-index.f, lib/object-index-test.f, and only callers whose tests require an unchanged generated effect. Replace PRODUCT rec 0 ... ;PRODUCT with STRUCTURE rec 0 ... ;STRUCTURE inside package OBJIDX. Preserve ptr/len field names and order, ptr u8/n schemas, OBJIDX-REC:MAKE/UNMAKE spelling, two-cell layout, OPTION:SOME/OBJIDX:LOAD behavior, key-buffer ownership, errors, and allocation behavior. Update stale comments. Forbidden: aliases, legacy parser edits, object-store or resolve redesign, raw casts, copied schemas, unrelated cleanup. Acceptance: the real object-index test covers present, absent, malformed, and destructor paths before/after; object-resolve continues to consume the exact public record effect; reflection/layout stay exact; no executable PRODUCT remains in lib/object-index.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 has green unified STRUCTURE production and object-index baselines.

Claim: agent=codex-object-index workspace=.jj-ws/habu-libs-migrate-obj-b34d2962
