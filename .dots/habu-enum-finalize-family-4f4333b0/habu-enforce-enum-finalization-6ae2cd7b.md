---
title: Enforce ENUM finalization before publish
status: open
priority: 1
issue-type: task
created-at: "2026-07-24T07:31:02.960948+02:00"
blocks:
  - habu-enum-bind-canonical-7aea1cf1
---

Why: after the core finalizer and ENUM front end call site exist, raw DECL-EVENT publication must not bypass final kind selection. Dependency: habu-enum-bind-canonical-7aea1cf1 must land first so every production ENUM path finalizes before this guard becomes mandatory. Owner and interface: package DECL-EVENT owns the existing transient finalization latch and DEV-FAM-ENUM-LIKE? bridge. DEV-PREPARE checks the exact open top frame before any field or event publication. If its bound family is TK-SUM or TK-ENUM and the latch is not finalized, reject E-DEV-ENUM-FINAL 7176. Unbound frames and non-enum families retain their existing rules; PRODUCT publication and rollback before or after enum finalization remain unchanged. Owned result: publication enforcement only. Exact files: src/core/decl-event.f and test/decl-event-suite.f. Checkpoint: on the exact bind parent, a standalone public DECL-EVENT frame can bind a provisional TK-SUM family and PUBLISH without FINALIZE-ENUM; the path succeeds. Acceptance: that bypass and the corresponding TK-ENUM bypass reject 7176 before field publication, DEV-PUB-N, transaction depth, family kind, identity, or event state changes; a finalized payloadless family publishes TK-ENUM and a finalized payload family publishes TK-SUM; PRODUCT and rollback controls remain byte-identical; nested finalized frames publish independently. Removing the PREPARE guard or placing it after a mutating operation makes a public production-path mutation fail. Forbidden: a second kind query, registry, state cell, watermark, persisted flag, frontend edit, owner capability, new error, rollback restriction, or public interface. Smallest owning check: bin/hb < test/decl-event-suite.f. Claim: unassigned.
