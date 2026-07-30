---
title: Query owner package
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:48:17.715170+02:00"
blocks:
  - habu-record-owner-construction-65ddd22f
---

Problem: native package close must decide whether the active package declared an owner-construction product before checker state is cleared. Result: add TFAM-CONSTRUCT-OWNER-PKG? ( ptr u8 n -- bool ) beside the existing family queries. It scans live families by canonical folded package name and returns true exactly when one public product has DRV-CONSTRUCT-OWNER. This query does not alter CHECKER-PACKAGE or enforce reopening; the persistent XREF marker and native package preflight own that rule. Owner: type-family aggregate query only. Production red: the family registry cannot answer whether the active package needs an owner marker. Acceptance: a real package with one directly flagged public family returns true; unflagged, private, absent, and rolled-back families return false; query results follow nested registry rollback without mutation. Forbidden: parser syntax, CHECKER-PACKAGE edit, native marker or sink edit, table, protected WID, snapshot payload, persisted owner identifier, source allowlist, friend latch, new error, compatibility branch, or lint. Smallest owning check: create and flag one public family through the real registry, query its canonical package name, then roll it back and get false. Claim: unassigned.
