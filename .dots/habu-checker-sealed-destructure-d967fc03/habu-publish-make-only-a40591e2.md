---
title: Publish make-only owner structures
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:50:37.924687+02:00"
blocks:
  - habu-lower-owner-only-72bd5026
---

Problem: STRUCTURE-MAKE always publishes both FAMILY:MAKE and FAMILY:UNMAKE, so owner-policy metadata alone does not close the forgery surface. Required result: for DESTRUCT owner, publish MAKE and any requested derived words but no UNMAKE metadata row, checker symbol, dictionary word, reflection entry, replay entry, or protected-name claim. Generated product equality/hash must use destructure family while the declaring package is active. This is not hypothetical: the derived generators call the family's own UNMAKE today, at `src/core/sumtype.f:1640-1644` (`TDGEN-EQ-PROD`) and `:1695-1699` (`TDGEN-HASH-PROD`), both emitting `TDGEN-UNBIND`, so an owner-policy product that also carries `DERIVE eq` or `DERIVE hash` breaks the moment UNMAKE stops being published unless those two emitters switch to the new form in the same change. The generated words themselves are rendered at `src/core/sumtype.f:1516-1531` and planned at `:1727-1734`, where the make and unmake rows share one plan and one evaluate; dropping the metadata row at `src/core/structure-make.f:106` and the rendered row is the atomic operation, because dropping only the text would leave a stamped-but-undefined checker symbol. PUBLIC policy remains byte-for-byte compatible with MAKE/UNMAKE. Owner-policy structures must not publish field-projection accessors; expose a single registry predicate for the future field generator rather than a name exception. Publication remains one atomic generated-declaration transaction with complete rollback. Owner: structure generator/publication only. Dependency: habu-lower-owner-only-72bd5026. Acceptance: hostile tests prove outside UNMAKE, tick, export, re-export, qualified lookup, reflection, and package reopen all fail; in-package destructure and derived words work; rejected generation leaves all registries/dictionary unchanged; public structures retain existing behavior.
