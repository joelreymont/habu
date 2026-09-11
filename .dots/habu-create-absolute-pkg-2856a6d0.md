---
title: Create absolute package prefix nodes
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T21:36:54.696414+02:00"
---

Current defect: the real engine child for `package A:B:C` exits 75. Implement absolute creation and reopen: validate the full path, then create or reuse A, A:B, and A:B:C. Each 48-byte DREC keeps marker -1, the full case-preserving path, folded identity, and the existing kind byte, package=0 and type=1. Package rows have distinct dynamic public/private WIDs; type rows have one dynamic public WID and private zero. One folded path exists at most once across both kinds. Derive parents from prefixes; store no parent or side state.

Keep exact inline/EXT storage: paths through 16 bytes stay inline, longer paths use existing CP-backed EXT storage without truncation, and the rounded CP end stays separate from the source cursor. Preflight the complete prefix set and capacity before publication; on failure restore CP and NDICT while WIDN remains monotonic. ENGINE-EMIT owns the frozen LNSFIND/EMIT-NS-FIND ABI: x9 is the absolute-path pointer and x10 its exact length; return x5 as the matching namespace DREC or zero and x11 as kind only on a hit; preserve x2, x9, and x10; use x30 only as the leaf return; clobber x3-x8 and x12-x17. Match only -1 namespace rows by exact folded inline or EXT name.

XREF owns FINALIZE-NAMESPACE ( ptr u8 n -- n ). It resolves exactly one row, requires package kind and distinct valid public/private roles, mutates that same row to type, zeros its private role, returns its public WID, and allocates nothing. Hard-rename TDECL-PROT-WID-XT to TDECL-FINALIZE-XT across every caller, stub, and test, with no alias. Finalize only after every generated word succeeds, then stage the returned WID for protection.

Snapshot restore translates EXT from RBASE-VA before bounding the span. Reject malformed or duplicate folded identities; unknown kinds; invalid or repeated namespace roles; package roles present in PROT-WID; a type public role absent from PROT-WID or present other than exactly once; nonzero type private; and WIDN not above every namespace and protected role. Namespace roles are in [FIRST-DYNAMIC-WID,WID:MAX), and WIDN is at most WID:MAX. Pair allocation preflights old WIDN <= WID:MAX-2; one-WID allocation preflights old WIDN < WID:MAX. Capacity failure is rc 77 before stack, name, dictionary, role, or WIDN publication.

Compact AOT contains ordinary rows only. Capture and boot perform whole-set preflight and reject marker -1 before any write, row publication, or WIDN advance. Every ordinary WID is zero or in [FIRST-DYNAMIC-WID,WID:MAX).

Production owners and write set: src/habu/layout.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/aot-capture.f, src/habu/xref.f, src/core/sumtype.f, src/core/generated-declaration-protection.f, test/gate-dictionary-lib.f, test/type-ctor-suite.f, test/bootstrap-wide-interpret-src.f, test/bootstrap-wide-memory-src.f, test/bootstrap-wide-tick-src.f, test/snapshot-writer.f, tools/build-fixpoint-test.f, test/aot-wid-build.f, test/aot-data-span-forge.f, and test/aot-wid-suite.f. Do not add a dot, gate, lint, manifest, schema, version, compatibility path, side table, parent field, counter, lazy repair, bootstrap feature, package seal, checker feature, deep or ancestor lookup, using change, nested package blocks, generated-name work, or E2/E3 expansion. Do not restore any deleted ownership registry, fixture, ledger, or status mechanism.

Acceptance: the real production path creates A:B:C; a case-varied reopen calls its private word bare; reopening every prefix allocates nothing; malformed paths and capacity failures publish nothing; caught failure restores CP/NDICT; generated declaration leaves one type row with public nonzero, private zero, and its public WID protected exactly once; snapshot restore and compact AOT hostile cases fail before publication; restored WIDs cannot collide. Run gate-dictionary, type-ctor, snapshot-writer, aot-wid-suite, build-fixpoint, typed-local and package diff gates, then the exact-tree M17 native, Maki, PTX-stdlib, and host gates.

Claim: unassigned.
