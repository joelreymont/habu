---
title: Parse owner construction policy
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:53:42.570989+02:00"
blocks:
  - habu-add-decl-event-a50e4104
  - habu-prove-verifier-event-f3454331
  - habu-rebuild-evaluate-txn-6c6e5b3c
  - habu-guard-owner-namespace-baef64c5
  - habu-guard-owner-name-27f9e5bb
  - habu-guard-owner-patch-0cf2322f
---

Problem: STRUCTURE source cannot request package-owned construction. Result: accept only CONSTRUCT owner before the first FIELD. Require an active package and a public, nonempty product. Record the flag through the existing DECL-EVENT:DERIVE token and DEV-K-DERIVE event; SD-CLOSE reads it to reject zero fields. Duplicate derive event returns E-DEV-DUP-DERIVE 7164. Missing or unknown policy, global/private use, and zero fields return E-DERIVE 7119 with the exact armed reason. A clause after FIELD and every DESTRUCT spelling returns E-SYNTAX 7107. Absence means the existing public MAKE and UNMAKE behavior; there is no CONSTRUCT public spelling, destruct policy/predicate/event, legacy alias, default row, substring check, or second registry. The only new trusted forwarders are STRUCTURE-DECL:FAM-CONSTRUCT-OWNER!, STRUCTURE-DECL:FAM-CONSTRUCT-OWNER?, STRUCTURE-DECL:DV-CONSTRUCT-OWNER, and STRUCTURE-MAKE:SM-CONSTRUCT-OWNER?. Owner: src/core/structure-decl.f grammar and existing event/replay seam. Production red: a finite closed-stdin process given `package own public STRUCTURE box 0 CONSTRUCT owner FIELD x n ;STRUCTURE ;package` exits 67, writes no stdout, and reports unexpected token at CONSTRUCT plus uncaught throw 7107. Acceptance: the new syntax passes source/replay; hostile comments, strings, duplicate text, wrong roles, all-errors, AOT, and fixpoint cases prove exact diagnostics and every declaration-transaction baseline rolls back; process tests use RUN-ARGV-STDIN-CAPTURE-OUTCOME; docs/type-system.md states the same one-spelling grammar; declaration, replay, package, native, and exact diff gates pass. Claim: unassigned.
