---
title: Preserve verified input constraints when publishing JIT definitions
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-13T16:34:56.307345+03:00\\\""
closed-at: "2026-09-16T14:34:48.794654+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Tier 0 still republishes textual declarations and erases inferred fixed input cells, so a wrong-type caller is accepted where tier 1 rejects it"
blocks:
  - habu-keep-a-row-f2c4f3d4
---

Problem: tier0 republishes textual declarations and erases inferred fixed input cells/types, while tier1 preserves the verified graph. Safe compile-only reproduction on host28e11361: 0 set-tier; : ROW-ADD ( R -- R ) 1 + ; : WRONG ( ptr u8 -- ptr u8 ) ROW-ADD ; is accepted. Changing tier to1 rejects WRONG with expected n actual ptr u8. No wrong body or empty-stack call was executed. RB1 ( R -- R ) dup drop likewise records min-input0 in JIT versus1 in native. Fix JIT definition publication to retain the same verified cell/type/minimum-input constraints and declared row-kind semantics as the native path, without NEW/reparse erasing live facts. Do not simply copy the original scheme or weaken checker rejection. Acceptance: all provider/caller tier pairs reject the wrong-type caller, retain minimum-input guards, and preserve higher-order named-row positives/anonymous-window restrictions; ordinary REPL/load remains JIT. Files: checker.f effect publication, check-hook/source-owner/JIT finish publication path as proven; test/compiler/native-provider-rows.f plus actual REPL/load checks. Depends: provider-row fixf2c4f3d4; coordinate pending owner/tier1dc23a17. Ownership: unassigned. Verify: fresh rebuild, focused matrix, native suite. Use one verified publication contract; no duplicate helper or lint workaround.

Candidate (Cedar, base c028ced8): `EM-COMPILE-PUBLISH-TRUSTED` ran the hook's
verified `CHECKER-USIG-CERT-PARSED` publication, then reparsed TSIG through
`DEF-TRUST:REGISTER`, replacing that graph and its min-input latch. The JIT tail
now reserves textual registration for explicit `TRUSTED:` declarations and
retains the existing verified graph for checked definitions, including pass two.
`CHECK-DOES!` runs first because it clears publication latches; the checked
definer's graph and flags are finalized last. The recovery emitter mirrors this
split. No checker, owner, or row-kind policy changed.

Verification: native-build from private host 541586f5 produced B1
5388e51da928eb1cf9a83aa063d76794a1f8d0314ebcf7c8840d36cee6461e83; B1 rebuilt the
same source successfully as B2
878f19df4598692d34bb68fb2fdaf25d8818ceccc68ac43e9d16577c9c7e425d. Both pass the
strengthened native-provider-rows matrix, native-create-does, does-clause-record,
type-layout-lower-pending, and native-wide-mem. The matrix checks all four
provider/caller tier pairs, explicit trusted declarations, higher-order positives,
wrong-type and anonymous-window refusals, stored input widths and dictionary
min-input guards. Its JIT definer creates a usable pointer word and preserves
the inferred input constraint. No bad caller or empty-stack underflow is executed.
Actual B2 PTY and ordinary --load report tier 0, produce 42 for the valid row call,
and refuse the wrong caller; the PTY recovers and stays at tier 0. A Gforth-built
stage0 also produces 42 and refuses the same wrong caller (rc70).

Limit: p2-map-rewind assertions 2 and 11 fail identically on unchanged host
541586f5 and candidate B1 (narrow twin has two recorded calls; first targets
differ). The other relocation assertions pass. This candidate does not claim
the full native gate or a byte fixpoint; root owns independent review and the
combined gate. Private candidates: /tmp/cedar-jit-effect-B1 and -B2.

Cedar independently reviewed publication and the pass-two/does> interaction, and reran the provider matrix and native-create-does on B2: all pass. The combined does>/wide case explicitly refuses75 before pass two, preserving the existing unsupported boundary. Claim: Cedar integration, combined full gate pending.
