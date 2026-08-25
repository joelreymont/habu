---
title: "Nominal storage: raw variable kinds"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:25.881837+02:00"
closed-at: "2026-07-15T00:37:09.805205+02:00"
close-reason: "Merged 085cf242 on master: TVK kind table parallel to TVT (TVK-ANY/TVK-RAW) - raw storage definers can no longer mint nominal families in value position. here publishes PE-PTR-A-RAW; create/variable/constant/PTR-VARIABLE registered RAW via verify-source SIG-RAW-MODE!/SIG-RAW-DEFINER!; U-TYPE RAW-BLOCK?/RAW-OK? rejects T-PARAM families + linear cons on RAW vars while admitting scalars/roles/pointers/xts; kind survives trail rollback (tag-2 entries), copy/freshening (EN.B baked into USIGS), snapshot/AOT. Negative regressions both paths: here mint verdict 0 (layout-buffer.f) + variable/create/constant mints verdict 0 with plain-n positives -1 (pointer-storage-test.f). Fixpoint byte-identical, full run.f RUN_EXIT=0, trust manifest 702 rows balanced, all suites green on the exact merged tree with refreshed binary. HONEST RESIDUALS dotted/documented: (1) native/REPL definer path (habu2.f C-CALL-TRUST-LASTC-* + forth.fs mirror, owner-persist fenced) still registers non-RAW - enforcing gate path IS sealed; follow-up dot minted. (2) RAW admits role atoms/xts by design for now (engine codegen raw-stores labels/xts; fencing needs typed-cell migration first - typed-definers territory, documented in docs/effects.md)."
---

Problem: here, create, variable and constant publish unrestricted polymorphic effects, so raw storage can mint any nominal family. Acceptance: add TVK-ANY and TVK-RAW to effect variables; mark raw definers and verifier-generated effects RAW; propagate through copy, freshening, unification, rollback, snapshot, native and bootstrap paths; RAW accepts plain scalar representation but rejects nominal atoms, arity-zero families, linear/layout values and pointers containing nominal state; numeric generic cells remain valid; no per-fetch lookup. Files: src/core/checker.f, src/habu/verify-source.f, src/habu/habu2.f, bootstrap checker/compiler mirrors, engine fixtures, docs/effects.md. Verify: red-first variable/create/constant/here laundering cases, numeric positives, native fixpoint, bootstrap and full test/run.f. Depends: none. Ownership: raw type-variable kind only; no typed definer or CAD migrations.

## EVIDENCE ATTACHMENT 2026-07-13 (from the TK-CELL capability review; duplicate
dot habu-checker-nominal-value-d6533898 closed into this one)

Live probes on the unified tree: `( n -- CAD-KIND:region ) VAR ! VAR @`
CERTIFIES - a fetch from an untyped cell is a fresh var that binds a declared
family output in value position; the SAME raw variable also certifies as
`ptr CAD-KIND:region` AND `ptr CAD-KIND:cols` before the NOMPTR-BLOCK? guard
(pointee side now closed by 93a3b968; VALUE side is this dot). The TK-CELL
landing migrated all maki scratch cells to typed LAYOUT-BUFFER slots, so the
in-tree consumers are ready; this dot's TVK-RAW design closes the remaining
mint path (raw definers publishing unrestricted polymorphic effects).

Claim: agent=rawkinds workspace=.jj-ws/fable-rawkinds
