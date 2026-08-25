---
title: TFAM 13 repair-packet ADT variant/tag field
status: closed
priority: 2
issue-type: task
created-at: "2026-07-10T14:59:02.249233+02:00"
---

Scoped chain for the remaining ADT fields on the E-MISMATCH repair packet. DONE: family [render-only, landed with c41afe48 TERM-FAM]. DONE: variant/tag [re-ported onto master's U-FAIL/TERM-FAM path, landed 1e0f6c95 — CVLIVE latched around CONSTRUCT-STEP-XT, UF>DIAG records DVAR beside DF-ACT/DF-EXP, SV-DVAR rollback, GDX-ADT-VARIANT fixture]. DONE: payload-pos [structural slot capture — UWL-POS/CUR-UPOS per-pair position sidecar mirrors UWL-STR/CUR-STRICT; the root row pair carries an encoded spine cursor (-(c+2)), U-ROW-DESCEND gives the type pair slot c and the rest pair cursor c+1, nested rows inherit the slot, LOGHID expansion drops to no-position fail-safe; U-FAIL latches UF-POSN, UF>DIAG records DPOS, SV-DPOS rollback; render DIAG-PAYLOAD-POS converts slot-from-top to declaration-order index. Structural, not type-matched: MK-CON interns, so a two-slot same-type variant proves the position (GDX-ADT-PAYLOAD-POS)]. DONE: arity [E-MISMATCH same-family different-argc is unreachable from checked source (signature parser enforces declared arity), so the field pair lives on the E-WRONG-ARITY / fix_signature_arity packet: SIG-END-PARAM's SGBAD-ARITY! latches SGBAD-AR-DECL/SGBAD-AR-GOT first-wins with SV rollback, render emits arity_expected/arity_actual, GDX-SIG-ARITY fixture]. Schema parity for all fields in docs/repair-diagnostics.md + tools/repair-schema-doc-test.f (CBAD drives payload_pos, ABAD drives arity). Chain complete.
