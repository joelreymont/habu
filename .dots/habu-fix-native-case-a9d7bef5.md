---
title: Fix native CASE underflow on consuming default branches
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-14T08:35:10.886486+03:00\""
---

Owner cedar-case; workspace `.jj-ws/cedar-native-case` from `662d19a2`.
Coordination: `habu-attr-and-remove-2b13e978`. Root owns independent Astra
review and the composed integration gate.

The customer-free Tender reduction at
`/home/joel/.cache/tender/verification/native-case-underflow/section.f`
passes four assertions at tier 0 but fails SELECT? compilation at tier 1,
rc 67 / E-NELAB-UNDER -8304, on M SHA
`9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c`.
The smaller `: DROP-PREFIX ( n n -- ) case 1 of drop endof nip endcase ;`
has the same before result, so MATCH, strings and the return stack are not
necessary to trigger the defect.

`NELAB:DO-CLOSE-CASE` incorrectly required the default's current value count to
remain at least the CASE entry depth plus its selector. A default may consume
entry values. Remove that entry-depth check; `1 VDROP` still checks the actual
ENDCASE pop, and `TERM-BR` / `EDGE-STAGE` still check the live data/return rows
at the join. This is the only compiler source change. The elaborator otherwise
matches M's source `99caf411` exactly at baseline `662d19a2`.

Focused acceptance passes on a fresh M-hosted native build (137.29 s),
`/tmp/cedar-family-stage-abi/hb-native-case`, SHA
`2b4eb9a7b34f8dbda045feb4c0031da4b706920fc50c65789374b31399ab7851`:

- Original reduction at tiers 0 and 1, and minimal reduction at tier 1.
- New native-case suite through both JIT and native hosts, covering consuming
  and empty-result defaults, zero-arm CASE, nesting, parked and branch-produced
  return values, arm/default/all-path exits, and the original composition.
- Missing selectors and inconsistent data/return joins still reject with
  rc 70 at ENDCASE in both tiers. The new suite fails on M's old native compiler.
- Existing native-match, native-elaborate, native-rstack and native-exit
  suites at tier 1, plus reserved-name lint.
- `code-origin` is 1 for NCOMP:COMPILE, NELAB:COLON and the original SELECT?.

Evidence is `/tmp/cedar-native-case-{before,after}-{jit,native}.{out,err}`,
`/tmp/cedar-native-case-regression*.{out,err}` and
`/tmp/cedar-native-case-accept-*.{out,err}`. Existing MATCH/return-stack tests
emit their expected negative-case diagnostics. No application workaround,
fallback, installed binary replacement or performance claim is involved.
