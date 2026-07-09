---
title: "ADG lowering: multi-save recompute threading"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T10:13:30.397910+02:00"
---

lib/ptx/ad-gen.f ADG-LOWER-BWD fail-closes forwards with more than one saves-op (E-PTX-NOIMPL before any emit; lib/ptx/ad-gen-test.f ADGT-MULTI-SAVES). Multi-save generated backwards (e.g. the full softmax op list with EXP./BLOCK-MAX/PTX:B/ = 3 saves-ops) currently route through the DAG pass (lib/ptx/ad-dag.f, device-proven) or the closed-form IR bridge (lib/ptx/ad-ir.f). Capability: extend the string-lowering recompute to bind PER-OP saved registers (a bindings table keyed by op occurrence instead of the single SV-X/Y/Z/MX/S/A cells) so AD-BACKWARD$ output for multi-save forwards lowers directly. Keep the pre-emit scan fail-closed until then. Acceptance: softmax generated backward lowers via ADG-LOWER-BWD, device gradcheck green, ADGT-MULTI-SAVES flipped to a positive fixture.

## Adopted rows (2026-07-06 ledger audit)

Owner-of-record for TRUSTED.md `lib/ptx/ad-saved.f stdlib-boundary ... 6` (from
archived habu-ad-thread-saved-36bad526, whose rows outlived its close): the
SAVED-X/Y/Z/MX/S placeholder mints plus the NEG wrapper (ad-saved.f:17-26) exist
because the save-vs-recompute pass materialises values the checker cannot see.
Completing the per-op saved-register bindings here retires the SAVED-*
placeholders; NEG rides until the emitters carry kernel types (see
habu-ptx-phantom-preserving-3df9db92).
