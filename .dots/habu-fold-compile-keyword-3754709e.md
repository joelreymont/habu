---
title: Fold compile keyword dispatch into lookup
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:31:41.049002+02:00"
---

EM-COMPILE (src/habu/habu2.f:2803-2813) resolves each token via a linear chain: EM-COMPILE-SEMI -> ~22 keywords (habu2.f:2577-2622) -> ~30 ops (habu2.f:2650-2698), each an inline char-by-char case-folded LKWCMP (habu2.f:733-745), before LFIND. ~50 string compares per compiled token. Fix: register keywords/ops as dictionary entries with a kind tag (or perfect-hash table) so one hash lookup dispatches; depends on hash-indexed dictionary dot. Interpret path EM-INTERPRET has the same shape (habu2.f:2255,2475-2495).

## Status (2026-07-03, opus-engine) — STOP-WITH-EVIDENCE, not started

Feasibility findings (blocking the naive fold):

- **Ops are dual.** `+ - * = <> < and or xor dup drop swap over nip 1+ 1- 0= 0<
  negate invert f+ f- f* f/` are BOTH dict primitives (`FPRIM-L`, src/habu/
  habu1.f:1627-1645, used by INTERPRET mode) AND compile-loop keywords that emit
  INLINE codegen (VOP/VSHUF/VCMP/VUN/FOP, not a call). A plain "hash-find first"
  would resolve `+` to the `B+` primitive and compile a CALL, losing the inline
  fold and breaking the byte-exact fixpoint. The fold MUST keep compile-mode
  keyword priority over the dict primitive.
- **Dispatch is not uniform.** ~9 entry variants with different bodies and param
  counts: CF-ENTRY, CFN-ENTRY (no spill), CFB-ENTRY / CFBN-ENTRY (branch on VS
  top, 2 hxts), VOP-ENTRY / VOPI-ENTRY (float/int fold, 2-3 hxts), VSHUF-ENTRY
  (arity+hxt), VCMP-ENTRY (cmp code), VUN-ENTRY (2 hxts), FOP-ENTRY (fp opcode).
  A single kind tag must select among all of these, each byte-identical to today.
- **Blast radius of "dict entries".** Control keywords (`if then begin ...`) are
  NOT in the dict today; making them dict entries changes `find-name`/`'`/
  reserved-name semantics. The contained option is a SEPARATE keyword perfect-hash
  side-table checked before LFIND in compile mode (main dict untouched).

Measurement (heavy load = native fixpoint refresh, the canonical compile+codegen
load): baseline `install --force` 2.79/3.31/2.92s. First-principles: ~50 fast
length-checks/token (LKWCMP fails fast when TKL != kwlen) ≈ ~500 instr/token for
dispatch vs ~100 for the LFIND hash; on ~150k engine tokens that is ~3-5% of the
build (codegen/signing/I-O dominate). Real but modest.

Recommended design when taken up: a startup-built keyword perfect-hash side-table
mapping token -> (KIND, hxt/hxt2/opcode/arity); EM-COMPILE does one side-table
probe, dispatches on KIND to the 9 variant emitters (reusing the existing J-*/VF*/
E* bodies), and only falls to EM-COMPILE-CALL (LFIND) on a miss; EM-INTERPRET gets
the smaller define/string keyword set the same way. Verify byte-for-byte fixpoint
+ full engine-suite (all keyword/op codegen paths) before merge.

Stopped here: high-blast-radius rewrite of the hottest compiler path for a modest,
build-dominated-by-codegen win; not started to avoid a rushed subtle codegen
regression. No code changed for this dot.
