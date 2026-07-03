---
title: Role-typed FFI signatures
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.845851+02:00"
---

Discharge FFI-class TRUSTED sites: lib/ffi.f boundary words are trusted because C ABI calls cannot be expressed. Add role-typed FFI declarations: FFI: name ( roles -- roles ) binding symbol + arity + role-checked params (ptr/u8/n/fd roles exist), so the UNCHECKED surface shrinks to the single call primitive (ffi-call-abi) as an axiom, and every individual binding is checked against its declared roles. Count sites from inventory; migrate maki/device FFI first (cuInit/cuLaunchKernel bindings - also hardens habu-rca-culaunchkernel-ee5babba class bugs).

## STOPPED by the checker worker — no checker capability is missing; this is a lib/maki task

RCA (empirical, on this tree with dots #1-2 landed). The checker ALREADY expresses
everything role-typed FFI needs; the remaining work is a lib/ffi.f `FFI:` defining
word plus the maki migration, which is stdlib/application code OUTSIDE this
worker's exclusive checker territory (src/core/checker.f, check-hook.f,
docs/effects.md, tests). It also needs no engine/defining-word change in habu2.f.
Evidence:

1. `ffi-call-abi` (and `ffi-call`, `ffi-call-n`, `ffi-call-abi-r`) are already
   charted axioms in the checker PES (src/core/checker.f:3173-3177). Checked code
   can call them directly.

2. Role types in FFI signatures already check, including role-typed LOCALS. Proven
   checked binding (loads clean through `bin/hb`):
   ```
   deftype gpu-handle
   : FT-GET ( ptr u8 gpu-handle -- rc ) {: out:ptr h:gpu-handle :}
      out P>N 0 FFI-ARG!  h gpu-handle>N 1 FFI-ARG!  0 999 FFI-CALLABI  >RC ;
   ```
   The body is fully checked; the ONLY axioms it rests on are P>N (pointer->cell
   reinterpret) and ffi-call-abi. A call site that swaps a role is REJECTED:
   `: BADCALL ( ptr u8 fd -- rc ) {: out:ptr f:fd :} out f FT-GET ;` ->
   `in badcall: at 'FT-GET' expected: ptr u8 gpu-handle actual: ptr u8 fd`.

3. Dot #2 (declarable nominal ints) already provides the clean role-erasure cast
   at the FFI boundary: `deftype gpu-handle` auto-derives `>GPU-HANDLE` /
   `GPU-HANDLE>N`, exactly the marshalling a role-typed binding needs. So the
   substrate for role-typed FFI is fully in place.

4. FFI-class TRUSTED sites in the whole repo (trusted-inventory): ONLY `P>N`
   (`ptr a -- n`) and `N>P` (`n -- ptr u8`) in lib/ffi-abi.f. These are the
   irreducible pointer<->cell reinterprets — the checker fundamentally cannot
   verify a raw cell is a valid pointer, so NO checker change can discharge them;
   they are the axiom boundary alongside ffi-call-abi. maki/gpu.f's cuInit /
   cuDeviceGet / cuLaunchGrid / ... bindings are ALREADY checked code (they use
   the charted CALLn / P>N), just typed as generic `n` args rather than roles.

CONCLUSION: dot #3 discharges no checker-expressible TRUSTED site and needs no
checker capability. Its value is ergonomic/safety: a library `FFI:` defining word
so each binding carries a role-typed signature (catching arg swaps like the
cuLaunchKernel class bug) instead of hand-written generic-`n` marshalling. That is
lib/ffi.f + maki work, to be done by a stdlib/maki worker, not the checker worker.

DESIGN SKETCH for the lib/maki worker (all pieces already check today):
- `FFI: NAME ( in-roles -- out-role ) s" symbol" ARITY FFI;` in lib/ffi.f. At
  definition time record the symbol + arity; generate (via `evaluate`, one audited
  wrapper as in roles.f DTC-EVAL) a checked binding whose body: erases each input
  role to a cell (`P>N` for `ptr`, `ROLE>N` for a role/nominal, direct for `n`),
  `FFI-ARG!`s them, resolves the symbol once (DLSYM, cached), `FFI-CALLABI`s, and
  refines the result to the declared out-role (`>RC`, `N>P`, etc.). The generated
  binding is CHECKED against its declared role signature; the only axioms remain
  P>N/N>P + ffi-call-abi.
- Migrate maki/gpu.f cuInit/cuDeviceGet/cuLaunchGrid/... to `FFI:` with real role
  signatures (device handles, sizes as `len`, ordinals as `idx`, results as `rc`),
  hardening the habu-rca-culaunchkernel-ee5babba arg-order class.
- No baseline change beyond the one `evaluate` wrapper (like DTC-EVAL); the
  generated bindings are checked, not new TRUSTED sites.
