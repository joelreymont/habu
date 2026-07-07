---
title: "Compiler: package-scoped model definer"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T03:28:16.756886+02:00\""
---

cad-1 follow-up (worker finding): MODEL: capture runs body tokens through checked planning words, but the body is not compiled into a single checker-verified word - fully static composition checking needs a package-scoped colon-style defining word: open the planning package, compile the body as a real definition over tensor descriptors, seed inputs from the signature, so the checker verifies the whole composition statically (CAD-PLAN section 3 full vision) AND named value references become possible (current linear-consumption capture cannot express DAG references like residual to an earlier intermediate). Compiler work authorized. Depends: cad-1 (landed). Related: habu-maki-subsystem-pkgs (package machinery), TFAM sealed-package rules.

UPDATE 2026-07-04: the named-value half LANDED on fable (maki/cad.f capture
layer, no src/core needed): signature input names bind, >V NAME names the
current value, a bare NAME pushes a parameter operand (FIFO queue; positional
capture provably unchanged). True skip connections + fan-out DAGs work; FFN-SKIP
in maki/demo-ffn-test.f proves node.3.in "n2 i0" and an OP-ADD summed x-gradient;
fail-closed paths in maki/cad-ref-test.f. REMAINING (this dot stays open): the
full CAD-PLAN section 3 static composition - compile the MODEL body as ONE
checker-verified definition over tensor descriptors (package-scoped colon-style
definer), so the checker proves the whole composition statically instead of the
capture layer resolving refs at parse time. v1 limitation to lift with it: a
named ref supplies PARAMETER operands only; the data operand is always the
running value (H1 GELU cannot re-root the chain).

UPDATE 2026-07-06 (feasibility verdict, second-half investigation on fable):
the full static composition (MODEL: compiles its body as ONE checker-verified
colon definition) is NOT feasible on the current engine. Two independent
blockers, both proven:

1. HARD (native) - checker NON-REENTRANCY. Any path where MODEL: (an executing
   word) triggers compilation of the body as a ": ... ;" WHILE the check hook is
   active CRASHES the native (SIGBUS / EXC_BAD_ACCESS, jump to PC=0x1 via blr x9
   from a clobbered interpreter-state slot [x20,#0x1b0]). Reproduced with BOTH
   evaluate and included; top-level evaluate of the same colon def (not nested in
   a word execution) is fine, isolating the gap to reentrancy of the compile +
   check hook across a word-execution boundary. Minimal reproducer:
     0 set-check  : W ( -- ) s" : ZZ ( -- n ) 5 ;" evaluate ;  1 set-check  W
   Split to dot habu-checker-reentrancy-certify-86771a6f (owner: src/core
   checker + colon compiler + check-hook).

2. SOFT (type-model) - `tensor` is a single opaque DEFTYPE, so a checked
   composition can only prove ARITY + tensor-vs-nontensor, NOT op KIND (all ops
   share the type) nor SHAPE legality (RxC/dtype/layout are runtime record
   fields). So "arity/KIND errors as checker diagnostics" is only partially
   reachable; E-CAD-PARAM-SHAPE stays a runtime throw until tensor types carry
   static extents. Split to dot habu-checker-shape-kind-4c6a3f4c.

Because the data-operand lift (H1 GELU re-rooting) is only natural in the
compositional stack form, it is gated on blocker 1 too.

LANDED (the achievable half, no crash, no new speculative library surface):
maki/plan-compose-test.f - a checked regression proving section 3's core claim
for HAND-AUTHORED top-level blocks. A model block written as an ordinary
top-level ": ( tensor ... -- tensor ) ... ;" over the existing public planning
words IS checker-verified (arity/type of the whole composition, at author time -
negative arity is a load-time exit-70 checker diagnostic), executes once to
capture a correct plan, and the stack form NATURALLY expresses the data-operand
re-rooting + fan-out DAG (PCT-BRANCH: GELU applied to the ORIGINAL input, x fanned
to two consumers, branches joined by ADD) that the v1 linear-consumption capture
rejects with E-CAD-REF. Wired into maki/test.f (70 suites green). The named
section-3 planning vocabulary (LINEAR/GELU/... in a planning package) is a clean
forward step split to habu-maki-named-descriptor-720fdc74; MODEL:-driven capture
stays blocked on habu-checker-reentrancy-certify-86771a6f. This dot stays open,
blocked on those two capabilities.

UNBLOCKED 2026-07-07: the reentrancy blocker dissolved - the natural
TRUSTED:-definer nested-compile path works on the stock engine (regressions in
test/gate-engine-lib.f; see habu-checker-reentrancy-certify RESOLUTION). The
remaining defect is confined to the hook-swap idiom no natural path uses.
MODEL:-driven static composition over package PLAN (maki/plan-vocab.f, landed)
can now be implemented; Blocker 2 (tensor kind-opacity,
habu-checker-shape-kind) still bounds what the checker proves to arity/kind.
