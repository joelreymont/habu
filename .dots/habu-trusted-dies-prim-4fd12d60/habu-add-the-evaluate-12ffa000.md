---
title: Add the evaluate-closed primitive
status: open
priority: 1
issue-type: task
created-at: "2026-10-01T12:53:25.113563+02:00"
---

Leaf 1 of the evaluate-closed design (~/.cache/tmp/heron-arm64/design-evaluate-closed.md, Fable Plan 2026-10-01). evaluate-closed ( ptr u8 n -- ) evaluates source with the stack floor at the caller's depth and refuses residue by name, so every TRUSTED: evaluate wrapper can become a checked word.
Mechanism: a BRUNSTACK-shaped framed primitive B-EVAL-CLOSED in src/habu/habu1.f: push a frame (x30, caller BASE, CAP); after popping the string cells set BASE := XDS and CAP := CAP - (XDS - oldBASE); inline the B-EVAL emitter with x30 at a continuation label; on clean exit, XDS <> BASE restores BASE/CAP, pops the frame and throws E-EVAL-RESIDUE (G-PUSH BTHROW), else restores and returns; a throw out of the text unwinds through LEVALREC and the caller's catch. Register it in EMIT-DICT-PRIMS (habu1.f:3370-3379, 2 GDEREF-F); row EPRIM: evaluate-closed PE-PTR-U8 PE-IN PE-N PE-IN EPRIM; beside run-in-stack (src/habu/prims.f:387); -3803 constant E-EVAL-RESIDUE in the E-ENGINE block of lib/errors.f mirrored in src/habu/stack-abi.f like E-STACK-UNGUARDED; seed mirror in bootstrap/cg/forth.fs. evaluate itself and UNSAFE-TOK? stay unchanged.
Acceptance: test/compiler/native-eval.f rewritten as the contract test (red before: E-UNDEFINED): a definitions text loads; residue 1 2 throws E-EVAL-RESIDUE; 7 then a closed drop throws 70 and leaves the 7; a refused definition throws 70; depth inside is 0; nested closed evaluate; E-EVAL-RESIDUE equals STACK-ABI:E-EVAL-RESIDUE; CHECK! refuses a body naming evaluate and certifies one naming evaluate-closed. docs/forth.md and docs/forth-card.md name evaluate-closed as the checked way to evaluate source, with the three limits the design lists, plus a Rules-learned-by-refusal entry.
Verify: native build byte fixpoint; two generations identical; full bin/hb --load test/run.f; gforth recovery for the seed mirror.
