---
title: "Foreign class: zero new axioms, one bounded-arg capability"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.390020+02:00"
---

Phase 4 of 4fd12d60, class C (92 sites): the foreign surface is 6 surfaces / ~39 engine axioms and ALL ALREADY EXIST (24 POSIX syscall prims callable from checked code; 7 FFI trampoline prims currently PRIM-TRUSTED-ONLY!; 5 sealed code-emission prims - keep sealed; SYMBOL; 2 clock). What is missing: (a) a bounded-FFI-arg-block capability so checked code may call the trampoline prims (FFI:RESET/ARGS/REG-LENS/READABLE!/WRITABLE!/VALUE! becomes a checked protocol), and (b) a FOREIGN: declaration form generating marshalling from an arg-role list - then the 44 declared entry points (33 CUDA driver, 8 pthread/libc, 3 test libc) cost zero new axioms and cuda-driver.f's 33 TRUSTED: become 33 FOREIGN: declarations. Blocks the final deletion.

USER CHALLENGE RECORDED (2026-08-20): why FOREIGN: in addition to PRIM:? The
form was the census's sketch, not a ruling. REVISED DEFAULT: no new form. The
capability is the bounded-FFI-arg protocol opened to checked code; each foreign
function is then an ORDINARY CHECKED WORD calling the checked marshalling plus
the axiom'd trampoline - PRIM: axioms remain only on the trampoline/syscall
bottom. A FOREIGN: declaration form may exist ONLY if the probe produces a
failing case the checked-word shape cannot express: C-side argument roles
(readable/writable spans, out-params) that the stack signature does not carry
AND that hand-written checked marshalling demonstrably gets wrong where a
generator would not. No failing probe, no form - the Simplify Relentlessly
gate applies with its own words.
