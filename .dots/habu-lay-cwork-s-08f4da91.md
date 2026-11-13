---
title: "Lay CWORK's storage without generated text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T12:00:30.875933+02:00"
---

Problem: src/compiler/workspace.f CWORK DECLARE (CWORK-BUFFER, CWORK-SPAN, CWORK-VARIABLE) builds each accessor as source text and evaluates it (LBUF-EVAL!), against the user's rule that no definer generates source text; it is also what keeps layout-buffer.f's text helpers (LBUF-EVAL, LBUF-NAMES-OK?, LBUF-BASE,, LBUF-NAME,) alive. Once tier 1's compiler is deleted, src/compiler/digest.f's DG and SHA-CTX spans are its only users. A program can store into the cells: CWORK-BUFFER's text declares `variable NAME#base` (src/compiler/workspace.f:84) and CWORK:MEM#base and MEM#ctl (:11-12) are public, so 'FOOB CWORK:MEM#base !' makes a CWORK buffer answer a typed pointer past a 1-byte FOOB. Acceptance: no CWORK text is generated or evaluated; checked source outside CWORK cannot store into MEM#base, MEM#ctl or a buffer's base cell, and the FOOB program is refused; digest.f's spans are laid without it; workspace.f and the text helpers are deleted if nothing else uses them. Depends: tier 1's deletion on master (jerry's lane jerry-t1). Claim: unassigned.
