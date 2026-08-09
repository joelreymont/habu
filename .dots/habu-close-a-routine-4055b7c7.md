---
title: "Close a routine's caller-save against the callee's clobber set"
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T10:38:37.613933+02:00"
---

A call site in src/compiler/native/select.f writes EVERY value the caller still holds into its own data stack and reads all of them back, whatever the callee destroys. That is what makes the discipline correct against a callee this compiler did not produce - an engine-compiled word, or one from a later back end - and it is why chain-calls-old works today. It is also maximally conservative: a callee whose contract destroys only part of the register pool leaves the rest live, and a value in one of those registers need not cross the stack at all. The callee's contract is already the thing the arity comes from once habu-resolve-a-callee-0340dfde lands, so the clobber set can come from the same place. Fix: give the source dialect's wordcall operation the callee's destroyed set, have the elaborator consume and re-answer only the values that set can reach, and leave the rest as ordinary SSA values crossing the operation. Measure it on the corpus before believing it: a call site that saves nothing is not obviously faster if the register pressure it creates spills instead. Blocked on habu-resolve-a-callee-0340dfde.


NOTE 2026-08-09: the blocker this leaf names, habu-resolve-a-callee-0340dfde,
no longer exists (closed and merged - name resolution landed). This dot is
unblocked; re-derive its premise on the current tree before claiming.
