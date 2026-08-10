---
title: Mint a routine form for a convention that never returns
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T05:21:40.761986+02:00"
---

An all-dead body (every path ends in a certified-dead call) elaborates correctly but is refused E-A64RAV-ORDER: it still executes a64.reserve + a64.lnkstr and nothing releases them (measured, dead-path lane 2026-08-10; pinned as a refusal in test/compiler/native-dead-path.f with a comment naming this dot's work). LEAF-FRAMED was probed and the selector refuses it by name - correct, the routine contains a bl. What is missing: a64-effect.f already models control no-return, but src/compiler/native/abi.f mints no routine FORM for it; the form must declare the direct-call trait with NO frame and NO link save, which gives abi.f, frame.f, the selector prologue and LINK-CK each a say - interface design, not a one-file edit. 23 census definitions (the -8522 bodies) wait on it. Note: the no-return trap exit currently uses ENGINE-ERROR:CODE-CERT (88, reserved-undocumented) on the derivation that the falsified thing is the certificate; re-derive or bless that choice when building this. Acceptance: the pinned refusal case inverts to a publication; an all-dead routine executes to exit 88 naming the callee; the 23 census bodies compile or refuse for a new honest reason; LINK-CK still refuses a frameless routine that RETURNS. Files: src/compiler/{a64-effect.f,native/{abi,frame,select,regalloc-verify}.f}. Depends: none.
