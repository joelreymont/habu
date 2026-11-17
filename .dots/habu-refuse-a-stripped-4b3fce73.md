---
title: Call the engine's definers without PROVIDED-XT cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:06:59.126654+02:00"
---

Problem: src/habu/interpreter.f INSTALL-EVALUATE (:3353-3363) fills PROVIDED-XT cells at load, and EFFECT:ITEM, EFFECT:VARIABLE-OF, define-word (src/core/effect.f), EFFECT:OP (:22) and TYPE-DECL:TDECL-CHILD (src/core/sumtype.f:1030) reach DEFINE-RUN, ITEM-RUN, CHILD-RUN, TYPE-OP and LBUF:TYPED-VARIABLE-NODE only through an `execute` of a cell. A load-order hook (docs/design.md "Machinery being replaced") hides each callee from the link and the checker. The link: src/habu/link-band.f UNSAFE-NAME? refuses a stripped program that reaches create, evaluate, literal or compile,, but a stripped program does not hold the cells, so one calling EFFECT:EMPTY links and dies with SIGSEGV (rc 139). The checker: an immediate whose body calls EFFECT:CALL, EFFECT:VARIABLE-OF or define-word adds to the definition being compiled, yet carries no CTL-RENDERS fact, since the checker sees only the `execute`; the axiom form cannot name them (effect.f loads after the axioms, generation 2 stops rc 74) and an owner annotation would need src/habu/interpreter.f HOST-WORD?'s spelled list (:2895). Acceptance: the definers call the engine's run words by plain calls, with no PROVIDED-XT cell; a stripped program that calls a definer is refused at link as one calling create is, and a test builds one; `: V ( -- ) s" dup" EFFECT:CALL ; immediate` then `: P ( -- ) V ;` gives E-UNMODELED-IMMEDIATE at V under --load, tools/check.f and --verify-only, as `5 literal` in V's body does, and so do bodies calling EFFECT:VARIABLE-OF and define-word. Depends: none. Claim: unassigned.
