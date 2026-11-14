---
title: Refuse a stripped program that reaches a definer through an engine cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:06:59.126654+02:00"
---

Problem: src/habu/link-band.f UNSAFE-NAME? refuses a stripped program that reaches a kernel word it cannot run (create, evaluate, postponed, literal). In the typed-children feature (qpkmovxk), TYPE-DECL:TDECL-CHILD (src/core/sumtype.f:1069) and EFFECT:OP (src/core/effect.f:22) reach the running engine through PROVIDED-XT:CHILD-CELL and TYPE-OP-CELL, which only src/habu/interpreter.f INSTALL-EVALUATE fills and a stripped program does not hold, and define-word reaches DEFINE-CELL the same way; none has a kernel name, so the link cannot refuse a program that calls them. A stripped program that calls EFFECT:NEW links, then dies with SIGSEGV (rc 139) at run time. Acceptance: a stripped program that calls a definer reaching an engine cell is refused at link as one calling create is, and a test builds one. Depends: none. Claim: unassigned.
