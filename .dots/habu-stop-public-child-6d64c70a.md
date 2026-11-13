---
title: Stop public child makers minting pointers
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:01:25.701159+01:00"
blocks:
  - habu-reach-child-routines-2f130f22
---

Problem: checked code turns an integer into a pointer with no private CAST: (card §5) through the public child makers. On the installed hb: 's" Y" CHILD-ABI:STORED s" ptr u8" TYPE-DECL:TDECL-STORAGE-CHILD drop  77 ,  Y @ NULL-PTR - . cr' prints 77, and prints 78 when the TDECL-STORAGE-CHILD call sits in a checked body ': MK ( -- ) ... ;' run at top level; 'variable V 77 V !  s" F" 0 TYPE-DECL:TDECL-PTR-CHILD  V F @ NULL-PTR - . cr' prints 77; 'BEGIN-STRUCTURE S1 PTR-FIELD: F1 END-STRUCTURE  V F1 @ NULL-PTR - . cr' prints 77. Cause: src/core/sumtype.f TDECL-STORAGE-CHILD (:1077) and TDECL-PTR-CHILD (:1085) are public and make a child whose effect answers a typed pointer over storage the caller lays or picks, and src/core/structures.f PTR-FIELD: (:42) makes a field word over any 'ptr a'. Their callers are global definers (src/core/layout-buffer.f, structures.f), and no global word can see a package's privates (docs/forth.md: no qualifier for the global wordlist). Kernel words keep their stack comments as rows a checked body calls (': G ( -- bool ) s" X" s" -A" LBUF-SUFFIX-OK? ;' loads). Acceptance: the three programs above are refused; a checked program mints a pointer or a quotation only through a private CAST:; LAYOUT-BUFFER, DEFER-LAYOUT-BUFFER, TYPED-BUFFER, TYPED-VARIABLE, DYNAMIC-BUFFER, the structure definers and TASK:+USER/+USERS keep working; Zen, Tender and Maki build and run. Depends: habu-reach-child-routines-2f130f22 (it changes the same child makers). Claim: unassigned.
