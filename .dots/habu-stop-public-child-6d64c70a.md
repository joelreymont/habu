---
title: Stop public child makers minting pointers
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T22:01:25.701159+01:00"
---

Problem: checked code turns an integer into a pointer with no private CAST: (card §5) through the public child makers. On hb: 's" Y" s" ptr u8" TYPE-DECL:TDECL-STORED-CHILD drop  77 ,  Y @ NULL-PTR - . cr' prints 77; 'variable V 77 V !  s" F" 0 TYPE-DECL:TDECL-PTR-CHILD  V F @ NULL-PTR - . cr' prints 77; 'BEGIN-STRUCTURE S1 PTR-FIELD: F1 END-STRUCTURE  V F1 @ NULL-PTR - . cr' prints 77; 'variable V 77 V !  s" EVIL" s" n" LBUF:BUFFER-CHILD drop  V NULL-PTR - here NULL-PTR - - ,  0 ,  0 ,  1 ,  0 EVIL @ . cr' prints 77. Cause: src/core/sumtype.f TDECL-STORED-CHILD (:1077) and TDECL-PTR-CHILD (:1085) are public and make a child whose effect answers a typed pointer over storage the caller lays or picks; src/core/layout-buffer.f LBUF:BUFFER-CHILD (:356) and LBUF:COMPANIONS (:362) are public and make a buffer child, or -BIND and -GROW companions that write into and zero a block, over storage the caller lays; and src/core/structures.f PTR-FIELD: (:42) makes a field word over any 'ptr a'. Their callers are global definers (src/core/layout-buffer.f, structures.f), and no global word can see a package's privates (docs/forth.md: no qualifier for the global wordlist). Kernel words keep their stack comments as rows a checked body calls (': G ( -- bool ) s" X" s" -A" LBUF-SUFFIX-OK? ;' loads). Acceptance: the four programs above are refused; a checked program mints a pointer or a quotation only through a private CAST:; LAYOUT-BUFFER, DEFER-LAYOUT-BUFFER, TYPED-BUFFER, TYPED-VARIABLE, DYNAMIC-BUFFER, the structure definers and TASK:+USER/+USERS keep working; Zen, Tender and Maki build and run. Depends: none. Claim: unassigned.
