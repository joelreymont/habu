---
title: Bootstrap Maxima source/test search lists
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T09:35:52.715233+01:00\\\"\""
closed-at: "2026-03-07T09:36:12.818183+01:00"
close-reason: done (source-tree search lists now initialized in lib/maxima-post-load.lisp; validated *maxima-srcdir*, *maxima-testsdir*, , and (file_search(...)) for test_readbase_maxima)
---

lib/maxima-post-load.lisp:129-170; ../maxima/src/init-cl.lisp:243-301; ../maxima/src/mload.lisp:94-120,568-575. Under source-tree loading, *maxima-srcdir*, *maxima-testsdir*, , , and  are nil. Populate them with ../maxima/src and ../maxima/tests wildcard pathnames so file_search and load stop failing in /.
