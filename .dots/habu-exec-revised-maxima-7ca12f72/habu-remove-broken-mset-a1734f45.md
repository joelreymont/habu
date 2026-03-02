---
title: Remove broken mset wrapper from Maxima stubs
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-07T20:26:35.389285+01:00\""
closed-at: "2026-03-07T21:12:06.899762+01:00"
close-reason: "done (removed pre-Maxima mset wrapper from lib/maxima-stubs.lisp so stubs no longer depend on symbol-function 'mset before Maxima defines it; validated with maxima loader smoke via ./zig-out/bin/habu loading lib/maxima-loader.lisp and maxima-load-all)"
---

lib/maxima-stubs.lisp:397-406 duplicates the post-load mset alias wrapper but executes before Maxima defines mset. (symbol-function 'mset) is unbound in a clean Habu image, so the stub wrapper is a latent crash / dead code path. Remove the stubs copy and leave the post-load wrapper in lib/maxima-post-load.lisp:105-124 as the single hard-cutover implementation.
