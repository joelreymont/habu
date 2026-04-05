---
title: Fix Maxima runtime init
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T23:50:21.639324+02:00"
---

Problem: authoritative full Maxima load leaves MAXIMA::*MAXIMA-USERDIR* nil until MAXIMA::INITIALIZE-RUNTIME-GLOBALS runs, but calling INITIALIZE-RUNTIME-GLOBALS under Habu currently mutates state and then signals TYPE-ERROR nil. Evidence: full load probe reports GETENV fbound=t, DEFAULT-USERDIR works, USERDIR=nil before init, USERDIR="/tmp/.maxima" after init side effects, but INITIALIZE-RUNTIME-GLOBALS signals. Need isolate the failing clause in ../maxima/src/init-cl.lisp and fix the underlying Habu compiler/runtime bug generically. Files: ../maxima/src/init-cl.lisp, src/compiler/compile.zig, src/interp/vm.zig, tools/maxima-rtest.lisp. Acceptance: (load loader) + maxima-load-all + post-load + initialize-runtime-globals completes without condition and leaves string MAXIMA::*MAXIMA-USERDIR*.
