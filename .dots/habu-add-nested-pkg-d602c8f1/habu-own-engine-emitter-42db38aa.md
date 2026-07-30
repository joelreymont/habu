---
title: Own engine emitter package
status: active
priority: 1
issue-type: task
created-at: "2026-07-30T22:13:55.502748+02:00"
blocks:
  - habu-delete-native-pkg-d80850de
---

Problem: changing the native emitter exposes every altered transitive definition to the mandatory package gate. The reviewed first slice, commit e6c56d71a872a92d0c054ab42db68817e8ae8856, correctly owns the terminal callback, production drivers, generated caller, and trust identity, but a representative package-wall deletion still reports legacy global callees beginning with BPROTWIDADD and C-POSTPONE. Complete the same package boundary rather than creating helper-owner dots.

Result: retain the reviewed first slice. Reopen ENGINE-EMIT in src/habu/habu1.f and make BPROTWIDADD, EMIT-ENGINE-PRIMS, and EMIT-PRIMS private. In src/habu/habu2.f, put these remaining private groups under ENGINE-EMIT: C-POSTPONE, C-BTICK, EM-COMPILE-META-KEYWORDS, EM-COMPILE-KEYWORDS, and EM-COMPILE; C-TICK, C-PACKAGE, C-EXPORT, EM-INTERPRET-DEFINE-KEYWORDS, EM-INTERPRET-WORDS, EM-INTERPRET, and EMIT-MAIN; LRESTAB, RESTAB-BUF, RESTAB-LEN, EMIT-KWDATA, C-SEAL-PACKAGE-FAIL, C-SEAL-MATCH, C-QUALIFY-SEAL-GUARD, EMIT-QUALIFY-DEF, C-PACKAGE-PROT-GUARD, C-PACKAGE-SEAL-GUARD, and the TRUST rows already attached to that block; EMIT-STORE-DEF-NAME and EM-AOTWIDGATE. Keep every internal private. The only public emitter API is ENGINE-EMIT:FORTH ( ptr u8 n -- ). No other public name or forwarding word is added.

The retained first slice hard-cuts global EMIT-FORTH, makes ENGINE-BUILD:BUILD call ENGINE-EMIT:FORTH, publishes BUILD-DRIVER:RUN and STDIN-DRIVER:RUN, updates AOT-WID-BUILD, and renames the single trust identity. Bootstrap/cg/forth.fs owns its separate Gforth emitter and remains outside this dot.

Owner and files: src/habu/habu1.f and src/habu/habu2.f for the completion; the retained first slice also owns src/habu/build.f, src/habu/stdin.f, test/aot-wid-build.f, and TRUSTED.md. Production defect: an exact representative deletion through the real native emitter produces package findings for BPROTWIDADD and C-POSTPONE before this completion. Acceptance before M17: the rooted exact-diff package gate reports no finding for the complete transitive chain; typed-local and trust source gates pass; old global EMIT-FORTH has no native definition, caller, or trust row; only ENGINE-EMIT:FORTH is public; the focused AOT-WID production build passes. E1 and the native package-wall deletion rebase onto the completed commit.

Forbidden: behavior changes, namespace semantics, compatibility aliases, forwarding shims, new gates or exemptions, bootstrap edits, whole-driver packaging, helper-owner dots, and unrelated emitter refactors. Claim: agent=engine_emitter_impl workspace=.jj-ws/habu-own-engine-emitter-42db38aa.
