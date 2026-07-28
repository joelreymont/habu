---
title: Model bare wordlists in package context
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.911570+02:00"
---

Full context: test/gate-dictionary-lib.f:214 (hb long dictionary wordlist isolation) creates a plain wordlist, set-current to it, and compiles a checked definition; on the proofs branch the definition path throws 7136 E-PKG-CONTEXT (src/core/checker.f:527) with empty stdout/stderr, because CHECKER-PKG-CONTEXT's live provider only recognizes global scope or a real package record's public/private pair, and a bare user wordlist matches neither. Decide the invariant: either the checker's package-context model gains a legitimate anonymous-wordlist state (definitions allowed, no package authority minted), or raw set-current to a non-package wordlist is formally rejected with a named diagnostic and the ANS test migrates to packages. Silent 7136 with no diagnostic is wrong either way. Acceptance: the dictionary gate slice green (or the case rewritten against the decided semantics), a negative regression pinning the chosen behavior, and a stderr diagnostic naming the offending state.

Update 2026-07-28: the same root reds test/aot-wid-suite.f standalone — three asserts expect 0, get 67 (uncaught 7136) from plain-engine forge children doing `wordlist set-current : FOO ( -- n ) 1 ;`. Reproduced on the pre-seal baseline engine too (SHA 86ed2dec...), so it predates the seal merge. Whatever invariant this dot decides must re-green both the dictionary gate slice and test/aot-wid-suite.f.
