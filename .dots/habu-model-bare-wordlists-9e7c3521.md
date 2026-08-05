---
title: Model bare wordlists in package context
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.911570+02:00"
---

Full context: test/gate-dictionary-lib.f:214 (hb long dictionary wordlist isolation) creates a plain wordlist, set-current to it, and compiles a checked definition; on the proofs branch the definition path throws 7136 E-PKG-CONTEXT (src/core/checker.f:527) with empty stdout/stderr, because CHECKER-PKG-CONTEXT's live provider only recognizes global scope or a real package record's public/private pair, and a bare user wordlist matches neither. Decide the invariant: either the checker's package-context model gains a legitimate anonymous-wordlist state (definitions allowed, no package authority minted), or raw set-current to a non-package wordlist is formally rejected with a named diagnostic and the ANS test migrates to packages. Silent 7136 with no diagnostic is wrong either way. Acceptance: the dictionary gate slice green (or the case rewritten against the decided semantics), a negative regression pinning the chosen behavior, and a stderr diagnostic naming the offending state.

Update 2026-07-28: the same root reds test/aot-wid-suite.f standalone — three asserts expect 0, get 67 (uncaught 7136) from plain-engine forge children doing `wordlist set-current : FOO ( -- n ) 1 ;`. Reproduced on the pre-seal baseline engine too (SHA 86ed2dec...), so it predates the seal merge. Whatever invariant this dot decides must re-green both the dictionary gate slice and test/aot-wid-suite.f.

Update 2026-07-29, from investigation dot habu-attr-three-unowned-3e144928: confirmed again on the proofs base with the current bin/hb, and the escaping throw is now pinned by mutation rather than by inspection. This dot owns the aot-wid-restore gate phase; no new dot was minted for it.

Which assertions. The three red ones in test/aot-wid-suite.f are numbers 25, 27 and 29, all the `RC @ 0 T=` leg of ASSERT-OK (test/aot-wid-suite.f:146): number 25 is the variant engine hb-pwid running `wordlist set-current : FOO ( -- n ) 1 ;`, number 27 is the shipped bin/hb running `300 set-current : FOO ( -- n ) 1 ;`, number 29 is the shipped bin/hb running `70000 set-current : FOO ( -- n ) 1 ;`. They report an empty label because T-LABEL is consumed by the preceding assertion in the same pair, which is why the phase looked label-less.

Minimal reproducer, no test harness needed: put `300 set-current : FOO ( -- n ) 1 ;` in a file and run `bin/hb --load` on it. Stdout is empty, stderr is exactly `hb: uncaught throw code 7136`, exit status 67 (UNCAUGHT-RC, src/habu/layout.f:166). The same holds for the `wordlist` and `70000` variants.

Which throw escapes. It is the one at src/core/checker.f:634, inside CHECKER-PKG-CONTEXT. Proof by mutation on the real load path: copy src/ to a private root, change only that site from `E-PKG-CONTEXT throw` to `7911 throw` (padded to the same length), and boot a child engine with that root as its working directory, since the engine re-reads its prefix from source at boot. Stderr becomes `hb: uncaught throw code 7911`, so none of the other eight E-PKG-CONTEXT throw sites (src/core/checker.f lines 653, 654, 658, 659, 665, 666, 668, 679, all inside the private verifier scope) is involved.

Why the context provider returns false. CHECKER-PKG-CONTEXT calls PKG-LIVE-XT, which after src/habu/xref.f loads is bound to LIVE (src/habu/xref.f:200) and thence LIVE-PKG (src/habu/xref.f:171-199). LIVE reads the package record cell, the public word-list cell, the private word-list cell and get-current. With no package open the first three are zero but get-current is the user word-list, so the all-zero global tuple test at src/habu/xref.f:174-176 fails and the very next branch rejects, returning false. That is the exact code path this dot has to model or formally reject.

Static invariant to record whichever way the semantics go: the checked load path must decide the package context of a definition at check time and name the offending state; a bare numeric 7136 reaching the top-level interpreter as an uncaught throw is a diagnostic gap on its own, independent of the modelling decision.

Related: dot habu-restore-fail-closed-4f1d6375 suspects the same CHECKER-PKG-CONTEXT plumbing behind test/engine-error-package.f getting 67 where it expects 70. The two should be reviewed together, since a fix that gives 7136 a named fail-closed exit may re-green both.

Update 2026-07-30 (from the fail-closed fix, commit 0a5b92d6): the reproducer
`300 set-current : FOO ( -- n ) 1 ;` now exits with the NAMED fail-closed 70
and the stderr line "hb: no authenticated package context for this
definition", instead of the bare uncaught 67. That discharges this dot's
"stderr diagnostic naming the offending state" clause; the remaining work here
is only the SEMANTIC decision (legitimate anonymous-wordlist state versus
formal rejection) and the regression pinning it. The refusal surface stays
CHECKER-PKG-CONTEXT-REJECT in src/core/checker.f, single authority.
