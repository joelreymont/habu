---
title: Share reopen name resolution authority
status: closed
priority: 1
issue-type: task
created-at: "2026-07-27T17:25:37.545455+02:00"
closed-at: "2026-10-06T17:50:00+02:00"
close-reason: "One resolver: stage 2 (2d1e8965, 644827aa, merged 14987358) made scope-find the lookup for tier 0, tier 1 and outer.f, and the leaf 4 chain (master d31d4395) made it the checker's only binder, replays included; REPLAY-BIND and mirror authority deleted; INTRINSIC rename done; reopen-binding and replay-binding green; run.f 621/621, lints rc 0."
---

Leaf (a) of the reopen-name-resolution parent, and the only one that changes how names are resolved. Read the parent for the full soundness statement and the two-file reproducer.

The defect in one sentence: when a package defines a tail that case-insensitively shadows a core primitive, a body compiled in ANOTHER file that reopens that package resolves a bare reference to that name one way in the checker and the other way in the compiler, so a definition certifies against the core primitive and then executes the package word. VEC:@ is the production instance; the observed result was a certified program dying with SIGSEGV, exit 134.

DECIDED 2026-07-27 (orchestrator): the compiler's binding is the semantic. Where the compiler binds the package word, the package word is the right answer - it matches what a same-file body after the definition already sees, and it is what a reader of a reopened package expects. The checker does not get its own opinion; it must certify the exact binding the compiler will use.

Owned result: one shared resolution authority. The checker consults the identical lookup the compiler uses on the reopen path, rather than performing a parallel lookup that happens to agree most of the time. This is the whole point of the leaf: two lookups that are kept in sync by care will drift again, so the fix is that there is only one lookup. Forbidden: a special case for the reopen path, a list of known-shadowing names, a rule keyed on whether the tail spells a core primitive, or any repair that leaves two lookup implementations in the tree.

On the reproducer: after this leaf, the two-file fixture in the parent must bind consistently - the checker certifies against the package word, and the compiler binds the package word, so the program's behavior matches its certificate. If a program is reached where consistent binding is genuinely impossible, it must REJECT at check time with a named diagnostic; what must never survive is certifying one binding and executing another.

Acceptance: the parent's two-file fixture certifies and runs with the package word's meaning, proven through the real CHECK! and bin/hb path; a reader can point at one place in the source where reopen-path name resolution happens, and that place is shared by checker and compiler; the checker, package, and vector suites stay green; both diff lints pass.

Progress (heron, 2026-10-01): "Bind shadowed spellings to their scope" makes the checker, the tier-0 JIT and tier 1 bind scope before spelling, so the parent's fixture now rejects at check time (E-MISMATCH at `@`) instead of certifying a crash, pinned red-on-base by test/reopen-binding.f at both tiers. An independent review found it sound on every probed path, but the acceptance's one-place clause is not met: three walks (checker.f CHECKER-BIND, habu1.f LFIND with habu2.f C-SCOPED-SKIP, dict.f SPELL-REC) keep the same order by care. Remaining: one resolver, the engine's asm `scope-find` (LSCOPEREC) consumed by tier 0, tier 1, outer.f and the checker's live path. It is in progress as change xonwltsq and lands in two stages per docs/bootstrap.md. Two parts are still open. First, the replay path (REPLAY-BIND, under mirror authority) still walks the checker's own records. Second, the checker's spelling rules (RECORD-AT-STEP?, FIELD-PROJ-STEP? and others) must identify the engine word they model structurally, because `record-at`, `field-project`, `true` and `false` are global but not boot-seeded, so a seeded-only gate would switch their typing off. A Fable design pass covers both.

## Rename: INTRINSIC, not BUILTIN (Joel, 2026-10-01)

Joel: "yes, rename to intrinsic after dotting the change". PRIM is an effect axiom (the trusted typed effect of a primitive at a machine-code boundary); the identity tag says which engine word a record is, so the checker's dedicated rules (cell fetch and store, raw field, definers) dispatch on identity instead of spelling. That is a compiler intrinsic, and "built-in" reads as a synonym of primitive in Forth. Acceptance: the tag family is named INTRINSIC throughout before stage 2 lands: `INTRINSIC ( n -- )`, `INTRINSIC-*` ids, `CTL-INTRINSIC`, `CTL-INTRINSIC-MASK`, `TOK-INTRINSIC`, `INTRINSIC>CTL`, `CTL>INTRINSIC`, with comments, docs/forth.md, the card and the regression tests using the same word; `rg -n 'BUILTIN' src lib tools test docs bootstrap` finds no tag-family spelling. Mechanical rename inside the stage-2 commit; the full gate covers it.

Stage 2 landed 2026-10-03 with batch 5: xonwltsq 2d1e8965 (scope-find primitive), pporuvmo 644827aa (one name lookup across tiers, F1-F5 review fixes), merged with the batch side as qtvyusmx 14987358 (LIVE-BIND/REPLAY-BIND return codes; WALK then RAISE). SCOPE-FIND-AMBIGUOUS has a cell-effects.f axiom row (mzxpvznr). Fable review ACCEPT on both commits, the fixes and the merge. A definition may reuse a name two used packages export (its own pending name binds first); bare references stay E-USING-AMBIGUOUS.

## Outcome (master d273e641)

Stage 2 (2d1e8965 "Add the scope-find primitive", 644827aa "Share one name lookup across tiers", merged as 14987358) made `scope-find` the one lookup for tier 0, tier 1 and outer.f. The leaf 4 chain, merged on master as d31d4395, settles the two parts left open above. Every id here is an ancestor of master.

The replay path:
- 9b366a82 "Bind replayed names through an engine overlay" makes LIVE-BIND, which asks the engine's scope-find, the one binder for checker replays too. A replay publishes codeless records into an engine overlay. REPLAY-BIND, CHECKER-PKG-MIRROR-AUTHORITY?, CHECKER-USED-BIND and the mirror machinery are deleted; `rg` finds none of the three names in src at d273e641.
- Seven commits make a replay define and bind as the live engine does: bf7daf62 "Make replays bind as the live engine does", 733c6ca7 (the replay-private writer), 98de6dd7 and 7c2c2750 (clause-name duplicates and twins, exports), 892f1bb9 (trusted definers' clauses), 66c853d9 (primitives as duplicates) and 41ec5077 (overlay tables that grow with the dictionary).
- a5b59c74 and 5b326c22 add the owner rows and writers this needs (habu-add-the-replay-afb2bae9).
- 36628e0e and 5124f4b5 are the refresh note and the check.f source-list fix that the chain gate needed.
- TRUSTED: stayed at 687 and trusted-only rows at 56 across the chain (5124f4b5).

The spelling rules: the checker dispatches its dedicated rules on the INTRINSIC id, for example checker.f:20040 `id INTRINSIC-RECORD-AT = IF RECORD-AT-STEP?`. `rg -n BUILTIN src lib tools test docs bootstrap` finds no tag-family spelling, only a test word named BUILTIN in test/compiler/native-local-case.f:58.

Acceptance, met on master:
- One place: src/habu/habu2.f LSCOPEREC (EMIT-SCOPE-REC), the `scope-find` leaf. It runs the tier-0 call site's own leaves, LFIND and then the used publics, in the call site's order. Every other reader goes through it: tier 1 (src/compiler/native/dict.f), outer.f, the operator-row gate C-OP-ROW-GATE, and the checker through LIVE-BIND (CHECKER-FIND-ACTIVE-SYM -> CHECKER-BIND -> CHECKER-RESOLVE:WALK -> LIVE-BIND).
- The fixture: test/reopen-binding.f (with its -aot twin) is the two-file fixture in maintained form. test/reopen-binding-lib.f defines package REOPEN-BIND with `@ ( ptr n n -- n )`, `dup` and `+`, and test/reopen-binding.f reopens it from another file. A reopened `REC 2 @` certifies and runs with the package word's meaning (RE-GET and RE-BOTH answer 7), and `REC @` with the engine word's effect is refused at check time. The parent's verbatim text no longer reaches the reopen: under a later checker rule, its library's `@ ( ptr a n -- n )` stops at E-NONPARAMETRIC-EFFECT, rc 70, measured with `bin/hb --load pkguse.f`.
- Rows, run with the d31d4395 engine on the d273e641 tree: reopen-binding, replay-binding and engine-writers all rc 0.
- On the leaf 4 head (d31d4395 description): test/run.f 621/621, error-code-lint and dot-dep-lint rc 0.
