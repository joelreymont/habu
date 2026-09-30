---
title: Share reopen name resolution authority
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T17:25:37.545455+02:00"
---

Leaf (a) of the reopen-name-resolution parent, and the only one that changes how names are resolved. Read the parent for the full soundness statement and the two-file reproducer.

The defect in one sentence: when a package defines a tail that case-insensitively shadows a core primitive, a body compiled in ANOTHER file that reopens that package resolves a bare reference to that name one way in the checker and the other way in the compiler, so a definition certifies against the core primitive and then executes the package word. VEC:@ is the production instance; the observed result was a certified program dying with SIGSEGV, exit 134.

DECIDED 2026-07-27 (orchestrator): the compiler's binding is the semantic. Where the compiler binds the package word, the package word is the right answer - it matches what a same-file body after the definition already sees, and it is what a reader of a reopened package expects. The checker does not get its own opinion; it must certify the exact binding the compiler will use.

Owned result: one shared resolution authority. The checker consults the identical lookup the compiler uses on the reopen path, rather than performing a parallel lookup that happens to agree most of the time. This is the whole point of the leaf: two lookups that are kept in sync by care will drift again, so the fix is that there is only one lookup. Forbidden: a special case for the reopen path, a list of known-shadowing names, a rule keyed on whether the tail spells a core primitive, or any repair that leaves two lookup implementations in the tree.

On the reproducer: after this leaf, the two-file fixture in the parent must bind consistently - the checker certifies against the package word, and the compiler binds the package word, so the program's behavior matches its certificate. If a program is reached where consistent binding is genuinely impossible, it must REJECT at check time with a named diagnostic; what must never survive is certifying one binding and executing another.

Acceptance: the parent's two-file fixture certifies and runs with the package word's meaning, proven through the real CHECK! and bin/hb path; a reader can point at one place in the source where reopen-path name resolution happens, and that place is shared by checker and compiler; the checker, package, and vector suites stay green; both diff lints pass.

Progress (heron, 2026-10-01): "Bind shadowed spellings to their scope" makes the checker, the tier-0 JIT and tier 1 bind scope before spelling, so the parent's fixture now rejects at check time (E-MISMATCH at `@`) instead of certifying a crash, pinned red-on-base by test/reopen-binding.f at both tiers. An independent review found it sound on every probed path, but the acceptance's one-place clause is not met: three walks (checker.f CHECKER-BIND, habu1.f LFIND with habu2.f C-SCOPED-SKIP, dict.f SPELL-REC) keep the same order by care. Remaining: one resolver, the engine's asm `scope-find` (LSCOPEREC) consumed by tier 0, tier 1, outer.f and the checker's live path. It is in progress as change xonwltsq and lands in two stages per docs/bootstrap.md. Two parts are still open. First, the replay path (REPLAY-BIND, under mirror authority) still walks the checker's own records. Second, the checker's spelling rules (RECORD-AT-STEP?, FIELD-PROJ-STEP? and others) must identify the engine word they model structurally, because `record-at`, `field-project`, `true` and `false` are global but not boot-seeded, so a seeded-only gate would switch their typing off. A Fable design pass covers both.
