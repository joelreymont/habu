---
title: Refuse a stepped null pointer passed through a callee
status: open
priority: 2
issue-type: task
created-at: "2026-09-19T18:48:15.577562+03:00"
---

Problem (adversarial review of 77fe4a87..fba695f8, CRITICAL, measured): `: ID ( ptr a -- ptr a ) ;  : W2 ( ptr a -- ptr a ) 8 + ID ;  : F ( -- ptr ptr n ) NULL-PTR W2 ;  F @ @` certifies and reads a cell (engine 2bb46473; the dots audit ran it again on an engine some commits behind master and it still certifies), while the one-word wrapper is refused by docs/forth.md:1461 "An offset null is a DATA-base pointer". Cause: the offset-null fence loses its mark when a stepped pointer is bound to a callee's fresh type variable, because TVK-MEET (src/core/checker.f:610) answers ANY for OFF meeting an ordinary variable. Two fixes were measured and fail. (1) OFF meeting ANY as OFF reddens 43 suites: a callee that admits a null through the input-only excuse (src/lib/vector.f vec-dispose -> VEC-RELEASE-STORAGE) persists kind NULL, and meet(OFF, NULL) = DBASE turns "this word accepts a null" into "this pointer is a null offset". (2) Adding that the excused-null variable instantiates as ANY at the call still fails: the mark lives on the pointee type variable, so stepping one `ptr a` poisons every other value of type `ptr a` in the definition; vec-dispose steps the header pointer (VEC-CAP-FIELD) and then stores a literal null into the element cell of the same type, and NP-CHECK refuses it. Offset provenance is a property of a value, not of a type variable, so the fix belongs where pointer arithmetic itself is bounded (a stepped `ptr a` is not a `ptr a` until its extent is proven). That work has no dot: habu-bound-ptr-arithmetic-8bf6b54a, which this dot named, is in neither the tracker nor its archive, and the measured branch (a10584f9, change zkymvqqp, with fixtures and a docs paragraph) and f31a24ff no longer exist. A design is needed first.

Acceptance: a design decision, filed as its own dot: how the checker records that a value was stepped by pointer arithmetic, how a stepped `ptr a` regains its type, and how vec-dispose's stepped header pointer beside a literal null still certifies. After it lands: the two-word program is refused with the DATA-base reason, the same route on an ordinary pointer certifies, and every null idiom in the tree still certifies (the full suite, not a sample).

Files: src/core/checker.f (TVK-MEET, OFF-RIDE, E-INST/E-COPY, NP-KIND-*), test/compiler/base-pointer-arith-refusals.f, docs/forth.md (the offset-null paragraph).

Verify: the design dot exists; then the fixture, three generations compared with cmp, test/run.f.

Depends: habu-bound-byte-stepping-01f99cb9 (the design at /Users/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md deletes pointer stepping from ordinary words, so nothing need record a step; this dot closes when that lane refuses its program).

Ownership: unassigned. Claim: unassigned.
