---
title: "Shrink TRUSTED: to real boundaries"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:27:22.486239+02:00"
---

Joel 2026-10-31: shrink TRUSTED: after the T0 release, not before. Today: 102 TRUSTED: definitions in src (27 files; src/lib/xml/c2.f 31, checker.f 8, verify-source.f 7, include.f 7), 16 in tools, 69 in test. docs/forth.md says never add TRUSTED: to bypass a missing checker model and keeps it for bodies the checker cannot model (evaluate, metaprogramming) behind a named, tested boundary; many uses are plain variable reads (checker.f FIELD-PROJ-NAME$) or xt fetches. Census every use; give each the checked form or the checker the model it lacks at its owner; keep only the uncheckable ones. t0.f TYPE-ENTRY/XT-STORE-ENTRY/DOES-PATCH-ENTRY duplicate elaborate.f's: one shared copy or none.

Batch 1 landed (habu-shrink-trusted-to-f4e84345, eight commits ending "Row the checker words the owner calls"): 75 TRUSTED: definitions became checked : or CAST: definitions; TRUSTED: definitions in src 102 -> 55, tools 16 -> 5, test 69 -> 52. The tools sites were the 11 REC-* writers in tools/native-unit-object.f.

The xt feature landed (habu-let-signatures-name-5931014b, habu-check-the-primitive-4b61908e): a signature names a ticked primitive's type as xt, so the tick-only entry words became checked. TRUSTED: definitions in src 55 -> 47 (elaborate.f 4 -> 0, t0.f 3 -> 0, select-x64.f 1 -> 0, trap.f 1 -> 0, dict.f 0 -> 1 for the one shared DOES-PATCH-ENTRY); tools 5, test 52 unchanged.

The checker verdict words landed (habu-publish-json-diags-116b6142): REPORT-UNCHECKABLE, MULTI-ERR-BEGIN and MULTI-ERR-END moved into checker.f: the TRUSTED: CHECK-BODY copy is deleted and MULTI-ERR-BEGIN is checked. TRUSTED: definitions in src 47 -> 45 (verify-source.f 7 -> 6, cell-effects.f 4 -> 3).

The C2 disposer landed (habu-check-the-c2-99fd69f8, on habu-pass-a-fresh-bb992e31): ALLOC-DISPOSE is checked through a C2-MEM callback pack. TRUSTED: definitions in src 45 -> 44 (c2-owner.f 1 -> 0); tools 5, test 52 unchanged.

Repair advice: E-UNSAFE's repair class is now `remove_unsafe_word` (src/core/render.f REPAIR-CLASS); E-CAP-TRUSTED keeps `trusted_boundary_required`.

Census on master 7160e349 (/tmp/jerry-big/trusted-census-tc{1,2,3}.md): 99 TRUSTED: definitions, src 44, tools 3, test 52. The three in tools (wasm-build.f and build-fixpoint-certify.f REWIND-CHECK-OFF, tier0-profile.f ABLATE) turn checking off with set-check: boundaries. Every test/ use the census found convertible is converted; the one checker gap left is test/stripped-quotation-subject.f:29 PRIMITIVE-XT, which as `:` still fails "at '[']' expected: [ n -- n ] actual: xt" (a tick of a primitive with several rows). In src every use is a boundary except src/lib/xml/c2.f's 8, which wait on carl's lane cown (XML:EMPTY-CURSOR), and two the lead ruled boundaries: definers.f:623 DEF-PREFIX-BIND calls CHECKER-PREFIX:BIND?, whose primitive row the checker marks trusted-only on purpose, and native-runtime.f:217 BIND-OWNER calls DECLARATIONS, which returns the checker's private declaration record. Each binds engine cells to checker-private state; giving the callees checked rows would hand that state to every checked caller. Once c2.f's 8 go, the shrink is at its floor. Not designed: what replaces the checker's trusted-only primitive rows (PRIM-TRUSTED-ONLY!, 13 marks at checker.f 11458-11798 plus the rows PRIM-SPEC flags at 11371) when TRUSTED: retires, since those marks are the trust boundary itself.

The shrink is at its floor. cown landed (792a7a10), and src/lib/xml/c2.f's 8 were each measured (habu-check-the-xml-a8c68880): every checked form is refused (E-CAST-LINEAR, E-LINEAR-OWNER, E-CAST-SCOPE, exclusive copy, E-CAP-TRUSTED), and lifting the refusal needs a public word that would let any package forge an XML reader or alias an exclusive view. They stay as boundaries with DEF-PREFIX-BIND and BIND-OWNER. habu-check-a-cast-86464052 closed as dropped by the cast ruling. What is left here: habu-keep-renderer-working-c5d7b8e6, test/stripped-quotation-subject.f:29 PRIMITIVE-XT, and the trusted-only primitive rows' replacement when TRUSTED: retires.
