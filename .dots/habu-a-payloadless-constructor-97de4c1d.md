---
title: A payloadless constructor bound to a wide-payload typed local is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T14:12:14.815066+02:00"
---

Found reviewing habu-checker-refuses-a-c59fdc2d's fix (ywvvtkls). Problem: ': VB ( -- n ) OPTION:NONE {: r:option<pr> :} 0 ;' (pr a 2-cell STRUCTURE) is refused at ':}' with 'actual: option<a>', on master too, while the same bind is accepted when the signature names option<pr>. Cause (hunt, instrumented engine, probes /tmp/carl-mhunt/p): a generated constructor takes its family args only from the definition's declared output: CONSTRUCT-DECL-TERM (src/core/checker.f:4528) scans SGOUT and CONSTRUCT-DECL-LAYOUT (:4557) feeds TFAM-CTOR-STEP? (src/core/type-family.f:5592); seeded, OPTION:NONE pushes option<pr>'s 3 hidden cells plus a pad (TFC-CONSTRUCT-STEP-VID :5528); unseeded it is one logical option<a> of the 2 cells it declares at run time (:5471). The bind's trial UNIFY (LOC-AS-BUNDLE?) then cannot ground a with the 2-cell pr (PARAM-BIND-OK? :3453, LAYOUT-BLOCK? :3225), so the refusal is sound: the value is 2 cells and the local wants 3. Acceptance: a payloadless constructor's value binds to a typed local whose annotation names a wide payload, with the right cells at run time; the approach is a design for Joel (the annotation is read after the constructor token). Files: src/core/checker.f, src/core/type-family.f, test/wide-typed-local-probe.f. Verify: VB and VK shapes accepted and run; wide-typed-local-probe; full suite (checker). Depends: habu-checker-refuses-a-c59fdc2d. Ownership: carl. Claim: unassigned.
