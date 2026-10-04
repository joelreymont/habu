---
title: Build UI descriptions with checked scopes
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.510451+03:00"
---

Problem: HBR2 §7.2 fixes the builder's stack contracts (BUILD, ROW, ;ROW, COLUMN, ;COLUMN, TEXT, BUTTON, FIELD, CHILD, EACH, WHEN, VIEWPORT, LAYOUT, SEMANTICS, FINISH, ABORT, DESCRIPTION-RELEASE) and rules: a runtime scope stack recording expected kind and sibling keys, bounded no-ops after a wrong closer, FINISH returning a valid description or a null one with a typed BuildError, 4 KiB strings, 256 direct children and no half-tree; §4.2 caps a component callback at 256 nodes and 16 KiB copied. Acceptance: the §7.2 words except UI:SEMANTICS (deferred with the semantics record) with exactly those stack effects, in package UI; ui-build is a DEFLINEAR minted by one private TRUSTED: pair; strings and action payloads copied before return; EACH and VIEWPORT store descriptors and never iterate or touch a scene; duplicate sibling keys, a wrong or missing closer, a 257th child, a 4,097-byte span and over 16 KiB copied each yield their BuildError at FINISH; descriptions are immutable and released explicitly; §7.6's CHROME example builds as written. Files: lib/ui/build.f (new, package UI; mints E-UI-FIRST/LAST -9690..-9699 in its owning file), lib/errors.f (one comment line), lib/ui/build-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load lib/ui/build-test.f: each BuildError by code, with no description published; a ui-build used after FINISH or ABORT is a checker refusal; releasing a description returns every node to its pool; bin/hb --load test/run.f. Depends: habu-declare-ui-tokens-b291d0ae, habu-pool-and-reclaim-a0f574a7. Ownership: lib/ui/build.f. Lane: tim. Claim: unassigned.
