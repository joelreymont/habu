---
title: Turn deliberate cast sites into declared casts
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:57:00.773557+03:00"
---

Problem: 99 TRUSTED: sites under test/ state an effect their body does not have on purpose (a body `;` declared ( n -- ptr u8 ), a body `0` declared ( -- fresh-region-a-a ), a body `n` declared ( -- [ n -- n ] )): they are unchecked casts, not helpers, and a sweep cannot convert them (sweep lane, 2026-09-16). The tree has a declared cast form (CAST: in lib/string.f, docs/forth.md), audited by name. Acceptance: every cast site becomes a CAST: declaration (or the CAST: form is extended to the shapes the tests need, with the checker rule stated), each with a one-line reason; sites that turn out to be lies about a working word become checked definitions; per-suite case counts unchanged; the count of unchecked casts is reported per file. Files: the test files the sweep lane listed, src/core/checker.f if CAST: needs a shape, docs/forth.md. Verify: the affected suites; test/run.f. Depends: none. Ownership: test tree. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

## Design facts and leaves (2026-10-01 plan)

CAST: today (checker.f:11645-11657, docs/forth.md:516-523, 709-712, 757): one input and one output term (E-CAST-ARITY), both single retype-eligible machine cells (E-CAST-CLASS refuses a pointer operand), no linear content (E-CAST-LINEAR), family output only in its owner (E-CAST-OWNER). So `( n -- ptr u8 )`, `( n -- [ -- n ] )`, `( -- matrix<...> ) 0`, `( ptr a -- JR:reader )` are not expressible: the extension branch of this dot's acceptance is required for most cast sites (src 58 incl. checker-owner.f AS-* x19 and checker.f ARENA-RC>PTR; tools xt/pointer casts; test ~90 incl. rigid-region and engine-suite phantom makers). Linear tokens (EDIT:editor, JR:reader, XML:source, own, mtok) stay E-CAST-LINEAR by design: those sites are mint/state/consume leaves of their library, redesigned per library, not swept.
B10a (Fable design, then worker-max): CAST: admits a `ptr t` and a typed-quotation destination, with the checker rule in docs/forth.md and fixtures for each admitted and refused shape.
B10b (worker-light, after B10a; workspace trusted-casts): convert the test casts, then the tools/lib pointer casts left by B1/B2; B6 in 41e973ce converts src.
