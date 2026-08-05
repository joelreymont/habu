---
title: Freeze the deep MATCH vector
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T01:08:48.310307+02:00"
---

Full context: the checker model parity gate now proves, in formal/Common/Control.v (a_match_at_the_deepest_frame_that_fits_certifies), that a MATCH at the deepest frame stack that still fits CERTIFIES rather than merely escaping the depth guard. The body has to be row-neutral to sit inside a loop, so its arms rebuild the value with construct: BEGIN x30, MATCH cmres cmok OF construct cmres cmok ENDOF cmerr OF construct cmres cmerr ENDOF ;MATCH, then (MK-BOOL UNTIL) x30, declared ( cmres -- cmres ). Measured through CHECK-CANDIDATE! it answers -1 at 30 opens, 0 at 31, -1 at 29 and at 0.

That pair is NOT a frozen shared vector, and it should be. The obstacle is one number: test/compiler/checker-model-schema.f builds a vector's Habu text with the shared string builder in lib/string.f, whose SB-CAP is 1024 bytes, and the certifying body is about 1.1 KB because every closer has to name a package-qualified word. Raising SB-CAP is a change to a shared library buffer with its own callers to think about, so it does not belong inside a model leaf.

What to do: decide whether SB-CAP grows or whether the schema file gets its own larger builder for vector text, then add the two rows (certifying at MATCH-DEPTH-MAX, refused one frame deeper) beside the existing CMV15/CMV16 depth rows, using the same derived MATCH-DEPTH-MAX constant. Falsify by lowering the shipped MATCH depth guard: the certifying row must go red.
