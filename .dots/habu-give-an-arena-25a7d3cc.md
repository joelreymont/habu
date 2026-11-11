---
title: Give an arena fixed-length typed arrays
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:42:59.654895+02:00"
---

Problem (Maki, tim 2026-11-11, request kept at .out/tim-arena-arrays.md): ARENA:ALLOC ( ptr ARENA:arena n -- ptr u8 n ) answers bytes; reading them as cells or reals needs a private ptr u8 -> ptr n cast in every Maki package. Callers: maki src/trim.f (each surface, curve, vertex and edge keeps a fixed run of 4 to 15 value cells, today offsets into one process-wide DYNAMIC-BUFFER DATA :82-83, PUT :176, DECLARE :213) and src/assembly.f (an instance holds pose[12], shift[3], shift-err[3] reals and 24 turn rows of four columns :108-113, :260). Needed: n zeroed elements of a stored type from a given arena, the count fixed at the allocation, indexed with a bounds check like DYNAMIC-BUFFER's i NAME, released with the arena. Gates Maki TRIM C3 and ASSEMBLY C2. Acceptance: designed (public words to Joel), then Maki's two callers hold their runs in the arena with no cast of their own. Files: src/lib/arena.f, src/core/arena.f. Verify: test/arena.f cases; Maki builds cold and warm. Depends: habu-add-arena-with-9c2919e3 landing. Ownership: carl. Claim: agent=carl workspace=none (design running: .out/briefs/design-arena-arrays.md).

## Design (Plan agent 2026-11-11, cut by the lead; probes ~/.cache/tmp/carl-arr on installed 38496dc3)

Layer: the `ARENA NAME t` definer (src/core/arena.f:28-109), which today makes runs of one. It generates two more words in the declaring section, with the element width baked as NAME-NEW bakes it:
- an allocation word `( ptr ARENA:arena n -- SPAN:span<t> )`: n zeroed elements, reach n x width bytes; a count of zero or less, or one whose bytes pass MEM-MAX-N, is E-MEM-SIZE before the arena changes; a refused mapping is E-MEM-MAP as ARENA:ALLOC's.
- an indexer `( SPAN:span<t> n -- ptr t )`: E-SPAN-RANGE unless 0 <= i and i * width + width <= reach; no division.
No dispose form, no growth, no empty run: no caller. No new definer, error code or cast in a user package.

Probed (hand-written copies of the generated words, load and tools/check.f rc 0): arr1.f elements n and r, a span stored in an ARENA record's field and fetched; arr3.f/arr4.f elements a STRUCTURE and `ptr node` (`ARENA NODE-REFS ptr node` is admitted). Corrections to the Plan's text: SPAN:MAKE refuses a layout pointee (`expected: ptr a n actual: ptr turn n`), so the generated word calls SPAN-SPAN:MAKE; `byte-view` refuses a layout pointee, so the definer also generates the reverse private cast `( ptr t -- ptr u8 )` for the indexer. A record field of `SPAN:span<ptr node>` is refused today: dot habu-let-a-field-bc04ba74.

Callers (planned, none reached from a Maki entry today): TRIM constructors maki src/trim.f:389-521 (15, 14, 7 cells), VERTEX :677, EDGE :710; ASSEMBLY STAGE-INSTANCE src/assembly.f:1488 (pose 12, shift 3, turn rows 24); OPS reference lists (tim 2026-11-11: a count and a fixed array of node pointers per node, replacing REFS, ops.f:1615).

Not verified: a refusal while the generated words are evaluated, after NAME-NEW is defined, rolls back; load time of two more generated words on each of Maki's ~70 record declarations; index cost against `i NAME` (inferred: one call, three compares, two multiplies).

Open with Joel: the two words' names; whether the indexer is generated per declaration (as every typed buffer's `i NAME` is) or spans become indexable by element in the language (src/lib/span.f:75-80: checked code has no element size for a type parameter).
