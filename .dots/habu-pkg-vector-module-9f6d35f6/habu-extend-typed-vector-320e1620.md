---
title: Extend typed vector API for retirement
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:47:57.169606+02:00"
blocks:
  - habu-own-nominal-linear-491d11e4
---

Design leaf, runs FIRST (the checkpoint proved the original ordering inverted: 39 of 61 raw references need operations the typed API lacks, so the surface grows before the migration). Four additions to package VEC in lib/vector.f, all inside the already-packaged region (gate-clean), all surviving the retirement - this is typed API growth, not a RAW bridge, consistent with the standing ruling. FROZEN SHAPES: (1) a VEC:VECTOR declaring word owning the header layout (callers stop hand-rolling create NAME <header> cells allot; the header-size constant stays private) - retires the 21 VEC-HEADER-CELLS sites; (2) a typed item-count-bounded iterator (the fix the in-tree residual at maki/sched-key.f:444-448 names) so ?do-bound sites never launder a count to raw n, plus migration sites that STORE counts adopt the CAD-NUM:item-count role at their own definitions - no public count-to-n projection, that laundering is what the checker rightly refuses; (3) VEC:MAX-CELLS public capacity ceiling (2 sites); (4) VEC:SORT! - in-place sort keeping the raw backing pointer inside the package (privately calls SORT:SORT! with the same comparator interface) - retires the VEC-DATA@ sort sites; the liveness-probe site converts to VEC:CAP@ 0= with no new surface (measured: CAP@ returns 0 on a dead vector). Each addition gets its own positive and hostile fixtures in lib/vector-test.f's typed section; T{ }T per word; both diff lints. Acceptance: the four shapes usable from a foreign package for exactly the blocked reference classes (prove with one representative conversion per class, not committed to the caller files - those belong to the migration leaf).

Amended (fifth addition, 2026-07-26 late): VEC:FIND joins this leaf - a searching iterator with early exit (predicate quotation in, index-or-refusal role out; exact row frozen against SK-FIND's measured need), because a search that stops on a hit is a real operation the API owns and the migration leaf may not mint surface. Four of five additions are delivered as bb9ca422; FIND is delivered as a follow-on commit in the same lane with the same fixture discipline.

DELIVERED IN-LANE (not on master): lane commit bb9ca422 "Extend typed vector API for migration" carries the first four shapes, lane commit deaf32d0 "Register VEC:VECTOR in ownership gate" registers the declaring word with the ownership gate, lane commit ab06d9a6 "Add typed vector search with refusal arm" adds VEC:FIND, lane commit 06695000 "Revise typed vector extension per review" carries the review revision, and lane commit bc5b0131 "Add missing vector manifest rows" adds the four lib/std.manifest rows the extension first published without (VEC:VECTOR, VEC:EACH-INDEX, VEC:FIND, VEC:MAX-CELLS), sourced verbatim from the public-signatures scanner's own emission. None of these commits is reachable from master; they live only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

PARKED 2026-07-27. The vector lane is stopped at a clean boundary and this
contract is not dispatchable. Two independent destruction reviews rejected the
work it rests on. The six-blocker vector verdict (blackboard message
20260727-155303.315-codex-9253 on channel habu-extend-typed-vector-320e1620)
found that the public typed interface still takes a bare pointer, so arbitrary
byte storage is accepted as a vector header and no vector owner or element
identity exists; that disposal clears capacity and length before a fallible
release, so a refused unmap makes retry a no-op and leaks the mapping; and that
the closed-predicate premise behind the typed search is false. The seven-blocker
interner verdict (blackboard message 20260727-154724.143-codex-da26 on channel
habu-pkg-intern-lint-e735c0f6) found that the chunk append copies and advances
before it reserves, that lazy initialization is non-recoverable, that the fault
tests do not prove allocator failure, and that chunk ownership is erased into
three independent vectors with no rollback or disposal lifecycle. Any lane
commit named above is preserved as rejected evidence in
.jj-ws/habu-pkg-vecmem; none of it is work to resume. This dot now blocks on
habu-own-nominal-linear-491d11e4, the design parent that has to freeze the
nominal linear vector owner first, and it may not be re-dispatched until that
design review is clean.

Claim: RELEASED 2026-07-27 with the park above. The vecmem lane worker is released and .jj-ws/habu-pkg-vecmem is kept as rejected evidence only.
