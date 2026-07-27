---
title: Make vector sort safe and reentrant
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-27T08:33:54.503626+02:00\""
blocks:
  - habu-own-nominal-linear-491d11e4
---

Prerequisite split from the vector extension (final-reject third facet, measured): lib/sort.f keeps heap state in package globals, so a valid comparator that sorts another vector corrupts the outer sort (reentrancy), and a comparator that disposes the vector under sort leaves SORT:SORT! writing through a stale pointer (SIGSEGV, production probe exit 134). Required long-term shape per the frozen review: caller-owned reentrant sort state (no package-global heap workspace) plus structural exclusive-mutation protection across the full comparator extent with exception-safe release, then VEC:SORT! rebuilt on it. Hostile proofs required: dispose, resize, push, store, nested-sort, and comparator-throw during sort - each rejecting or safe, mutation-verified. Purity-by-documentation is forbidden; the language does not enforce purity. The two dot-dep-lint-core sort sites stay on the raw form (legal in their packaged file) citing this dot until it lands. Owner: lib/sort.f and package VEC. Dependencies: design intersects the exclusive-mutation question the linear-scope capability answers - freeze against it if the timing aligns, or design standalone exclusion if not (decided at freeze).

FREEZE DECISION (2026-07-27, orchestrator): standalone exclusion. The linear-scope checker capability (transaction plan pillar B) is unstarted and behind the hard cutover, so the timing does not align; do not wait on it and do not design against its unfrozen interface. Required shape: caller-owned sort state (workspace passed or stack-allocated per sort, zero package-global heap state), plus a runtime exclusive-mutation seal on the vector under sort - every VEC mutator (dispose, resize, push, store, clear, nested SORT!) consults the seal and refuses with a typed error while sealed; the seal releases exception-safely when the comparator throws (catch, release, rethrow - the sort must not swallow the comparator's error). When the linear-scope capability lands later it ADDS static enforcement above this runtime seal; nothing here is throwaway. The two dot-dep-lint-core raw sort sites convert onto the new VEC:SORT! in this same leaf and the citation comments come out - after this lands the retirement leaf's tree-wide zero-raw-reference sweep becomes satisfiable.

DELIVERED IN-LANE (not on master): the reentrant caller-owned heapsort landed as lane commit e18ffb14 "Make heapsort reentrant", and the exclusive-mutation seal plus the rebuilt VEC:SORT! landed as lane commit f2c3b29f "Retire the raw vector surface and seal sorts" (with lane commit b48a5737 "Add scoped mapping trace to mem-fault" supplying the memory-fault trace seam the seal tests use). None of these commits is reachable from master; they live only in the vecmem lane workspace .jj-ws/habu-pkg-vecmem.

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
