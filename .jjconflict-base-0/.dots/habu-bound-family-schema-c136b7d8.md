---
title: Bound family schema walk cost
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T18:05:03.401436+02:00"
---

Measured on the round-six family-schema stack (doubling pointer-linked family chain, engine-startup baseline subtracted): the shared pure walk re-scans applied families without memoization, so traversal is Theta(2^depth) on a valid acyclic DAG - per-level ratio converges to 2.0; depth 21 costs 3 seconds, depth 30 extrapolates ~25 minutes. A legal declaration can be made to take arbitrarily long, a soundness-adjacent property the query owns and does not fix. Methodological note (recorded so nobody repeats it): a width-doubling chain dies 76 before cost shows; pointer links keep width at 2 cells and isolate the branching factor. DESIGN DECISION REQUIRED before any fix - the walk is contractually pure and allocation-free (no visited arena; the round-3/4 rejections govern), so admissible shapes are: (a) accept the cost with a measured depth bound and a named refusal at the bound; (b) a caller-owned bounded workspace threaded like the existing self/under-pointer parameters (changes the walk signature, stays state-free between calls); (c) registry-level memoization owned by the registry (a committed-family linearity cache invalidated by the registry's own writers - authority-owned, not query-owned); or (d) a checker-capability ruling that declaration-graph depth is already bounded small by other limits (measure the actual bound if claimed). The decision belongs to the checker/registry design authority; no implementation before the ruling and no normalization of the exponential in the meantime. Reproduction: the doubling-chain harness from the round-six report (recipe file names it), engine-startup baseline subtracted, per-level ratio reported.

DESIGN FREEZE DEMANDED 2026-07-27, blackboard message
20260727-162245.326-codex-cd92 on channel habu-validate-family-schema-34f9e1de.
Shape (c) is not approved as a label. "Registry memoization" names a place to
put a cache; it does not say what the cache means, and until it does there is
nothing to review. Before any implementation, freeze all six of these for it:

- Authority. Which component is allowed to write the cache, and on what event.
  A cache the query can write is the query owning state it is contractually
  forbidden to own.
- Invalidation. What makes an entry wrong, and what erases it at exactly that
  moment rather than later.
- Rollback. What happens to cached entries when a declaration transaction is
  rolled back, so a family that never committed cannot leave a linearity answer
  behind.
- Persistence. Whether entries survive an image or snapshot boundary, and if
  they do, what proves the cached answer still describes the same family.
- Provisional-family behavior. What the cache says about a family that is
  declared but not yet closed, which is precisely the window where a wrong
  cached answer would be invisible.
- Parameter environment. How an entry is keyed when the answer depends on
  application arguments, which is where this dot meets
  habu-substitute-app-args-f1185099 - substitution deepens the walk and changes
  what "the linearity of this family" even means as a cache key.

The frozen shape must then be compared, in the same design note, against the
two cheaper alternatives the same message names: a caller-owned finite work set,
and an immutable per-family summary computed once at close. Either may turn out
to dominate memoization, and the comparison is what makes the choice reviewable
rather than assumed.

The decision comes through that design note under review - the smallest complete
dependency tree returned by the design model - and not through this dot. No
implementation until that review is clean, and no local guard or measured cap
lands in the meantime as a way of normalizing the exponential.
